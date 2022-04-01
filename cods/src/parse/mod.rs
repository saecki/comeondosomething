use std::cmp;
use std::rc::Rc;

use crate::util::array_of;
use crate::{
    Ast, AstT, Block, CRange, CondBlock, Context, ForLoop, Fun, Group, IdentRange, IfExpr, Item,
    Kw, KwT, OpT, ParKind, Param, PctT,
};

pub use builtin::*;
pub use op::*;
use parser::*;

mod builtin;
mod op;
mod parser;
#[cfg(test)]
mod test;

impl Context {
    pub fn parse(&mut self, items: Vec<Item>) -> crate::Result<Vec<Ast>> {
        let range = items_range(&items).unwrap_or(CRange::of(0, 0));
        self.parse_items(items, range)
    }

    fn parse_items(&mut self, items: Vec<Item>, mut range: CRange) -> crate::Result<Vec<Ast>> {
        let mut asts: Vec<Ast> = Vec::new();

        let mut parser = Parser::new(items);
        while parser.peek().is_some() {
            if let Some(a) = asts.last() {
                range.start = a.range.end;
            };

            let ast = self.parse_bp(&mut parser, 0, range, false)?;
            parser.eat_semi();

            if !ast.is_empty() {
                asts.push(ast);
            }
        }

        Ok(asts)
    }

    fn parse_one_bp(
        &mut self,
        parser: &mut Parser,
        min_bp: u8,
        range: CRange,
    ) -> crate::Result<Ast> {
        let ast = self.parse_bp(parser, min_bp, range, false)?;
        while let Some(i) = parser.next() {
            if !i.is_newln() {
                return Err(crate::Error::UnexpectedItem(i));
            }
        }
        Ok(ast)
    }

    fn parse_bp(
        &mut self,
        parser: &mut Parser,
        min_bp: u8,
        range: CRange,
        cond: bool,
    ) -> crate::Result<Ast> {
        let mut lhs = match parser.peek() {
            Some(Item::Group(_)) => {
                let g = parser.next().unwrap().into_group().unwrap();
                match g.par_kind {
                    ParKind::Round => {
                        let mut group_parser = Parser::new(g.items);
                        let mut ast = self.parse_one_bp(&mut group_parser, 0, g.range)?;
                        ast.range = g.range;
                        ast
                    }
                    ParKind::Curly => {
                        let b = self.parse_block(g)?;
                        Ast::new(AstT::Block(b.asts), b.range)
                    }
                    ParKind::Square => {
                        self.errors.push(crate::Error::NotImplemented(
                            "Arrays are not yet implemented",
                            g.range,
                        ));
                        return Ok(Ast::new(AstT::Error, range));
                    }
                }
            }
            Some(Item::Expr(_)) => {
                let e = parser.next().unwrap().into_expr().unwrap();
                let r = e.range;
                Ast::new(AstT::Expr(e), r)
            }
            Some(&Item::Op(o)) => {
                parser.next();

                let (prefix, r_bp) = match o.prefix_bp() {
                    Some(p) => p,
                    None => return Err(crate::Error::UnexpectedOperator(o)),
                };

                if parser.peek().is_none() {
                    let r = o.range.after();
                    return Err(crate::Error::MissingOperand(r));
                }

                let val_r = CRange::of(o.range.end, range.end);
                let val = self.parse_bp(parser, r_bp, val_r, cond)?;
                let ast_r = CRange::span(o.range, val.range);
                let ast = match prefix {
                    Prefix::UnaryPlus => val.typ,
                    Prefix::UnaryMinus => AstT::Neg(Box::new(val)),
                    Prefix::Not => AstT::Not(Box::new(val)),
                };

                Ast::new(ast, ast_r)
            }
            Some(&Item::Pct(s)) => match s.typ {
                PctT::Comma | PctT::Colon | PctT::Arrow => {
                    let i = parser.next().unwrap();
                    self.errors.push(crate::Error::UnexpectedItem(i));
                    return Ok(Ast::new(AstT::Error, range));
                }
                PctT::Semi | PctT::Newln => {
                    let r = CRange::of(range.start, s.range.end);
                    return Ok(Ast::new(AstT::Empty, r));
                }
            },
            Some(&Item::Kw(k)) => {
                parser.next();
                let r = CRange::span(k.range, range);
                self.parse_lang_construct(parser, k, r)?
            }
            None => return Ok(Ast::new(AstT::Empty, range)),
        };

        loop {
            parser.eat_newlns();
            let newln = parser.current_newln;
            let i = match parser.peek() {
                Some(i) => i,
                None => break,
            };

            let op = match i {
                Item::Group(g) => {
                    if newln {
                        break;
                    }
                    // stop if parsing an if condition
                    // TODO: cleanup
                    if cond && g.par_kind.is_curly() {
                        break;
                    }

                    if g.par_kind.is_round() {
                        if let Some(id) = lhs.as_ident() {
                            let g = parser.next().unwrap().into_group().unwrap();
                            lhs = self.parse_fun_call(id, g)?;
                            continue;
                        }
                    }

                    let r = CRange::between(lhs.range, g.range);
                    return Err(crate::Error::MissingOperator(r));
                }
                Item::Expr(e) => {
                    if newln {
                        break;
                    }

                    let r = CRange::between(lhs.range, e.range);
                    return Err(crate::Error::MissingOperator(r));
                }
                &Item::Op(o) => o,
                &Item::Pct(s) => match s.typ {
                    PctT::Comma | PctT::Colon | PctT::Arrow => {
                        let i = parser.next().unwrap();
                        return Err(crate::Error::UnexpectedItem(i));
                    }
                    PctT::Semi | PctT::Newln => break,
                },
                Item::Kw(k) => {
                    if newln {
                        break;
                    }

                    let r = CRange::between(lhs.range, k.range);
                    return Err(crate::Error::MissingOperator(r));
                }
            };

            if let Some((l_bp, postfix)) = op.postfix_bp() {
                if l_bp < min_bp {
                    break;
                }
                parser.next();

                let val_r = CRange::span(lhs.range, op.range);
                let val = match postfix {
                    Postfix::Factorial => AstT::Factorial(Box::new(lhs)),
                };

                lhs = Ast::new(val, val_r);
                continue;
            }

            let (l_bp, infix, r_bp) = match op.infix_bp() {
                Some(bp) => bp,
                None => {
                    let r = CRange::between(lhs.range, op.range);
                    return Err(crate::Error::MissingOperator(r));
                }
            };
            if l_bp < min_bp {
                break;
            } else {
                parser.next();
            }

            let rhs_r = CRange::of(op.range.end, range.end);
            let rhs = self.parse_bp(parser, r_bp, rhs_r, cond)?;

            if let AstT::Empty = rhs.typ {
                let r = op.range.after();
                return Err(crate::Error::MissingOperand(r));
            }

            let val_r = CRange::span(lhs.range, rhs.range);
            let val = match infix {
                Infix::Assign => match lhs.as_ident() {
                    Some(id) => AstT::Assign(id, Box::new(rhs)),
                    None => return Err(crate::Error::InvalidAssignment(lhs.range, op.range)),
                },
                Infix::AddAssign => match lhs.as_ident() {
                    Some(id) => AstT::AddAssign(id, Box::new(rhs)),
                    None => return Err(crate::Error::InvalidAssignment(lhs.range, op.range)),
                },
                Infix::SubAssign => match lhs.as_ident() {
                    Some(id) => AstT::SubAssign(id, Box::new(rhs)),
                    None => return Err(crate::Error::InvalidAssignment(lhs.range, op.range)),
                },
                Infix::MulAssign => match lhs.as_ident() {
                    Some(id) => AstT::MulAssign(id, Box::new(rhs)),
                    None => return Err(crate::Error::InvalidAssignment(lhs.range, op.range)),
                },
                Infix::DivAssign => match lhs.as_ident() {
                    Some(id) => AstT::DivAssign(id, Box::new(rhs)),
                    None => return Err(crate::Error::InvalidAssignment(lhs.range, op.range)),
                },
                Infix::RangeEx => AstT::RangeEx(Box::new(lhs), Box::new(rhs)),
                Infix::RangeIn => AstT::RangeIn(Box::new(lhs), Box::new(rhs)),
                Infix::Add => AstT::Add(Box::new(lhs), Box::new(rhs)),
                Infix::Sub => AstT::Sub(Box::new(lhs), Box::new(rhs)),
                Infix::Mul => AstT::Mul(Box::new(lhs), Box::new(rhs)),
                Infix::Div => AstT::Div(Box::new(lhs), Box::new(rhs)),
                Infix::IntDiv => AstT::IntDiv(Box::new(lhs), Box::new(rhs)),
                Infix::Rem => AstT::Rem(Box::new(lhs), Box::new(rhs)),
                Infix::Pow => AstT::Pow(Box::new(lhs), Box::new(rhs)),
                Infix::Eq => AstT::Eq(Box::new(lhs), Box::new(rhs)),
                Infix::Ne => AstT::Ne(Box::new(lhs), Box::new(rhs)),
                Infix::Lt => AstT::Lt(Box::new(lhs), Box::new(rhs)),
                Infix::Le => AstT::Le(Box::new(lhs), Box::new(rhs)),
                Infix::Gt => AstT::Gt(Box::new(lhs), Box::new(rhs)),
                Infix::Ge => AstT::Ge(Box::new(lhs), Box::new(rhs)),
                Infix::Or => AstT::Or(Box::new(lhs), Box::new(rhs)),
                Infix::And => AstT::And(Box::new(lhs), Box::new(rhs)),
                Infix::BwOr => AstT::BwOr(Box::new(lhs), Box::new(rhs)),
                Infix::BwAnd => AstT::BwAnd(Box::new(lhs), Box::new(rhs)),
                Infix::Dot => {
                    return Err(crate::Error::NotImplemented(
                        "Field access or method calls are not yet implemented",
                        op.range,
                    ))
                }
            };
            lhs = Ast::new(val, val_r);
        }

        Ok(lhs)
    }

    fn parse_block(&mut self, g: Group) -> crate::Result<Block> {
        match self.parse_items(g.items, g.range) {
            Ok(asts) => Ok(Block::new(asts, g.range)),
            Err(e) => {
                self.errors.push(e);
                Ok(Block::new(vec![Ast::new(AstT::Error, g.range)], g.range))
            }
        }
    }

    fn parse_fun_call(&mut self, id: IdentRange, g: Group) -> crate::Result<Ast> {
        let b = match BuiltinFun::from(self.idents.name(id.ident)) {
            Some(b) => b,
            None => {
                let args = self.parse_dyn_fun_args(0, usize::MAX, g.items, g.range)?;
                let r = CRange::span(id.range, g.range);
                return Ok(Ast::new(AstT::FunCall(id, args), r));
            }
        };

        let f = match b {
            BuiltinFun::Pow => {
                let [base, exp] = self.parse_fun_args(g.items, g.range)?;
                AstT::Pow(Box::new(base), Box::new(exp))
            }
            BuiltinFun::Ln => {
                let [n] = self.parse_fun_args(g.items, g.range)?;
                AstT::Ln(Box::new(n))
            }
            BuiltinFun::Log => {
                let [base, n] = self.parse_fun_args(g.items, g.range)?;
                AstT::Log(Box::new(base), Box::new(n))
            }
            BuiltinFun::Sqrt => {
                let [n] = self.parse_fun_args(g.items, g.range)?;
                AstT::Sqrt(Box::new(n))
            }
            BuiltinFun::Ncr => {
                let [n, r] = self.parse_fun_args(g.items, g.range)?;
                AstT::Ncr(Box::new(n), Box::new(r))
            }
            BuiltinFun::ToDeg => {
                let [n] = self.parse_fun_args(g.items, g.range)?;
                AstT::ToDeg(Box::new(n))
            }
            BuiltinFun::ToRad => {
                let [n] = self.parse_fun_args(g.items, g.range)?;
                AstT::ToRad(Box::new(n))
            }
            BuiltinFun::Sin => {
                let [n] = self.parse_fun_args(g.items, g.range)?;
                AstT::Sin(Box::new(n))
            }
            BuiltinFun::Cos => {
                let [n] = self.parse_fun_args(g.items, g.range)?;
                AstT::Cos(Box::new(n))
            }
            BuiltinFun::Tan => {
                let [n] = self.parse_fun_args(g.items, g.range)?;
                AstT::Tan(Box::new(n))
            }
            BuiltinFun::Asin => {
                let [n] = self.parse_fun_args(g.items, g.range)?;
                AstT::Asin(Box::new(n))
            }
            BuiltinFun::Acos => {
                let [n] = self.parse_fun_args(g.items, g.range)?;
                AstT::Acos(Box::new(n))
            }
            BuiltinFun::Atan => {
                let [n] = self.parse_fun_args(g.items, g.range)?;
                AstT::Atan(Box::new(n))
            }
            BuiltinFun::Gcd => {
                let [a, b] = self.parse_fun_args(g.items, g.range)?;
                AstT::Gcd(Box::new(a), Box::new(b))
            }
            BuiltinFun::Min => {
                let args = self.parse_dyn_fun_args(2, usize::MAX, g.items, g.range)?;
                AstT::Min(args)
            }
            BuiltinFun::Max => {
                let args = self.parse_dyn_fun_args(2, usize::MAX, g.items, g.range)?;
                AstT::Max(args)
            }
            BuiltinFun::Clamp => {
                let [n, min, max] = self.parse_fun_args(g.items, g.range)?;
                AstT::Clamp(Box::new(n), Box::new(min), Box::new(max))
            }
            BuiltinFun::Print => {
                let args = self.parse_dyn_fun_args(0, usize::MAX, g.items, g.range)?;
                AstT::Print(args)
            }
            BuiltinFun::Println => {
                let args = self.parse_dyn_fun_args(0, usize::MAX, g.items, g.range)?;
                AstT::Println(args)
            }
            BuiltinFun::Spill => {
                self.parse_fun_args::<0>(g.items, g.range)?;
                AstT::Spill
            }
            BuiltinFun::Assert => {
                let [a] = self.parse_fun_args(g.items, g.range)?;
                AstT::Assert(Box::new(a))
            }
            BuiltinFun::AssertEq => {
                let [a, b] = self.parse_fun_args(g.items, g.range)?;
                AstT::AssertEq(Box::new(a), Box::new(b))
            }
        };
        let r = CRange::span(id.range, g.range);
        Ok(Ast::new(f, r))
    }

    fn parse_dyn_fun_args(
        &mut self,
        min: usize,
        max: usize,
        items: Vec<Item>,
        range: CRange,
    ) -> crate::Result<Vec<Ast>> {
        let arg_count = items.iter().filter(|i| i.is_pct()).count() + 1;
        let mut args = Vec::with_capacity(cmp::min(arg_count, max));
        let mut unexpected_args = Vec::new();
        let mut parsed_args = 0;
        let mut start = (0, range.start);
        let mut arg_items = Vec::new();

        for (i, it) in items.into_iter().enumerate() {
            if it.is_comma() {
                let r = CRange::of(start.1, it.range().start);
                if parsed_args < max {
                    let mut parser = Parser::new(arg_items.clone());
                    arg_items.clear();
                    args.push(self.parse_one_bp(&mut parser, 0, r)?);
                } else {
                    unexpected_args.push(r);
                }
                start = (i + 1, it.range().end);
                parsed_args += 1;
            } else {
                arg_items.push(it);
            }
        }

        if !arg_items.is_empty() {
            let r = CRange::of(start.1, range.end);
            if parsed_args < max {
                let mut parser = Parser::new(arg_items);
                args.push(self.parse_one_bp(&mut parser, 0, r)?);
            } else {
                unexpected_args.push(r);
            }
            parsed_args += 1;
        }

        if !unexpected_args.is_empty() {
            self.errors.push(crate::Error::UnexpectedFunArgs {
                ranges: unexpected_args,
                expected: max,
                found: parsed_args,
            });
        } else if parsed_args < min {
            let range = match args.last() {
                Some(a) => CRange::of(a.range.end, range.end),
                None => range.after(),
            };
            self.errors.push(crate::Error::MissingFunArgs {
                range,
                expected: min,
                found: parsed_args,
            });
        }

        Ok(args)
    }

    fn parse_fun_args<const COUNT: usize>(
        &mut self,
        items: Vec<Item>,
        range: CRange,
    ) -> crate::Result<[Ast; COUNT]> {
        let mut args: [Ast; COUNT] = array_of(|_| Ast::new(AstT::Error, range));
        let mut unexpected_args = Vec::new();
        let mut parsed_args = 0;
        let mut start = (0, range.start);
        let mut arg_items = Vec::new();

        for (i, it) in items.into_iter().enumerate() {
            if it.is_comma() {
                let r = CRange::of(start.1, it.range().start);
                if parsed_args < COUNT {
                    let mut parser = Parser::new(arg_items.clone());
                    arg_items.clear();
                    args[parsed_args] = self.parse_one_bp(&mut parser, 0, r)?;
                } else {
                    unexpected_args.push(r);
                }
                start = (i + 1, it.range().end);
                parsed_args += 1;
            } else {
                arg_items.push(it);
            }
        }

        if !arg_items.is_empty() {
            let r = CRange::of(start.1, range.end);
            if parsed_args < COUNT {
                let mut parser = Parser::new(arg_items);
                args[parsed_args] = self.parse_one_bp(&mut parser, 0, r)?;
            } else {
                unexpected_args.push(r);
            }
            parsed_args += 1;
        }

        if !unexpected_args.is_empty() {
            self.errors.push(crate::Error::UnexpectedFunArgs {
                ranges: unexpected_args,
                expected: COUNT,
                found: parsed_args,
            });
        } else if parsed_args < COUNT {
            let range = match args.last() {
                Some(a) => CRange::of(a.range.end, range.end),
                None => range.after(),
            };
            self.errors.push(crate::Error::MissingFunArgs {
                range,
                expected: COUNT,
                found: parsed_args,
            });
        }

        Ok(args)
    }

    fn parse_lang_construct(
        &mut self,
        parser: &mut Parser,
        kw: Kw,
        range: CRange,
    ) -> crate::Result<Ast> {
        match kw.typ {
            KwT::If => {
                let mut cases = Vec::new();
                let mut else_block = None;
                let first_case = self.parse_cond_block(parser, kw, range)?;
                let mut last_range = first_case.range;
                cases.push(first_case);

                loop {
                    match parser.peek() {
                        Some(Item::Kw(k)) if k.typ == KwT::Else => {
                            parser.next();
                        }
                        _ => break,
                    }

                    match parser.next() {
                        Some(Item::Kw(k)) if k.typ == KwT::If => {
                            let case = self.parse_cond_block(parser, k, range)?;
                            last_range = case.range;
                            cases.push(case);
                        }
                        Some(Item::Group(g)) if g.par_kind.is_curly() => {
                            else_block = Some(self.parse_block(g)?);
                            break;
                        }
                        Some(i) => return Err(crate::Error::ExpectedBlock(i.range())),
                        None => {
                            let r = kw.range.after();
                            return Err(crate::Error::ExpectedBlock(r));
                        }
                    }
                }

                let if_r = CRange::span(kw.range, last_range);
                let if_expr = IfExpr::new(cases, else_block);
                Ok(Ast::new(AstT::IfExpr(if_expr), if_r))
            }
            KwT::Else => Err(crate::Error::WrongContext(kw)),
            KwT::While => {
                let case = self.parse_cond_block(parser, kw, range)?;
                let r = case.range;
                Ok(Ast::new(AstT::WhileLoop(Box::new(case)), r))
            }
            KwT::For => {
                let ident = parser.expect_ident(kw.range.end)?;

                parser.expect_kw(KwT::In, ident.range.end)?;

                let iter = self.parse_bp(parser, 0, range, true)?;

                let group = parser.expect_block(iter.range.end)?;
                let block = self.parse_block(group)?;

                let range = CRange::span(kw.range, block.range);
                let for_loop = ForLoop::new(ident, Box::new(iter), block);
                Ok(Ast::new(AstT::ForLoop(for_loop), range))
            }
            KwT::In => Err(crate::Error::WrongContext(kw)),
            KwT::Fun => {
                let ident = parser.expect_ident(kw.range.end)?;

                let name = self.idents.name(ident.ident);
                if BuiltinFun::from(name).is_some() {
                    return Err(crate::Error::RedefinedBuiltinFun(
                        name.to_owned(),
                        ident.range,
                    ));
                }

                let param_group = parser.expect_fun_pars(ident.range.end)?;
                let params = {
                    let mut group_parser = Parser::new(param_group.items);
                    let mut params = Vec::new();
                    while let Some(i) = group_parser.next() {
                        let r = i.range();
                        let ident = i
                            .into_expr()
                            .and_then(|e| e.as_ident())
                            .ok_or(crate::Error::ExpectedIdent(r))?;

                        let colon_r = group_parser.expect_pct(PctT::Colon, ident.range.end)?;
                        let typ = group_parser.expect_ident(colon_r.end)?;

                        params.push(Param::new(ident, typ));

                        match group_parser.next() {
                            Some(i) if i.is_comma() => (),
                            Some(i) => {
                                let r = i.range().after();
                                return Err(crate::Error::ExpectedPct(PctT::Comma, r));
                            }
                            None => break,
                        }
                    }
                    params
                };

                let mut last_r = param_group.range;
                let mut return_type = None;
                if let Some(Item::Pct(p)) = parser.peek() {
                    if let PctT::Arrow = p.typ {
                        last_r = p.range;
                        parser.next();

                        return_type = Some(parser.expect_ident(last_r.end)?);
                    }
                }

                let block_group = parser.expect_block(last_r.end)?;
                let block = self.parse_block(block_group)?;

                let range = CRange::span(kw.range, block.range);
                let fun = Fun::new(ident, params, return_type, block);
                Ok(Ast::new(AstT::FunDef(Rc::new(fun)), range))
            }
            KwT::Val | KwT::Var => {
                let ident = parser.expect_ident(kw.range.end)?;

                let name = self.idents.name(ident.ident);
                if BuiltinConst::from(name).is_some() {
                    return Err(crate::Error::RedefinedBuiltinConst(
                        name.to_owned(),
                        ident.range,
                    ));
                }

                let assign_r = parser.expect_op(OpT::Assign, ident.range.end)?;

                let r = CRange::of(assign_r.end, range.end);
                let val = self.parse_bp(parser, 0, r, false)?;

                let mutable = kw.typ == KwT::Var;

                let range = CRange::span(kw.range, val.range);
                Ok(Ast::new(AstT::VarDef(ident, Box::new(val), mutable), range))
            }
        }
    }

    fn parse_cond_block(
        &mut self,
        parser: &mut Parser,
        kw: Kw,
        range: CRange,
    ) -> crate::Result<CondBlock> {
        let cond_r = CRange::of(kw.range.end, range.end);
        let cond = self.parse_bp(parser, 0, cond_r, true)?;

        let group = parser.expect_block(cond.range.end)?;
        let range = CRange::span(kw.range, group.range);
        let block = self.parse_block(group)?;

        Ok(CondBlock::new(cond, block.asts, range))
    }
}

fn items_range(items: &[Item]) -> Option<CRange> {
    let first = items.first();
    let last = items.last();

    match (first, last) {
        (Some(f), Some(l)) => Some(CRange::span(f.range(), l.range())),
        _ => None,
    }
}
