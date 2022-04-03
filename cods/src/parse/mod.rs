use std::cmp;
use std::rc::Rc;

use crate::util::array_of;
use crate::{
    Ast, AstT, Block, CondBlock, Context, ForLoop, Fun, Group, IdentSpan, IfExpr, Item, Kw, KwT,
    OpT, ParKind, Param, PctT, Span,
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
        let span = items_span(&items).unwrap_or(Span::of(0, 0));
        self.parse_items(items, span)
    }

    fn parse_items(&mut self, items: Vec<Item>, span: Span) -> crate::Result<Vec<Ast>> {
        let mut asts: Vec<Ast> = Vec::new();

        let mut parser = Parser::new(items, span.start);
        while parser.peek().is_some() {
            let ast = self.parse_bp(&mut parser, 0, false)?;
            parser.eat_semi();

            if !ast.is_empty() {
                asts.push(ast);
            }
        }

        Ok(asts)
    }

    fn parse_one_bp(&mut self, parser: &mut Parser, min_bp: u8) -> crate::Result<Ast> {
        let ast = self.parse_bp(parser, min_bp, false)?;
        while let Some(i) = parser.next() {
            if !i.is_newln() {
                return Err(crate::Error::UnexpectedItem(i));
            }
        }
        Ok(ast)
    }

    fn parse_bp(&mut self, parser: &mut Parser, min_bp: u8, cond: bool) -> crate::Result<Ast> {
        let mut lhs = match parser.peek() {
            Some(Item::Group(_)) => {
                let g = parser.next().unwrap().into_group().unwrap();
                match g.par_kind() {
                    ParKind::Round => {
                        let s = g.span();
                        let mut group_parser = Parser::new(g.items, s.start);
                        let mut ast = self.parse_one_bp(&mut group_parser, 0)?;
                        ast.span = s;
                        ast
                    }
                    ParKind::Curly => {
                        let b = self.parse_block(g)?;
                        Ast::new(AstT::Block(b.asts), b.span)
                    }
                    ParKind::Square => {
                        self.errors.push(crate::Error::NotImplemented(
                            "Arrays are not yet implemented",
                            g.span(),
                        ));
                        return Ok(Ast::new(AstT::Error, g.span()));
                    }
                }
            }
            Some(Item::Val(_)) => {
                let v = parser.next().unwrap().into_val().unwrap();
                let s = v.span;
                Ast::new(AstT::Val(v), s)
            }
            Some(Item::Ident(_)) => {
                let i = parser.next().unwrap().into_ident().unwrap();
                let s = i.span;
                Ast::new(AstT::Ident(i), s)
            }
            Some(&Item::Op(o)) => {
                parser.next();

                let (prefix, r_bp) = match o.prefix_bp() {
                    Some(p) => p,
                    None => return Err(crate::Error::UnexpectedOperator(o)),
                };

                if parser.peek().is_none() {
                    let s = o.span.after();
                    return Err(crate::Error::MissingOperand(s));
                }

                let val = self.parse_bp(parser, r_bp, cond)?;
                let ast_s = Span::across(o.span, val.span);
                let ast = match prefix {
                    PrefixT::UnaryPlus => val.typ,
                    PrefixT::UnaryMinus => AstT::Neg(Box::new(val)),
                    PrefixT::Not => AstT::Not(Box::new(val)),
                };

                Ast::new(ast, ast_s)
            }
            Some(&Item::Pct(p)) => match p.typ {
                PctT::Comma | PctT::Colon | PctT::Arrow => {
                    let i = parser.next().unwrap();
                    self.errors.push(crate::Error::UnexpectedItem(i));
                    return Ok(Ast::new(AstT::Error, p.span));
                }
                PctT::Semi | PctT::Newln => {
                    return Ok(Ast::new(AstT::Empty, p.span));
                }
            },
            Some(&Item::Kw(k)) => {
                parser.next();
                self.parse_lang_construct(parser, k)?
            }
            None => return Ok(Ast::new(AstT::Empty, Span::pos(parser.pos))),
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
                    if cond && g.par_kind().is_curly() {
                        break;
                    }

                    if let AstT::Ident(id) = lhs.typ {
                        match g.par_kind() {
                            ParKind::Round => {
                                let g = parser.next().unwrap().into_group().unwrap();
                                lhs = self.parse_fun_call(id, g)?;
                                continue;
                            }
                            ParKind::Square => {
                                return Err(crate::Error::NotImplemented(
                                    "Array access is not yet implemented",
                                    g.span(),
                                ));
                            }
                            _ => (),
                        }
                    }

                    let s = Span::between(lhs.span, g.span());
                    return Err(crate::Error::MissingOperator(s));
                }
                Item::Val(v) => {
                    if newln {
                        break;
                    }

                    let s = Span::between(lhs.span, v.span);
                    return Err(crate::Error::MissingOperator(s));
                }
                Item::Ident(i) => {
                    if newln {
                        break;
                    }

                    let s = Span::between(lhs.span, i.span);
                    return Err(crate::Error::MissingOperator(s));
                }
                &Item::Op(o) => o,
                &Item::Pct(s) => {
                    if newln {
                        break;
                    }
                    if let PctT::Semi = s.typ {
                        break;
                    }

                    let i = parser.next().unwrap();
                    return Err(crate::Error::UnexpectedItem(i));
                }
                Item::Kw(k) => {
                    if newln {
                        break;
                    }

                    let s = Span::between(lhs.span, k.span);
                    return Err(crate::Error::MissingOperator(s));
                }
            };

            if let Some((l_bp, postfix)) = op.postfix_bp() {
                if l_bp < min_bp {
                    break;
                }
                parser.next();

                let val_s = Span::across(lhs.span, op.span);
                let val = match postfix {
                    PostfixT::Factorial => AstT::Factorial(Box::new(lhs)),
                };

                lhs = Ast::new(val, val_s);
                continue;
            }

            let (l_bp, infix, r_bp) = match op.infix_bp() {
                Some(bp) => bp,
                None => {
                    let s = Span::between(lhs.span, op.span);
                    return Err(crate::Error::MissingOperator(s));
                }
            };
            if l_bp < min_bp {
                break;
            } else {
                parser.next();
            }

            let rhs = self.parse_bp(parser, r_bp, cond)?;

            if let AstT::Empty = rhs.typ {
                let s = op.span.after();
                return Err(crate::Error::MissingOperand(s));
            }

            let val_s = Span::across(lhs.span, rhs.span);
            let val = match infix {
                InfixT::Assign => match lhs.typ {
                    AstT::Ident(id) => AstT::Assign(id, Box::new(rhs)),
                    _ => return Err(crate::Error::InvalidAssignment(lhs.span, op.span)),
                },
                InfixT::AddAssign => match lhs.typ {
                    AstT::Ident(id) => AstT::AddAssign(id, Box::new(rhs)),
                    _ => return Err(crate::Error::InvalidAssignment(lhs.span, op.span)),
                },
                InfixT::SubAssign => match lhs.typ {
                    AstT::Ident(id) => AstT::SubAssign(id, Box::new(rhs)),
                    _ => return Err(crate::Error::InvalidAssignment(lhs.span, op.span)),
                },
                InfixT::MulAssign => match lhs.typ {
                    AstT::Ident(id) => AstT::MulAssign(id, Box::new(rhs)),
                    _ => return Err(crate::Error::InvalidAssignment(lhs.span, op.span)),
                },
                InfixT::DivAssign => match lhs.typ {
                    AstT::Ident(id) => AstT::DivAssign(id, Box::new(rhs)),
                    _ => return Err(crate::Error::InvalidAssignment(lhs.span, op.span)),
                },
                InfixT::RangeEx => AstT::RangeEx(Box::new(lhs), Box::new(rhs)),
                InfixT::RangeIn => AstT::RangeIn(Box::new(lhs), Box::new(rhs)),
                InfixT::Add => AstT::Add(Box::new(lhs), Box::new(rhs)),
                InfixT::Sub => AstT::Sub(Box::new(lhs), Box::new(rhs)),
                InfixT::Mul => AstT::Mul(Box::new(lhs), Box::new(rhs)),
                InfixT::Div => AstT::Div(Box::new(lhs), Box::new(rhs)),
                InfixT::IntDiv => AstT::IntDiv(Box::new(lhs), Box::new(rhs)),
                InfixT::Rem => AstT::Rem(Box::new(lhs), Box::new(rhs)),
                InfixT::Pow => AstT::Pow(Box::new(lhs), Box::new(rhs)),
                InfixT::Eq => AstT::Eq(Box::new(lhs), Box::new(rhs)),
                InfixT::Ne => AstT::Ne(Box::new(lhs), Box::new(rhs)),
                InfixT::Lt => AstT::Lt(Box::new(lhs), Box::new(rhs)),
                InfixT::Le => AstT::Le(Box::new(lhs), Box::new(rhs)),
                InfixT::Gt => AstT::Gt(Box::new(lhs), Box::new(rhs)),
                InfixT::Ge => AstT::Ge(Box::new(lhs), Box::new(rhs)),
                InfixT::Or => AstT::Or(Box::new(lhs), Box::new(rhs)),
                InfixT::And => AstT::And(Box::new(lhs), Box::new(rhs)),
                InfixT::BwOr => AstT::BwOr(Box::new(lhs), Box::new(rhs)),
                InfixT::BwAnd => AstT::BwAnd(Box::new(lhs), Box::new(rhs)),
                InfixT::Dot => {
                    return Err(crate::Error::NotImplemented(
                        "Field access or method calls are not yet implemented",
                        op.span,
                    ))
                }
            };
            lhs = Ast::new(val, val_s);
        }

        Ok(lhs)
    }

    fn parse_block(&mut self, g: Group) -> crate::Result<Block> {
        let s = g.span();
        match self.parse_items(g.items, s) {
            Ok(asts) => Ok(Block::new(asts, s)),
            Err(e) => {
                self.errors.push(e);
                Ok(Block::new(vec![Ast::new(AstT::Error, s)], s))
            }
        }
    }

    fn parse_fun_call(&mut self, id: IdentSpan, g: Group) -> crate::Result<Ast> {
        let span = Span::across(id.span, g.span());
        let b = match BuiltinFun::from(self.idents.name(id.ident)) {
            Some(b) => b,
            None => {
                let args = self.parse_dyn_fun_args(0, usize::MAX, g)?;
                return Ok(Ast::new(AstT::FunCall(id, args), span));
            }
        };

        let f = match b {
            BuiltinFun::Pow => {
                let [base, exp] = self.parse_fun_args(g)?;
                AstT::Pow(Box::new(base), Box::new(exp))
            }
            BuiltinFun::Ln => {
                let [n] = self.parse_fun_args(g)?;
                AstT::Ln(Box::new(n))
            }
            BuiltinFun::Log => {
                let [base, n] = self.parse_fun_args(g)?;
                AstT::Log(Box::new(base), Box::new(n))
            }
            BuiltinFun::Sqrt => {
                let [n] = self.parse_fun_args(g)?;
                AstT::Sqrt(Box::new(n))
            }
            BuiltinFun::Ncr => {
                let [n, r] = self.parse_fun_args(g)?;
                AstT::Ncr(Box::new(n), Box::new(r))
            }
            BuiltinFun::ToDeg => {
                let [n] = self.parse_fun_args(g)?;
                AstT::ToDeg(Box::new(n))
            }
            BuiltinFun::ToRad => {
                let [n] = self.parse_fun_args(g)?;
                AstT::ToRad(Box::new(n))
            }
            BuiltinFun::Sin => {
                let [n] = self.parse_fun_args(g)?;
                AstT::Sin(Box::new(n))
            }
            BuiltinFun::Cos => {
                let [n] = self.parse_fun_args(g)?;
                AstT::Cos(Box::new(n))
            }
            BuiltinFun::Tan => {
                let [n] = self.parse_fun_args(g)?;
                AstT::Tan(Box::new(n))
            }
            BuiltinFun::Asin => {
                let [n] = self.parse_fun_args(g)?;
                AstT::Asin(Box::new(n))
            }
            BuiltinFun::Acos => {
                let [n] = self.parse_fun_args(g)?;
                AstT::Acos(Box::new(n))
            }
            BuiltinFun::Atan => {
                let [n] = self.parse_fun_args(g)?;
                AstT::Atan(Box::new(n))
            }
            BuiltinFun::Gcd => {
                let [a, b] = self.parse_fun_args(g)?;
                AstT::Gcd(Box::new(a), Box::new(b))
            }
            BuiltinFun::Min => {
                let args = self.parse_dyn_fun_args(2, usize::MAX, g)?;
                AstT::Min(args)
            }
            BuiltinFun::Max => {
                let args = self.parse_dyn_fun_args(2, usize::MAX, g)?;
                AstT::Max(args)
            }
            BuiltinFun::Clamp => {
                let [n, min, max] = self.parse_fun_args(g)?;
                AstT::Clamp(Box::new(n), Box::new(min), Box::new(max))
            }
            BuiltinFun::Print => {
                let args = self.parse_dyn_fun_args(0, usize::MAX, g)?;
                AstT::Print(args)
            }
            BuiltinFun::Println => {
                let args = self.parse_dyn_fun_args(0, usize::MAX, g)?;
                AstT::Println(args)
            }
            BuiltinFun::Spill => {
                self.parse_fun_args::<0>(g)?;
                AstT::Spill
            }
            BuiltinFun::Assert => {
                let [a] = self.parse_fun_args(g)?;
                AstT::Assert(Box::new(a))
            }
            BuiltinFun::AssertEq => {
                let [a, b] = self.parse_fun_args(g)?;
                AstT::AssertEq(Box::new(a), Box::new(b))
            }
        };

        Ok(Ast::new(f, span))
    }

    fn parse_dyn_fun_args(
        &mut self,
        min: usize,
        max: usize,
        group: Group,
    ) -> crate::Result<Vec<Ast>> {
        let span = group.span();
        let items = group.items;
        let arg_count = items.iter().filter(|i| i.is_comma()).count() + 1;
        let mut args = Vec::with_capacity(cmp::min(arg_count, max));
        let mut unexpected_args = Vec::new();
        let mut parsed_args = 0;
        let mut start = (0, span.start);
        let mut arg_items = Vec::new();

        // TODO: use parser on group items
        for (i, it) in items.into_iter().enumerate() {
            if it.is_comma() {
                let s = Span::of(start.1, it.span().start);
                if parsed_args < max {
                    let mut parser = Parser::new(arg_items.clone(), s.start);
                    arg_items.clear();
                    args.push(self.parse_one_bp(&mut parser, 0)?);
                } else {
                    unexpected_args.push(s);
                }
                start = (i + 1, it.span().end);
                parsed_args += 1;
            } else {
                arg_items.push(it);
            }
        }

        if !arg_items.is_empty() {
            let s = Span::of(start.1, span.end);
            if parsed_args < max {
                let mut parser = Parser::new(arg_items, s.start);
                args.push(self.parse_one_bp(&mut parser, 0)?);
            } else {
                unexpected_args.push(s);
            }
            parsed_args += 1;
        }

        if !unexpected_args.is_empty() {
            self.errors.push(crate::Error::UnexpectedFunArgs {
                spans: unexpected_args,
                expected: max,
                found: parsed_args,
            });
        } else if parsed_args < min {
            let span = match args.last() {
                Some(a) => Span::of(a.span.end, span.end),
                None => span.after(),
            };
            self.errors.push(crate::Error::MissingFunArgs {
                span,
                expected: min,
                found: parsed_args,
            });
        }

        Ok(args)
    }

    fn parse_fun_args<const COUNT: usize>(&mut self, group: Group) -> crate::Result<[Ast; COUNT]> {
        let span = group.span();
        let items = group.items;
        let mut args: [Ast; COUNT] = array_of(|_| Ast::new(AstT::Error, span));
        let mut unexpected_args = Vec::new();
        let mut parsed_args = 0;
        let mut start = (0, span.start);
        let mut arg_items = Vec::new();

        // TODO: use parser on group items
        for (i, it) in items.into_iter().enumerate() {
            if it.is_comma() {
                let s = Span::of(start.1, it.span().start);
                if parsed_args < COUNT {
                    let mut parser = Parser::new(arg_items.clone(), s.start);
                    arg_items.clear();
                    args[parsed_args] = self.parse_one_bp(&mut parser, 0)?;
                } else {
                    unexpected_args.push(s);
                }
                start = (i + 1, it.span().end);
                parsed_args += 1;
            } else {
                arg_items.push(it);
            }
        }

        if !arg_items.is_empty() {
            let s = Span::of(start.1, span.end);
            if parsed_args < COUNT {
                let mut parser = Parser::new(arg_items, s.start);
                args[parsed_args] = self.parse_one_bp(&mut parser, 0)?;
            } else {
                unexpected_args.push(s);
            }
            parsed_args += 1;
        }

        if !unexpected_args.is_empty() {
            self.errors.push(crate::Error::UnexpectedFunArgs {
                spans: unexpected_args,
                expected: COUNT,
                found: parsed_args,
            });
        } else if parsed_args < COUNT {
            let span = match args.last() {
                Some(a) => Span::of(a.span.end, span.end),
                None => span.after(),
            };
            self.errors.push(crate::Error::MissingFunArgs {
                span,
                expected: COUNT,
                found: parsed_args,
            });
        }

        Ok(args)
    }

    fn parse_lang_construct(&mut self, parser: &mut Parser, kw: Kw) -> crate::Result<Ast> {
        match kw.typ {
            KwT::If => {
                let mut cases = Vec::new();
                let mut else_block = None;
                let first_case = self.parse_cond_block(parser, kw)?;
                let mut last_span = first_case.span;
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
                            let case = self.parse_cond_block(parser, k)?;
                            last_span = case.span;
                            cases.push(case);
                        }
                        Some(Item::Group(g)) if g.par_kind().is_curly() => {
                            else_block = Some(self.parse_block(g)?);
                            break;
                        }
                        Some(i) => return Err(crate::Error::ExpectedBlock(i.span())),
                        None => {
                            let s = kw.span.after();
                            return Err(crate::Error::ExpectedBlock(s));
                        }
                    }
                }

                let if_s = Span::across(kw.span, last_span);
                let if_expr = IfExpr::new(cases, else_block);
                Ok(Ast::new(AstT::IfExpr(if_expr), if_s))
            }
            KwT::Else => Err(crate::Error::WrongContext(kw)),
            KwT::While => {
                let case = self.parse_cond_block(parser, kw)?;
                let s = case.span;
                Ok(Ast::new(AstT::WhileLoop(Box::new(case)), s))
            }
            KwT::For => {
                let ident = parser.expect_ident()?;

                parser.expect_kw(KwT::In)?;

                let iter = self.parse_bp(parser, 0, true)?;

                let group = parser.expect_block()?;
                let block = self.parse_block(group)?;

                let span = Span::across(kw.span, block.span);
                let for_loop = ForLoop::new(ident, Box::new(iter), block);
                Ok(Ast::new(AstT::ForLoop(for_loop), span))
            }
            KwT::In => Err(crate::Error::WrongContext(kw)),
            KwT::Fun => {
                let ident = parser.expect_ident()?;

                let name = self.idents.name(ident.ident);
                if BuiltinFun::from(name).is_some() {
                    return Err(crate::Error::RedefinedBuiltinFun(
                        name.to_owned(),
                        ident.span,
                    ));
                }

                let param_group = parser.expect_fun_pars()?;
                let params = {
                    let s = param_group.span();
                    let mut group_parser = Parser::new(param_group.items, s.start);
                    let mut params = Vec::new();
                    while let Some(i) = group_parser.next() {
                        let s = i.span();

                        let ident = match i {
                            Item::Ident(id) => id,
                            _ => return Err(crate::Error::ExpectedIdent(s)),
                        };

                        group_parser.expect_pct(PctT::Colon)?;
                        let typ = group_parser.expect_ident()?;

                        params.push(Param::new(ident, typ));

                        match group_parser.next() {
                            Some(i) if i.is_comma() => (),
                            Some(i) => {
                                let s = i.span().after();
                                return Err(crate::Error::ExpectedPct(PctT::Comma, s));
                            }
                            None => break,
                        }
                    }
                    params
                };

                let mut return_type = None;
                if let Some(Item::Pct(p)) = parser.peek() {
                    if let PctT::Arrow = p.typ {
                        parser.next();

                        return_type = Some(parser.expect_ident()?);
                    }
                }

                let block_group = parser.expect_block()?;
                let block = self.parse_block(block_group)?;

                let span = Span::across(kw.span, block.span);
                let fun = Fun::new(ident, params, return_type, block);
                Ok(Ast::new(AstT::FunDef(Rc::new(fun)), span))
            }
            KwT::Val | KwT::Var => {
                let ident = parser.expect_ident()?;

                let name = self.idents.name(ident.ident);
                if BuiltinConst::from(name).is_some() {
                    return Err(crate::Error::RedefinedBuiltinConst(
                        name.to_owned(),
                        ident.span,
                    ));
                }

                parser.expect_op(OpT::Assign)?;

                let val = self.parse_bp(parser, 0, false)?;

                let mutable = kw.typ == KwT::Var;

                let span = Span::across(kw.span, val.span);
                Ok(Ast::new(AstT::VarDef(ident, Box::new(val), mutable), span))
            }
        }
    }

    fn parse_cond_block(&mut self, parser: &mut Parser, kw: Kw) -> crate::Result<CondBlock> {
        let cond = self.parse_bp(parser, 0, true)?;

        let group = parser.expect_block()?;
        let span = Span::across(kw.span, group.span());
        let block = self.parse_block(group)?;

        Ok(CondBlock::new(cond, block.asts, span))
    }
}

fn items_span(items: &[Item]) -> Option<Span> {
    let first = items.first();
    let last = items.last();

    match (first, last) {
        (Some(f), Some(l)) => Some(Span::across(f.span(), l.span())),
        _ => None,
    }
}
