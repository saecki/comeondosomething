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

    fn parse_items(&mut self, items: Vec<Item>, mut span: Span) -> crate::Result<Vec<Ast>> {
        let mut asts: Vec<Ast> = Vec::new();

        let mut parser = Parser::new(items);
        while parser.peek().is_some() {
            if let Some(a) = asts.last() {
                span.start = a.span.end;
            };

            let ast = self.parse_bp(&mut parser, 0, span, false)?;
            parser.eat_semi();

            if !ast.is_empty() {
                asts.push(ast);
            }
        }

        Ok(asts)
    }

    fn parse_one_bp(&mut self, parser: &mut Parser, min_bp: u8, span: Span) -> crate::Result<Ast> {
        let ast = self.parse_bp(parser, min_bp, span, false)?;
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
        span: Span,
        cond: bool,
    ) -> crate::Result<Ast> {
        let mut lhs = match parser.peek() {
            Some(Item::Group(_)) => {
                let g = parser.next().unwrap().into_group().unwrap();
                match g.par_kind {
                    ParKind::Round => {
                        let mut group_parser = Parser::new(g.items);
                        let mut ast = self.parse_one_bp(&mut group_parser, 0, g.span)?;
                        ast.span = g.span;
                        ast
                    }
                    ParKind::Curly => {
                        let b = self.parse_block(g)?;
                        Ast::new(AstT::Block(b.asts), b.span)
                    }
                    ParKind::Square => {
                        self.errors.push(crate::Error::NotImplemented(
                            "Arrays are not yet implemented",
                            g.span,
                        ));
                        return Ok(Ast::new(AstT::Error, span));
                    }
                }
            }
            Some(Item::Expr(_)) => {
                let e = parser.next().unwrap().into_expr().unwrap();
                let s = e.span;
                Ast::new(AstT::Expr(e), s)
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

                let val_s = Span::of(o.span.end, span.end);
                let val = self.parse_bp(parser, r_bp, val_s, cond)?;
                let ast_s = Span::span(o.span, val.span);
                let ast = match prefix {
                    Prefix::UnaryPlus => val.typ,
                    Prefix::UnaryMinus => AstT::Neg(Box::new(val)),
                    Prefix::Not => AstT::Not(Box::new(val)),
                };

                Ast::new(ast, ast_s)
            }
            Some(&Item::Pct(s)) => match s.typ {
                PctT::Comma | PctT::Colon | PctT::Arrow => {
                    let i = parser.next().unwrap();
                    self.errors.push(crate::Error::UnexpectedItem(i));
                    return Ok(Ast::new(AstT::Error, span));
                }
                PctT::Semi | PctT::Newln => {
                    let s = Span::of(span.start, s.span.end);
                    return Ok(Ast::new(AstT::Empty, s));
                }
            },
            Some(&Item::Kw(k)) => {
                parser.next();
                let s = Span::span(k.span, span);
                self.parse_lang_construct(parser, k, s)?
            }
            None => return Ok(Ast::new(AstT::Empty, span)),
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

                    let s = Span::between(lhs.span, g.span);
                    return Err(crate::Error::MissingOperator(s));
                }
                Item::Expr(e) => {
                    if newln {
                        break;
                    }

                    let s = Span::between(lhs.span, e.span);
                    return Err(crate::Error::MissingOperator(s));
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

                    let s = Span::between(lhs.span, k.span);
                    return Err(crate::Error::MissingOperator(s));
                }
            };

            if let Some((l_bp, postfix)) = op.postfix_bp() {
                if l_bp < min_bp {
                    break;
                }
                parser.next();

                let val_s = Span::span(lhs.span, op.span);
                let val = match postfix {
                    Postfix::Factorial => AstT::Factorial(Box::new(lhs)),
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

            let rhs_s = Span::of(op.span.end, span.end);
            let rhs = self.parse_bp(parser, r_bp, rhs_s, cond)?;

            if let AstT::Empty = rhs.typ {
                let s = op.span.after();
                return Err(crate::Error::MissingOperand(s));
            }

            let val_s = Span::span(lhs.span, rhs.span);
            let val = match infix {
                Infix::Assign => match lhs.as_ident() {
                    Some(id) => AstT::Assign(id, Box::new(rhs)),
                    None => return Err(crate::Error::InvalidAssignment(lhs.span, op.span)),
                },
                Infix::AddAssign => match lhs.as_ident() {
                    Some(id) => AstT::AddAssign(id, Box::new(rhs)),
                    None => return Err(crate::Error::InvalidAssignment(lhs.span, op.span)),
                },
                Infix::SubAssign => match lhs.as_ident() {
                    Some(id) => AstT::SubAssign(id, Box::new(rhs)),
                    None => return Err(crate::Error::InvalidAssignment(lhs.span, op.span)),
                },
                Infix::MulAssign => match lhs.as_ident() {
                    Some(id) => AstT::MulAssign(id, Box::new(rhs)),
                    None => return Err(crate::Error::InvalidAssignment(lhs.span, op.span)),
                },
                Infix::DivAssign => match lhs.as_ident() {
                    Some(id) => AstT::DivAssign(id, Box::new(rhs)),
                    None => return Err(crate::Error::InvalidAssignment(lhs.span, op.span)),
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
                        op.span,
                    ))
                }
            };
            lhs = Ast::new(val, val_s);
        }

        Ok(lhs)
    }

    fn parse_block(&mut self, g: Group) -> crate::Result<Block> {
        match self.parse_items(g.items, g.span) {
            Ok(asts) => Ok(Block::new(asts, g.span)),
            Err(e) => {
                self.errors.push(e);
                Ok(Block::new(vec![Ast::new(AstT::Error, g.span)], g.span))
            }
        }
    }

    fn parse_fun_call(&mut self, id: IdentSpan, g: Group) -> crate::Result<Ast> {
        let b = match BuiltinFun::from(self.idents.name(id.ident)) {
            Some(b) => b,
            None => {
                let args = self.parse_dyn_fun_args(0, usize::MAX, g.items, g.span)?;
                let s = Span::span(id.span, g.span);
                return Ok(Ast::new(AstT::FunCall(id, args), s));
            }
        };

        let f = match b {
            BuiltinFun::Pow => {
                let [base, exp] = self.parse_fun_args(g.items, g.span)?;
                AstT::Pow(Box::new(base), Box::new(exp))
            }
            BuiltinFun::Ln => {
                let [n] = self.parse_fun_args(g.items, g.span)?;
                AstT::Ln(Box::new(n))
            }
            BuiltinFun::Log => {
                let [base, n] = self.parse_fun_args(g.items, g.span)?;
                AstT::Log(Box::new(base), Box::new(n))
            }
            BuiltinFun::Sqrt => {
                let [n] = self.parse_fun_args(g.items, g.span)?;
                AstT::Sqrt(Box::new(n))
            }
            BuiltinFun::Ncr => {
                let [n, r] = self.parse_fun_args(g.items, g.span)?;
                AstT::Ncr(Box::new(n), Box::new(r))
            }
            BuiltinFun::ToDeg => {
                let [n] = self.parse_fun_args(g.items, g.span)?;
                AstT::ToDeg(Box::new(n))
            }
            BuiltinFun::ToRad => {
                let [n] = self.parse_fun_args(g.items, g.span)?;
                AstT::ToRad(Box::new(n))
            }
            BuiltinFun::Sin => {
                let [n] = self.parse_fun_args(g.items, g.span)?;
                AstT::Sin(Box::new(n))
            }
            BuiltinFun::Cos => {
                let [n] = self.parse_fun_args(g.items, g.span)?;
                AstT::Cos(Box::new(n))
            }
            BuiltinFun::Tan => {
                let [n] = self.parse_fun_args(g.items, g.span)?;
                AstT::Tan(Box::new(n))
            }
            BuiltinFun::Asin => {
                let [n] = self.parse_fun_args(g.items, g.span)?;
                AstT::Asin(Box::new(n))
            }
            BuiltinFun::Acos => {
                let [n] = self.parse_fun_args(g.items, g.span)?;
                AstT::Acos(Box::new(n))
            }
            BuiltinFun::Atan => {
                let [n] = self.parse_fun_args(g.items, g.span)?;
                AstT::Atan(Box::new(n))
            }
            BuiltinFun::Gcd => {
                let [a, b] = self.parse_fun_args(g.items, g.span)?;
                AstT::Gcd(Box::new(a), Box::new(b))
            }
            BuiltinFun::Min => {
                let args = self.parse_dyn_fun_args(2, usize::MAX, g.items, g.span)?;
                AstT::Min(args)
            }
            BuiltinFun::Max => {
                let args = self.parse_dyn_fun_args(2, usize::MAX, g.items, g.span)?;
                AstT::Max(args)
            }
            BuiltinFun::Clamp => {
                let [n, min, max] = self.parse_fun_args(g.items, g.span)?;
                AstT::Clamp(Box::new(n), Box::new(min), Box::new(max))
            }
            BuiltinFun::Print => {
                let args = self.parse_dyn_fun_args(0, usize::MAX, g.items, g.span)?;
                AstT::Print(args)
            }
            BuiltinFun::Println => {
                let args = self.parse_dyn_fun_args(0, usize::MAX, g.items, g.span)?;
                AstT::Println(args)
            }
            BuiltinFun::Spill => {
                self.parse_fun_args::<0>(g.items, g.span)?;
                AstT::Spill
            }
            BuiltinFun::Assert => {
                let [a] = self.parse_fun_args(g.items, g.span)?;
                AstT::Assert(Box::new(a))
            }
            BuiltinFun::AssertEq => {
                let [a, b] = self.parse_fun_args(g.items, g.span)?;
                AstT::AssertEq(Box::new(a), Box::new(b))
            }
        };
        let s = Span::span(id.span, g.span);
        Ok(Ast::new(f, s))
    }

    fn parse_dyn_fun_args(
        &mut self,
        min: usize,
        max: usize,
        items: Vec<Item>,
        span: Span,
    ) -> crate::Result<Vec<Ast>> {
        let arg_count = items.iter().filter(|i| i.is_pct()).count() + 1;
        let mut args = Vec::with_capacity(cmp::min(arg_count, max));
        let mut unexpected_args = Vec::new();
        let mut parsed_args = 0;
        let mut start = (0, span.start);
        let mut arg_items = Vec::new();

        for (i, it) in items.into_iter().enumerate() {
            if it.is_comma() {
                let s = Span::of(start.1, it.span().start);
                if parsed_args < max {
                    let mut parser = Parser::new(arg_items.clone());
                    arg_items.clear();
                    args.push(self.parse_one_bp(&mut parser, 0, s)?);
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
                let mut parser = Parser::new(arg_items);
                args.push(self.parse_one_bp(&mut parser, 0, s)?);
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

    fn parse_fun_args<const COUNT: usize>(
        &mut self,
        items: Vec<Item>,
        span: Span,
    ) -> crate::Result<[Ast; COUNT]> {
        let mut args: [Ast; COUNT] = array_of(|_| Ast::new(AstT::Error, span));
        let mut unexpected_args = Vec::new();
        let mut parsed_args = 0;
        let mut start = (0, span.start);
        let mut arg_items = Vec::new();

        for (i, it) in items.into_iter().enumerate() {
            if it.is_comma() {
                let s = Span::of(start.1, it.span().start);
                if parsed_args < COUNT {
                    let mut parser = Parser::new(arg_items.clone());
                    arg_items.clear();
                    args[parsed_args] = self.parse_one_bp(&mut parser, 0, s)?;
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
                let mut parser = Parser::new(arg_items);
                args[parsed_args] = self.parse_one_bp(&mut parser, 0, s)?;
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

    fn parse_lang_construct(
        &mut self,
        parser: &mut Parser,
        kw: Kw,
        span: Span,
    ) -> crate::Result<Ast> {
        match kw.typ {
            KwT::If => {
                let mut cases = Vec::new();
                let mut else_block = None;
                let first_case = self.parse_cond_block(parser, kw, span)?;
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
                            let case = self.parse_cond_block(parser, k, span)?;
                            last_span = case.span;
                            cases.push(case);
                        }
                        Some(Item::Group(g)) if g.par_kind.is_curly() => {
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

                let if_s = Span::span(kw.span, last_span);
                let if_expr = IfExpr::new(cases, else_block);
                Ok(Ast::new(AstT::IfExpr(if_expr), if_s))
            }
            KwT::Else => Err(crate::Error::WrongContext(kw)),
            KwT::While => {
                let case = self.parse_cond_block(parser, kw, span)?;
                let s = case.span;
                Ok(Ast::new(AstT::WhileLoop(Box::new(case)), s))
            }
            KwT::For => {
                let ident = parser.expect_ident(kw.span.end)?;

                parser.expect_kw(KwT::In, ident.span.end)?;

                let iter = self.parse_bp(parser, 0, span, true)?;

                let group = parser.expect_block(iter.span.end)?;
                let block = self.parse_block(group)?;

                let span = Span::span(kw.span, block.span);
                let for_loop = ForLoop::new(ident, Box::new(iter), block);
                Ok(Ast::new(AstT::ForLoop(for_loop), span))
            }
            KwT::In => Err(crate::Error::WrongContext(kw)),
            KwT::Fun => {
                let ident = parser.expect_ident(kw.span.end)?;

                let name = self.idents.name(ident.ident);
                if BuiltinFun::from(name).is_some() {
                    return Err(crate::Error::RedefinedBuiltinFun(
                        name.to_owned(),
                        ident.span,
                    ));
                }

                let param_group = parser.expect_fun_pars(ident.span.end)?;
                let params = {
                    let mut group_parser = Parser::new(param_group.items);
                    let mut params = Vec::new();
                    while let Some(i) = group_parser.next() {
                        let s = i.span();
                        let ident = i
                            .into_expr()
                            .and_then(|e| e.as_ident())
                            .ok_or(crate::Error::ExpectedIdent(s))?;

                        let colon_s = group_parser.expect_pct(PctT::Colon, ident.span.end)?;
                        let typ = group_parser.expect_ident(colon_s.end)?;

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

                let mut last_span = param_group.span;
                let mut return_type = None;
                if let Some(Item::Pct(p)) = parser.peek() {
                    if let PctT::Arrow = p.typ {
                        last_span = p.span;
                        parser.next();

                        return_type = Some(parser.expect_ident(last_span.end)?);
                    }
                }

                let block_group = parser.expect_block(last_span.end)?;
                let block = self.parse_block(block_group)?;

                let span = Span::span(kw.span, block.span);
                let fun = Fun::new(ident, params, return_type, block);
                Ok(Ast::new(AstT::FunDef(Rc::new(fun)), span))
            }
            KwT::Val | KwT::Var => {
                let ident = parser.expect_ident(kw.span.end)?;

                let name = self.idents.name(ident.ident);
                if BuiltinConst::from(name).is_some() {
                    return Err(crate::Error::RedefinedBuiltinConst(
                        name.to_owned(),
                        ident.span,
                    ));
                }

                let assign_s = parser.expect_op(OpT::Assign, ident.span.end)?;

                let s = Span::of(assign_s.end, span.end);
                let val = self.parse_bp(parser, 0, s, false)?;

                let mutable = kw.typ == KwT::Var;

                let span = Span::span(kw.span, val.span);
                Ok(Ast::new(AstT::VarDef(ident, Box::new(val), mutable), span))
            }
        }
    }

    fn parse_cond_block(
        &mut self,
        parser: &mut Parser,
        kw: Kw,
        span: Span,
    ) -> crate::Result<CondBlock> {
        let cond_s = Span::of(kw.span.end, span.end);
        let cond = self.parse_bp(parser, 0, cond_s, true)?;

        let group = parser.expect_block(cond.span.end)?;
        let span = Span::span(kw.span, group.span);
        let block = self.parse_block(group)?;

        Ok(CondBlock::new(cond, block.asts, span))
    }
}

fn items_span(items: &[Item]) -> Option<Span> {
    let first = items.first();
    let last = items.last();

    match (first, last) {
        (Some(f), Some(l)) => Some(Span::span(f.span(), l.span())),
        _ => None,
    }
}
