use std::cmp;
use std::collections::VecDeque;

use crate::util::array_of;
use crate::{Ast, AstT, Context, ExprT, Fun, FunT, Item, ParKind, Range, SepT};

pub use op::*;

mod op;
#[cfg(test)]
mod test;

struct Parser {
    items: VecDeque<Item>,
}

impl Parser {
    fn new(items: Vec<Item>) -> Self {
        Self {
            items: VecDeque::from(items),
        }
    }

    fn next(&mut self) -> Option<Item> {
        self.items.pop_front()
    }

    fn peek(&mut self) -> Option<&Item> {
        self.items.get(0)
    }

    fn peek2(&mut self) -> Option<&Item> {
        self.items.get(1)
    }

    fn eat_semi_or_newln(&mut self) -> bool {
        if let Some(i) = self.peek() {
            if i.is_semi() || i.is_newln() {
                self.next();
                return true;
            }
        }
        false
    }

    fn eat_newlns(&mut self) -> bool {
        let mut newln = false;
        while let Some(i) = self.peek() {
            if i.is_newln() {
                self.next();
                newln = true;
            } else {
                break;
            }
        }
        newln
    }

    fn eat_newlns_leaving_one(&mut self) -> bool {
        let mut newln = false;
        if let Some(i) = self.peek() {
            if i.is_newln() {
                newln = true;
            } else {
                return false;
            }
        }
        while let Some(i) = self.peek2() {
            if i.is_newln() {
                self.next();
                newln = true;
            } else {
                break;
            }
        }
        newln
    }
}

impl Context {
    pub fn parse(&mut self, items: Vec<Item>) -> crate::Result<Vec<Ast>> {
        let mut range = items_range(&items).unwrap_or(Range::of(0, 0));
        let sep_count = items.iter().filter(|i| i.is_semi()).count();
        let mut asts: Vec<Ast> = Vec::with_capacity(sep_count + 1);

        let mut parser = Parser::new(items);
        while parser.peek().is_some() {
            if let Some(a) = asts.last() {
                range.start = a.range.end;
            };

            let ast = self.parse_bp(&mut parser, 0, range)?;
            parser.eat_semi_or_newln();

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
        range: Range,
    ) -> crate::Result<Ast> {
        let ast = self.parse_bp(parser, min_bp, range)?;
        if let Some(i) = parser.next() {
            return Err(crate::Error::UnexpectedItem(i));
        }
        Ok(ast)
    }

    fn parse_bp(&mut self, parser: &mut Parser, min_bp: u8, range: Range) -> crate::Result<Ast> {
        parser.eat_newlns();

        let mut lhs = match parser.peek() {
            Some(Item::Group(_)) => {
                let g = parser.next().unwrap().into_group().unwrap();
                let mut group_parser = Parser::new(g.items);
                let mut val = self.parse_bp(&mut group_parser, 0, g.range)?;
                val.range = g.range;
                val
            }
            Some(Item::Expr(_)) => {
                let e = parser.next().unwrap().into_expr().unwrap();
                let r = e.range;
                Ast::new(AstT::Expr(e), r)
            }
            Some(&Item::Fun(f)) => {
                parser.next();
                self.parse_fun(parser, f)?
            }
            Some(&Item::Op(o)) => {
                parser.next();

                let (prefix, r_bp) = match o.prefix_bp() {
                    Some(p) => p,
                    None => return Err(crate::Error::UnexpectedOperator(o)),
                };

                if parser.peek().is_none() {
                    let r = Range::pos(o.range.end);
                    return Err(crate::Error::MissingOperand(r));
                }

                let val_r = Range::of(o.range.end, range.end);
                let val = self.parse_bp(parser, r_bp, val_r)?;
                let ast_r = Range::span(o.range, val.range);
                let ast = match prefix {
                    Prefix::UnaryPlus => val.typ,
                    Prefix::UnaryMinus => AstT::Neg(Box::new(val)),
                    Prefix::Not => AstT::Not(Box::new(val)),
                };

                Ast::new(ast, ast_r)
            }
            Some(&Item::Sep(s)) => match s.typ {
                SepT::Comma => {
                    parser.next();
                    self.errors.push(crate::Error::UnexpectedSeparator(s));
                    return Ok(Ast::new(AstT::Error, range));
                }
                SepT::Semi | SepT::Newln => {
                    let r = Range::of(range.start, s.range.end);
                    return Ok(Ast::new(AstT::Empty, r));
                }
            },
            Some(Item::Kw(_k)) => todo!(),
            None => return Ok(Ast::new(AstT::Empty, range)),
        };

        let newln = parser.eat_newlns_leaving_one();
        while let Some(i) = if newln { parser.peek2() } else { parser.peek() } {
            let op = match i {
                Item::Group(g) => {
                    if newln {
                        break;
                    }

                    let r = Range::between(lhs.range, g.range);
                    return Err(crate::Error::MissingOperator(r));
                }
                Item::Expr(e) => {
                    if newln {
                        break;
                    }

                    let r = Range::between(lhs.range, e.range);
                    return Err(crate::Error::MissingOperator(r));
                }
                Item::Fun(f) => {
                    if newln {
                        break;
                    }

                    let r = Range::between(lhs.range, f.range);
                    return Err(crate::Error::MissingOperator(r));
                }
                &Item::Op(o) => {
                    if newln {
                        parser.next();
                    }
                    match o.suffix_bp() {
                        Some((l_bp, suffix)) => {
                            if l_bp < min_bp {
                                break;
                            } else {
                                parser.next();
                            }

                            let val_r = Range::span(lhs.range, o.range);
                            let val = match suffix {
                                Suffix::Degree => AstT::Degree(Box::new(lhs)),
                                Suffix::Radian => AstT::Radian(Box::new(lhs)),
                                Suffix::Factorial => AstT::Factorial(Box::new(lhs)),
                            };

                            lhs = Ast::new(val, val_r);
                            continue;
                        }
                        None => o,
                    }
                }
                &Item::Sep(s) => {
                    if newln {
                        parser.next();
                    }
                    match s.typ {
                        SepT::Comma => return Err(crate::Error::UnexpectedSeparator(s)),
                        SepT::Semi | SepT::Newln => break,
                    }
                }
                Item::Kw(_k) => todo!(),
            };

            let (l_bp, infix, r_bp) = match op.infix_bp() {
                Some(bp) => bp,
                None => {
                    let r = Range::between(lhs.range, op.range);
                    return Err(crate::Error::MissingOperator(r));
                }
            };
            if l_bp < min_bp {
                break;
            } else {
                parser.next();
            }

            let rhs_r = Range::of(op.range.end, range.end);
            let rhs = self.parse_bp(parser, r_bp, rhs_r)?;

            if let AstT::Empty = rhs.typ {
                let r = Range::pos(op.range.end);
                return Err(crate::Error::MissingOperand(r));
            }

            let val_r = Range::span(lhs.range, rhs.range);
            let val = match infix {
                Infix::Assign => match lhs.typ {
                    AstT::Expr(e) => match e.typ {
                        ExprT::Var(id) => AstT::Assign(id, Box::new(rhs)),
                        _ => return Err(crate::Error::InvalidAssignment(lhs.range, op.range)),
                    },
                    _ => return Err(crate::Error::InvalidAssignment(lhs.range, op.range)),
                },
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
            };
            lhs = Ast::new(val, val_r);
        }

        Ok(lhs)
    }

    fn parse_fun(&mut self, parser: &mut Parser, fun: Fun) -> crate::Result<Ast> {
        let g = match parser.next() {
            Some(Item::Group(g)) => match g.par_kind {
                ParKind::Round => g,
                _ => {
                    let s_r = Range::pos(g.range.start);
                    let e_r = Range::pos(g.range.end - 1);
                    self.errors.push(crate::Error::NotFunPars(s_r, e_r));
                    return Ok(Ast::new(AstT::Error, fun.range));
                }
            },
            _ => {
                let r = Range::pos(fun.range.end);
                return Err(crate::Error::MissingFunPars(r));
            }
        };

        let f = match fun.typ {
            FunT::Pow => {
                let [base, exp] = self.parse_fun_args(g.items, g.range)?;
                AstT::Pow(Box::new(base), Box::new(exp))
            }
            FunT::Ln => {
                let [n] = self.parse_fun_args(g.items, g.range)?;
                AstT::Ln(Box::new(n))
            }
            FunT::Log => {
                let [base, n] = self.parse_fun_args(g.items, g.range)?;
                AstT::Log(Box::new(base), Box::new(n))
            }
            FunT::Sqrt => {
                let [n] = self.parse_fun_args(g.items, g.range)?;
                AstT::Sqrt(Box::new(n))
            }
            FunT::Ncr => {
                let [n, r] = self.parse_fun_args(g.items, g.range)?;
                AstT::Ncr(Box::new(n), Box::new(r))
            }
            FunT::Sin => {
                let [n] = self.parse_fun_args(g.items, g.range)?;
                AstT::Sin(Box::new(n))
            }
            FunT::Cos => {
                let [n] = self.parse_fun_args(g.items, g.range)?;
                AstT::Cos(Box::new(n))
            }
            FunT::Tan => {
                let [n] = self.parse_fun_args(g.items, g.range)?;
                AstT::Tan(Box::new(n))
            }
            FunT::Asin => {
                let [n] = self.parse_fun_args(g.items, g.range)?;
                AstT::Asin(Box::new(n))
            }
            FunT::Acos => {
                let [n] = self.parse_fun_args(g.items, g.range)?;
                AstT::Acos(Box::new(n))
            }
            FunT::Atan => {
                let [n] = self.parse_fun_args(g.items, g.range)?;
                AstT::Atan(Box::new(n))
            }
            FunT::Gcd => {
                let [a, b] = self.parse_fun_args(g.items, g.range)?;
                AstT::Gcd(Box::new(a), Box::new(b))
            }
            FunT::Min => {
                let args = self.parse_dyn_fun_args(2, usize::MAX, g.items, g.range)?;
                AstT::Min(args)
            }
            FunT::Max => {
                let args = self.parse_dyn_fun_args(2, usize::MAX, g.items, g.range)?;
                AstT::Max(args)
            }
            FunT::Clamp => {
                let [n, min, max] = self.parse_fun_args(g.items, g.range)?;
                AstT::Clamp(Box::new(n), Box::new(min), Box::new(max))
            }
            FunT::Print => {
                let args = self.parse_dyn_fun_args(0, usize::MAX, g.items, g.range)?;
                AstT::Print(args)
            }
            FunT::Println => {
                let args = self.parse_dyn_fun_args(0, usize::MAX, g.items, g.range)?;
                AstT::Println(args)
            }
            FunT::Spill => {
                self.parse_fun_args::<0>(g.items, g.range)?;
                AstT::Spill
            }
            FunT::Assert => {
                let [a] = self.parse_fun_args(g.items, g.range)?;
                AstT::Assert(Box::new(a))
            }
            FunT::AssertEq => {
                let [a, b] = self.parse_fun_args(g.items, g.range)?;
                AstT::AssertEq(Box::new(a), Box::new(b))
            }
        };
        let r = Range::span(fun.range, g.range);
        Ok(Ast::new(f, r))
    }

    fn parse_dyn_fun_args(
        &mut self,
        min: usize,
        max: usize,
        items: Vec<Item>,
        range: Range,
    ) -> crate::Result<Vec<Ast>> {
        let arg_count = items.iter().filter(|i| i.is_sep()).count() + 1;
        let mut args = Vec::with_capacity(cmp::min(arg_count, max));
        let mut unexpected_args = Vec::new();
        let mut parsed_args = 0;
        let mut start = (0, range.start);
        let mut ti = 0;
        let mut arg_items = Vec::new();

        for i in items {
            if i.is_comma() {
                let r = Range::of(start.1, i.range().start);
                if parsed_args < max {
                    let mut parser = Parser::new(arg_items.clone());
                    arg_items.clear();
                    args.push(self.parse_one_bp(&mut parser, 0, r)?);
                } else {
                    unexpected_args.push(r);
                }
                start = (ti + 1, i.range().end);
                parsed_args += 1;
            } else {
                arg_items.push(i);
            }
            ti += 1;
        }

        if !arg_items.is_empty() {
            let r = Range::of(start.1, range.end);
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
                Some(a) => Range::of(a.range.end, range.end),
                None => Range::pos(range.end),
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
        range: Range,
    ) -> crate::Result<[Ast; COUNT]> {
        let mut args: [Ast; COUNT] = array_of(|_| Ast::new(AstT::Error, range));
        let mut unexpected_args = Vec::new();
        let mut parsed_args = 0;
        let mut start = (0, range.start);
        let mut ti = 0;
        let mut arg_items = Vec::new();

        for i in items {
            if i.is_comma() {
                let r = Range::of(start.1, i.range().start);
                if parsed_args < COUNT {
                    let mut parser = Parser::new(arg_items.clone());
                    arg_items.clear();
                    args[parsed_args] = self.parse_one_bp(&mut parser, 0, r)?;
                } else {
                    unexpected_args.push(r);
                }
                start = (ti + 1, i.range().end);
                parsed_args += 1;
            } else {
                arg_items.push(i);
            }
            ti += 1;
        }

        if !arg_items.is_empty() {
            let r = Range::of(start.1, range.end);
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
                Some(a) => Range::of(a.range.end, range.end),
                None => Range::pos(range.end),
            };
            self.errors.push(crate::Error::MissingFunArgs {
                range,
                expected: COUNT,
                found: parsed_args,
            });
        }

        Ok(args)
    }
}

fn items_range(items: &[Item]) -> Option<Range> {
    let first = items.first();
    let last = items.last();

    match (first, last) {
        (Some(f), Some(l)) => Some(Range::span(f.range(), l.range())),
        _ => None,
    }
}
