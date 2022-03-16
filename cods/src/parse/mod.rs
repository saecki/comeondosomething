use std::cmp;
use std::mem::MaybeUninit;

use crate::{
    items_range, Ast, AstT, Context, ExprT, Fun, FunT, Item, ModT, OpT, Range, SepT, Sign,
};

#[cfg(test)]
mod test;

impl OpT {
    pub fn bp(&self) -> (u8, u8) {
        match self {
            Self::Pow => (17, 18),
            Self::Mul | Self::Div | Self::IntDiv | Self::Rem => (15, 16),
            Self::Add | Self::Sub => (13, 14),
            Self::BwAnd => (11, 12),
            Self::BwOr => (9, 10),
            Self::And => (7, 8),
            Self::Or => (5, 6),
            Self::Eq => (3, 4),
            Self::Assign => (1, 2),
        }
    }
}

impl Sign {
    pub fn r_bp(&self) -> u8 {
        match self {
            Self::Negative | Self::Positive => 14,
        }
    }
}

impl ModT {
    pub fn l_bp(&self) -> u8 {
        match self {
            Self::Degree | Self::Radian | Self::Factorial => 19,
        }
    }
}

struct Parser<'a> {
    items: &'a [Item],
    cursor: usize,
}

impl<'a> Parser<'a> {
    fn new(items: &'a [Item]) -> Self {
        Self { items, cursor: 0 }
    }

    fn back(&mut self) -> Option<&'a Item> {
        if self.cursor > 0 {
            self.cursor -= 1;
            return self.peek();
        }
        None
    }

    fn next(&mut self) -> Option<&'a Item> {
        let i = self.peek();
        self.cursor += 1;
        i
    }

    fn peek(&mut self) -> Option<&'a Item> {
        self.items.get(self.cursor)
    }

    fn peek_back(&mut self) -> Option<&'a Item> {
        if self.cursor > 0 {
            self.items.get(self.cursor)
        } else {
            None
        }
    }

    fn eat_semi(&mut self) -> bool {
        if let Some(i) = self.peek() {
            if i.is_semi() {
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
}

impl Context {
    pub fn parse(&mut self, items: &[Item]) -> crate::Result<Vec<Ast>> {
        let mut range = items_range(items).unwrap_or(Range::of(0, 0));
        let sep_count = items.iter().filter(|i| i.is_semi()).count();
        let mut asts = Vec::with_capacity(sep_count + 1);

        let mut parser = Parser::new(items);
        while parser.peek().is_some() {
            if let Some(i) = parser.peek_back() {
                range.start = i.range().end;
            };

            let ast = self.parse_bp(&mut parser, 0, range)?;
            parser.eat_semi();
            asts.push(ast);
        }

        Ok(asts)
    }

    fn parse_bp(
        &mut self,
        parser: &mut Parser<'_>,
        min_bp: u8,
        range: Range,
    ) -> crate::Result<Ast> {
        parser.eat_newlns();

        let mut lhs = match parser.next() {
            Some(Item::Group(g)) => {
                let mut group_parser = Parser::new(&g.items);
                let mut val = self.parse_bp(&mut group_parser, 0, g.range)?;
                val.range = g.range;
                val
            }
            Some(Item::Expr(e)) => Ast::new(AstT::Expr(*e), e.range),
            Some(Item::Fun(f)) => self.parse_fun(parser, *f)?,
            Some(Item::Op(o)) => match o.as_sign() {
                Some(s) => {
                    if parser.peek().is_none() {
                        let r = Range::pos(o.range.end);
                        return Err(crate::Error::MissingOperand(r));
                    }

                    let val_r = Range::of(o.range.end, range.end);
                    let val = self.parse_bp(parser, s.r_bp(), val_r)?;
                    let r = Range::span(o.range, val.range);
                    Ast::new(AstT::Neg(Box::new(val)), r)
                }
                None => return Err(crate::Error::UnexpectedOperator(*o)),
            },
            Some(Item::Mod(m)) => {
                return Err(crate::Error::MissingOperand(Range::pos(m.range.start)))
            }
            Some(Item::Sep(s)) => match s.typ {
                SepT::Comma => return Err(crate::Error::UnexpectedSeparator(*s)),
                SepT::Semi | SepT::Newln => {
                    let r = Range::of(range.start, s.range.end);
                    return Ok(Ast::new(AstT::Empty, r));
                }
            },
            None => return Ok(Ast::new(AstT::Empty, range)),
        };

        let newln = parser.eat_newlns();
        while let Some(i) = parser.peek() {
            let op = match i {
                Item::Group(g) => {
                    if newln {
                        parser.back();
                        break;
                    }

                    let r = Range::between(lhs.range, g.range);
                    return Err(crate::Error::MissingOperator(r));
                }
                Item::Expr(e) => {
                    if newln {
                        parser.back();
                        break;
                    }

                    let r = Range::between(lhs.range, e.range);
                    return Err(crate::Error::MissingOperator(r));
                }
                Item::Fun(f) => {
                    if newln {
                        parser.back();
                        break;
                    }

                    let r = Range::between(lhs.range, f.range);
                    return Err(crate::Error::MissingOperator(r));
                }
                Item::Op(o) => o,
                Item::Mod(m) => {
                    if m.l_bp() < min_bp {
                        break;
                    } else {
                        parser.next();
                    }

                    let val_r = Range::span(lhs.range, m.range);
                    let val = match m.typ {
                        ModT::Degree => AstT::Degree(Box::new(lhs)),
                        ModT::Radian => AstT::Radian(Box::new(lhs)),
                        ModT::Factorial => AstT::Factorial(Box::new(lhs)),
                    };

                    lhs = Ast::new(val, val_r);
                    continue;
                }
                Item::Sep(s) => match s.typ {
                    SepT::Comma => return Err(crate::Error::UnexpectedSeparator(*s)),
                    SepT::Semi | SepT::Newln => break,
                },
            };

            let (l_bp, r_bp) = op.bp();
            if l_bp < min_bp {
                break;
            } else {
                parser.next();
            }

            if parser.peek().is_none() {
                let r = Range::pos(op.range.end);
                return Err(crate::Error::MissingOperand(r));
            }

            let rhs_r = Range::of(op.range.end, range.end);
            let rhs = self.parse_bp(parser, r_bp, rhs_r)?;

            let val_r = Range::span(lhs.range, rhs.range);
            let val = match op.typ {
                OpT::Assign => match lhs.typ {
                    AstT::Expr(e) => match e.typ {
                        ExprT::Var(id) => AstT::Assign(id, Box::new(rhs)),
                        _ => return Err(crate::Error::InvalidAssignment(lhs.range, op.range)),
                    },
                    _ => return Err(crate::Error::InvalidAssignment(lhs.range, op.range)),
                },
                OpT::Add => AstT::Add(Box::new(lhs), Box::new(rhs)),
                OpT::Sub => AstT::Sub(Box::new(lhs), Box::new(rhs)),
                OpT::Mul => AstT::Mul(Box::new(lhs), Box::new(rhs)),
                OpT::Div => AstT::Div(Box::new(lhs), Box::new(rhs)),
                OpT::IntDiv => AstT::IntDiv(Box::new(lhs), Box::new(rhs)),
                OpT::Rem => AstT::Rem(Box::new(lhs), Box::new(rhs)),
                OpT::Pow => AstT::Pow(Box::new(lhs), Box::new(rhs)),
                OpT::Eq => AstT::Eq(Box::new(lhs), Box::new(rhs)),
                OpT::Or => AstT::Or(Box::new(lhs), Box::new(rhs)),
                OpT::And => AstT::And(Box::new(lhs), Box::new(rhs)),
                OpT::BwOr => AstT::BwOr(Box::new(lhs), Box::new(rhs)),
                OpT::BwAnd => AstT::BwAnd(Box::new(lhs), Box::new(rhs)),
            };
            lhs = Ast::new(val, val_r);
        }

        Ok(lhs)
    }

    fn parse_fun(&mut self, parser: &mut Parser, fun: Fun) -> crate::Result<Ast> {
        match parser.next() {
            Some(Item::Group(g)) => {
                match g.par_kind {
                    crate::ParKind::Round => (),
                    _ => self
                        .warnings
                        .push(crate::Warning::ConfusingFunctionParentheses {
                            fun,
                            open_par: Range::pos(g.range.start),
                            close_par: Range::pos(g.range.end - 1),
                        }),
                }

                let f = match fun.typ {
                    FunT::Pow => {
                        let [base, exp] = self.parse_fun_args(&g.items, g.range)?;
                        AstT::Pow(Box::new(base), Box::new(exp))
                    }
                    FunT::Ln => {
                        let [n] = self.parse_fun_args(&g.items, g.range)?;
                        AstT::Ln(Box::new(n))
                    }
                    FunT::Log => {
                        let [base, n] = self.parse_fun_args(&g.items, g.range)?;
                        AstT::Log(Box::new(base), Box::new(n))
                    }
                    FunT::Sqrt => {
                        let [n] = self.parse_fun_args(&g.items, g.range)?;
                        AstT::Sqrt(Box::new(n))
                    }
                    FunT::Ncr => {
                        let [n, r] = self.parse_fun_args(&g.items, g.range)?;
                        AstT::Ncr(Box::new(n), Box::new(r))
                    }
                    FunT::Sin => {
                        let [n] = self.parse_fun_args(&g.items, g.range)?;
                        AstT::Sin(Box::new(n))
                    }
                    FunT::Cos => {
                        let [n] = self.parse_fun_args(&g.items, g.range)?;
                        AstT::Cos(Box::new(n))
                    }
                    FunT::Tan => {
                        let [n] = self.parse_fun_args(&g.items, g.range)?;
                        AstT::Tan(Box::new(n))
                    }
                    FunT::Asin => {
                        let [n] = self.parse_fun_args(&g.items, g.range)?;
                        AstT::Asin(Box::new(n))
                    }
                    FunT::Acos => {
                        let [n] = self.parse_fun_args(&g.items, g.range)?;
                        AstT::Acos(Box::new(n))
                    }
                    FunT::Atan => {
                        let [n] = self.parse_fun_args(&g.items, g.range)?;
                        AstT::Atan(Box::new(n))
                    }
                    FunT::Gcd => {
                        let [a, b] = self.parse_fun_args(&g.items, g.range)?;
                        AstT::Gcd(Box::new(a), Box::new(b))
                    }
                    FunT::Min => {
                        let args = self.parse_dyn_fun_args(2, usize::MAX, &g.items, g.range)?;
                        AstT::Min(args)
                    }
                    FunT::Max => {
                        let args = self.parse_dyn_fun_args(2, usize::MAX, &g.items, g.range)?;
                        AstT::Max(args)
                    }
                    FunT::Clamp => {
                        let [n, min, max] = self.parse_fun_args(&g.items, g.range)?;
                        AstT::Clamp(Box::new(n), Box::new(min), Box::new(max))
                    }
                    FunT::Print => {
                        let args = self.parse_dyn_fun_args(0, usize::MAX, &g.items, g.range)?;
                        AstT::Print(args)
                    }
                    FunT::Println => {
                        let args = self.parse_dyn_fun_args(0, usize::MAX, &g.items, g.range)?;
                        AstT::Println(args)
                    }
                    FunT::Spill => {
                        self.parse_fun_args::<0>(&g.items, g.range)?;
                        AstT::Spill
                    }
                    FunT::Assert => {
                        let [a] = self.parse_fun_args(&g.items, g.range)?;
                        AstT::Assert(Box::new(a))
                    }
                    FunT::AssertEq => {
                        let [a, b] = self.parse_fun_args(&g.items, g.range)?;
                        AstT::AssertEq(Box::new(a), Box::new(b))
                    }
                };
                let r = Range::span(fun.range, g.range);
                Ok(Ast::new(f, r))
            }
            _ => {
                let r = Range::pos(fun.range.end);
                Err(crate::Error::MissingFunctionParentheses(r))
            }
        }
    }

    fn parse_dyn_fun_args(
        &mut self,
        min: usize,
        max: usize,
        items: &[Item],
        range: Range,
    ) -> crate::Result<Vec<Ast>> {
        let arg_count = items.iter().filter(|i| i.is_sep()).count() + 1;
        let mut args = Vec::with_capacity(cmp::min(arg_count, max));
        let mut unexpected_args = Vec::new();
        let mut parsed_args = 0;
        let mut start = (0, range.start);
        let mut ti = 0;

        for i in items.iter() {
            if let Item::Sep(s) = i {
                match s.typ {
                    SepT::Comma => (),
                    SepT::Semi => self.warnings.push(crate::Warning::ConfusingSeparator {
                        sep: *s,
                        expected: SepT::Comma,
                    }),
                    SepT::Newln => continue,
                }

                let r = Range::of(start.1, s.range.start);
                if parsed_args < max {
                    let is = &items[(start.0)..ti];
                    let mut parser = Parser::new(is);
                    args.push(self.parse_bp(&mut parser, 0, r)?);
                } else {
                    unexpected_args.push(r);
                }
                start = (ti + 1, s.range.end);
                parsed_args += 1;
            }
            ti += 1;
        }

        if let Some(i) = items.last() {
            if !i.is_sep() {
                let r = Range::of(start.1, range.end);
                if parsed_args < max {
                    let is = &items[(start.0)..ti];
                    let mut parser = Parser::new(is);
                    args.push(self.parse_bp(&mut parser, 0, r)?);
                } else {
                    unexpected_args.push(r);
                }
                parsed_args += 1;
            }
        }

        if !unexpected_args.is_empty() {
            self.errors.push(crate::Error::UnexpectedFunctionArguments {
                ranges: unexpected_args,
                expected: max,
                found: parsed_args,
            });
        } else if parsed_args < min {
            let range = match items.last() {
                Some(i) => Range::of(i.range().end, range.end),
                None => Range::pos(range.end),
            };
            self.errors.push(crate::Error::MissingFunctionArguments {
                range,
                expected: min,
                found: parsed_args,
            });
        }

        Ok(args)
    }

    fn parse_fun_args<const COUNT: usize>(
        &mut self,
        items: &[Item],
        range: Range,
    ) -> crate::Result<[Ast; COUNT]> {
        let mut args: [Ast; COUNT] = array_of(|_| Ast::new(AstT::Error, range));
        let mut unexpected_args = Vec::new();
        let mut parsed_args = 0;
        let mut start = (0, range.start);
        let mut ti = 0;

        for i in items.iter() {
            if let Item::Sep(s) = i {
                match s.typ {
                    SepT::Comma => (),
                    SepT::Semi => self.warnings.push(crate::Warning::ConfusingSeparator {
                        sep: *s,
                        expected: SepT::Comma,
                    }),
                    SepT::Newln => continue,
                }

                let r = Range::of(start.1, s.range.start);
                if parsed_args < COUNT {
                    let is = &items[(start.0)..ti];
                    let mut parser = Parser::new(is);
                    args[parsed_args] = self.parse_bp(&mut parser, 0, r)?;
                } else {
                    unexpected_args.push(r);
                }
                start = (ti + 1, s.range.end);
                parsed_args += 1;
            }
            ti += 1;
        }

        if let Some(i) = items.last() {
            if !i.is_sep() {
                let r = Range::of(start.1, range.end);
                if parsed_args < COUNT {
                    let is = &items[(start.0)..ti];
                    let mut parser = Parser::new(is);
                    args[parsed_args] = self.parse_bp(&mut parser, 0, r)?;
                } else {
                    unexpected_args.push(r);
                }
                parsed_args += 1;
            }
        }

        if !unexpected_args.is_empty() {
            self.errors.push(crate::Error::UnexpectedFunctionArguments {
                ranges: unexpected_args,
                expected: COUNT,
                found: parsed_args,
            });
        } else if parsed_args < COUNT {
            let range = match items.last() {
                Some(i) => Range::of(i.range().end, range.end),
                None => Range::pos(range.end),
            };
            self.errors.push(crate::Error::MissingFunctionArguments {
                range,
                expected: COUNT,
                found: parsed_args,
            });
        }

        Ok(args)
    }
}

fn array_of<T, const SIZE: usize>(f: impl Fn(usize) -> T) -> [T; SIZE] {
    let mut arr: MaybeUninit<[T; SIZE]> = MaybeUninit::uninit();
    let mut ptr = arr.as_mut_ptr() as *mut T;

    for i in 0..SIZE {
        let elem = f(i);
        unsafe {
            ptr.write(elem);
            ptr = ptr.add(1);
        }
    }

    unsafe { arr.assume_init() }
}
