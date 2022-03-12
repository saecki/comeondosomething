use std::cmp::{self, Ordering};
use std::mem::MaybeUninit;

use crate::{
    items_range, Ast, AstT, Context, ExprT, FunT, Item, ModT, Op, OpT, ParKind, Range, SepT, Sign,
    Warning,
};

impl Context {
    pub fn parse(&mut self, items: &[Item]) -> crate::Result<Vec<Ast>> {
        items
            .split(|i| match i {
                Item::Sep(s) => s.is_semi(),
                _ => false,
            })
            .map(|i| {
                // TODO: determine range of empty items by looking at seperators
                let r = items_range(i).unwrap_or_else(|| Range::pos(0));
                self.parse_items(r, i)
            })
            .collect()
    }

    fn parse_items(&mut self, range: Range, items: &[Item]) -> crate::Result<Ast> {
        if items.is_empty() {
            return Ok(Ast::new(AstT::Empty, range));
        } else if items.len() == 1 {
            let err = match &items[0] {
                Item::Group(g) => return self.parse_items(g.range, &g.items),
                Item::Val(n) => return Ok(Ast::val(*n)),
                Item::Op(o) => crate::Error::UnexpectedOperator(*o),
                Item::Fun(f) => crate::Error::MissingFunctionParentheses(Range::pos(f.range.end)),
                Item::Mod(m) => crate::Error::MissingOperand(Range::pos(m.range.start)),
                Item::Sep(s) => crate::Error::UnexpectedSeparator(*s),
            };
            self.errors.push(err);
            return Ok(Ast::new(AstT::Error, range));
        }

        let mut ops: Vec<(usize, Op)> = items
            .iter()
            .enumerate()
            .filter_map(|(i, t)| t.as_op().map(|o| (i, o)))
            .collect();
        ops.sort_unstable_by(|(i1, o1), (i2, o2)| {
            match o1.priority().cmp(&o2.priority()) {
                // greater priority -> to the start
                Ordering::Greater => Ordering::Less,
                // lower priority -> to the end
                Ordering::Less => Ordering::Greater,
                Ordering::Equal => i1.cmp(i2),
            }
        });

        let mut op_iter = ops.iter().rev();

        if let Some(&(mut first_i, mut first_o)) = op_iter.next() {
            // at least one operator is present
            let mut i = first_i;
            let mut op = first_o;
            let mut sign = Sign::Positive;

            while let Some(s) = op.as_sign() {
                if s.is_negative() {
                    sign = !sign;
                }

                if i == 0 {
                    // operator is the first item -> sign
                    let a = &items[(first_i + 1)..];
                    let ra = items_range(a).unwrap_or_else(|| Range::of(op.range.end, range.end));
                    let ca = self.parse_items(ra, a)?;

                    if first_i != i {
                        let sign_range = Range::span(op.range, first_o.range);
                        self.warnings.push(Warning::MultipleSigns(sign_range, sign));
                    }

                    if sign.is_positive() {
                        return Ok(ca);
                    } else {
                        return Ok(Ast::new(AstT::Neg(Box::new(ca)), Range::span(op.range, ra)));
                    }
                } else if items[i - 1].is_op() {
                    // adjacent item is an operator
                    if let Some(&(next_i, next_o)) = op_iter.next() {
                        if next_i != i - 1 {
                            // adjacent operator isn't next in line -> reset state
                            first_i = next_i;
                            first_o = next_o;
                            i = next_i;
                            op = next_o;
                            sign = Sign::Positive;
                        } else {
                            i = next_i;
                            op = next_o;
                        }
                    }
                } else {
                    // neither the first item, nor following another operator -> seems like an operator
                    break;
                }
            }

            if first_i != i {
                let sign_range = Range::span(items[i + 1].range(), first_o.range);
                match op.typ {
                    OpT::Add => self.warnings.push(Warning::SignFollowingAddition(
                        op.range,
                        sign_range,
                        sign,
                        first_i - i,
                    )),
                    OpT::Sub => self.warnings.push(Warning::SignFollowingSubtraction(
                        op.range,
                        sign_range,
                        sign,
                        first_i - i,
                    )),
                    OpT::Mul => (),
                    OpT::Div => (),
                    OpT::IntDiv => (),
                    OpT::Rem => (),
                    OpT::Pow => (),
                    OpT::Equals => (),
                }
            }

            let a = &items[0..i];
            let range_a = items_range(a).unwrap_or_else(|| Range::of(range.start, op.range.start));
            let ast_a = self.parse_items(range_a, a)?;

            let b = &items[(first_i + 1)..];
            let range_b = items_range(b).unwrap_or_else(|| Range::of(op.range.end, range.end));
            let ast_b = Box::new(self.parse_items(range_b, b)?);

            let r = Range::span(range_a, range_b);

            return match op.typ {
                OpT::Add | OpT::Sub => {
                    // all + and - signs/operators were accumulated
                    if sign.is_positive() {
                        Ok(Ast::new(AstT::Add(Box::new(ast_a), ast_b), r))
                    } else {
                        Ok(Ast::new(AstT::Sub(Box::new(ast_a), ast_b), r))
                    }
                }
                _ => {
                    // negate if nessecary
                    let ast_b = if sign.is_positive() {
                        ast_b
                    } else {
                        Box::new(Ast::new(AstT::Neg(ast_b), range_b))
                    };
                    match op.typ {
                        OpT::Add | OpT::Sub => unreachable!(),
                        OpT::Mul => Ok(Ast::new(AstT::Mul(Box::new(ast_a), ast_b), r)),
                        OpT::Div => Ok(Ast::new(AstT::Div(Box::new(ast_a), ast_b), r)),
                        OpT::IntDiv => Ok(Ast::new(AstT::IntDiv(Box::new(ast_a), ast_b), r)),
                        OpT::Rem => Ok(Ast::new(AstT::Rem(Box::new(ast_a), ast_b), r)),
                        OpT::Pow => Ok(Ast::new(AstT::Pow(Box::new(ast_a), ast_b), r)),
                        OpT::Equals => {
                            if let AstT::Expr(v) = ast_a.typ {
                                if let ExprT::Var(id) = v.typ {
                                    return Ok(Ast::new(AstT::Assignment(id, ast_b), r));
                                }
                            }
                            Err(crate::Error::InvalidAssignment(ast_a.range, op.range))
                        }
                    }
                }
            };
        }

        let modifier = items
            .iter()
            .enumerate()
            .find_map(|(i, t)| t.as_mod().map(|o| (i, o)));

        if let Some((i, m)) = modifier {
            let a = &items[0..i];
            let ar = items_range(a).unwrap_or_else(|| Range::of(range.start, m.range.start));
            let ac = Box::new(self.parse_items(ar, a)?);

            if let Some(i) = items.get(i + 1) {
                let r = Range::between(m.range, i.range());
                self.errors.push(crate::Error::MissingOperator(r));
                return Ok(Ast::new(AstT::Error, range));
            }

            return Ok(match m.typ {
                ModT::Degree => Ast::new(AstT::Degree(ac), range),
                ModT::Radian => Ast::new(AstT::Radian(ac), range),
                ModT::Factorial => Ast::new(AstT::Factorial(ac), range),
            });
        }

        if items.len() == 2 {
            if let Item::Fun(fun) = items[0] {
                let g = match &items[1] {
                    Item::Group(g) => g,
                    i => {
                        let range = Range::of(fun.range.end, i.range().start);
                        self.errors
                            .push(crate::Error::MissingFunctionParentheses(range));
                        return Ok(Ast::new(AstT::Error, range));
                    }
                };

                if g.par_kind != ParKind::Round {
                    self.warnings
                        .push(crate::Warning::ConfusingFunctionParentheses {
                            fun,
                            open_par: Range::pos(g.range.start - 1),
                            close_par: Range::pos(g.range.end),
                        });
                }

                let r = Range::span(fun.range, g.range);
                let fun = match fun.typ {
                    FunT::Pow => {
                        let [base, exp] = self.parse_fun_args(g.range, &g.items)?;
                        Ast::new(AstT::Pow(Box::new(base), Box::new(exp)), r)
                    }
                    FunT::Ln => {
                        let [val] = self.parse_fun_args(g.range, &g.items)?;
                        Ast::new(AstT::Ln(Box::new(val)), r)
                    }
                    FunT::Log => {
                        let [base, val] = self.parse_fun_args(g.range, &g.items)?;
                        Ast::new(AstT::Log(Box::new(base), Box::new(val)), r)
                    }
                    FunT::Sqrt => {
                        let [val] = self.parse_fun_args(g.range, &g.items)?;
                        Ast::new(AstT::Sqrt(Box::new(val)), r)
                    }
                    FunT::Ncr => {
                        let [n, k] = self.parse_fun_args(g.range, &g.items)?;
                        Ast::new(AstT::Ncr(Box::new(n), Box::new(k)), r)
                    }
                    FunT::Sin => {
                        let [val] = self.parse_fun_args(g.range, &g.items)?;
                        Ast::new(AstT::Sin(Box::new(val)), r)
                    }
                    FunT::Cos => {
                        let [val] = self.parse_fun_args(g.range, &g.items)?;
                        Ast::new(AstT::Cos(Box::new(val)), r)
                    }
                    FunT::Tan => {
                        let [val] = self.parse_fun_args(g.range, &g.items)?;
                        Ast::new(AstT::Tan(Box::new(val)), r)
                    }
                    FunT::Asin => {
                        let [val] = self.parse_fun_args(g.range, &g.items)?;
                        Ast::new(AstT::Asin(Box::new(val)), r)
                    }
                    FunT::Acos => {
                        let [val] = self.parse_fun_args(g.range, &g.items)?;
                        Ast::new(AstT::Acos(Box::new(val)), r)
                    }
                    FunT::Atan => {
                        let [val] = self.parse_fun_args(g.range, &g.items)?;
                        Ast::new(AstT::Atan(Box::new(val)), r)
                    }
                    FunT::Gcd => {
                        let [a, b] = self.parse_fun_args(g.range, &g.items)?;
                        Ast::new(AstT::Gcd(Box::new(a), Box::new(b)), r)
                    }
                    FunT::Min => {
                        let args = self.parse_dyn_fun_args(1, usize::MAX, g.range, &g.items)?;
                        Ast::new(AstT::Min(args), r)
                    }
                    FunT::Max => {
                        let args = self.parse_dyn_fun_args(1, usize::MAX, g.range, &g.items)?;
                        Ast::new(AstT::Max(args), r)
                    }
                    FunT::Clamp => {
                        let [val, min, max] = self.parse_fun_args(g.range, &g.items)?;
                        Ast::new(AstT::Clamp(Box::new(val), Box::new(min), Box::new(max)), r)
                    }
                    FunT::Print => {
                        let args = self.parse_dyn_fun_args(0, usize::MAX, g.range, &g.items)?;
                        Ast::new(AstT::Print(args), r)
                    }
                    FunT::Println => {
                        let args = self.parse_dyn_fun_args(0, usize::MAX, g.range, &g.items)?;
                        Ast::new(AstT::Println(args), r)
                    }
                    FunT::Spill => {
                        self.parse_fun_args::<0>(g.range, &g.items)?;
                        Ast::new(AstT::Spill, r)
                    }
                };

                return Ok(fun);
            }
        }

        self.errors.push(crate::Error::MissingOperator(Range::of(
            items[0].range().end,
            items[1].range().start,
        )));
        Ok(Ast::new(AstT::Error, range))
    }

    fn parse_dyn_fun_args(
        &mut self,
        min: usize,
        max: usize,
        range: Range,
        items: &[Item],
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
                }

                let r = Range::of(start.1, s.range.start);
                if parsed_args < max {
                    let is = &items[(start.0)..ti];
                    args.push(self.parse_items(r, is)?);
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
                    args.push(self.parse_items(r, is)?);
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
        range: Range,
        items: &[Item],
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
                }

                let r = Range::of(start.1, s.range.start);
                if parsed_args < COUNT {
                    let is = &items[(start.0)..ti];
                    args[parsed_args] = self.parse_items(r, is)?;
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
                    args[parsed_args] = self.parse_items(r, is)?;
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
