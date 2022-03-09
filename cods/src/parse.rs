use std::cmp::{self, Ordering};
use std::mem::MaybeUninit;

use crate::{
    items_range, CmdT, Context, Expr, ExprType, Item, ModT, Op, OpT, ParKind, Range, SepT, Sign,
    ValT, Warning,
};

impl Context {
    pub fn parse(&mut self, items: &[Item]) -> crate::Result<Vec<Expr>> {
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

    fn parse_items(&mut self, range: Range, items: &[Item]) -> crate::Result<Expr> {
        if items.is_empty() {
            return Ok(Expr::new(ExprType::Empty, range));
        } else if items.len() == 1 {
            let err = match &items[0] {
                Item::Group(g) => return self.parse_items(g.range, &g.items),
                Item::Val(n) => return Ok(Expr::val(*n)),
                Item::Op(o) => crate::Error::UnexpectedOperator(*o),
                Item::Cmd(c) => crate::Error::MissingOperand(Range::pos(c.range.end)),
                Item::Mod(m) => crate::Error::MissingOperand(Range::pos(m.range.start)),
                Item::Sep(s) => crate::Error::MissingOperand(Range::pos(s.range.start)),
            };
            self.errors.push(err);
            return Ok(Expr::new(ExprType::Error, range));
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
                        return Ok(Expr::new(
                            ExprType::Neg(Box::new(ca)),
                            Range::span(op.range, ra),
                        ));
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
            let expr_a = self.parse_items(range_a, a)?;

            let b = &items[(first_i + 1)..];
            let range_b = items_range(b).unwrap_or_else(|| Range::of(op.range.end, range.end));
            let expr_b = Box::new(self.parse_items(range_b, b)?);

            let r = Range::span(range_a, range_b);

            return match op.typ {
                OpT::Add | OpT::Sub => {
                    // all + and - signs/operators were accumulated
                    if sign.is_positive() {
                        Ok(Expr::new(ExprType::Add(Box::new(expr_a), expr_b), r))
                    } else {
                        Ok(Expr::new(ExprType::Sub(Box::new(expr_a), expr_b), r))
                    }
                }
                _ => {
                    // negate if nessecary
                    let expr_b = if sign.is_positive() {
                        expr_b
                    } else {
                        Box::new(Expr::new(ExprType::Neg(expr_b), range_b))
                    };
                    match op.typ {
                        OpT::Add | OpT::Sub => unreachable!(),
                        OpT::Mul => Ok(Expr::new(ExprType::Mul(Box::new(expr_a), expr_b), r)),
                        OpT::Div => Ok(Expr::new(ExprType::Div(Box::new(expr_a), expr_b), r)),
                        OpT::IntDiv => Ok(Expr::new(ExprType::IntDiv(Box::new(expr_a), expr_b), r)),
                        OpT::Rem => Ok(Expr::new(ExprType::Rem(Box::new(expr_a), expr_b), r)),
                        OpT::Pow => Ok(Expr::new(ExprType::Pow(Box::new(expr_a), expr_b), r)),
                        OpT::Equals => {
                            if let ExprType::Val(v) = expr_a.typ {
                                if let ValT::Var(id) = v.typ {
                                    return Ok(Expr::new(ExprType::Assignment(id, expr_b), r));
                                }
                            }
                            Err(crate::Error::InvalidAssignment(expr_a.range, op.range))
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
                return Ok(Expr::new(ExprType::Error, range));
            }

            return Ok(match m.typ {
                ModT::Degree => Expr::new(ExprType::Degree(ac), m.range),
                ModT::Factorial => Expr::new(ExprType::Factorial(ac), m.range),
            });
        }

        if items.len() == 2 {
            if let Item::Cmd(cmd) = items[0] {
                let g = match &items[1] {
                    Item::Group(g) => g,
                    i => {
                        let range = Range::of(cmd.range.end, i.range().start);
                        self.errors
                            .push(crate::Error::MissingCommandParenthesis(range));
                        return Ok(Expr::new(ExprType::Error, range));
                    }
                };

                if g.par_kind != ParKind::Round {
                    self.warnings
                        .push(crate::Warning::ConfusingCommandParentheses {
                            cmd,
                            open_par: Range::pos(g.range.start - 1),
                            close_par: Range::pos(g.range.end),
                        });
                }

                let r = Range::span(cmd.range, g.range);
                let cmd = match cmd.typ {
                    CmdT::Pow => {
                        let [base, exp] = self.parse_cmd_args(g.range, &g.items)?;
                        Expr::new(ExprType::Pow(Box::new(base), Box::new(exp)), r)
                    }
                    CmdT::Ln => {
                        let [val] = self.parse_cmd_args(g.range, &g.items)?;
                        Expr::new(ExprType::Ln(Box::new(val)), r)
                    }
                    CmdT::Log => {
                        let [base, val] = self.parse_cmd_args(g.range, &g.items)?;
                        Expr::new(ExprType::Log(Box::new(base), Box::new(val)), r)
                    }
                    CmdT::Sqrt => {
                        let [val] = self.parse_cmd_args(g.range, &g.items)?;
                        Expr::new(ExprType::Sqrt(Box::new(val)), r)
                    }
                    CmdT::Ncr => {
                        let [n, k] = self.parse_cmd_args(g.range, &g.items)?;
                        Expr::new(ExprType::Ncr(Box::new(n), Box::new(k)), r)
                    }
                    CmdT::Sin => {
                        let [val] = self.parse_cmd_args(g.range, &g.items)?;
                        Expr::new(ExprType::Sin(Box::new(val)), r)
                    }
                    CmdT::Cos => {
                        let [val] = self.parse_cmd_args(g.range, &g.items)?;
                        Expr::new(ExprType::Cos(Box::new(val)), r)
                    }
                    CmdT::Tan => {
                        let [val] = self.parse_cmd_args(g.range, &g.items)?;
                        Expr::new(ExprType::Tan(Box::new(val)), r)
                    }
                    CmdT::Asin => {
                        let [val] = self.parse_cmd_args(g.range, &g.items)?;
                        Expr::new(ExprType::Asin(Box::new(val)), r)
                    }
                    CmdT::Acos => {
                        let [val] = self.parse_cmd_args(g.range, &g.items)?;
                        Expr::new(ExprType::Acos(Box::new(val)), r)
                    }
                    CmdT::Atan => {
                        let [val] = self.parse_cmd_args(g.range, &g.items)?;
                        Expr::new(ExprType::Atan(Box::new(val)), r)
                    }
                    CmdT::Gcd => {
                        let [a, b] = self.parse_cmd_args(g.range, &g.items)?;
                        Expr::new(ExprType::Gcd(Box::new(a), Box::new(b)), r)
                    }
                    CmdT::Min => {
                        let args = self.parse_dyn_cmd_args(1, usize::MAX, g.range, &g.items)?;
                        Expr::new(ExprType::Min(args), r)
                    }
                    CmdT::Max => {
                        let args = self.parse_dyn_cmd_args(1, usize::MAX, g.range, &g.items)?;
                        Expr::new(ExprType::Max(args), r)
                    }
                    CmdT::Clamp => {
                        let [val, min, max] = self.parse_cmd_args(g.range, &g.items)?;
                        Expr::new(
                            ExprType::Clamp(Box::new(val), Box::new(min), Box::new(max)),
                            r,
                        )
                    }
                    CmdT::Print => {
                        let args = self.parse_dyn_cmd_args(0, usize::MAX, g.range, &g.items)?;
                        Expr::new(ExprType::Print(args), r)
                    }
                    CmdT::Println => {
                        let args = self.parse_dyn_cmd_args(0, usize::MAX, g.range, &g.items)?;
                        Expr::new(ExprType::Println(args), r)
                    }
                    CmdT::Spill => {
                        self.parse_cmd_args::<0>(g.range, &g.items)?;
                        Expr::new(ExprType::Spill, r)
                    }
                };

                return Ok(cmd);
            }
        }

        self.errors.push(crate::Error::MissingOperator(Range::of(
            items[0].range().end,
            items[1].range().start,
        )));
        Ok(Expr::new(ExprType::Error, range))
    }

    fn parse_dyn_cmd_args(
        &mut self,
        min: usize,
        max: usize,
        range: Range,
        items: &[Item],
    ) -> crate::Result<Vec<Expr>> {
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
            self.errors.push(crate::Error::UnexpectedCommandArguments {
                ranges: unexpected_args,
                expected: max,
                found: parsed_args,
            });
        } else if parsed_args < min {
            let range = match items.last() {
                Some(i) => Range::of(i.range().end, range.end),
                None => Range::pos(range.end),
            };
            self.errors.push(crate::Error::MissingCommandArguments {
                range,
                expected: min,
                found: parsed_args,
            });
        }

        Ok(args)
    }

    fn parse_cmd_args<const COUNT: usize>(
        &mut self,
        range: Range,
        items: &[Item],
    ) -> crate::Result<[Expr; COUNT]> {
        let mut args: [Expr; COUNT] = array_of(|_| Expr::new(ExprType::Error, range));
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
            self.errors.push(crate::Error::UnexpectedCommandArguments {
                ranges: unexpected_args,
                expected: COUNT,
                found: parsed_args,
            });
        } else if parsed_args < COUNT {
            let range = match items.last() {
                Some(i) => Range::of(i.range().end, range.end),
                None => Range::pos(range.end),
            };
            self.errors.push(crate::Error::MissingCommandArguments {
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
