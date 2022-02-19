use std::cmp::Ordering;
use std::mem::MaybeUninit;

use crate::{items_range, Calc, Cmd, Context, Item, Mod, Op, ParType, Range, Sign, Var, Warning};

impl<T: Var> Context<T> {
    pub fn parse(&mut self, items: &[Item<T>]) -> crate::Result<Calc<T>, T> {
        let r = items_range(items).unwrap_or_else(|| Range::pos(0));
        self.parse_items(r, items)
    }

    fn parse_items(&mut self, range: Range, items: &[Item<T>]) -> crate::Result<Calc<T>, T> {
        if items.is_empty() {
            self.errors.push(crate::Error::MissingOperand(range));
            return Ok(Calc::Error(range));
        } else if items.len() == 1 {
            let err = match &items[0] {
                Item::Group(g) => return self.parse_items(g.range, &g.items),
                Item::Num(n) => return Ok(Calc::Num(*n)),
                Item::Op(o) => crate::Error::UnexpectedOperator(*o),
                Item::Cmd(c) => crate::Error::MissingOperand(Range::pos(c.range().end)),
                Item::Mod(m) => crate::Error::MissingOperand(Range::pos(m.range().start)),
                Item::Sep(s) => crate::Error::MissingOperand(Range::pos(s.range().start)),
            };
            self.errors.push(err);
            return Ok(Calc::Error(range));
        }

        let mut ops: Vec<(usize, Op)> = items
            .iter()
            .enumerate()
            .filter_map(|(i, t)| t.op().map(|o| (i, o)))
            .collect();
        ops.sort_unstable_by(|(i1, o1), (i2, o2)| {
            if o1.priority() > o2.priority() {
                // greater priority -> to the start
                Ordering::Less
            } else if o1.priority() < o2.priority() {
                // lower priority -> to the end
                Ordering::Greater
            } else {
                i1.cmp(i2)
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
                    let ra = items_range(a).unwrap_or_else(|| Range::of(op.range().end, range.end));
                    let ca = self.parse_items(ra, a)?;

                    if first_i != i {
                        let sign_range = Range::span(op.range(), first_o.range());
                        self.warnings.push(Warning::MultipleSigns(sign_range, sign));
                    }

                    if sign.is_positive() {
                        return Ok(ca);
                    } else {
                        return Ok(Calc::Neg(Box::new(ca), Range::span(op.range(), ra)));
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
                let sign_range = Range::span(items[i + 1].range(), first_o.range());
                match op {
                    Op::Add(_) => self.warnings.push(Warning::SignFollowingAddition(
                        op.range(),
                        sign_range,
                        sign,
                        first_i - i,
                    )),
                    Op::Sub(_) => self.warnings.push(Warning::SignFollowingSubtraction(
                        op.range(),
                        sign_range,
                        sign,
                        first_i - i,
                    )),
                    Op::Mul(_) => (),
                    Op::Div(_) => (),
                    Op::Pow(_) => (),
                }
            }

            let a = &items[0..i];
            let range_a =
                items_range(a).unwrap_or_else(|| Range::of(range.start, op.range().start));
            let calc_a = Box::new(self.parse_items(range_a, a)?);

            let b = &items[(first_i + 1)..];
            let range_b = items_range(b).unwrap_or_else(|| Range::of(op.range().end, range.end));
            let calc_b = Box::new(self.parse_items(range_b, b)?);

            return match op {
                Op::Add(_) | Op::Sub(_) => {
                    // all + and - signs/operators were accumulated
                    if sign.is_positive() {
                        Ok(Calc::Add(calc_a, calc_b))
                    } else {
                        Ok(Calc::Sub(calc_a, calc_b))
                    }
                }
                _ => {
                    // negate if nessecary
                    let calc_b = if sign.is_positive() {
                        calc_b
                    } else {
                        Box::new(Calc::Neg(calc_b, range_b))
                    };
                    match op {
                        Op::Add(_) | Op::Sub(_) => unreachable!(),
                        Op::Mul(_) => Ok(Calc::Mul(calc_a, calc_b)),
                        Op::Div(_) => Ok(Calc::Div(calc_a, calc_b)),
                        Op::Pow(_) => Ok(Calc::Pow(calc_a, calc_b, Range::span(range_a, range_b))),
                    }
                }
            };
        }

        let modifier = items
            .iter()
            .enumerate()
            .find_map(|(i, t)| t.modifier().map(|o| (i, o)));

        if let Some((i, m)) = modifier {
            let a = &items[0..i];
            let ar = items_range(a).unwrap_or_else(|| Range::of(range.start, m.range().start));
            let ac = Box::new(self.parse_items(ar, a)?);

            if let Some(i) = items.get(i + 1) {
                let r = Range::between(m.range(), i.range());
                self.errors.push(crate::Error::MissingOperator(r));
                return Ok(Calc::Error(range));
            }

            return Ok(match m {
                Mod::Degree(r) => Calc::Degree(ac, r),
                Mod::Factorial(r) => Calc::Factorial(ac, r),
            });
        }

        if items.len() == 2 {
            if let Item::Cmd(cmd) = items[0] {
                return match &items[1] {
                    Item::Group(g) => {
                        if g.par_type != ParType::Round {
                            self.warnings
                                .push(crate::Warning::ConfusingCommandParentheses {
                                    cmd,
                                    open_par: Range::pos(g.range.start - 1),
                                    close_par: Range::pos(g.range.end),
                                });
                        }

                        let r = Range::span(cmd.range(), g.range);
                        Ok(match cmd {
                            Cmd::Pow(_) => {
                                let [base, exp] = self.parse_cmd_args(g.range, &g.items)?;
                                Calc::Pow(Box::new(base), Box::new(exp), r)
                            }
                            Cmd::Ln(_) => {
                                let [val] = self.parse_cmd_args(g.range, &g.items)?;
                                Calc::Ln(Box::new(val), r)
                            }
                            Cmd::Log(_) => {
                                let [base, val] = self.parse_cmd_args(g.range, &g.items)?;
                                Calc::Log(Box::new(base), Box::new(val), r)
                            }
                            Cmd::Sqrt(_) => {
                                let [val] = self.parse_cmd_args(g.range, &g.items)?;
                                Calc::Sqrt(Box::new(val), r)
                            }
                            Cmd::Ncr(_) => {
                                let [n, k] = self.parse_cmd_args(g.range, &g.items)?;
                                Calc::Ncr(Box::new(n), Box::new(k), r)
                            }
                            Cmd::Sin(_) => {
                                let [val] = self.parse_cmd_args(g.range, &g.items)?;
                                Calc::Sin(Box::new(val), r)
                            }
                            Cmd::Cos(_) => {
                                let [val] = self.parse_cmd_args(g.range, &g.items)?;
                                Calc::Cos(Box::new(val), r)
                            }
                            Cmd::Tan(_) => {
                                let [val] = self.parse_cmd_args(g.range, &g.items)?;
                                Calc::Tan(Box::new(val), r)
                            }
                            Cmd::Asin(_) => {
                                let [val] = self.parse_cmd_args(g.range, &g.items)?;
                                Calc::Asin(Box::new(val), r)
                            }
                            Cmd::Acos(_) => {
                                let [val] = self.parse_cmd_args(g.range, &g.items)?;
                                Calc::Acos(Box::new(val), r)
                            }
                            Cmd::Atan(_) => {
                                let [val] = self.parse_cmd_args(g.range, &g.items)?;
                                Calc::Atan(Box::new(val), r)
                            }
                        })
                    }
                    i => {
                        let range = Range::of(cmd.range().end, i.range().start);
                        self.errors
                            .push(crate::Error::MissingCommandParenthesis(range));
                        Ok(Calc::Error(range))
                    }
                };
            }
        }

        self.errors.push(crate::Error::MissingOperator(Range::of(
            items[0].range().end,
            items[1].range().start,
        )));
        Ok(Calc::Error(range))
    }

    fn parse_cmd_args<const COUNT: usize>(
        &mut self,
        range: Range,
        items: &[Item<T>],
    ) -> crate::Result<[Calc<T>; COUNT], T> {
        let mut args: [Calc<T>; COUNT] = array_of(|_| Calc::Error(range));
        let mut unexpected_args = Vec::new();
        let mut parsed_args = 0;
        let mut start = (0, range.start);
        let mut ti = 0;

        for i in items.iter() {
            if let Some(s) = i.sep() {
                let r = Range::of(start.1, s.range().start);
                if parsed_args < COUNT {
                    let is = &items[(start.0)..ti];
                    args[parsed_args] = self.parse_items(r, is)?;
                } else {
                    unexpected_args.push(r);
                }
                start = (ti + 1, s.range().end);
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
