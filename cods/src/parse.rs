use std::cmp::{self, Ordering};
use std::mem::MaybeUninit;

use crate::{
    items_range, Calc, CalcType, CmdType, Context, Ext, Item, ModType, Op, OpType, ParKind,
    Provider, Range, SepType, Sign, Val, Warning,
};

impl<T: Ext, P: Provider<T>> Context<T, P> {
    pub fn parse(&mut self, items: &[Item<T>]) -> crate::Result<Vec<Calc<T>>, T> {
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

    fn parse_items(&mut self, range: Range, items: &[Item<T>]) -> crate::Result<Calc<T>, T> {
        if items.is_empty() {
            return Ok(Calc::new(CalcType::Empty, range));
        } else if items.len() == 1 {
            let err = match &items[0] {
                Item::Group(g) => return self.parse_items(g.range, &g.items),
                Item::Num(n) => return Ok(Calc::num(*n)),
                Item::Op(o) => crate::Error::UnexpectedOperator(*o),
                Item::Cmd(c) => crate::Error::MissingOperand(Range::pos(c.range.end)),
                Item::Mod(m) => crate::Error::MissingOperand(Range::pos(m.range.start)),
                Item::Sep(s) => crate::Error::MissingOperand(Range::pos(s.range.start)),
            };
            self.errors.push(err);
            return Ok(Calc::new(CalcType::Error, range));
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
                        return Ok(Calc::new(
                            CalcType::Neg(Box::new(ca)),
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
                    OpType::Add => self.warnings.push(Warning::SignFollowingAddition(
                        op.range,
                        sign_range,
                        sign,
                        first_i - i,
                    )),
                    OpType::Sub => self.warnings.push(Warning::SignFollowingSubtraction(
                        op.range,
                        sign_range,
                        sign,
                        first_i - i,
                    )),
                    OpType::Mul => (),
                    OpType::Div => (),
                    OpType::IntDiv => (),
                    OpType::Rem => (),
                    OpType::Pow => (),
                    OpType::Equals => (),
                }
            }

            let a = &items[0..i];
            let range_a = items_range(a).unwrap_or_else(|| Range::of(range.start, op.range.start));
            let calc_a = self.parse_items(range_a, a)?;

            let b = &items[(first_i + 1)..];
            let range_b = items_range(b).unwrap_or_else(|| Range::of(op.range.end, range.end));
            let calc_b = Box::new(self.parse_items(range_b, b)?);

            let r = Range::span(range_a, range_b);

            return match op.typ {
                OpType::Add | OpType::Sub => {
                    // all + and - signs/operators were accumulated
                    if sign.is_positive() {
                        Ok(Calc::new(CalcType::Add(Box::new(calc_a), calc_b), r))
                    } else {
                        Ok(Calc::new(CalcType::Sub(Box::new(calc_a), calc_b), r))
                    }
                }
                _ => {
                    // negate if nessecary
                    let calc_b = if sign.is_positive() {
                        calc_b
                    } else {
                        Box::new(Calc::new(CalcType::Neg(calc_b), range_b))
                    };
                    match op.typ {
                        OpType::Add | OpType::Sub => unreachable!(),
                        OpType::Mul => Ok(Calc::new(CalcType::Mul(Box::new(calc_a), calc_b), r)),
                        OpType::Div => Ok(Calc::new(CalcType::Div(Box::new(calc_a), calc_b), r)),
                        OpType::IntDiv => {
                            Ok(Calc::new(CalcType::IntDiv(Box::new(calc_a), calc_b), r))
                        }
                        OpType::Rem => Ok(Calc::new(CalcType::Rem(Box::new(calc_a), calc_b), r)),
                        OpType::Pow => Ok(Calc::new(CalcType::Pow(Box::new(calc_a), calc_b), r)),
                        OpType::Equals => {
                            if let CalcType::Num(n) = calc_a.typ {
                                if let Val::Var(id) = n.val {
                                    return Ok(Calc::new(CalcType::Assignment(id, calc_b), r));
                                }
                            }
                            Err(crate::Error::InvalidAssignment(calc_a.range, op.range))
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
                return Ok(Calc::new(CalcType::Error, range));
            }

            return Ok(match m.typ {
                ModType::Degree => Calc::new(CalcType::Degree(ac), m.range),
                ModType::Factorial => Calc::new(CalcType::Factorial(ac), m.range),
            });
        }

        if items.len() == 2 {
            if let Item::Cmd(cmd) = items[0] {
                return match &items[1] {
                    Item::Group(g) => {
                        if g.par_kind != ParKind::Round {
                            self.warnings
                                .push(crate::Warning::ConfusingCommandParentheses {
                                    cmd,
                                    open_par: Range::pos(g.range.start - 1),
                                    close_par: Range::pos(g.range.end),
                                });
                        }

                        let r = Range::span(cmd.range, g.range);
                        Ok(match cmd.typ {
                            CmdType::Pow => {
                                let [base, exp] = self.parse_cmd_args(g.range, &g.items)?;
                                Calc::new(CalcType::Pow(Box::new(base), Box::new(exp)), r)
                            }
                            CmdType::Ln => {
                                let [val] = self.parse_cmd_args(g.range, &g.items)?;
                                Calc::new(CalcType::Ln(Box::new(val)), r)
                            }
                            CmdType::Log => {
                                let [base, val] = self.parse_cmd_args(g.range, &g.items)?;
                                Calc::new(CalcType::Log(Box::new(base), Box::new(val)), r)
                            }
                            CmdType::Sqrt => {
                                let [val] = self.parse_cmd_args(g.range, &g.items)?;
                                Calc::new(CalcType::Sqrt(Box::new(val)), r)
                            }
                            CmdType::Ncr => {
                                let [n, k] = self.parse_cmd_args(g.range, &g.items)?;
                                Calc::new(CalcType::Ncr(Box::new(n), Box::new(k)), r)
                            }
                            CmdType::Sin => {
                                let [val] = self.parse_cmd_args(g.range, &g.items)?;
                                Calc::new(CalcType::Sin(Box::new(val)), r)
                            }
                            CmdType::Cos => {
                                let [val] = self.parse_cmd_args(g.range, &g.items)?;
                                Calc::new(CalcType::Cos(Box::new(val)), r)
                            }
                            CmdType::Tan => {
                                let [val] = self.parse_cmd_args(g.range, &g.items)?;
                                Calc::new(CalcType::Tan(Box::new(val)), r)
                            }
                            CmdType::Asin => {
                                let [val] = self.parse_cmd_args(g.range, &g.items)?;
                                Calc::new(CalcType::Asin(Box::new(val)), r)
                            }
                            CmdType::Acos => {
                                let [val] = self.parse_cmd_args(g.range, &g.items)?;
                                Calc::new(CalcType::Acos(Box::new(val)), r)
                            }
                            CmdType::Atan => {
                                let [val] = self.parse_cmd_args(g.range, &g.items)?;
                                Calc::new(CalcType::Atan(Box::new(val)), r)
                            }
                            CmdType::Gcd => {
                                let [a, b] = self.parse_cmd_args(g.range, &g.items)?;
                                Calc::new(CalcType::Gcd(Box::new(a), Box::new(b)), r)
                            }
                            CmdType::Min => {
                                let args =
                                    self.parse_dyn_cmd_args(1, usize::MAX, g.range, &g.items)?;
                                Calc::new(CalcType::Min(args), r)
                            }
                            CmdType::Max => {
                                let args =
                                    self.parse_dyn_cmd_args(1, usize::MAX, g.range, &g.items)?;
                                Calc::new(CalcType::Max(args), r)
                            }
                            CmdType::Clamp => {
                                let [val, min, max] = self.parse_cmd_args(g.range, &g.items)?;
                                Calc::new(
                                    CalcType::Clamp(Box::new(val), Box::new(min), Box::new(max)),
                                    r,
                                )
                            }
                        })
                    }
                    i => {
                        let range = Range::of(cmd.range.end, i.range().start);
                        self.errors
                            .push(crate::Error::MissingCommandParenthesis(range));
                        Ok(Calc::new(CalcType::Error, range))
                    }
                };
            }
        }

        self.errors.push(crate::Error::MissingOperator(Range::of(
            items[0].range().end,
            items[1].range().start,
        )));
        Ok(Calc::new(CalcType::Error, range))
    }

    fn parse_dyn_cmd_args(
        &mut self,
        min: usize,
        max: usize,
        range: Range,
        items: &[Item<T>],
    ) -> crate::Result<Vec<Calc<T>>, T> {
        let arg_count = items.iter().filter(|i| i.is_sep()).count() + 1;
        let mut args = Vec::with_capacity(cmp::min(arg_count, max));
        let mut unexpected_args = Vec::new();
        let mut parsed_args = 0;
        let mut start = (0, range.start);
        let mut ti = 0;

        for i in items.iter() {
            if let Item::Sep(s) = i {
                match s.typ {
                    SepType::Comma => (),
                    SepType::Semi => self.warnings.push(crate::Warning::ConfusingSeparator {
                        sep: *s,
                        expected: SepType::Comma,
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
        items: &[Item<T>],
    ) -> crate::Result<[Calc<T>; COUNT], T> {
        let mut args: [Calc<T>; COUNT] = array_of(|_| Calc::new(CalcType::Error, range));
        let mut unexpected_args = Vec::new();
        let mut parsed_args = 0;
        let mut start = (0, range.start);
        let mut ti = 0;

        for i in items.iter() {
            if let Item::Sep(s) = i {
                match s.typ {
                    SepType::Comma => (),
                    SepType::Semi => self.warnings.push(crate::Warning::ConfusingSeparator {
                        sep: *s,
                        expected: SepType::Comma,
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
