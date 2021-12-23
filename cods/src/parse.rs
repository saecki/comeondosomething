use std::cmp::Ordering;
use std::mem::MaybeUninit;

use crate::{items_range, Calc, Cmd, Context, Item, Mod, Op, ParType, Range, Warning};

impl Context {
    pub fn parse(&mut self, items: &[Item]) -> crate::Result<Calc> {
        let r = items_range(items).unwrap_or_else(|| Range::pos(0));
        self.parse_items(r, items)
    }

    fn parse_items(&mut self, range: Range, items: &[Item]) -> crate::Result<Calc> {
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

        let mut ops: Vec<_> = items
            .iter()
            .enumerate()
            .filter_map(|(i, t)| t.op().map(|o| (i, o)))
            .collect();

        ops.sort_by(|(i1, t1), (i2, t2)| {
            if t1.priority() < t2.priority() {
                Ordering::Less
            } else if t1.priority() > t2.priority() {
                Ordering::Greater
            } else if i1 < i2 {
                Ordering::Less
            } else {
                Ordering::Greater
            }
        });

        if let Some(&(last_i, last_o)) = ops.last() {
            let mut i = last_i;
            let mut op = last_o;
            let mut sign = true;

            while let Some(s) = op.sign() {
                if s.is_negative() {
                    sign = !sign;
                }

                if i == 0 {
                    let a = &items[(last_i + 1)..];
                    let ra = items_range(a).unwrap_or_else(|| Range::of(op.range().end, range.end));
                    let ca = self.parse_items(ra, a)?;

                    if last_i != i {
                        let sign_range = Range::span(op.range(), last_o.range());
                        self.warnings.push(Warning::MultipleSigns(sign_range, sign));
                    }

                    if sign {
                        return Ok(ca);
                    } else {
                        return Ok(Calc::Neg(Box::new(ca), Range::span(op.range(), ra)));
                    }
                } else if let Some(o) = items[i - 1].op() {
                    i -= 1;
                    op = o;
                } else {
                    break;
                }
            }

            if last_i != i {
                let sign_range = Range::span(items[i + 1].range(), last_o.range());
                match op {
                    Op::Add(_) => self.warnings.push(Warning::SignFollowingAddition(
                        op.range(),
                        sign_range,
                        sign,
                        last_i - i,
                    )),
                    Op::Sub(_) => self.warnings.push(Warning::SignFollowingSubtraction(
                        op.range(),
                        sign_range,
                        sign,
                        last_i - i,
                    )),
                    Op::Mul(_) => (),
                    Op::Div(_) => (),
                    Op::Pow(_) => (),
                }
            }

            let a = &items[0..i];
            let ra = items_range(a).unwrap_or_else(|| Range::of(range.start, op.range().start));
            let ca = Box::new(self.parse_items(ra, a)?);

            let b = &items[(last_i + 1)..];
            let rb = items_range(b).unwrap_or_else(|| Range::of(op.range().end, range.end));
            let cb = Box::new(self.parse_items(rb, b)?);

            return match op {
                Op::Add(_) | Op::Sub(_) => {
                    if sign {
                        Ok(Calc::Add(ca, cb))
                    } else {
                        Ok(Calc::Sub(ca, cb))
                    }
                }
                _ => {
                    let cb = if sign {
                        cb
                    } else {
                        Box::new(Calc::Neg(cb, rb))
                    };
                    match op {
                        Op::Add(_) | Op::Sub(_) => unreachable!(),
                        Op::Mul(_) => Ok(Calc::Mul(ca, cb)),
                        Op::Div(_) => Ok(Calc::Div(ca, cb)),
                        Op::Pow(_) => Ok(Calc::Pow(ca, cb, Range::span(ra, rb))),
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
        items: &[Item],
    ) -> crate::Result<[Calc; COUNT]> {
        let mut args: [Calc; COUNT] = array_of(|_| Calc::Error(range));
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
