use crate::{items_range, Ast, AstT, BindingPower, Context, Item, OpT, Range, Sign};

struct Parser<'a> {
    items: &'a [Item],
    cursor: usize,
}

impl<'a> Parser<'a> {
    fn new(items: &'a [Item]) -> Self {
        Self { items, cursor: 0 }
    }

    fn next(&mut self) -> Option<&'a Item> {
        let i = self.peek();
        self.cursor += 1;
        i
    }

    fn peek(&mut self) -> Option<&'a Item> {
        self.items.get(self.cursor)
    }
}

const R: Range = Range::pos(0); // TODO

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
                let mut parser = Parser::new(items);
                self.parse_bp(&mut parser, 0)
            })
            .collect()
    }

    fn parse_bp(&mut self, parser: &mut Parser<'_>, min_bp: u8) -> crate::Result<Ast> {
        let mut lhs = match parser.next() {
            Some(Item::Group(g)) => todo!(),
            Some(Item::Expr(e)) => Ast::new(AstT::Expr(*e), R),
            Some(Item::Op(o)) => match o.as_sign() {
                Some(s) => {
                    let val = match parser.next() {
                        Some(Item::Group(g)) => todo!(),
                        Some(Item::Expr(e)) => Ast::new(AstT::Expr(*e), R),
                        Some(Item::Op(o)) => todo!(),
                        Some(Item::Mod(m)) => todo!(),
                        Some(Item::Fun(f)) => todo!(),
                        Some(Item::Sep(s)) => todo!(),
                        None => todo!()
                    };

                    Ast::new(AstT::Neg(Box::new(val)), R)
                },
                None => return Err(crate::Error::UnexpectedOperator(*o)),
            },
            Some(Item::Mod(m)) => todo!(),
            Some(Item::Fun(f)) => todo!(),
            Some(Item::Sep(s)) => todo!(),
            None => return Ok(Ast::new(AstT::Empty, R)),
        };

        while let Some(i) = parser.peek() {
            let op = match i {
                Item::Group(g) => todo!(),
                Item::Expr(e) => todo!(),
                Item::Op(o) => o,
                Item::Mod(m) => todo!(),
                Item::Fun(f) => todo!(),
                Item::Sep(s) => todo!(),
            };

            let (l_bp, r_bp) = op.bp();
            if l_bp < min_bp {
                break;
            }

            parser.next();
            let rhs = self.parse_bp(parser, r_bp)?;

            lhs = match op.typ {
                OpT::Add => Ast::new(AstT::Add(Box::new(lhs), Box::new(rhs)), R),
                OpT::Sub => Ast::new(AstT::Sub(Box::new(lhs), Box::new(rhs)), R),
                OpT::Mul => Ast::new(AstT::Mul(Box::new(lhs), Box::new(rhs)), R),
                OpT::Div => Ast::new(AstT::Mul(Box::new(lhs), Box::new(rhs)), R),
                OpT::IntDiv => Ast::new(AstT::Mul(Box::new(lhs), Box::new(rhs)), R),
                OpT::Rem => Ast::new(AstT::Mul(Box::new(lhs), Box::new(rhs)), R),
                OpT::Pow => Ast::new(AstT::Mul(Box::new(lhs), Box::new(rhs)), R),
                OpT::Equals => todo!(),
            };
        }

        Ok(lhs)
    }

    // fn parse_dyn_fun_args(
    //     &mut self,
    //     min: usize,
    //     max: usize,
    //     range: Range,
    //     items: &[Item],
    // ) -> crate::Result<Vec<Ast>> {
    //     let arg_count = items.iter().filter(|i| i.is_sep()).count() + 1;
    //     let mut args = Vec::with_capacity(cmp::min(arg_count, max));
    //     let mut unexpected_args = Vec::new();
    //     let mut parsed_args = 0;
    //     let mut start = (0, range.start);
    //     let mut ti = 0;

    //     for i in items.iter() {
    //         if let Item::Sep(s) = i {
    //             match s.typ {
    //                 SepT::Comma => (),
    //                 SepT::Semi => self.warnings.push(crate::Warning::ConfusingSeparator {
    //                     sep: *s,
    //                     expected: SepT::Comma,
    //                 }),
    //             }

    //             let r = Range::of(start.1, s.range.start);
    //             if parsed_args < max {
    //                 let is = &items[(start.0)..ti];
    //                 args.push(self.parse_items(r, is)?);
    //             } else {
    //                 unexpected_args.push(r);
    //             }
    //             start = (ti + 1, s.range.end);
    //             parsed_args += 1;
    //         }
    //         ti += 1;
    //     }

    //     if let Some(i) = items.last() {
    //         if !i.is_sep() {
    //             let r = Range::of(start.1, range.end);
    //             if parsed_args < max {
    //                 let is = &items[(start.0)..ti];
    //                 args.push(self.parse_items(r, is)?);
    //             } else {
    //                 unexpected_args.push(r);
    //             }
    //             parsed_args += 1;
    //         }
    //     }

    //     if !unexpected_args.is_empty() {
    //         self.errors.push(crate::Error::UnexpectedFunctionArguments {
    //             ranges: unexpected_args,
    //             expected: max,
    //             found: parsed_args,
    //         });
    //     } else if parsed_args < min {
    //         let range = match items.last() {
    //             Some(i) => Range::of(i.range().end, range.end),
    //             None => Range::pos(range.end),
    //         };
    //         self.errors.push(crate::Error::MissingFunctionArguments {
    //             range,
    //             expected: min,
    //             found: parsed_args,
    //         });
    //     }

    //     Ok(args)
    // }

    // fn parse_fun_args<const COUNT: usize>(
    //     &mut self,
    //     range: Range,
    //     items: &[Item],
    // ) -> crate::Result<[Ast; COUNT]> {
    //     let mut args: [Ast; COUNT] = array_of(|_| Ast::new(AstT::Error, range));
    //     let mut unexpected_args = Vec::new();
    //     let mut parsed_args = 0;
    //     let mut start = (0, range.start);
    //     let mut ti = 0;

    //     for i in items.iter() {
    //         if let Item::Sep(s) = i {
    //             match s.typ {
    //                 SepT::Comma => (),
    //                 SepT::Semi => self.warnings.push(crate::Warning::ConfusingSeparator {
    //                     sep: *s,
    //                     expected: SepT::Comma,
    //                 }),
    //             }

    //             let r = Range::of(start.1, s.range.start);
    //             if parsed_args < COUNT {
    //                 let is = &items[(start.0)..ti];
    //                 args[parsed_args] = self.parse_items(r, is)?;
    //             } else {
    //                 unexpected_args.push(r);
    //             }
    //             start = (ti + 1, s.range.end);
    //             parsed_args += 1;
    //         }
    //         ti += 1;
    //     }

    //     if let Some(i) = items.last() {
    //         if !i.is_sep() {
    //             let r = Range::of(start.1, range.end);
    //             if parsed_args < COUNT {
    //                 let is = &items[(start.0)..ti];
    //                 args[parsed_args] = self.parse_items(r, is)?;
    //             } else {
    //                 unexpected_args.push(r);
    //             }
    //             parsed_args += 1;
    //         }
    //     }

    //     if !unexpected_args.is_empty() {
    //         self.errors.push(crate::Error::UnexpectedFunctionArguments {
    //             ranges: unexpected_args,
    //             expected: COUNT,
    //             found: parsed_args,
    //         });
    //     } else if parsed_args < COUNT {
    //         let range = match items.last() {
    //             Some(i) => Range::of(i.range().end, range.end),
    //             None => Range::pos(range.end),
    //         };
    //         self.errors.push(crate::Error::MissingFunctionArguments {
    //             range,
    //             expected: COUNT,
    //             found: parsed_args,
    //         });
    //     }

    //     Ok(args)
    // }
}

// fn array_of<T, const SIZE: usize>(f: impl Fn(usize) -> T) -> [T; SIZE] {
//     let mut arr: MaybeUninit<[T; SIZE]> = MaybeUninit::uninit();
//     let mut ptr = arr.as_mut_ptr() as *mut T;
//
//     for i in 0..SIZE {
//         let elem = f(i);
//         unsafe {
//             ptr.write(elem);
//             ptr = ptr.add(1);
//         }
//     }
//
//     unsafe { arr.assume_init() }
// }
