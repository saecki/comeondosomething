use crate::{Op, OpT, Range, ValT};

use super::*;

#[test]
fn no_parenthesis() {
    let mut ctx = Context::default();
    let tokens = ctx.tokenize("423.42 * 64.52").unwrap();
    let items = ctx.group(&tokens).unwrap();

    assert_eq!(
        items,
        vec![
            Item::Val(Val::new(ValT::float(423.42), Range::of(0, 6))),
            Item::Op(Op::new(OpT::Mul, Range::pos(7))),
            Item::Val(Val::new(ValT::float(64.52), Range::of(9, 14))),
        ]
    );
}

#[test]
fn add_parenthesis() {
    let mut ctx = Context::default();
    let tokens = ctx.tokenize("(23.13 + 543.23) * 34").unwrap();
    let items = ctx.group(&tokens).unwrap();

    assert_eq!(
        items,
        vec![
            Item::Group(Group::new(
                vec![
                    Item::Val(Val::new(ValT::float(23.13), Range::of(1, 6))),
                    Item::Op(Op::new(OpT::Add, Range::pos(7))),
                    Item::Val(Val::new(ValT::float(543.23), Range::of(9, 15)))
                ],
                Range::of(1, 15),
                ParKind::Round,
            )),
            Item::Op(Op::new(OpT::Mul, Range::pos(17))),
            Item::Val(Val::new(ValT::int(34), Range::of(19, 21))),
        ]
    );
}
