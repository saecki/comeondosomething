use crate::{Expr, ExprT, Op, OpT, ParKind, ParT, Range};

use super::*;

#[test]
fn no_parenthesis() {
    let mut ctx = Context::default();
    let tokens = ctx.lex("423.42 * 64.52").unwrap();
    let items = ctx.group(tokens).unwrap();

    assert_eq!(
        items,
        vec![
            Item::Expr(Expr::new(ExprT::float(423.42), Range::of(0, 6))),
            Item::Op(Op::new(OpT::Mul, Range::pos(7))),
            Item::Expr(Expr::new(ExprT::float(64.52), Range::of(9, 14))),
        ]
    );
}

#[test]
fn add_parenthesis() {
    let mut ctx = Context::default();
    let tokens = ctx.lex("(23.13 + 543.23) * 34").unwrap();
    let items = ctx.group(tokens).unwrap();

    assert_eq!(
        items,
        vec![
            Item::Group(Group::new(
                vec![
                    Item::Expr(Expr::new(ExprT::float(23.13), Range::of(1, 6))),
                    Item::Op(Op::new(OpT::Add, Range::pos(7))),
                    Item::Expr(Expr::new(ExprT::float(543.23), Range::of(9, 15)))
                ],
                Range::of(0, 16),
                ParKind::Round,
            )),
            Item::Op(Op::new(OpT::Mul, Range::pos(17))),
            Item::Expr(Expr::new(ExprT::int(34), Range::of(19, 21))),
        ]
    );
}

#[test]
fn ignore_inner_par() {
    let mut ctx = Context::default();
    let tokens = ctx.lex("{ (3 + 5 }").unwrap();
    let items = ctx.group(tokens).unwrap();

    assert_eq!(
        items,
        vec![Item::Group(Group::new(
            vec![
                Item::Expr(Expr::new(ExprT::int(3), Range::pos(3))),
                Item::Op(Op::new(OpT::Add, Range::pos(5))),
                Item::Expr(Expr::new(ExprT::int(5), Range::pos(7))),
            ],
            Range::of(0, 10),
            ParKind::Curly
        ))]
    );

    assert_eq!(
        ctx.errors,
        vec![crate::Error::MissingClosingPar(Par::new(
            ParT::RoundOpen,
            Range::pos(2),
        ))]
    )
}

#[test]
fn inner_par_limit() {
    let mut ctx = Context::default();
    let tokens = ctx.lex("{ (((3 + 5 })))").unwrap();
    let items = ctx.group(tokens).unwrap();

    assert_eq!(
        items,
        vec![Item::Group(Group::new(
            vec![Item::Group(Group::new(
                vec![Item::Group(Group::new(
                    vec![
                        Item::Expr(Expr::new(ExprT::int(3), Range::pos(5))),
                        Item::Op(Op::new(OpT::Add, Range::pos(7))),
                        Item::Expr(Expr::new(ExprT::int(5), Range::pos(9))),
                    ],
                    Range::of(4, 13),
                    ParKind::Round,
                ))],
                Range::of(3, 14),
                ParKind::Round,
            ))],
            Range::of(2, 15),
            ParKind::Round,
        ))],
    );

    assert_eq!(
        ctx.errors,
        vec![
            crate::Error::UnexpectedPar(Par::new(
                ParT::CurlyClose,
                Range::pos(11),
            )),
            crate::Error::MissingClosingPar(Par::new(
                ParT::CurlyOpen,
                Range::pos(0),
            )),
        ]
    )
}
