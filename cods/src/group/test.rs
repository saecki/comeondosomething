use crate::{CRange, Expr, ExprT, Op, OpT, ParKind, ParT};

use super::*;

#[test]
fn no_parenthesis() {
    let mut ctx = Context::default();
    let tokens = ctx.lex("423.42 * 64.52").unwrap();
    let items = ctx.group(tokens).unwrap();

    assert_eq!(
        items,
        vec![
            Item::Expr(Expr::new(ExprT::float(423.42), CRange::of(0, 6))),
            Item::Op(Op::new(OpT::Mul, CRange::pos(7))),
            Item::Expr(Expr::new(ExprT::float(64.52), CRange::of(9, 14))),
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
                    Item::Expr(Expr::new(ExprT::float(23.13), CRange::of(1, 6))),
                    Item::Op(Op::new(OpT::Add, CRange::pos(7))),
                    Item::Expr(Expr::new(ExprT::float(543.23), CRange::of(9, 15)))
                ],
                CRange::of(0, 16),
                ParKind::Round,
            )),
            Item::Op(Op::new(OpT::Mul, CRange::pos(17))),
            Item::Expr(Expr::new(ExprT::int(34), CRange::of(19, 21))),
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
                Item::Expr(Expr::new(ExprT::int(3), CRange::pos(3))),
                Item::Op(Op::new(OpT::Add, CRange::pos(5))),
                Item::Expr(Expr::new(ExprT::int(5), CRange::pos(7))),
            ],
            CRange::of(0, 10),
            ParKind::Curly
        ))]
    );

    assert_eq!(
        ctx.errors,
        vec![crate::Error::MissingClosingPar(Par::new(
            ParT::RoundOpen,
            CRange::pos(2),
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
                        Item::Expr(Expr::new(ExprT::int(3), CRange::pos(5))),
                        Item::Op(Op::new(OpT::Add, CRange::pos(7))),
                        Item::Expr(Expr::new(ExprT::int(5), CRange::pos(9))),
                    ],
                    CRange::of(4, 13),
                    ParKind::Round,
                ))],
                CRange::of(3, 14),
                ParKind::Round,
            ))],
            CRange::of(2, 15),
            ParKind::Round,
        ))],
    );

    assert_eq!(
        ctx.errors,
        vec![
            crate::Error::UnexpectedPar(Par::new(ParT::CurlyClose, CRange::pos(11),)),
            crate::Error::MissingClosingPar(Par::new(ParT::CurlyOpen, CRange::pos(0),)),
        ]
    )
}
