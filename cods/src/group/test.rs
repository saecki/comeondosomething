use crate::{OpT, ParKind, ParT, Span, Val};

use super::*;

#[test]
fn no_parenthesis() {
    let mut ctx = Context::default();
    let tokens = ctx.lex("423.42 * 64.52").unwrap();
    let items = ctx.group(tokens).unwrap();

    assert_eq!(
        items,
        vec![
            Item::val(Val::Float(423.42), Span::of(0, 6)),
            Item::op(OpT::Mul, Span::pos(7)),
            Item::val(Val::Float(64.52), Span::of(9, 14)),
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
                    Item::val(Val::Float(23.13), Span::of(1, 6)),
                    Item::op(OpT::Add, Span::pos(7)),
                    Item::val(Val::Float(543.23), Span::of(9, 15))
                ],
                Span::of(0, 16),
                ParKind::Round,
            )),
            Item::op(OpT::Mul, Span::pos(17)),
            Item::val(Val::Int(34), Span::of(19, 21)),
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
                Item::val(Val::Int(3), Span::pos(3)),
                Item::op(OpT::Add, Span::pos(5)),
                Item::val(Val::Int(5), Span::pos(7)),
            ],
            Span::of(0, 10),
            ParKind::Curly
        ))]
    );

    assert_eq!(
        ctx.errors,
        vec![crate::Error::MissingClosingPar(Par::new(
            ParT::RoundOpen,
            Span::pos(2),
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
                        Item::val(Val::Int(3), Span::pos(5)),
                        Item::op(OpT::Add, Span::pos(7)),
                        Item::val(Val::Int(5), Span::pos(9)),
                    ],
                    Span::of(4, 13),
                    ParKind::Round,
                ))],
                Span::of(3, 14),
                ParKind::Round,
            ))],
            Span::of(2, 15),
            ParKind::Round,
        ))],
    );

    assert_eq!(
        ctx.errors,
        vec![
            crate::Error::UnexpectedPar(Par::new(ParT::CurlyClose, Span::pos(11),)),
            crate::Error::MissingClosingPar(Par::new(ParT::CurlyOpen, Span::pos(0),)),
        ]
    )
}
