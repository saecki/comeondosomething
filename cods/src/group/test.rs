use crate::{OpT, ParT, Span, Val};

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
                Par::new(ParT::RoundOpen, Span::pos(0)),
                Par::new(ParT::RoundClose, Span::pos(15)),
                vec![
                    Item::val(Val::Float(23.13), Span::of(1, 6)),
                    Item::op(OpT::Add, Span::pos(7)),
                    Item::val(Val::Float(543.23), Span::of(9, 15))
                ],
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
            Par::new(ParT::CurlyOpen, Span::pos(0)),
            Par::new(ParT::CurlyClose, Span::pos(9)),
            vec![
                Item::val(Val::Int(3), Span::pos(3)),
                Item::op(OpT::Add, Span::pos(5)),
                Item::val(Val::Int(5), Span::pos(7)),
            ],
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
            Par::new(ParT::RoundOpen, Span::pos(2)),
            Par::new(ParT::RoundClose, Span::pos(14)),
            vec![Item::Group(Group::new(
                Par::new(ParT::RoundOpen, Span::pos(3)),
                Par::new(ParT::RoundClose, Span::pos(13)),
                vec![Item::Group(Group::new(
                    Par::new(ParT::RoundOpen, Span::pos(4)),
                    Par::new(ParT::RoundClose, Span::pos(12)),
                    vec![
                        Item::val(Val::Int(3), Span::pos(5)),
                        Item::op(OpT::Add, Span::pos(7)),
                        Item::val(Val::Int(5), Span::pos(9)),
                    ],
                ))],
            ))],
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

#[test]
fn par_stack_gets_popped() {
    let mut ctx = Context::default();
    let tokens = ctx.lex("{ ()()()() }").unwrap();
    let items = ctx.group(tokens).unwrap();

    assert_eq!(
        items,
        vec![Item::Group(Group::new(
            Par::new(ParT::CurlyOpen, Span::pos(0)),
            Par::new(ParT::CurlyClose, Span::pos(11)),
            vec![
                Item::Group(Group::new(
                    Par::new(ParT::RoundOpen, Span::pos(2)),
                    Par::new(ParT::RoundClose, Span::pos(3)),
                    vec![],
                )),
                Item::Group(Group::new(
                    Par::new(ParT::RoundOpen, Span::pos(4)),
                    Par::new(ParT::RoundClose, Span::pos(5)),
                    vec![],
                )),
                Item::Group(Group::new(
                    Par::new(ParT::RoundOpen, Span::pos(6)),
                    Par::new(ParT::RoundClose, Span::pos(7)),
                    vec![],
                )),
                Item::Group(Group::new(
                    Par::new(ParT::RoundOpen, Span::pos(8)),
                    Par::new(ParT::RoundClose, Span::pos(9)),
                    vec![],
                )),
            ],
        ))],
    );

    assert_eq!(ctx.errors, vec![])
}
