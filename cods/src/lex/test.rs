use crate::{Ident, IdentSpan};

use super::*;

fn assert(input: &str, expected: Vec<Token>) {
    let tokens = Context::default().lex(input).unwrap();
    assert_eq!(tokens, expected);
}

fn assert_err(input: &str, expected: crate::Error) {
    let mut ctx = Context::default();
    match ctx.lex(input) {
        Ok(_) if !ctx.errors.is_empty() => {
            assert_eq!(ctx.errors[0], expected)
        }
        Ok(_) => panic!("Expected error: {expected:?}"),
        Err(e) => assert_eq!(e, expected),
    }
}

fn assert_errs(input: &str, expected: Vec<crate::Error>) {
    let mut ctx = Context::default();
    match ctx.lex(input) {
        Ok(_) => {
            assert_eq!(ctx.errors, expected)
        }
        Err(e) => {
            ctx.errors.push(e);
            assert_eq!(ctx.errors, expected)
        }
    }
}

#[test]
fn simple_add() {
    assert(
        "432.432 + 24324.543",
        vec![
            Token::val(Val::Float(432.432), Span::cols(0, 0, 7)),
            Token::op(OpT::Add, Span::pos(0, 8)),
            Token::val(Val::Float(24324.543), Span::cols(0, 10, 19)),
        ],
    );
}

#[test]
fn simple_mul() {
    assert(
        "604.453 *3562.543",
        vec![
            Token::val(Val::Float(604.453), Span::cols(0, 0, 7)),
            Token::op(OpT::Mul, Span::pos(0, 8)),
            Token::val(Val::Float(3562.543), Span::cols(0, 9, 17)),
        ],
    );
}

#[test]
fn add_mul() {
    assert(
        "(32+ 604.453)* 3562.543",
        vec![
            Token::par(ParT::RoundOpen, Span::pos(0, 0)),
            Token::val(Val::Int(32), Span::cols(0, 1, 3)),
            Token::op(OpT::Add, Span::pos(0, 3)),
            Token::val(Val::Float(604.453), Span::cols(0, 5, 12)),
            Token::par(ParT::RoundClose, Span::pos(0, 12)),
            Token::op(OpT::Mul, Span::pos(0, 13)),
            Token::val(Val::Float(3562.543), Span::cols(0, 15, 23)),
        ],
    );
}

#[test]
fn bools() {
    assert(
        "false true",
        vec![
            Token::val(Val::Bool(false), Span::cols(0, 0, 5)),
            Token::val(Val::Bool(true), Span::cols(0, 6, 10)),
        ],
    );
}

#[test]
fn eq_span() {
    assert(
        "234 == 43",
        vec![
            Token::val(Val::Int(234), Span::cols(0, 0, 3)),
            Token::op(OpT::Eq, Span::cols(0, 4, 6)),
            Token::val(Val::Int(43), Span::cols(0, 7, 9)),
        ],
    );
}

#[test]
fn vars() {
    let mut ctx = Context::default();
    let tokens = ctx.lex("x64 = 2; arm = 3").unwrap();

    assert_eq!(ctx.idents.name(Ident(0)), "x64");
    assert_eq!(ctx.idents.name(Ident(1)), "arm");

    assert_eq!(
        tokens,
        [
            Token::ident(Ident(0), Span::cols(0, 0, 3)),
            Token::op(OpT::Assign, Span::pos(0, 4)),
            Token::val(Val::Int(2), Span::pos(0, 6)),
            Token::pct(PctT::Semi, Span::pos(0, 7)),
            Token::ident(Ident(1), Span::cols(0, 9, 12)),
            Token::op(OpT::Assign, Span::pos(0, 13)),
            Token::val(Val::Int(3), Span::pos(0, 15)),
        ],
    );
}

#[test]
fn invalid_char() {
    assert_err(
        "x6ä = 2; Arm = 3",
        crate::Error::InvalidChar(Span::pos(0, 2)),
    );
}

#[test]
fn str_span() {
    assert(
        r#" "abdd"   "#,
        vec![Token::val(Val::Str("abdd".into()), Span::cols(0, 1, 7))],
    );
}

#[test]
fn str_missing_esc_char() {
    assert_err(
        r#""hello there \"#,
        crate::Error::MissingEscapeChar(Span::pos(0, 14)),
    );
}

#[test]
fn str_invalid_unicode_esc_char() {
    assert_err(
        r#""hello there \x1G""#,
        crate::Error::InvalidUnicodeEscapeChar('G', Span::pos(0, 16)),
    );
}

#[test]
fn str_missing_unicode_esc_char() {
    assert_err(
        r#""hello there \u2 ""#,
        crate::Error::MissingUnicodeEscapeChar {
            expected: 4,
            found: 1,
            span: Span::pos(0, 16),
        },
    );
}

#[test]
fn str_missing_unicode_esc_par() {
    assert_err(
        r#""hello there \u{24432   ""#,
        crate::Error::MissingClosingUnicodeEscapePar(Span::pos(0, 15), Span::pos(0, 21)),
    );
}

#[test]
fn char_empty_literal() {
    assert_err("''", crate::Error::EmptyCharLiteral(Span::cols(0, 0, 2)));
}

#[test]
fn char_recover_from_unclosed_literal() {
    assert_errs(
        "
        val c = 'i
        var n = 234
        n += 43
        ",
        vec![crate::Error::MissingClosingQuote(Span::pos(1, 16))],
    );
}

#[test]
fn leading_dot() {
    assert(
        "abc .3",
        vec![
            Token::Ident(IdentSpan::new(Ident(0), Span::cols(0, 0, 3))),
            Token::Op(Op::new(OpT::Dot, Span::pos(0, 4))),
            Token::Val(ValSpan::new(Val::Int(3), Span::pos(0, 5))),
        ],
    )
}

#[test]
fn ident_tuple_index() {
    assert(
        "abc1.2",
        vec![
            Token::Ident(IdentSpan::new(Ident(0), Span::cols(0, 0, 4))),
            Token::Op(Op::new(OpT::Dot, Span::pos(0, 4))),
            Token::Val(ValSpan::new(Val::Int(2), Span::pos(0, 5))),
        ],
    )
}
