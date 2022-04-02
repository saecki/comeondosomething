use crate::Ident;

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

#[test]
fn simple_add() {
    assert(
        "432.432 + 24324.543",
        vec![
            Token::expr(ExprT::float(432.432), Span::of(0, 7)),
            Token::op(OpT::Add, Span::pos(8)),
            Token::expr(ExprT::float(24324.543), Span::of(10, 19)),
        ],
    );
}

#[test]
fn simple_mul() {
    assert(
        "604.453 *3562.543",
        vec![
            Token::expr(ExprT::float(604.453), Span::of(0, 7)),
            Token::op(OpT::Mul, Span::pos(8)),
            Token::expr(ExprT::float(3562.543), Span::of(9, 17)),
        ],
    );
}

#[test]
fn add_mul() {
    assert(
        "(32+ 604.453)* 3562.543",
        vec![
            Token::par(ParT::RoundOpen, Span::pos(0)),
            Token::expr(ExprT::int(32), Span::of(1, 3)),
            Token::op(OpT::Add, Span::pos(3)),
            Token::expr(ExprT::float(604.453), Span::of(5, 12)),
            Token::par(ParT::RoundClose, Span::pos(12)),
            Token::op(OpT::Mul, Span::pos(13)),
            Token::expr(ExprT::float(3562.543), Span::of(15, 23)),
        ],
    );
}

#[test]
fn bools() {
    assert(
        "false true",
        vec![
            Token::expr(ExprT::bool(false), Span::of(0, 5)),
            Token::expr(ExprT::bool(true), Span::of(6, 10)),
        ],
    );
}

#[test]
fn eq_span() {
    assert(
        "234 == 43",
        vec![
            Token::expr(ExprT::int(234), Span::of(0, 3)),
            Token::op(OpT::Eq, Span::of(4, 6)),
            Token::expr(ExprT::int(43), Span::of(7, 9)),
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
            Token::expr(ExprT::Ident(Ident(0)), Span::of(0, 3)),
            Token::op(OpT::Assign, Span::pos(4)),
            Token::expr(ExprT::int(2), Span::pos(6)),
            Token::pct(PctT::Semi, Span::pos(7)),
            Token::expr(ExprT::Ident(Ident(1)), Span::of(9, 12)),
            Token::op(OpT::Assign, Span::pos(13)),
            Token::expr(ExprT::int(3), Span::pos(15)),
        ],
    );
}

#[test]
fn invalid_char() {
    assert_err(
        "x6ä = 2; Arm = 3",
        crate::Error::InvalidChar(Span::pos(2)),
    );
}

#[test]
fn str_span() {
    assert(
        r#" "abdd"   "#,
        vec![Token::expr(
            ExprT::Val(Val::Str("abdd".into())),
            Span::of(1, 7),
        )],
    );
}

#[test]
fn str_missing_esc_char() {
    assert_err(
        r#""hello there \"#,
        crate::Error::MissingEscapeChar(Span::pos(14)),
    );
}

#[test]
fn str_invalid_unicode_esc_char() {
    assert_err(
        r#""hello there \x1G""#,
        crate::Error::InvalidUnicodeEscapeChar('G', Span::pos(16)),
    );
}

#[test]
fn str_missing_unicode_esc_char() {
    assert_err(
        r#""hello there \u2 ""#,
        crate::Error::MissingUnicodeEscapeChar {
            expected: 4,
            found: 1,
            span: Span::pos(16),
        },
    );
}

#[test]
fn str_missing_unicode_esc_par() {
    assert_err(
        r#""hello there \u{24432   ""#,
        crate::Error::MissingClosingUnicodeEscapePar(Span::pos(15), Span::pos(21)),
    );
}
