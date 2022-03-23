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
            Token::expr(ExprT::float(432.432), Range::of(0, 7)),
            Token::op(OpT::Add, Range::pos(8)),
            Token::expr(ExprT::float(24324.543), Range::of(10, 19)),
        ],
    );
}

#[test]
fn simple_mul() {
    assert(
        "604.453 *3562.543",
        vec![
            Token::expr(ExprT::float(604.453), Range::of(0, 7)),
            Token::op(OpT::Mul, Range::pos(8)),
            Token::expr(ExprT::float(3562.543), Range::of(9, 17)),
        ],
    );
}

#[test]
fn add_mul() {
    assert(
        "(32+ 604.453)* 3562.543",
        vec![
            Token::par(ParT::RoundOpen, Range::pos(0)),
            Token::expr(ExprT::int(32), Range::of(1, 3)),
            Token::op(OpT::Add, Range::pos(3)),
            Token::expr(ExprT::float(604.453), Range::of(5, 12)),
            Token::par(ParT::RoundClose, Range::pos(12)),
            Token::op(OpT::Mul, Range::pos(13)),
            Token::expr(ExprT::float(3562.543), Range::of(15, 23)),
        ],
    );
}

#[test]
fn bools() {
    assert(
        "false true",
        vec![
            Token::expr(ExprT::bool(false), Range::of(0, 5)),
            Token::expr(ExprT::bool(true), Range::of(6, 10)),
        ],
    );
}

#[test]
fn angle_literal_deg1() {
    assert(
        "234deg",
        vec![
            Token::expr(ExprT::int(234), Range::of(0, 3)),
            Token::op(OpT::Degree, Range::of(3, 6)),
        ],
    );
}

#[test]
fn angle_literal_deg2() {
    assert(
        "94.3_deg",
        vec![
            Token::expr(ExprT::float(94.3), Range::of(0, 4)),
            Token::op(OpT::Degree, Range::of(4, 8)),
        ],
    );
}

#[test]
fn angle_literal_rad1() {
    assert(
        "43_rad",
        vec![
            Token::expr(ExprT::int(43), Range::of(0, 2)),
            Token::op(OpT::Radian, Range::of(2, 6)),
        ],
    );
}

#[test]
fn angle_literal_rad2() {
    assert(
        "1.45rad",
        vec![
            Token::expr(ExprT::float(1.45_f64), Range::of(0, 4)),
            Token::op(OpT::Radian, Range::of(4, 7)),
        ],
    );
}

#[test]
fn eq_range() {
    assert(
        "234 == 43",
        vec![
            Token::expr(ExprT::int(234), Range::of(0, 3)),
            Token::op(OpT::Eq, Range::of(4, 6)),
            Token::expr(ExprT::int(43), Range::of(7, 9)),
        ],
    );
}

#[test]
fn vars() {
    let mut ctx = Context::default();
    let tokens = ctx.lex("x64 = 2; arm = 3").unwrap();

    assert_eq!(ctx.idents, ["x64", "arm"]);

    assert_eq!(
        tokens,
        [
            Token::expr(ExprT::Var(Ident(0)), Range::of(0, 3)),
            Token::op(OpT::Assign, Range::pos(4)),
            Token::expr(ExprT::int(2), Range::pos(6)),
            Token::sep(SepT::Semi, Range::pos(7)),
            Token::expr(ExprT::Var(Ident(1)), Range::of(9, 12)),
            Token::op(OpT::Assign, Range::pos(13)),
            Token::expr(ExprT::int(3), Range::pos(15)),
        ],
    );
}

#[test]
fn invalid_char() {
    assert_err("x6ä = 2; Arm = 3", crate::Error::InvalidChar(Range::pos(2)));
}

#[test]
fn str_range() {
    assert(
        r#" "abdd"   "#,
        vec![Token::expr(
            ExprT::Val(Val::Str("abdd".into())),
            Range::of(1, 7),
        )],
    );
}

#[test]
fn str_missing_esc_char() {
    assert_err(
        r#""hello there \"#,
        crate::Error::MissingEscapeChar(Range::pos(14)),
    );
}

#[test]
fn str_invalid_unicode_esc_char() {
    assert_err(
        r#""hello there \x1G""#,
        crate::Error::InvalidUnicodeEscapeChar('G', Range::pos(16)),
    );
}

#[test]
fn str_missing_unicode_esc_char() {
    assert_err(
        r#""hello there \u2 ""#,
        crate::Error::MissingUnicodeEscapeChar {
            expected: 4,
            found: 1,
            range: Range::pos(16),
        },
    );
}

#[test]
fn str_missing_unicode_esc_par() {
    assert_err(
        r#""hello there \u{24432   ""#,
        crate::Error::MissingClosingUnicodeEscapePar(Range::pos(15), Range::pos(21)),
    );
}
