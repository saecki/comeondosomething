use super::*;

fn check(input: &str, output: Vec<Token>) {
    let tokens = Context::default().tokenize(input).unwrap();
    assert_eq!(output, tokens);
}

#[test]
fn simple_add() {
    check(
        "432.432 + 24324.543",
        vec![
            Token::val(ExprT::float(432.432), Range::of(0, 7)),
            Token::op(OpT::Add, Range::pos(8)),
            Token::val(ExprT::float(24324.543), Range::of(10, 19)),
        ],
    );
}

#[test]
fn simple_mul() {
    check(
        "604.453 *3562.543",
        vec![
            Token::val(ExprT::float(604.453), Range::of(0, 7)),
            Token::op(OpT::Mul, Range::pos(8)),
            Token::val(ExprT::float(3562.543), Range::of(9, 17)),
        ],
    );
}

#[test]
fn add_mul() {
    check(
        "(32+ 604.453)* 3562.543",
        vec![
            Token::par(ParT::RoundOpen, Range::pos(0)),
            Token::val(ExprT::int(32), Range::of(1, 3)),
            Token::op(OpT::Add, Range::pos(3)),
            Token::val(ExprT::float(604.453), Range::of(5, 12)),
            Token::par(ParT::RoundClose, Range::pos(12)),
            Token::op(OpT::Mul, Range::pos(13)),
            Token::val(ExprT::float(3562.543), Range::of(15, 23)),
        ],
    );
}

#[test]
fn bools() {
    check(
        "false true",
        vec![
            Token::val(ExprT::bool(false), Range::of(0, 5)),
            Token::val(ExprT::bool(true), Range::of(6, 10)),
        ],
    );
}

#[test]
fn angle_literal_deg1() {
    check(
        "234deg",
        vec![
            Token::val(ExprT::int(234), Range::of(0, 3)),
            Token::mood(ModT::Degree, Range::of(3, 6)),
        ],
    );
}

#[test]
fn angle_literal_deg2() {
    check(
        "94.3_deg",
        vec![
            Token::val(ExprT::float(94.3), Range::of(0, 4)),
            Token::mood(ModT::Degree, Range::of(4, 8)),
        ],
    );
}

#[test]
fn angle_literal_rad1() {
    check(
        "43_rad",
        vec![
            Token::val(ExprT::int(43), Range::of(0, 2)),
            Token::mood(ModT::Radian, Range::of(2, 6)),
        ],
    );
}

#[test]
fn angle_literal_rad2() {
    check(
        "1.45rad",
        vec![
            Token::val(ExprT::float(1.45_f64), Range::of(0, 4)),
            Token::mood(ModT::Radian, Range::of(4, 7)),
        ],
    );
}

#[test]
fn vars() {
    let mut ctx = Context::default();
    let tokens = ctx.tokenize("x64 = 2; Arm = 3").unwrap();

    assert_eq!(
        ctx.vars,
        [Var::new("x64".into(), None), Var::new("Arm".into(), None)]
    );

    assert_eq!(
        tokens,
        [
            Token::val(ExprT::Var(VarId(0)), Range::of(0, 3)),
            Token::op(OpT::Assign, Range::pos(4)),
            Token::val(ExprT::int(2), Range::pos(6)),
            Token::sep(SepT::Semi, Range::pos(7)),
            Token::val(ExprT::Var(VarId(1)), Range::of(9, 12)),
            Token::op(OpT::Assign, Range::pos(13)),
            Token::val(ExprT::int(3), Range::pos(15)),
        ],
    );
}

#[test]
fn invalid_char() {
    let mut ctx = Context::default();
    let error = ctx.tokenize("x6ä = 2; Arm = 3").unwrap_err();

    assert_eq!(error, crate::Error::InvalidChar(Range::pos(2)));
}
