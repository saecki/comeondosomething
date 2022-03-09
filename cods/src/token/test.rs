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
            Token::val(ValT::float(432.432), Range::of(0, 7)),
            Token::op(OpT::Add, Range::pos(8)),
            Token::val(ValT::float(24324.543), Range::of(10, 19)),
        ],
    );
}

#[test]
fn simple_mul() {
    check(
        "604.453 *3562.543",
        vec![
            Token::val(ValT::float(604.453), Range::of(0, 7)),
            Token::op(OpT::Mul, Range::pos(8)),
            Token::val(ValT::float(3562.543), Range::of(9, 17)),
        ],
    );
}

#[test]
fn add_mul() {
    check(
        "(32+ 604.453)* 3562.543",
        vec![
            Token::par(ParT::RoundOpen, Range::pos(0)),
            Token::val(ValT::int(32), Range::of(1, 3)),
            Token::op(OpT::Add, Range::pos(3)),
            Token::val(ValT::float(604.453), Range::of(5, 12)),
            Token::par(ParT::RoundClose, Range::pos(12)),
            Token::op(OpT::Mul, Range::pos(13)),
            Token::val(ValT::float(3562.543), Range::of(15, 23)),
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
            Token::val(ValT::Var(VarId(0)), Range::of(0, 3)),
            Token::op(OpT::Equals, Range::pos(4)),
            Token::val(ValT::int(2), Range::pos(6)),
            Token::sep(SepT::Semi, Range::pos(7)),
            Token::val(ValT::Var(VarId(1)), Range::of(9, 12)),
            Token::op(OpT::Equals, Range::pos(13)),
            Token::val(ValT::int(3), Range::pos(15)),
        ],
    );
}

#[test]
fn invalid_char() {
    let mut ctx = Context::default();
    let error = ctx.tokenize("x6ä = 2; Arm = 3").unwrap_err();

    assert_eq!(error, crate::Error::InvalidChar(Range::pos(2)));
}
