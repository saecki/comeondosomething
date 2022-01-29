use cods::{calc, PlainVal, Error, Range, Par};

#[test]
fn float() {
    assert_eq!(
        PlainVal::Float(20713257.3385426),
        calc("234.4234 + 6345.423 * 3264.2462").0.unwrap(),
    );
}

#[test]
fn int() {
    assert_eq!(
        PlainVal::Int(-9717750),
        calc("6 + 3452 − (3252 × 5324) + (((2342 × 3242) ÷ 4234) × 4234) − 324").0.unwrap(),
    );
}

#[test]
fn unicode_ops() {
    assert_eq!(
        PlainVal::Float(41968480425.587155963),
        calc("(23423 × 423 + (423 − 234) ÷ 654 + 4324) × 4234").0.unwrap(),
    );
}

#[test]
fn unmatched_par() {
    assert_eq!(
        Error::UnexpectedParenthesis(Par::RoundClose(Range::pos(2))),
        calc("4 ) + 5)").1.errors[0],
    );
}

#[test]
fn factorial_fraction() {
    assert_eq!(
        Error::DecimalFactorial(Range::of(0, 3)),
        calc("4.1!").1.errors[0],
    );
}
