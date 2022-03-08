use cods::{Error, ExtDummy, Num, Par, ParType, PlainVal, Range, UserFacing, Val};

fn assert(expected: PlainVal, expr: &str) {
    match cods::eval(expr) {
        Ok(Some(val)) => assert_eq!(expected, val),
        Ok(None) => panic!("Expected a value found nothing"),
        Err(e) => {
            eprintln!("{}\n", e.display(expr));
            panic!();
        }
    }
}

fn assert_err(expected: Error<ExtDummy>, expr: &str) {
    match cods::eval(expr) {
        Ok(_) => panic!("Expected error: {expected:?}"),
        Err(e) => assert_eq!(expected, e),
    }
}

#[test]
fn float() {
    assert(
        PlainVal::Float(20713257.3385426),
        "234.4234 + 6345.423 * 3264.2462",
    );
}

#[test]
fn int() {
    assert(
        PlainVal::Int(-9717750),
        "6 + 3452 − (3252 × 5324) + (((2342 × 3242) ÷ 4234) × 4234) − 324",
    );
}

#[test]
fn unicode_ops() {
    assert(
        PlainVal::Float(41968480425.587155963),
        "(23423 × 423 + (423 − 234) ÷ 654 + 4324) × 4234",
    );
}

#[test]
fn signs() {
    assert(PlainVal::Int(5), "1 - 2 * -2");
}

#[test]
fn euclid_div() {
    assert(PlainVal::Int(2), "8 div 3");
}

#[test]
fn remainder() {
    assert(PlainVal::Int(2), "8 % 3");
}

#[test]
fn negative_remainder1() {
    assert(PlainVal::Int(1), "(-8) % 3");
}

#[test]
fn negative_remainder2() {
    assert(PlainVal::Int(-2), "8 % -5");
}

#[test]
fn gcd() {
    assert(PlainVal::Int(3), "gcd(6, 9)");
}

#[test]
fn gcd0() {
    assert(PlainVal::Int(4), "gcd(4, 0)");
    assert(PlainVal::Int(5), "gcd(0, 5)");
}

#[test]
fn factorial() {
    assert(PlainVal::Int(8 * 7 * 6 * 5 * 4 * 3 * 2 * 1), "8!");
}

#[test]
fn factorial_overflow() {
    assert_err(
        Error::FactorialOverflow(Num::new(Val::int(34), Range::of(0, 2))),
        "34!",
    );
}

#[test]
fn factorial_fraction() {
    assert_err(
        Error::FractionFactorial(Num::new(Val::float(4.1), Range::of(0, 3))),
        "4.1!",
    );
}

#[test]
fn factorial_negative() {
    assert_err(
        Error::NegativeFactorial(Num::new(Val::int(-3), Range::of(1, 3))),
        "(-3)!",
    );
}

#[test]
fn squareroot() {
    assert(PlainVal::Int(25), "sqrt(625)");
}

#[test]
fn binomial_coefficient() {
    assert(PlainVal::Int(15), "nCr(6, 2)");
}

#[test]
fn binomial_coefficient_zero() {
    assert(PlainVal::Int(1), "nCr(23, 0)");
}

#[test]
fn binomial_coefficient_invalid() {
    assert_err(
        Error::InvalidNcr(
            Num::new(Val::int(3), Range::pos(4)),
            Num::new(Val::int(4), Range::pos(7)),
        ),
        "nCr(3, 4)",
    );
}

#[test]
fn binomial_coefficient_negative() {
    assert_err(
        Error::NegativeNcr(
            Num::new(Val::int(5), Range::pos(4)),
            Num::new(Val::int(-3), Range::of(7, 9)),
        ),
        "nCr(5, -3)",
    );
}

#[test]
fn ln() {
    assert(PlainVal::Int(27), "ln(e^27)");
}

#[test]
fn log2() {
    assert(PlainVal::Int(3), "log(2, 8)");
}

#[test]
fn log10() {
    assert(PlainVal::Int(5), "log(10, 100000)");
}

#[test]
fn min() {
    assert(PlainVal::Int(3), "min(3, 7, 5)");
}

#[test]
fn max() {
    assert(PlainVal::Int(7), "max(3, 7, 5)");
}

#[test]
fn clamp() {
    assert(PlainVal::Int(9), "clamp(9, -2, 23)");
}

#[test]
fn clamp_low() {
    assert(PlainVal::Int(-5), "clamp(-12, -5, 5)");
}

#[test]
fn clamp_high() {
    assert(PlainVal::Int(7), "clamp(31, 0, 7)");
}

#[test]
fn clamp_bounds() {
    assert_err(
        Error::InvalidClampBounds(
            Num::new(Val::int(5), Range::pos(9)),
            Num::new(Val::int(4), Range::pos(12)),
        ),
        "clamp(0, 5, 4)",
    );
}

#[test]
fn clamp_bounds_float() {
    assert_err(
        Error::InvalidClampBounds(
            Num::new(Val::float(5.3), Range::of(9, 12)),
            Num::new(Val::float(4.5), Range::of(14, 17)),
        ),
        "clamp(0, 5.3, 4.5)",
    );
}

#[test]
fn unmatched_par() {
    assert_err(
        Error::UnexpectedParenthesis(Par::new(ParType::RoundClose, Range::pos(2))),
        "4 ) + 5)",
    );
}

#[test]
fn var() {
    assert(PlainVal::Int(7), "x = 7; x");
}
