use cods::{Error, Par, ParT, Range, Val, ValRange};

fn assert(expected: Val, input: &str) {
    match cods::eval(input) {
        Ok(Some(val)) => assert_eq!(expected, val),
        Ok(None) => panic!("Expected a value found nothing"),
        Err(e) => {
            panic!("{e:?}");
        }
    }
}

fn assert_err(expected: Error, input: &str) {
    match cods::eval(input) {
        Ok(_) => panic!("Expected error: {expected:?}"),
        Err(e) => assert_eq!(expected, e),
    }
}

#[test]
fn float() {
    assert(
        Val::Float(20713257.3385426),
        "234.4234 + 6345.423 * 3264.2462",
    );
}

#[test]
fn int() {
    assert(
        Val::Int(-9717750),
        "6 + 3452 − (3252 × 5324) + (((2342 × 3242) ÷ 4234) × 4234) − 324",
    );
}

#[test]
fn unicode_ops() {
    assert(
        Val::Float(41968480425.587155963),
        "(23423 × 423 + (423 − 234) ÷ 654 + 4324) × 4234",
    );
}

#[test]
fn signs() {
    assert(Val::Int(5), "1 - 2 * -2");
}

#[test]
fn euclid_div() {
    assert(Val::Int(2), "8 div 3");
}

#[test]
fn remainder() {
    assert(Val::Int(2), "8 % 3");
}

#[test]
fn negative_remainder1() {
    assert(Val::Int(1), "(-8) % 3");
}

#[test]
fn negative_remainder2() {
    assert(Val::Int(-2), "8 % -5");
}

#[test]
fn gcd() {
    assert(Val::Int(3), "gcd(6, 9)");
}

#[test]
fn gcd0() {
    assert(Val::Int(4), "gcd(4, 0)");
    assert(Val::Int(5), "gcd(0, 5)");
}

#[test]
fn factorial() {
    assert(Val::Int(8 * 7 * 6 * 5 * 4 * 3 * 2 * 1), "8!");
}

#[test]
fn factorial_overflow() {
    assert_err(
        Error::FactorialOverflow(ValRange::new(Val::Int(34), Range::of(0, 2))),
        "34!",
    );
}

#[test]
fn factorial_fraction() {
    assert_err(
        Error::FractionFactorial(ValRange::new(Val::Float(4.1), Range::of(0, 3))),
        "4.1!",
    );
}

#[test]
fn factorial_negative() {
    assert_err(
        Error::NegativeFactorial(ValRange::new(Val::Int(-3), Range::of(0, 4))),
        "(-3)!",
    );
}

#[test]
fn squareroot() {
    assert(Val::Int(25), "sqrt(625)");
}

#[test]
fn binomial_coefficient() {
    assert(Val::Int(15), "nCr(6, 2)");
}

#[test]
fn binomial_coefficient_zero() {
    assert(Val::Int(1), "nCr(23, 0)");
}

#[test]
fn binomial_coefficient_invalid() {
    assert_err(
        Error::InvalidNcr(
            ValRange::new(Val::Int(3), Range::pos(4)),
            ValRange::new(Val::Int(4), Range::pos(7)),
        ),
        "nCr(3, 4)",
    );
}

#[test]
fn binomial_coefficient_negative() {
    assert_err(
        Error::NegativeNcr(ValRange::new(Val::Int(-3), Range::of(7, 9))),
        "nCr(5, -3)",
    );
}

#[test]
fn ln() {
    assert(Val::Int(27), "ln(e^27)");
}

#[test]
fn log2() {
    assert(Val::Int(3), "log(2, 8)");
}

#[test]
fn log10() {
    assert(Val::Int(5), "log(10, 100000)");
}

#[test]
fn min() {
    assert(Val::Int(3), "min(3, 7, 5)");
}

#[test]
fn max() {
    assert(Val::Int(7), "max(3, 7, 5)");
}

#[test]
fn clamp() {
    assert(Val::Int(9), "clamp(9, -2, 23)");
}

#[test]
fn clamp_low() {
    assert(Val::Int(-5), "clamp(-12, -5, 5)");
}

#[test]
fn clamp_high() {
    assert(Val::Int(7), "clamp(31, 0, 7)");
}

#[test]
fn clamp_bounds() {
    assert_err(
        Error::InvalidClampBounds(
            ValRange::new(Val::Int(5), Range::pos(9)),
            ValRange::new(Val::Int(4), Range::pos(12)),
        ),
        "clamp(0, 5, 4)",
    );
}

#[test]
fn clamp_bounds_float() {
    assert_err(
        Error::InvalidClampBounds(
            ValRange::new(Val::Float(5.3), Range::of(9, 12)),
            ValRange::new(Val::Float(4.5), Range::of(14, 17)),
        ),
        "clamp(0, 5.3, 4.5)",
    );
}

#[test]
fn eq_bool() {
    assert(Val::Bool(false), "false == true");
}

#[test]
fn eq_int_float() {
    assert(Val::Bool(true), "2.0 == 2");
}

#[test]
fn or() {
    assert(Val::Bool(true), "false || true");
}

#[test]
fn and() {
    assert(Val::Bool(false), "false && true");
}

#[test]
fn bw_or_bool() {
    assert(Val::Bool(true), "true | false");
}

#[test]
fn bw_and_bool() {
    assert(Val::Bool(false), "true & false");
}

#[test]
fn bw_or_int() {
    assert(Val::Int(14), "12 | 2");
}

#[test]
fn bw_and_int() {
    assert(Val::Int(4), "6 & 12");
}

#[test]
fn unmatched_par() {
    assert_err(
        Error::UnexpectedParenthesis(Par::new(ParT::RoundClose, Range::pos(2))),
        "4 ) + 5)",
    );
}

#[test]
fn var() {
    assert(Val::Int(7), "x = 7; x");
}
