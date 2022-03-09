use cods::{Data, Error, Par, ParT, Range, Val, ValT};

fn assert(expected: Data, expr: &str) {
    match cods::eval(expr) {
        Ok(Some(val)) => assert_eq!(expected, val),
        Ok(None) => panic!("Expected a value found nothing"),
        Err(e) => {
            panic!("{e:?}");
        }
    }
}

fn assert_err(expected: Error, expr: &str) {
    match cods::eval(expr) {
        Ok(_) => panic!("Expected error: {expected:?}"),
        Err(e) => assert_eq!(expected, e),
    }
}

#[test]
fn float() {
    assert(
        Data::Float(20713257.3385426),
        "234.4234 + 6345.423 * 3264.2462",
    );
}

#[test]
fn int() {
    assert(
        Data::Int(-9717750),
        "6 + 3452 − (3252 × 5324) + (((2342 × 3242) ÷ 4234) × 4234) − 324",
    );
}

#[test]
fn unicode_ops() {
    assert(
        Data::Float(41968480425.587155963),
        "(23423 × 423 + (423 − 234) ÷ 654 + 4324) × 4234",
    );
}

#[test]
fn signs() {
    assert(Data::Int(5), "1 - 2 * -2");
}

#[test]
fn euclid_div() {
    assert(Data::Int(2), "8 div 3");
}

#[test]
fn remainder() {
    assert(Data::Int(2), "8 % 3");
}

#[test]
fn negative_remainder1() {
    assert(Data::Int(1), "(-8) % 3");
}

#[test]
fn negative_remainder2() {
    assert(Data::Int(-2), "8 % -5");
}

#[test]
fn gcd() {
    assert(Data::Int(3), "gcd(6, 9)");
}

#[test]
fn gcd0() {
    assert(Data::Int(4), "gcd(4, 0)");
    assert(Data::Int(5), "gcd(0, 5)");
}

#[test]
fn factorial() {
    assert(Data::Int(8 * 7 * 6 * 5 * 4 * 3 * 2 * 1), "8!");
}

#[test]
fn factorial_overflow() {
    assert_err(
        Error::FactorialOverflow(Val::new(ValT::int(34), Range::of(0, 2))),
        "34!",
    );
}

#[test]
fn factorial_fraction() {
    assert_err(
        Error::FractionFactorial(Val::new(ValT::float(4.1), Range::of(0, 3))),
        "4.1!",
    );
}

#[test]
fn factorial_negative() {
    assert_err(
        Error::NegativeFactorial(Val::new(ValT::int(-3), Range::of(1, 3))),
        "(-3)!",
    );
}

#[test]
fn squareroot() {
    assert(Data::Int(25), "sqrt(625)");
}

#[test]
fn binomial_coefficient() {
    assert(Data::Int(15), "nCr(6, 2)");
}

#[test]
fn binomial_coefficient_zero() {
    assert(Data::Int(1), "nCr(23, 0)");
}

#[test]
fn binomial_coefficient_invalid() {
    assert_err(
        Error::InvalidNcr(
            Val::new(ValT::int(3), Range::pos(4)),
            Val::new(ValT::int(4), Range::pos(7)),
        ),
        "nCr(3, 4)",
    );
}

#[test]
fn binomial_coefficient_negative() {
    assert_err(
        Error::NegativeNcr(
            Val::new(ValT::int(5), Range::pos(4)),
            Val::new(ValT::int(-3), Range::of(7, 9)),
        ),
        "nCr(5, -3)",
    );
}

#[test]
fn ln() {
    assert(Data::Int(27), "ln(e^27)");
}

#[test]
fn log2() {
    assert(Data::Int(3), "log(2, 8)");
}

#[test]
fn log10() {
    assert(Data::Int(5), "log(10, 100000)");
}

#[test]
fn min() {
    assert(Data::Int(3), "min(3, 7, 5)");
}

#[test]
fn max() {
    assert(Data::Int(7), "max(3, 7, 5)");
}

#[test]
fn clamp() {
    assert(Data::Int(9), "clamp(9, -2, 23)");
}

#[test]
fn clamp_low() {
    assert(Data::Int(-5), "clamp(-12, -5, 5)");
}

#[test]
fn clamp_high() {
    assert(Data::Int(7), "clamp(31, 0, 7)");
}

#[test]
fn clamp_bounds() {
    assert_err(
        Error::InvalidClampBounds(
            Val::new(ValT::int(5), Range::pos(9)),
            Val::new(ValT::int(4), Range::pos(12)),
        ),
        "clamp(0, 5, 4)",
    );
}

#[test]
fn clamp_bounds_float() {
    assert_err(
        Error::InvalidClampBounds(
            Val::new(ValT::float(5.3), Range::of(9, 12)),
            Val::new(ValT::float(4.5), Range::of(14, 17)),
        ),
        "clamp(0, 5.3, 4.5)",
    );
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
    assert(Data::Int(7), "x = 7; x");
}
