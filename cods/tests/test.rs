use cods::{calc, Error, Par, PlainVal, Range, UserFacing};

fn assert(expected: PlainVal, expr: &str) {
    match calc(expr) {
        (Ok(val), _) => assert_eq!(expected, val),
        (Err(_), ctx) => {
            for w in ctx.warnings.iter().rev() {
                eprintln!("{}\n", w.display(expr));
            }
            for w in ctx.errors.iter().rev() {
                eprintln!("{}\n", w.display(expr));
            }
            panic!();
        }
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
fn squareroot() {
    assert(PlainVal::Int(25), "sqrt(625)");
}

#[test]
fn binomial_coefficient() {
    assert(PlainVal::Int(15), "ncr(6, 2)");
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
fn unmatched_par() {
    assert_eq!(
        Error::UnexpectedParenthesis(Par::RoundClose(Range::pos(2))),
        calc("4 ) + 5)").1.errors[0],
    );
}

#[test]
fn factorial_fraction() {
    assert_eq!(
        Error::FractionFactorial(Range::of(0, 3)),
        calc("4.1!").1.errors[0],
    );
}
