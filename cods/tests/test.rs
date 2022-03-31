use std::f64::consts;

use cods::{CRange, Error, Par, ParT, Val, ValRange};

fn assert(input: &str, expected: Val) {
    match cods::eval(input) {
        Ok(Some(val)) => assert_eq!(val, expected),
        Ok(None) => panic!("Expected a value found nothing"),
        Err(e) => {
            panic!("{e:?}");
        }
    }
}

fn assert_unit(input: &str) {
    match cods::eval(input) {
        Ok(Some(val)) => panic!("Expected unit found value: '{val}'"),
        Ok(None) => (),
        Err(e) => {
            panic!("{e:?}");
        }
    }
}

fn assert_err(input: &str, expected: Error) {
    match cods::eval(input) {
        Ok(_) => panic!("Expected error: {expected:?}"),
        Err(e) => assert_eq!(e, expected),
    }
}

#[test]
fn neg() {
    assert("-32", Val::Int(-32));
    assert("-5.3", Val::Float(-5.3));
}

#[test]
fn float() {
    assert(
        "234.4234 + 6345.423 * 3264.2462",
        Val::Float(20713257.3385426),
    );
}

#[test]
fn int() {
    assert(
        "6 + 3452 − (3252 × 5324) + (((2342 × 3242) ÷ 4234) × 4234) − 324",
        Val::Int(-9717750),
    );
}

#[test]
fn add_assign() {
    assert("var a = 4; a += 8; a", Val::Int(12));
}

#[test]
fn sub_assign() {
    assert("var a = 4; a -= 8; a", Val::Int(-4));
}

#[test]
fn mul_assign() {
    assert("var a = 4; a *= 8; a", Val::Int(32));
}

#[test]
fn div_assign() {
    assert("var a = 8; a /= 4; a", Val::Int(2));
}

#[test]
fn to_deg() {
    assert("to_deg(PI)", Val::Int(180));
    assert("to_deg(PI / 2)", Val::Int(90));
}

#[test]
fn to_rad() {
    assert("to_rad(180)", Val::Float(consts::PI));
    assert("to_rad(45)", Val::Float(consts::FRAC_PI_4));
}

#[test]
fn string_escape() {
    assert(r#""\b""#, Val::Str("\x08".into()));
    assert(r#""\t""#, Val::Str("\t".into()));
    assert(r#""\n""#, Val::Str("\n".into()));
    assert(r#""\r""#, Val::Str("\r".into()));
    assert(r#""\x1b""#, Val::Str("\x1b".into()));
    assert(r#""\x{1a}""#, Val::Str("\u{1a}".into()));
    assert(r#""\u03a0""#, Val::Str("\u{03a0}".into()));
    assert(r#""\u{102230}""#, Val::Str("\u{102230}".into()));
    assert(r#"  "\u03c0"  "#, Val::Str("\u{03c0}".into()));
}

#[test]
fn string_multi_line() {
    assert(
        r#""hello \
        there""#,
        Val::Str("hello there".into()),
    );
    assert("\"hello\nthere\n\"", Val::Str("hello\nthere\n".into()))
}

#[test]
fn unicode_operators() {
    assert(
        "(23423 × 423 + (423 − 234) ÷ 654 + 4324) × 4234",
        Val::Float(41968480425.587155963),
    );
}

#[test]
fn sign() {
    assert("1 - 2 * -2", Val::Int(5));
}

#[test]
fn pow() {
    assert("2^-1", Val::Float(0.5));
    assert("3^3", Val::Int(27));
    assert("9^0.5", Val::Int(3));
}

#[test]
fn euclid_div() {
    assert("8 div 3", Val::Int(2));
}

#[test]
fn remainder() {
    assert("8 % 3", Val::Int(2));
}

#[test]
fn negative_remainder() {
    assert("-8 % 3", Val::Int(1));
    assert("8 % -5", Val::Int(-2));
}

#[test]
fn gcd() {
    assert("gcd(6, 9)", Val::Int(3));
    assert("gcd(4, 0)", Val::Int(4));
    assert("gcd(0, 5)", Val::Int(5));
}

#[test]
fn factorial() {
    assert("8!", Val::Int(8 * 7 * 6 * 5 * 4 * 3 * 2 * 1));
}

#[test]
fn factorial_overflow() {
    assert_err(
        "34!",
        Error::FactorialOverflow(ValRange::new(Val::Int(34), CRange::of(0, 2))),
    );
}

#[test]
fn factorial_fraction() {
    assert_err(
        "4.1!",
        Error::FractionFactorial(ValRange::new(Val::Float(4.1), CRange::of(0, 3))),
    );
}

#[test]
fn factorial_negative() {
    assert_err(
        "(-3)!",
        Error::NegativeFactorial(ValRange::new(Val::Int(-3), CRange::of(0, 4))),
    );
}

#[test]
fn squareroot() {
    assert("sqrt(625)", Val::Int(25));
}

#[test]
fn binomial_coefficient() {
    assert("ncr(6, 2)", Val::Int(15));
    assert("ncr(23, 0)", Val::Int(1));
}

#[test]
fn binomial_coefficient_invalid() {
    assert_err(
        "ncr(3, 4)",
        Error::InvalidNcr(
            ValRange::new(Val::Int(3), CRange::pos(4)),
            ValRange::new(Val::Int(4), CRange::pos(7)),
        ),
    );
}

#[test]
fn binomial_coefficient_negative() {
    assert_err(
        "ncr(5, -3)",
        Error::NegativeNcr(ValRange::new(Val::Int(-3), CRange::of(7, 9))),
    );
}

#[test]
fn ln() {
    assert("ln(E^27)", Val::Int(27));
}

#[test]
fn log2() {
    assert("log(2, 8)", Val::Int(3));
}

#[test]
fn log10() {
    assert("log(10, 100000)", Val::Int(5));
}

#[test]
fn min() {
    assert("min(3, 7, 5)", Val::Int(3));
}

#[test]
fn max() {
    assert("max(3, 7, 5)", Val::Int(7));
}

#[test]
fn clamp() {
    assert("clamp(9, -2, 23)", Val::Int(9));
    assert("clamp(-12, -5, 5)", Val::Int(-5));
    assert("clamp(31, 0, 7)", Val::Int(7));
}

#[test]
fn clamp_bounds() {
    assert_err(
        "clamp(0, 5, 4)",
        Error::InvalidClampBounds(
            ValRange::new(Val::Int(5), CRange::pos(9)),
            ValRange::new(Val::Int(4), CRange::pos(12)),
        ),
    );
}

#[test]
fn clamp_bounds_float() {
    assert_err(
        "clamp(0, 5.3, 4.5)",
        Error::InvalidClampBounds(
            ValRange::new(Val::Float(5.3), CRange::of(9, 12)),
            ValRange::new(Val::Float(4.5), CRange::of(14, 17)),
        ),
    );
}

#[test]
fn eq() {
    assert("false == false", Val::Bool(true));
    assert("false == true", Val::Bool(false));
    assert("2.0 == 2", Val::Bool(true));
    assert("4 == 4.2", Val::Bool(false));
    assert("7.8 == 7.8", Val::Bool(true));
    assert("5.1 == 5.12", Val::Bool(false));
}

#[test]
fn ne() {
    assert("false != false", Val::Bool(false));
    assert("false != true", Val::Bool(true));
    assert("2.0 != 2", Val::Bool(false));
    assert("4 != 4.2", Val::Bool(true));
    assert("7.8 != 7.8", Val::Bool(false));
    assert("5.1 != 5.12", Val::Bool(true));
}

#[test]
fn lt() {
    assert("2 < 5", Val::Bool(true));
    assert("3 < 3", Val::Bool(false));
    assert("8 < 7", Val::Bool(false));
}

#[test]
fn le() {
    assert("2 <= 5", Val::Bool(true));
    assert("3 <= 3", Val::Bool(true));
    assert("8 <= 7", Val::Bool(false));
}

#[test]
fn gt() {
    assert("2 > 5", Val::Bool(false));
    assert("3 > 3", Val::Bool(false));
    assert("8 > 7", Val::Bool(true));
}

#[test]
fn ge() {
    assert("2 >= 5", Val::Bool(false));
    assert("3 >= 3", Val::Bool(true));
    assert("8 >= 7", Val::Bool(true));
}

#[test]
fn or() {
    assert("false || false", Val::Bool(false));
    assert("false || true", Val::Bool(true));
    assert("true || false", Val::Bool(true));
    assert("true || true", Val::Bool(true));
}

#[test]
fn and() {
    assert("false && false", Val::Bool(false));
    assert("false && true", Val::Bool(false));
    assert("true && false", Val::Bool(false));
    assert("true && true", Val::Bool(true));
}

#[test]
fn bw_or() {
    assert("true | false", Val::Bool(true));
    assert("12 | 2", Val::Int(14));
}

#[test]
fn bw_and() {
    assert("true & false", Val::Bool(false));
    assert("6 & 12", Val::Int(4));
}

#[test]
fn not() {
    assert("!true", Val::Bool(false));
    assert("!false", Val::Bool(true));
}

#[test]
fn unmatched_par() {
    assert_err(
        "4 ) + 5)",
        Error::UnexpectedPar(Par::new(ParT::RoundClose, CRange::pos(2))),
    );
}

#[test]
fn newline_sep() {
    assert("val x = 7\n x", Val::Int(7));
}

#[test]
fn newline_ignored_after_op() {
    assert("val x = 9 + \n 12 \n x", Val::Int(21));
}

#[test]
fn newline_ignored_before_op() {
    assert("val x = 34\n - 45 \n x", Val::Int(-11));
}

#[test]
fn assertion() {
    assert_unit("assert(5 == 5)");
}

#[test]
fn assertion_failed() {
    assert_err("assert(4 == 5)", Error::AssertFailed(CRange::of(7, 13)));
}

#[test]
fn assertion_eq() {
    assert_unit("assert_eq(false, 4 == 3)");
}

#[test]
fn assertion_eq_failed() {
    assert_err(
        "assert_eq(false, 5 == 5)",
        Error::AssertEqFailed(
            ValRange::new(Val::Bool(false), CRange::of(10, 15)),
            ValRange::new(Val::Bool(true), CRange::of(17, 23)),
        ),
    );
}

#[test]
fn if_statement() {
    assert("var x = 2; if 4 == 3 + 1 { x += 3 }; x", Val::Int(5));
    assert(
        r#"
        var x = 2
        if false {
            x += 3
        } else if true {
            x -= 4
        } else {
            x = 9
        }
        x
        "#,
        Val::Int(-2),
    );
}

#[test]
fn if_expr() {
    assert("val x = if false { 3 } else { 9 }; x", Val::Int(9));
    assert(
        r#"val x = if true { "hi" } else { "there" }; x"#,
        Val::Str("hi".into()),
    );
}

#[test]
fn while_loop() {
    assert(
        r#"
        var i = 0
        while i < 10 {
            i += 1
        }
        i
        "#,
        Val::Int(10),
    );
    assert(
        r#"
        var i = 20
        var c = 0;
        while i > 0 {
            i = i div 2
            c += 1
        }
        c
        "#,
        Val::Int(5),
    );
}

#[test]
fn for_loop() {
    assert(
        r#"
        var sum = 0
        for i in 0..10 {
            sum += i
        }
        sum
        "#,
        Val::Int((0..10).sum()),
    );
    assert(
        r#"
        var sum = 0
        for i in 1..=10 {
            sum += i
        }
        sum
        "#,
        Val::Int((1..=10).sum()),
    );
}

#[test]
fn function() {
    assert(
        r#"
        fun factorial(n) {
            var f = 1
            for i in 1..=n {
                f *= i;
            }
            f
        }
        factorial(4)
        "#,
        Val::Int(1 * 2 * 3 * 4),
    )
}
