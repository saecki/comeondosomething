use std::f64::consts;

use cods::{Error, Par, ParT, Span, Val, ValSpan};

fn assert(input: &str, expected: Val) {
    match cods::eval_str(input) {
        Ok(val) => assert_eq!(val, expected),
        Err(e) => {
            panic!("{e:?}");
        }
    }
}

fn assert_err(input: &str, expected: Error) {
    match cods::eval_str(input) {
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
        "6 + 3452 - (3252 * 5324) + (((2342 * 3242) / 4234) * 4234) - 324",
        Val::Int(-9718952),
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
    assert("to_deg(PI)", Val::Float(180.0));
    assert("to_deg(PI / 2.0)", Val::Float(90.0));
}

#[test]
fn to_rad() {
    assert("to_rad(180.0)", Val::Float(consts::PI));
    assert("to_rad(45.0)", Val::Float(consts::FRAC_PI_4));
}

#[test]
fn int_literals() {
    assert("0b010010", Val::Int(0b010010));
    assert("0o72454", Val::Int(0o72454));
    assert("0xbc34", Val::Int(0xbc34));
    assert("442423", Val::Int(442423));
}

#[test]
fn string_escape() {
    assert(r#""\b""#, Val::Str("\x08".into()));
    assert(r#""\e""#, Val::Str("\x1b".into()));
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
fn char_escape() {
    assert(r#"'\b'"#, Val::Char('\x08'));
    assert(r#"'\e'"#, Val::Char('\x1b'.into()));
    assert(r#"'\t'"#, Val::Char('\t'));
    assert(r#"'\n'"#, Val::Char('\n'));
    assert(r#"'\r'"#, Val::Char('\r'));
    assert(r#"'\x1b'"#, Val::Char('\x1b'));
    assert(r#"'\x{1a}'"#, Val::Char('\u{1a}'));
    assert(r#"'\u03a0'"#, Val::Char('\u{03a0}'));
    assert(r#"'\u{102230}'"#, Val::Char('\u{102230}'));
    assert(r#"  '\u03c0'  "#, Val::Char('\u{03c0}'));
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
    assert_err(
        "(23423.0 × 423.0 + (423.0 − 234.0) ÷ 654.0 + 4324.0) × 4234.0",
        crate::Error::InvalidChar(Span::pos(0, 9)),
    );
}

#[test]
fn sign() {
    assert("1 - 2 * -2", Val::Int(5));
}

#[test]
fn pow() {
    assert("pow(2.0, -1.0)", Val::Float(0.5));
    assert("pow(3, 3)", Val::Int(27));
    assert("pow(9.0, 0.5)", Val::Float(3.0));
}

#[test]
fn int_div() {
    assert("8 / 3", Val::Int(2));
}

#[test]
fn remainder() {
    assert("8 % 3", Val::Int(2));
    assert("-8 % 3", Val::Int(-2));
    assert("8 % -3", Val::Int(2));
    assert("-8 % -3", Val::Int(-2));
}

#[test]
fn remainder_euclid() {
    assert("8 % 3", Val::Int(2));
    assert("-8 mod 3", Val::Int(1));
    assert("8 mod -5", Val::Int(-2));
    assert("-8 mod -5", Val::Int(-3));
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
        Error::FactorialOverflow(ValSpan::new(Val::Int(34), Span::cols(0, 0, 2))),
    );
}

#[test]
fn factorial_negative() {
    assert_err(
        "(-3)!",
        Error::NegativeFactorial(ValSpan::new(Val::Int(-3), Span::cols(0, 1, 3))),
    );
}

#[test]
fn squareroot() {
    assert("sqrt(625.0)", Val::Float(25.0));
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
            ValSpan::new(Val::Int(3), Span::pos(0, 4)),
            ValSpan::new(Val::Int(4), Span::pos(0, 7)),
        ),
    );
}

#[test]
fn binomial_coefficient_negative() {
    assert_err(
        "ncr(5, -3)",
        Error::NegativeNcr(ValSpan::new(Val::Int(-3), Span::cols(0, 7, 9))),
    );
}

#[test]
fn ln() {
    assert("ln(pow(E, 27.0))", Val::Float(27.0));
}

#[test]
fn log() {
    assert("log(2.0, 8.0)", Val::Float(3.0));
    assert("log(10.0, 100000.0)", Val::Float(5.0));
}

#[test]
fn min() {
    assert("min(7, 3, 5)", Val::Int(3));
    assert("min(7.0, 3.0, 5.0)", Val::Float(3.0));
}

#[test]
fn max() {
    assert("max(3, 7, 5)", Val::Int(7));
    assert("max(3.0, 7.0, 5.0)", Val::Float(7.0));
}

#[test]
fn clamp() {
    assert("clamp(9, -2, 23)", Val::Int(9));
    assert("clamp(-12, -5, 5)", Val::Int(-5));
    assert("clamp(31, 0, 7)", Val::Int(7));

    assert("clamp(9.0, -2.0, 23.0)", Val::Float(9.0));
    assert("clamp(-12.0, -5.0, 5.0)", Val::Float(-5.0));
    assert("clamp(31.0, 0.0, 7.0)", Val::Float(7.0));
}

#[test]
fn clamp_bounds() {
    assert_err(
        "clamp(0, 5, 4)",
        Error::InvalidClampBounds(
            ValSpan::new(Val::Int(5), Span::pos(0, 9)),
            ValSpan::new(Val::Int(4), Span::pos(0, 12)),
        ),
    );
}

#[test]
fn clamp_bounds_float() {
    assert_err(
        "clamp(0.0, 5.3, 4.5)",
        Error::InvalidClampBounds(
            ValSpan::new(Val::Float(5.3), Span::cols(0, 11, 14)),
            ValSpan::new(Val::Float(4.5), Span::cols(0, 16, 19)),
        ),
    );
}

#[test]
fn abs() {
    assert("abs(34)", Val::Int(34));
    assert("abs(-34)", Val::Int(34));
    assert("abs(54.32)", Val::Float(54.32));
    assert("abs(-54.32)", Val::Float(54.32));
}

#[test]
fn eq() {
    assert("false == false", Val::Bool(true));
    assert("false == true", Val::Bool(false));
    assert("4.0 == 4.2", Val::Bool(false));
    assert("7.8 == 7.8", Val::Bool(true));
    assert("5.1 == 5.12", Val::Bool(false));
}

#[test]
fn ne() {
    assert("false != false", Val::Bool(false));
    assert("false != true", Val::Bool(true));
    assert("4.0 != 4.2", Val::Bool(true));
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
        Error::UnexpectedPar(Par::new(ParT::RoundClose, Span::pos(0, 2))),
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
    assert("assert(5 == 5)", Val::Unit);
}

#[test]
fn assertion_failed() {
    assert_err("assert(4 == 5)", Error::AssertFailed(Span::cols(0, 7, 13)));
}

#[test]
fn assertion_eq() {
    assert("assert_eq(false, 4 == 3)", Val::Unit);
}

#[test]
fn assertion_eq_failed() {
    assert_err(
        "assert_eq(false, 5 == 5)",
        Error::AssertEqFailed(
            ValSpan::new(Val::Bool(false), Span::cols(0, 10, 15)),
            ValSpan::new(Val::Bool(true), Span::cols(0, 17, 23)),
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
            i = i / 2
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
        fun factorial(n: int) -> int {
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

#[test]
fn spill() {
    assert("spill()", Val::Unit)
}

#[test]
fn line_comment() {
    assert("val a = // yeeet\n 5; a", Val::Int(5));
}

#[test]
fn block_comment() {
    assert("val a = /* yeeet */ 5; a", Val::Int(5));
}
