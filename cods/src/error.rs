use std::fmt;

use crate::{Fun, Sep, SepT, Sign, ValRange};
use crate::{Op, Par, Range};

pub type Result<T> = std::result::Result<T, Error>;

pub trait UserFacing: Sized + fmt::Debug {
    fn description(&self) -> String;
    fn ranges(&self) -> Vec<Range>;
}

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    Parsing(Range),
    MissingOperand(Range),
    MissingOperator(Range),
    MissingClosingParenthesis(Par),
    MissingFunctionParentheses(Range),
    MissingFunctionArguments {
        range: Range,
        expected: usize,
        found: usize,
    },
    UnexpectedFunctionArguments {
        ranges: Vec<Range>,
        expected: usize,
        found: usize,
    },
    UnexpectedOperator(Op),
    UnexpectedSeparator(Sep),
    UnexpectedParenthesis(Par),
    InvalidChar(Range),
    UndefinedVar(String, Range),
    InvalidNumberFormat(Range),
    AddOverflow(ValRange, ValRange),
    SubOverflow(ValRange, ValRange),
    MulOverflow(ValRange, ValRange),
    PowOverflow(ValRange, ValRange),
    DivideByZero(ValRange, ValRange),
    FractionEuclidDiv(ValRange, ValRange),
    RemainderByZero(ValRange, ValRange),
    FractionRemainder(ValRange, ValRange),
    FractionGcd(ValRange, ValRange),
    NegativeNcr(ValRange, ValRange),
    InvalidNcr(ValRange, ValRange),
    FractionNcr(ValRange, ValRange),
    FactorialOverflow(ValRange),
    NegativeFactorial(ValRange),
    FractionFactorial(ValRange),
    InvalidClampBounds(ValRange, ValRange),
    MissingExpr,
    InvalidAssignment(Range, Range),
    ExpectedValue(Range),
    ExpectedNumber(ValRange),
}

impl UserFacing for Error {
    fn description(&self) -> String {
        match self {
            Self::Parsing(_) => "A parsing error occured".into(),
            Self::MissingOperand(_) => "Missing an operand".into(),
            Self::MissingOperator(_) => "Missing an operator".into(),
            Self::MissingFunctionParentheses(_) => "Missing function call parentheses".into(),
            Self::MissingClosingParenthesis(_) => "Missing a closing parenthesis".into(),
            Self::MissingFunctionArguments {
                expected, found, ..
            } => {
                let missing = expected - found;
                let arg_s = if missing == 1 { "" } else { "s" };
                let are_is = if *expected == 1 { "is" } else { "are" };
                let were_was = if *found == 1 { "was" } else { "were" };
                format!("Missing {missing} function argument{arg_s}, {expected} {are_is} required, but only {found} {were_was} found")
            }
            Self::UnexpectedFunctionArguments {
                expected, found, ..
            } => {
                let over = found - expected;
                let arg_s = if over == 1 { "" } else { "s" };
                let are_is = if *expected == 1 { "is" } else { "are" };
                let were_was = if *found == 1 { "was" } else { "were" };
                format!("Found {over} unexpected function argument{arg_s}, only {expected} {are_is} required, but {found} {were_was} found")
            }
            Self::UnexpectedOperator(_) => "Found an unexpected operator".into(),
            Self::UnexpectedSeparator(_) => "Found an unexpected separator".into(),
            Self::UnexpectedParenthesis(_) => "Found an unexpected parenthesis".into(),
            Self::InvalidChar(_) => "Unknown value".into(),
            Self::UndefinedVar(name, _) => format!("Undefined variable '{name}'"),
            Self::InvalidNumberFormat(_) => "Invalid number format".into(),
            Self::AddOverflow(_, _) => "Addition would overflow".into(),
            Self::SubOverflow(_, _) => "Subtraction would overflow".into(),
            Self::MulOverflow(_, _) => "Multiplication would overflow".into(),
            Self::PowOverflow(_, _) => "Exponentiation would overflow".into(),
            Self::DivideByZero(_, _) => "Attempted to divide by 0".into(),
            Self::FractionEuclidDiv(_, _) => "Attempted divide fractions with remainder".into(),
            Self::RemainderByZero(_, _) => {
                "Attempted to calculate the remainder with a divisor of 0".into()
            }
            Self::FractionRemainder(_, _) => {
                "Attempted to calculate the remainder of a division of fractions".into()
            }
            Self::FractionGcd(_, _) => {
                "Attempted to calculate the greatest common divisor of fractions".into()
            }
            Self::FractionNcr(_, _) => {
                "Attempted to calculate the binomial coefficent of fractions".into()
            }
            Self::NegativeNcr(_, _) => {
                "Attempted to calculate the binomial coefficent with r < 0".into()
            }
            Self::InvalidNcr(_, _) => {
                "Attempted to calculate the binomial coefficent with n < r".into()
            }
            Self::NegativeFactorial(_) => {
                "Attempted to calculate the factorial of a negative number".into()
            }
            Self::FactorialOverflow(v) => format!("Factorial of '{v}' would overflow"),
            Self::FractionFactorial(_) => {
                "Attempted to calculate the factorial of a fraction".into()
            }
            Self::InvalidClampBounds(min, max) => {
                format!("Invalid clamp bounds '{min}' is greater than '{max}'")
            }
            Self::MissingExpr => "Missing expression".into(),
            Self::InvalidAssignment(_, _) => {
                "Cannot assign to something that is not a variable".into()
            }
            Self::ExpectedValue(_) => "Expected a value found unit".into(),
            Self::ExpectedNumber(v) => {
                format!("Expected a number found '{v}' of type {}", v.type_name())
            }
        }
    }

    fn ranges(&self) -> Vec<Range> {
        match self {
            Self::Parsing(r) => vec![*r],
            Self::MissingOperand(r) => vec![*r],
            Self::MissingOperator(r) => vec![*r],
            Self::MissingFunctionParentheses(r) => vec![*r],
            Self::MissingClosingParenthesis(p) => vec![p.range],
            Self::MissingFunctionArguments { range: pos, .. } => vec![*pos],
            Self::UnexpectedFunctionArguments { ranges, .. } => ranges.clone(),
            Self::UnexpectedOperator(o) => vec![o.range],
            Self::UnexpectedSeparator(s) => vec![s.range],
            Self::UnexpectedParenthesis(p) => vec![p.range],
            Self::InvalidChar(r) => vec![*r],
            Self::UndefinedVar(_, r) => vec![*r],
            Self::InvalidNumberFormat(r) => vec![*r],
            Self::AddOverflow(a, b) => vec![a.range, b.range],
            Self::SubOverflow(a, b) => vec![a.range, b.range],
            Self::MulOverflow(a, b) => vec![a.range, b.range],
            Self::PowOverflow(a, b) => vec![a.range, b.range],
            Self::DivideByZero(a, b) => vec![a.range, b.range],
            Self::FractionEuclidDiv(a, b) => vec![a.range, b.range],
            Self::RemainderByZero(a, b) => vec![a.range, b.range],
            Self::FractionRemainder(a, b) => vec![a.range, b.range],
            Self::FractionGcd(a, b) => vec![a.range, b.range],
            Self::FractionNcr(a, b) => vec![a.range, b.range],
            Self::NegativeNcr(a, b) => vec![a.range, b.range],
            Self::InvalidNcr(a, b) => vec![a.range, b.range],
            Self::FactorialOverflow(v) => vec![v.range],
            Self::NegativeFactorial(v) => vec![v.range],
            Self::FractionFactorial(v) => vec![v.range],
            Self::InvalidClampBounds(min, max) => vec![min.range, max.range],
            Self::MissingExpr => vec![],
            Self::InvalidAssignment(a, b) => vec![*a, *b],
            Self::ExpectedValue(r) => vec![*r],
            Self::ExpectedNumber(v) => vec![v.range],
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Warning {
    ConfusingCase(Range, &'static str),
    SignFollowingAddition(Range, Range, Sign, usize),
    SignFollowingSubtraction(Range, Range, Sign, usize),
    MultipleSigns(Range, Sign),
    MismatchedParentheses(Par, Par),
    ConfusingFunctionParentheses {
        fun: Fun,
        open_par: Range,
        close_par: Range,
    },
    ConfusingSeparator {
        sep: Sep,
        expected: SepT,
    },
}

impl UserFacing for Warning {
    fn description(&self) -> String {
        match self {
            Self::ConfusingCase(_, lit) => format!("Confusing casing, consider writing '{lit}'"),
            Self::SignFollowingAddition(_, _, s, c) => {
                let sign_s = if *c == 1 { "" } else { "s" };
                let suggestion = if s.is_positive() {
                    "consider removing them"
                } else {
                    "consider making this a subtraction"
                };
                format!("Sign{sign_s} following an addition, {suggestion}")
            }
            Self::SignFollowingSubtraction(_, _, s, c) => {
                let sign_s = if *c == 1 { "" } else { "s" };
                let suggestion = if s.is_positive() {
                    "consider making this an addition"
                } else {
                    "consider removing them"
                };
                format!("Sign{sign_s} following a subtraction, {suggestion}")
            }
            Self::MultipleSigns(_, s) => {
                if s.is_positive() {
                    "Multiple consecutive signs canceling each other out, consider removing them"
                        .into()
                } else {
                    "Multiple consecutive signs, consider using a single negation".into()
                }
            }
            Self::MismatchedParentheses(_, _) => "Parentheses do not match".into(),
            Self::ConfusingFunctionParentheses { .. } => {
                "Functions should use round parentheses".into()
            }
            Self::ConfusingSeparator { sep, expected } => {
                format!("Confusing separator, expected {expected} found {}", sep.typ)
            }
        }
    }

    fn ranges(&self) -> Vec<Range> {
        match self {
            Self::ConfusingCase(r, _) => vec![*r],
            Self::SignFollowingAddition(or, sr, s, _) => {
                if s.is_positive() {
                    vec![*sr]
                } else {
                    vec![*or, *sr]
                }
            }
            Self::SignFollowingSubtraction(or, sr, s, _) => {
                if s.is_positive() {
                    vec![*or, *sr]
                } else {
                    vec![*sr]
                }
            }
            Self::MultipleSigns(r, _) => vec![*r],
            Self::MismatchedParentheses(a, b) => vec![a.range, b.range],
            Self::ConfusingFunctionParentheses {
                fun,
                open_par,
                close_par,
            } => vec![fun.range, *open_par, *close_par],
            Self::ConfusingSeparator { sep, .. } => vec![sep.range],
        }
    }
}
