use crate::{Cmd, Ext, LRed, LYellow, Sep, SepType, Sign, UserFacing};
use crate::{Num, Op, Par, Range};

pub type Result<T, V> = std::result::Result<T, Error<V>>;

#[derive(Clone, Debug, PartialEq)]
pub enum Error<T: Ext> {
    Parsing(Range),
    MissingOperand(Range),
    MissingOperator(Range),
    MissingClosingParenthesis(Par),
    MissingCommandParenthesis(Range),
    MissingCommandArguments {
        range: Range,
        expected: usize,
        found: usize,
    },
    UnexpectedCommandArguments {
        ranges: Vec<Range>,
        expected: usize,
        found: usize,
    },
    UnexpectedOperator(Op),
    UnexpectedParenthesis(Par),
    UnexpectedAssignment(Range),
    UnknownValue(Range),
    UndefinedVar(String, Range),
    InvalidNumberFormat(Range),
    AddOverflow(Num<T>, Num<T>),
    SubOverflow(Num<T>, Num<T>),
    MulOverflow(Num<T>, Num<T>),
    DivideByZero(Num<T>, Num<T>),
    FractionEuclidDiv(Num<T>, Num<T>),
    RemainderByZero(Num<T>, Num<T>),
    FractionRemainder(Num<T>, Num<T>),
    FractionGcd(Num<T>, Num<T>),
    NegativeNcr(Num<T>, Num<T>),
    InvalidNcr(Num<T>, Num<T>),
    FractionNcr(Num<T>, Num<T>),
    FactorialOverflow(Num<T>),
    NegativeFactorial(Num<T>),
    FractionFactorial(Num<T>),
    InvalidClampBounds(Num<T>, Num<T>),
}

impl<T: Ext> UserFacing<LRed> for Error<T> {
    fn description(&self) -> String {
        match self {
            Self::Parsing(_) => "A parsing error occured".into(),
            Self::MissingOperand(_) => "Missing an operand".into(),
            Self::MissingOperator(_) => "Missing an operator".into(),
            Self::MissingCommandParenthesis(_) => "Missing a command parenthesis".into(),
            Self::MissingClosingParenthesis(_) => "Missing a closing parenthesis".into(),
            Self::MissingCommandArguments {
                expected, found, ..
            } => {
                let missing = expected - found;
                let arg_s = if missing == 1 { "" } else { "s" };
                let are_is = if *expected == 1 { "is" } else { "are" };
                let were_was = if *found == 1 { "was" } else { "were" };
                format!("Missing {missing} command argument{arg_s}, {expected} {are_is} required, but only {found} {were_was} found")
            }
            Self::UnexpectedCommandArguments {
                expected, found, ..
            } => {
                let over = found - expected;
                let arg_s = if over == 1 { "" } else { "s" };
                let are_is = if *expected == 1 { "is" } else { "are" };
                let were_was = if *found == 1 { "was" } else { "were" };
                format!("Found {over} unexpected command argument{arg_s}, only {expected} {are_is} required, but {found} {were_was} found")
            }
            Self::UnexpectedOperator(_) => "Found an unexpected operator".into(),
            Self::UnexpectedParenthesis(_) => "Found an unexpected parenthesis".into(),
            Self::UnexpectedAssignment(_) => "Unexpected Assignment".into(),
            Self::UnknownValue(_) => "Unknown value".into(),
            Self::UndefinedVar(name, _) => format!("Undefined variable '{name}'"),
            Self::InvalidNumberFormat(_) => "Invalid number format".into(),
            Self::AddOverflow(_, _) => "Addition would overflow".into(),
            Self::SubOverflow(_, _) => "Subtraction would overflow".into(),
            Self::MulOverflow(_, _) => "Multiplication would overflow".into(),
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
            Self::FactorialOverflow(_) => {
                "Factorial would overflow".into()
            }
            Self::FractionFactorial(_) => {
                "Attempted to calculate the factorial of a fraction".into()
            }
            Self::InvalidClampBounds(_, _) => {
                "Invalid clamp bounds min is greater than max".into()
            }
        }
    }

    fn ranges(&self) -> Vec<Range> {
        match self {
            Self::Parsing(r) => vec![*r],
            Self::MissingOperand(r) => vec![*r],
            Self::MissingOperator(r) => vec![*r],
            Self::MissingCommandParenthesis(r) => vec![*r],
            Self::MissingClosingParenthesis(p) => vec![p.range],
            Self::MissingCommandArguments { range: pos, .. } => vec![*pos],
            Self::UnexpectedCommandArguments { ranges, .. } => ranges.clone(),
            Self::UnexpectedOperator(o) => vec![o.range],
            Self::UnexpectedParenthesis(p) => vec![p.range],
            Self::UnexpectedAssignment(r) => vec![*r],
            Self::UnknownValue(r) => vec![*r],
            Self::UndefinedVar(_, r) => vec![*r],
            Self::InvalidNumberFormat(r) => vec![*r],
            Self::AddOverflow(a, b) => vec![a.range, b.range],
            Self::SubOverflow(a, b) => vec![a.range, b.range],
            Self::MulOverflow(a, b) => vec![a.range, b.range],
            Self::DivideByZero(a, b) => vec![a.range, b.range],
            Self::FractionEuclidDiv(a, b) => vec![a.range, b.range],
            Self::RemainderByZero(a, b) => vec![a.range, b.range],
            Self::FractionRemainder(a, b) => vec![a.range, b.range],
            Self::FractionGcd(a, b) => vec![a.range, b.range],
            Self::FractionNcr(a, b) => vec![a.range, b.range],
            Self::NegativeNcr(a, b) => vec![a.range, b.range],
            Self::InvalidNcr(a, b) => vec![a.range, b.range],
            Self::FactorialOverflow(a) => vec![a.range],
            Self::NegativeFactorial(a) => vec![a.range],
            Self::FractionFactorial(a) => vec![a.range],
            Self::InvalidClampBounds(min, max) => vec![min.range, max.range],
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
    ConfusingCommandParentheses {
        cmd: Cmd,
        open_par: Range,
        close_par: Range,
    },
    ConfusingSeparator {
        sep: Sep,
        expected: SepType,
    },
}

impl UserFacing<LYellow> for Warning {
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
            Self::ConfusingCommandParentheses { .. } => {
                "Commands should use round parentheses".into()
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
            Self::ConfusingCommandParentheses {
                cmd,
                open_par,
                close_par,
            } => vec![cmd.range, *open_par, *close_par],
            Self::ConfusingSeparator { sep, .. } => vec![sep.range],
        }
    }
}
