use crate::{LRed, LYellow, UserFacing};
use crate::{Num, Op, Par, Range};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
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
    InvalidNumberFormat(Range),
    DivideByZero(Num, Num),
    NegativeFactorial(Range),
}

impl UserFacing<LRed> for Error {
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
                format!(
                    "Missing {} command argument{}, {} {} required, but only {} {} found",
                    missing, arg_s, expected, are_is, found, were_was,
                )
            }
            Self::UnexpectedCommandArguments {
                expected, found, ..
            } => {
                let over = found - expected;
                let arg_s = if over == 1 { "s" } else { "" };
                let are_is = if *expected == 1 { "is" } else { "are" };
                let were_was = if *found == 1 { "was" } else { "were" };
                format!(
                    "Found {} unexpected command argument{}, only {} {} required, but {} {} found",
                    over, arg_s, expected, are_is, found, were_was,
                )
            }
            Self::UnexpectedOperator(_) => "Found an unexpected operator".into(),
            Self::UnexpectedParenthesis(_) => "Found an unexpected parenthesis".into(),
            Self::InvalidNumberFormat(_) => "Invalid number format".into(),
            Self::DivideByZero(_, _) => "Attempted to divide by 0".into(),
            Self::NegativeFactorial(_) => {
                "Attempted to calculate the factorial of a negative number".into()
            }
        }
    }

    fn ranges(&self) -> Vec<Range> {
        match self {
            Self::Parsing(r) => vec![*r],
            Self::MissingOperand(r) => vec![*r],
            Self::MissingOperator(r) => vec![*r],
            Self::MissingCommandParenthesis(r) => vec![*r],
            Self::MissingClosingParenthesis(p) => vec![p.range()],
            Self::MissingCommandArguments { range: pos, .. } => vec![*pos],
            Self::UnexpectedCommandArguments { ranges, .. } => ranges.clone(),
            Self::UnexpectedOperator(o) => vec![o.range()],
            Self::UnexpectedParenthesis(p) => vec![p.range()],
            Self::InvalidNumberFormat(r) => vec![*r],
            Self::DivideByZero(a, b) => vec![a.range, b.range],
            Self::NegativeFactorial(r) => vec![*r],
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Warning {
    ConfusingCase(Range, &'static str),
    NegationBehindAdd(Range, Range),
    NegationBehindSub(Range, Range),
    NegationBehindMul(Range, Range),
    NegationBehindDiv(Range, Range),
    MismatchedParentheses(Par, Par),
}

impl UserFacing<LYellow> for Warning {
    fn description(&self) -> String {
        match self {
            Self::ConfusingCase(_, lit) => format!("Confusing casing, consider writing '{}'", lit),
            Self::NegationBehindAdd(_, _) => {
                "Negation directly behind addition, consider making this a subtraction".into()
            }
            Self::NegationBehindSub(_, _) => {
                "Negation directly behind subtraction, consider making this an addition".into()
            }
            Self::NegationBehindMul(_, _) => {
                "Negation directly behind multiplication, consider negating the whole term".into()
            }
            Self::NegationBehindDiv(_, _) => {
                "Negation directly behind division, consider negating the whole term".into()
            }
            Self::MismatchedParentheses(_, _) => "Parentheses do not match".into(),
        }
    }

    fn ranges(&self) -> Vec<Range> {
        match self {
            Self::ConfusingCase(r, _) => vec![*r],
            Self::NegationBehindAdd(r1, r2) => vec![*r1, *r2],
            Self::NegationBehindSub(r1, r2) => vec![*r1, *r2],
            Self::NegationBehindMul(r1, r2) => vec![*r1, *r2],
            Self::NegationBehindDiv(r1, r2) => vec![*r1, *r2],
            Self::MismatchedParentheses(a, b) => vec![a.range(), b.range()],
        }
    }
}
