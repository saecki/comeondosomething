use std::error;
use std::fmt::{self, Debug, Display};

use crate::{CRange, Item, Kw, KwT, Op, OpT, Par, SepT};
use crate::{Sep, ValRange};

pub type Result<T> = std::result::Result<T, Error>;

pub trait UserFacing: Sized + Debug + Display {
    fn ranges(&self) -> Vec<CRange>;
}

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    NotImplemented(&'static str, CRange),

    // Lex
    InvalidChar(CRange),
    InvalidNumberFormat(CRange),
    InvalidEscapeChar(char, CRange),
    MissingEscapeChar(CRange),
    InvalidUnicodeEscapeChar(char, CRange),
    MissingUnicodeEscapeChar {
        range: CRange,
        expected: usize,
        found: usize,
    },
    MissingClosingUnicodeEscapePar(CRange, CRange),
    OverlongUnicodeEscape(CRange),
    InvalidUnicodeScalar(u32, CRange),
    MissingClosingQuote(CRange),

    // Group
    MissingClosingPar(Par),
    UnexpectedPar(Par),

    // Parse
    MissingOperand(CRange),
    MissingOperator(CRange),
    MissingFunArgs {
        range: CRange,
        expected: usize,
        found: usize,
    },
    UnexpectedFunArgs {
        ranges: Vec<CRange>,
        expected: usize,
        found: usize,
    },
    UnexpectedItem(Item),
    UnexpectedOperator(Op),
    UnexpectedSeparator(Sep),
    ExpectedBlock(CRange),
    ExpectedFunPars(CRange),
    ExpectedIdent(CRange),
    ExpectedOp(OpT, CRange),
    ExpectedSep(SepT, CRange),
    ExpectedKw(KwT, CRange),
    WrongContext(Kw),

    // Eval
    MissingExpr,
    ExpectedValue(CRange),
    ExpectedNumber(ValRange),
    ExpectedInt(ValRange),
    ExpectedBool(ValRange),
    ExpectedStr(ValRange),
    ExpectedRange(ValRange),
    Parsing(CRange),
    UndefinedVar(String, CRange),
    ImmutableAssign(String, CRange, CRange),
    UndefinedFun(String, CRange),
    RedefinedFun(String, CRange, CRange),
    RedefinedBuiltinFun(String, CRange),
    AddOverflow(ValRange, ValRange),
    SubOverflow(ValRange, ValRange),
    MulOverflow(ValRange, ValRange),
    PowOverflow(ValRange, ValRange),
    DivideByZero(ValRange, ValRange),
    FractionEuclidDiv(ValRange, ValRange),
    RemainderByZero(ValRange, ValRange),
    FractionRemainder(ValRange, ValRange),
    FractionGcd(ValRange, ValRange),
    NegativeNcr(ValRange),
    InvalidNcr(ValRange, ValRange),
    FractionNcr(ValRange, ValRange),
    FactorialOverflow(ValRange),
    NegativeFactorial(ValRange),
    FractionFactorial(ValRange),
    InvalidClampBounds(ValRange, ValRange),
    InvalidBwOr(ValRange, ValRange),
    InvalidBwAnd(ValRange, ValRange),
    InvalidAssignment(CRange, CRange),
    AssertFailed(CRange),
    AssertEqFailed(ValRange, ValRange),
}

impl error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NotImplemented(m, _) => write!(f, "{m}"),

            // Lex
            Self::InvalidChar(_) => write!(f, "Invalid character"),
            Self::InvalidNumberFormat(_) => write!(f, "Invalid number format"),
            Self::InvalidEscapeChar(c, _) => {
                write!(f, "Invalid escape character: '{}'", c.escape_default())
            }
            Self::MissingEscapeChar(_) => write!(f, "Missing escape character"),
            Self::InvalidUnicodeEscapeChar(c, _) => {
                write!(
                    f,
                    "Invalid unicode escape character: '{}'",
                    c.escape_default()
                )
            }
            Self::MissingUnicodeEscapeChar {
                expected, found, ..
            } => {
                let missing = expected - found;
                let char_s = if missing == 1 { "" } else { "s" };
                write!(f, "Missing {missing} unicode escape character{char_s}, expected {expected}, but found {found}")
            }
            Self::MissingClosingUnicodeEscapePar(_, _) => write!(
                f,
                "Missing a closing parenthesis for the unicode escape sequence"
            ),
            Self::OverlongUnicodeEscape(_) => {
                write!(
                    f,
                    "Overlong unicode escape sequence, must be at most 6 digits"
                )
            }
            Self::InvalidUnicodeScalar(cp, _) => {
                write!(f, "Invalid unicode scalar value: '{cp:x}'")
            }
            Self::MissingClosingQuote(_) => write!(f, "Missing closing quote"),

            // Group
            Self::MissingClosingPar(_) => write!(f, "Missing closing parenthesis"),
            Self::UnexpectedPar(_) => write!(f, "Unexpected parenthesis"),

            // Parse
            Self::MissingOperand(_) => write!(f, "Missing operand"),
            Self::MissingOperator(_) => write!(f, "Missing operator"),
            Self::ExpectedFunPars(_) => write!(f, "Missing function parentheses"),
            Self::MissingFunArgs {
                expected, found, ..
            } => {
                let missing = expected - found;
                let arg_s = if missing == 1 { "" } else { "s" };
                let are_is = if *expected == 1 { "is" } else { "are" };
                let were_was = if *found == 1 { "was" } else { "were" };
                write!(f, "Missing {missing} function argument{arg_s}, {expected} {are_is} required, but only {found} {were_was} found")
            }
            Self::UnexpectedItem(_) => write!(f, "Unexpected item"),
            Self::UnexpectedFunArgs {
                expected, found, ..
            } => {
                let over = found - expected;
                let arg_s = if over == 1 { "" } else { "s" };
                let are_is = if *expected == 1 { "is" } else { "are" };
                let were_was = if *found == 1 { "was" } else { "were" };
                write!(f, "Found {over} unexpected function argument{arg_s}, only {expected} {are_is} required, but {found} {were_was} found")
            }
            Self::UnexpectedOperator(_) => write!(f, "Unexpected operator"),
            Self::UnexpectedSeparator(_) => write!(f, "Unexpected separator"),
            Self::ExpectedBlock(_) => write!(f, "Expected a block"),
            Self::ExpectedIdent(_) => write!(f, "Expected identifier"),
            Self::ExpectedOp(o, _) => write!(f, "Expected '{o}'"),
            Self::ExpectedKw(k, _) => write!(f, "Expected '{}'", k.name()),
            Self::ExpectedSep(s, _) => write!(f, "Expected '{s}'"),
            Self::WrongContext(k) => write!(f, "'{}' wasn't expected in this context", k.name()),

            // Eval
            Self::MissingExpr => write!(f, "Missing expression"),
            Self::ExpectedValue(_) => write!(f, "Expected a value found unit"),
            Self::ExpectedNumber(v) => {
                write!(f, "Expected a number found '{v}' of type {}", v.type_name())
            }
            Self::ExpectedInt(v) => {
                write!(f, "Expected an int found '{v}' of type {}", v.type_name())
            }
            Self::ExpectedBool(v) => {
                write!(f, "Expected a bool found '{v}' of type {}", v.type_name())
            }
            Self::ExpectedStr(v) => {
                write!(f, "Expected a str found '{v}' of type {}", v.type_name())
            }
            Self::ExpectedRange(v) => {
                write!(f, "Expected a range found '{v}' of type {}", v.type_name())
            }
            Self::Parsing(_) => write!(f, "A parsing error occured"),

            Self::UndefinedVar(name, _) => write!(f, "Undefined variable '{name}'"),
            Self::ImmutableAssign(name, _, _) => {
                write!(f, "Cannot assign twice to immutable variable '{name}'")
            }
            Self::UndefinedFun(name, _) => write!(f, "Undefined function '{name}'"),
            Self::RedefinedFun(name, _, _) => write!(f, "Redefined function '{name}'"),
            Self::RedefinedBuiltinFun(name, _) => write!(f, "Redefined builtin function '{name}'"),
            Self::AddOverflow(_, _) => write!(f, "Addition would overflow"),
            Self::SubOverflow(_, _) => write!(f, "Subtraction would overflow"),
            Self::MulOverflow(_, _) => write!(f, "Multiplication would overflow"),
            Self::PowOverflow(_, _) => write!(f, "Exponentiation would overflow"),
            Self::DivideByZero(_, _) => write!(f, "Attempted to divide by 0"),
            Self::FractionEuclidDiv(_, _) => write!(f, "Attempted divide fractions with remainder"),
            Self::RemainderByZero(_, _) => {
                write!(
                    f,
                    "Attempted to calculate the remainder with a divisor of 0"
                )
            }
            Self::FractionRemainder(_, _) => {
                write!(
                    f,
                    "Attempted to calculate the remainder of a division of fractions"
                )
            }
            Self::FractionGcd(_, _) => {
                write!(
                    f,
                    "Attempted to calculate the greatest common divisor of fractions"
                )
            }
            Self::FractionNcr(_, _) => {
                write!(
                    f,
                    "Attempted to calculate the binomial coefficent of fractions"
                )
            }
            Self::NegativeNcr(r) => {
                write!(
                    f,
                    "Attempted to calculate the binomial coefficent with a negative value for r: '{r}'"
                )
            }
            Self::InvalidNcr(n, r) => {
                write!(
                    f,
                    "Attempted to calculate the binomial coefficent with n: '{n}' less than r: '{r}'"
                )
            }
            Self::NegativeFactorial(_) => {
                write!(
                    f,
                    "Attempted to calculate the factorial of a negative number"
                )
            }
            Self::FactorialOverflow(v) => write!(f, "Factorial of '{v}' would overflow"),
            Self::FractionFactorial(_) => {
                write!(f, "Attempted to calculate the factorial of a fraction")
            }
            Self::InvalidClampBounds(min, max) => {
                write!(
                    f,
                    "Invalid clamp bounds min: '{min}' is greater than max: '{max}'"
                )
            }
            Self::InvalidBwOr(a, b) => {
                write!(
                    f,
                    "A bitwise or can only be applied to two ints or two bools, not '{a}' of type {} and '{b}' of type {}",
                    a.type_name(),
                    b.type_name(),
                )
            }
            Self::InvalidBwAnd(a, b) => {
                write!(
                    f,
                    "A bitwise and can only be applied to two ints or two bools, not '{a}' of type {} and '{b}' of type {}",
                    a.type_name(),
                    b.type_name(),
                )
            }
            Self::InvalidAssignment(_, _) => {
                write!(f, "Cannot assign to something that is not a variable")
            }
            Self::AssertFailed(_) => {
                write!(f, "Assertion failed")
            }
            Self::AssertEqFailed(a, b) => {
                write!(f, "Assertion failed: '{a}' == '{b}'")
            }
        }
    }
}

impl UserFacing for Error {
    fn ranges(&self) -> Vec<CRange> {
        match self {
            // Lex
            Self::InvalidChar(r) => vec![*r],
            Self::InvalidNumberFormat(r) => vec![*r],
            Self::InvalidEscapeChar(_, r) => vec![*r],
            Self::MissingEscapeChar(r) => vec![*r],
            Self::MissingUnicodeEscapeChar { range, .. } => vec![*range],
            Self::MissingClosingUnicodeEscapePar(s, e) => vec![*s, *e],
            Self::InvalidUnicodeEscapeChar(_, r) => vec![*r],
            Self::OverlongUnicodeEscape(r) => vec![*r],
            Self::InvalidUnicodeScalar(_, r) => vec![*r],
            Self::MissingClosingQuote(r) => vec![*r],

            // Group
            Self::MissingClosingPar(p) => vec![p.range],
            Self::UnexpectedPar(p) => vec![p.range],

            // Parse
            Self::MissingOperand(r) => vec![*r],
            Self::MissingOperator(r) => vec![*r],
            Self::MissingFunArgs { range: pos, .. } => vec![*pos],
            Self::UnexpectedItem(i) => vec![i.range()],
            Self::UnexpectedFunArgs { ranges, .. } => ranges.clone(),
            Self::UnexpectedOperator(o) => vec![o.range],
            Self::UnexpectedSeparator(s) => vec![s.range],
            Self::ExpectedBlock(r) => vec![*r],
            Self::ExpectedFunPars(r) => vec![*r],
            Self::ExpectedIdent(r) => vec![*r],
            Self::ExpectedOp(_, r) => vec![*r],
            Self::ExpectedKw(_, r) => vec![*r],
            Self::ExpectedSep(_, r) => vec![*r],
            Self::WrongContext(k) => vec![k.range],

            // Eval
            Self::MissingExpr => vec![],
            Self::ExpectedValue(r) => vec![*r],
            Self::ExpectedNumber(v) => vec![v.range],
            Self::ExpectedInt(v) => vec![v.range],
            Self::ExpectedBool(v) => vec![v.range],
            Self::ExpectedStr(v) => vec![v.range],
            Self::ExpectedRange(v) => vec![v.range],
            Self::Parsing(r) => vec![*r],
            Self::UndefinedVar(_, r) => vec![*r],
            Self::ImmutableAssign(_, a, b) => vec![*a, *b],
            Self::UndefinedFun(_, r) => vec![*r],
            Self::RedefinedFun(_, a, b) => vec![*a, *b],
            Self::RedefinedBuiltinFun(_, r) => vec![*r],
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
            Self::NegativeNcr(a) => vec![a.range],
            Self::InvalidNcr(a, b) => vec![a.range, b.range],
            Self::FactorialOverflow(v) => vec![v.range],
            Self::NegativeFactorial(v) => vec![v.range],
            Self::FractionFactorial(v) => vec![v.range],
            Self::InvalidClampBounds(min, max) => vec![min.range, max.range],
            Self::InvalidBwOr(a, b) => vec![a.range, b.range],
            Self::InvalidBwAnd(a, b) => vec![a.range, b.range],
            Self::InvalidAssignment(a, b) => vec![*a, *b],
            Self::AssertFailed(r) => vec![*r],
            Self::AssertEqFailed(a, b) => vec![a.range, b.range],
            Self::NotImplemented(_, r) => vec![*r],
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Warning {
    // SignFollowingAddition(Range, Range, Sign, usize),
// SignFollowingSubtraction(Range, Range, Sign, usize),
// MultipleSigns(Range, Sign),
}

impl Display for Warning {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl UserFacing for Warning {
    fn ranges(&self) -> Vec<CRange> {
        todo!()
    }
}
