use std::error;
use std::fmt::{self, Debug, Display};

use crate::ValSpan;
use crate::{Item, Kw, KwT, Op, OpT, Par, PctT, Span};

pub type Result<T> = std::result::Result<T, Error>;

pub trait UserFacing: Sized + Debug + Display {
    fn spans(&self) -> Vec<Span>;
}

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    NotImplemented(&'static str, Span),

    // Lex
    InvalidChar(Span),
    InvalidNumberFormat(Span),
    InvalidEscapeChar(char, Span),
    MissingEscapeChar(Span),
    InvalidUnicodeEscapeChar(char, Span),
    MissingUnicodeEscapeChar {
        span: Span,
        expected: usize,
        found: usize,
    },
    MissingClosingUnicodeEscapePar(Span, Span),
    OverlongUnicodeEscape(Span),
    InvalidUnicodeScalar(u32, Span),
    MissingClosingQuote(Span),

    // Group
    MissingClosingPar(Par),
    UnexpectedPar(Par),

    // Parse
    MissingOperand(Span),
    MissingOperator(Span),
    MissingFunArgs {
        span: Span,
        expected: usize,
        found: usize,
    },
    UnexpectedFunArgs {
        spans: Vec<Span>,
        expected: usize,
        found: usize,
    },
    UnexpectedItem(Item),
    UnexpectedOperator(Op),
    ExpectedBlock(Span),
    ExpectedFunPars(Span),
    ExpectedIdent(Span),
    ExpectedOp(OpT, Span),
    ExpectedPct(PctT, Span),
    ExpectedKw(KwT, Span),
    WrongContext(Kw),

    // Eval
    MissingExpr,
    ExpectedValue(Span),
    ExpectedNumber(ValSpan),
    ExpectedInt(ValSpan),
    ExpectedBool(ValSpan),
    ExpectedStr(ValSpan),
    ExpectedRange(ValSpan),
    Parsing(Span),
    UndefinedVar(String, Span),
    UninitializedVar(String, Span, Span),
    RedefinedBuiltinConst(String, Span),
    ImmutableAssign(String, Span, Span),
    UndefinedFun(String, Span),
    RedefinedFun(String, Span, Span),
    RedefinedBuiltinFun(String, Span),
    UnknownType(String, Span),
    AddOverflow(ValSpan, ValSpan),
    SubOverflow(ValSpan, ValSpan),
    MulOverflow(ValSpan, ValSpan),
    PowOverflow(ValSpan, ValSpan),
    DivideByZero(ValSpan, ValSpan),
    FractionEuclidDiv(ValSpan, ValSpan),
    RemainderByZero(ValSpan, ValSpan),
    FractionRemainder(ValSpan, ValSpan),
    FractionGcd(ValSpan, ValSpan),
    NegativeNcr(ValSpan),
    InvalidNcr(ValSpan, ValSpan),
    FractionNcr(ValSpan, ValSpan),
    FactorialOverflow(ValSpan),
    NegativeFactorial(ValSpan),
    FractionFactorial(ValSpan),
    InvalidClampBounds(ValSpan, ValSpan),
    InvalidBwOr(ValSpan, ValSpan),
    InvalidBwAnd(ValSpan, ValSpan),
    InvalidAssignment(Span, Span),
    AssertFailed(Span),
    AssertEqFailed(ValSpan, ValSpan),
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
            Self::ExpectedBlock(_) => write!(f, "Expected a block"),
            Self::ExpectedIdent(_) => write!(f, "Expected identifier"),
            Self::ExpectedOp(o, _) => write!(f, "Expected '{o}'"),
            Self::ExpectedKw(k, _) => write!(f, "Expected '{}'", k.name()),
            Self::ExpectedPct(s, _) => write!(f, "Expected '{s}'"),
            Self::WrongContext(k) => write!(f, "'{}' wasn't expected in this context", k.name()),

            // Eval
            Self::MissingExpr => write!(f, "Missing expression"),
            Self::ExpectedValue(_) => write!(f, "Expected a value found unit"),
            Self::ExpectedNumber(v) => {
                write!(f, "Expected a number found '{v}' of type {}", v.typ())
            }
            Self::ExpectedInt(v) => {
                write!(f, "Expected an int found '{v}' of type {}", v.typ())
            }
            Self::ExpectedBool(v) => {
                write!(f, "Expected a bool found '{v}' of type {}", v.typ())
            }
            Self::ExpectedStr(v) => {
                write!(f, "Expected a str found '{v}' of type {}", v.typ())
            }
            Self::ExpectedRange(v) => {
                write!(f, "Expected a range found '{v}' of type {}", v.typ())
            }
            Self::Parsing(_) => write!(f, "A parsing error occured"),

            Self::UndefinedVar(name, _) => write!(f, "Undefined variable '{name}'"),
            Self::UninitializedVar(name, _, _) => write!(f, "Uninitialized variable '{name}'"),
            Self::RedefinedBuiltinConst(name, _) => {
                write!(f, "Redefined builtin constant '{name}'")
            }
            Self::ImmutableAssign(name, _, _) => {
                write!(f, "Cannot assign twice to immutable variable '{name}'")
            }
            Self::UndefinedFun(name, _) => write!(f, "Undefined function '{name}'"),
            Self::RedefinedFun(name, _, _) => write!(f, "Redefined function '{name}'"),
            Self::RedefinedBuiltinFun(name, _) => write!(f, "Redefined builtin function '{name}'"),
            Self::UnknownType(name, _) => write!(f, "Unknown type '{name}'"),
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
                    a.typ(),
                    b.typ(),
                )
            }
            Self::InvalidBwAnd(a, b) => {
                write!(
                    f,
                    "A bitwise and can only be applied to two ints or two bools, not '{a}' of type {} and '{b}' of type {}",
                    a.typ(),
                    b.typ(),
                )
            }
            Self::InvalidAssignment(_, _) => {
                write!(f, "Cannot assign to something that is not a variable")
            }
            Self::AssertFailed(_) => {
                write!(f, "Assertion failed")
            }
            Self::AssertEqFailed(a, b) => {
                write!(f, "Assertion failed, values are not equal\n left: '{a}'\nright: '{b}'")
            }
        }
    }
}

impl UserFacing for Error {
    fn spans(&self) -> Vec<Span> {
        match self {
            // Lex
            Self::InvalidChar(s) => vec![*s],
            Self::InvalidNumberFormat(s) => vec![*s],
            Self::InvalidEscapeChar(_, s) => vec![*s],
            Self::MissingEscapeChar(s) => vec![*s],
            Self::MissingUnicodeEscapeChar { span, .. } => vec![*span],
            Self::MissingClosingUnicodeEscapePar(s, e) => vec![*s, *e],
            Self::InvalidUnicodeEscapeChar(_, s) => vec![*s],
            Self::OverlongUnicodeEscape(s) => vec![*s],
            Self::InvalidUnicodeScalar(_, s) => vec![*s],
            Self::MissingClosingQuote(s) => vec![*s],

            // Group
            Self::MissingClosingPar(p) => vec![p.span],
            Self::UnexpectedPar(p) => vec![p.span],

            // Parse
            Self::MissingOperand(s) => vec![*s],
            Self::MissingOperator(s) => vec![*s],
            Self::MissingFunArgs { span: pos, .. } => vec![*pos],
            Self::UnexpectedItem(i) => vec![i.span()],
            Self::UnexpectedFunArgs { spans, .. } => spans.clone(),
            Self::UnexpectedOperator(o) => vec![o.span],
            Self::ExpectedBlock(s) => vec![*s],
            Self::ExpectedFunPars(s) => vec![*s],
            Self::ExpectedIdent(s) => vec![*s],
            Self::ExpectedOp(_, s) => vec![*s],
            Self::ExpectedKw(_, s) => vec![*s],
            Self::ExpectedPct(_, s) => vec![*s],
            Self::WrongContext(k) => vec![k.span],

            // Eval
            Self::MissingExpr => vec![],
            Self::ExpectedValue(s) => vec![*s],
            Self::ExpectedNumber(v) => vec![v.span],
            Self::ExpectedInt(v) => vec![v.span],
            Self::ExpectedBool(v) => vec![v.span],
            Self::ExpectedStr(v) => vec![v.span],
            Self::ExpectedRange(v) => vec![v.span],
            Self::Parsing(s) => vec![*s],
            Self::UndefinedVar(_, s) => vec![*s],
            Self::UninitializedVar(_, a, b) => vec![*a, *b],
            Self::RedefinedBuiltinConst(_, s) => vec![*s],
            Self::ImmutableAssign(_, a, b) => vec![*a, *b],
            Self::UndefinedFun(_, s) => vec![*s],
            Self::RedefinedFun(_, a, b) => vec![*a, *b],
            Self::RedefinedBuiltinFun(_, s) => vec![*s],
            Self::UnknownType(_, s) => vec![*s],
            Self::AddOverflow(a, b) => vec![a.span, b.span],
            Self::SubOverflow(a, b) => vec![a.span, b.span],
            Self::MulOverflow(a, b) => vec![a.span, b.span],
            Self::PowOverflow(a, b) => vec![a.span, b.span],
            Self::DivideByZero(a, b) => vec![a.span, b.span],
            Self::FractionEuclidDiv(a, b) => vec![a.span, b.span],
            Self::RemainderByZero(a, b) => vec![a.span, b.span],
            Self::FractionRemainder(a, b) => vec![a.span, b.span],
            Self::FractionGcd(a, b) => vec![a.span, b.span],
            Self::FractionNcr(a, b) => vec![a.span, b.span],
            Self::NegativeNcr(a) => vec![a.span],
            Self::InvalidNcr(a, b) => vec![a.span, b.span],
            Self::FactorialOverflow(v) => vec![v.span],
            Self::NegativeFactorial(v) => vec![v.span],
            Self::FractionFactorial(v) => vec![v.span],
            Self::InvalidClampBounds(min, max) => vec![min.span, max.span],
            Self::InvalidBwOr(a, b) => vec![a.span, b.span],
            Self::InvalidBwAnd(a, b) => vec![a.span, b.span],
            Self::InvalidAssignment(a, b) => vec![*a, *b],
            Self::AssertFailed(s) => vec![*s],
            Self::AssertEqFailed(a, b) => vec![a.span, b.span],
            Self::NotImplemented(_, s) => vec![*s],
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Warning {}

impl Display for Warning {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl UserFacing for Warning {
    fn spans(&self) -> Vec<Span> {
        todo!()
    }
}
