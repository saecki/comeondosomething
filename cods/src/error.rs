use std::error;
use std::fmt::{self, Debug, Display};

use crate::DataType;
use crate::{Infix, Postfix, Prefix, ValSpan};
use crate::{Item, Kw, KwT, Op, OpT, Par, PctT, Span};

pub type Result<T> = std::result::Result<T, Error>;

pub trait UserFacing: Sized + Debug + Display {
    fn description(
        &self,
        f: &mut fmt::Formatter,
        line_prefix: &str,
        line_suffix: &str,
    ) -> fmt::Result;
    fn spans(&self) -> Vec<Span>;
}

#[derive(Debug, PartialEq)]
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
        expected: usize,
        found: usize,
        span: Span,
    },
    UnexpectedFunArgs {
        expected: usize,
        found: usize,
        spans: Vec<Span>,
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

    // Check
    InvalidAssignment(Span, Span),
    UnknownType(String, Span),
    MismatchedType {
        expected: DataType,
        found: DataType,
        spans: Vec<Span>,
    },
    IfBranchIncompatibleType((DataType, Span), (DataType, Span)),
    MissingElseBranch(DataType, Span),
    UndefinedVar(String, Span),
    UndefinedFun(String, Span),
    UninitializedVar(String, Span, Span),
    NotIterable(DataType, Span),
    PrefixNotApplicable(Prefix, (DataType, Span)),
    PostfixNotApplicable((DataType, Span), Postfix),
    InfixNotApplicable((DataType, Span), Infix, (DataType, Span)),
    AssignInfixNotApplicable((DataType, Span), Infix, (DataType, Span)),
    AssignNotApplicable((DataType, Span), (DataType, Span)),

    // Eval
    AddOverflow(Span, Span),
    SubOverflow(Span, Span),
    MulOverflow(Span, Span),
    DivideByZero(Span, Span),
    RemainderByZero(Span, Span),
    PowOverflow(Span, Span),
    NegativeIntPow(Span, Span),

    MissingExpr,
    ExpectedValue(Span),
    ExpectedNumber(ValSpan),
    ExpectedInt(ValSpan),
    ExpectedBool(ValSpan),
    ExpectedStr(ValSpan),
    ExpectedRange(ValSpan),
    Parsing(Span),
    RedefinedBuiltinConst(String, Span),
    ImmutableAssign(String, Span, Span),
    RedefinedFun(String, Span, Span),
    RedefinedBuiltinFun(String, Span),
    FractionEuclidDiv(ValSpan, ValSpan),
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
    AssertFailed(Span),
    AssertEqFailed(ValSpan, ValSpan),
}

impl error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.description(f, "", "")
    }
}

impl UserFacing for Error {
    fn description(
        &self,
        f: &mut fmt::Formatter<'_>,
        line_prefix: &str,
        line_suffix: &str,
    ) -> fmt::Result {
        f.write_str(line_prefix)?;
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

            // Check
            Self::InvalidAssignment(_, _) => {
                write!(f, "Cannot assign to something that is not a variable")
            }
            Self::UnknownType(name, _) => write!(f, "Unknown type '{name}'"),
            Self::MismatchedType {
                expected, found, ..
            } => write!(f, "Mismatched type expected '{expected}', found '{found}'"),
            Self::IfBranchIncompatibleType((a, _), (b, _)) => write!(
                f,
                "If and else branches have incompatible types: '{a}' and '{b}'"
            ),
            Self::MissingElseBranch(t, _) => {
                write!(f, "Missing else branch for if expression of type '{t}'")
            }
            Self::PrefixNotApplicable(p, (t, _)) => write!(
                f,
                "Prefix operator '{p}' not applicable to value of type '{t}'",
            ),
            Self::PostfixNotApplicable((t, _), p) => write!(
                f,
                "Postfix operator '{p}' not applicable to value of type '{t}'",
            ),
            Self::InfixNotApplicable((a, _), i, (b, _)) => write!(
                f,
                "Infix operator '{i}' not applicable to values of type '{a}' and '{b}'",
            ),
            Self::AssignInfixNotApplicable((a, _), i, (b, _)) => write!(
                f,
                "Operator '{i}' not applicable to variable of type '{a}' and value of type '{b}'"
            ),
            Self::AssignNotApplicable((a, _), (b, _)) => write!(
                f,
                "Cannot assign value of type '{b}' to variable of type '{a}'"
            ),

            // Eval
            Self::AddOverflow(_, _) => write!(f, "Addition would overflow"),
            Self::SubOverflow(_, _) => write!(f, "Subtraction would overflow"),
            Self::MulOverflow(_, _) => write!(f, "Multiplication would overflow"),
            Self::DivideByZero(_, _) => write!(f, "Attempted to divide by 0"),
            Self::RemainderByZero(_, _) => {
                write!(
                    f,
                    "Attempted to calculate the remainder with a divisor of 0"
                )
            }
            Self::PowOverflow(_, _) => write!(f, "Exponentiation would overflow"),
            Self::NegativeIntPow(_, _) => write!(f, ""),

            Self::MissingExpr => write!(f, "Missing expression"),
            Self::ExpectedValue(_) => write!(f, "Expected a value found unit"),
            Self::ExpectedNumber(v) => {
                write!(f, "Expected a number found '{v}' of type {}", v.data_type())
            }
            Self::ExpectedInt(v) => {
                write!(f, "Expected an int found '{v}' of type {}", v.data_type())
            }
            Self::ExpectedBool(v) => {
                write!(f, "Expected a bool found '{v}' of type {}", v.data_type())
            }
            Self::ExpectedStr(v) => {
                write!(f, "Expected a str found '{v}' of type {}", v.data_type())
            }
            Self::ExpectedRange(v) => {
                write!(f, "Expected a range found '{v}' of type {}", v.data_type())
            }
            Self::Parsing(_) => write!(f, "A parsing error occured"),

            Self::UndefinedVar(name, _) => write!(f, "Undefined variable '{name}'"),
            Self::UninitializedVar(name, _, _) => write!(f, "Uninitialized variable '{name}'"), // TODO separate definition and usage into hint and error
            Self::NotIterable(t, _) => write!(f, "Value of type '{t}' is not iterable"),
            Self::RedefinedBuiltinConst(name, _) => {
                write!(f, "Redefined builtin constant '{name}'")
            }
            Self::ImmutableAssign(name, _, _) => {
                write!(f, "Cannot assign twice to immutable variable '{name}'")
            }
            Self::UndefinedFun(name, _) => write!(f, "Undefined function '{name}'"),
            Self::RedefinedFun(name, _, _) => write!(f, "Redefined function '{name}'"),
            Self::RedefinedBuiltinFun(name, _) => write!(f, "Redefined builtin function '{name}'"),
            Self::FractionEuclidDiv(_, _) => write!(f, "Attempted divide fractions with remainder"),
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
                    a.data_type(),
                    b.data_type(),
                )
            }
            Self::InvalidBwAnd(a, b) => {
                write!(
                    f,
                    "A bitwise and can only be applied to two ints or two bools, not '{a}' of type {} and '{b}' of type {}",
                    a.data_type(),
                    b.data_type(),
                )
            }
            Self::AssertFailed(_) => {
                write!(f, "Assertion failed")
            }
            Self::AssertEqFailed(a, b) => {
                write!(
                    f,
                    "Assertion failed, values are not equal{ls}\n\
                    {lp} left: '{a}'{ls}\n\
                    {lp}right: '{b}'",
                    lp = line_prefix,
                    ls = line_suffix,
                )
            }
        }?;
        f.write_str(line_suffix)
    }

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

            // Check
            Self::InvalidAssignment(a, b) => vec![*a, *b],
            Self::UnknownType(_, s) => vec![*s],
            Self::MismatchedType { spans, .. } => spans.clone(),
            Self::IfBranchIncompatibleType((_, a), (_, b)) => vec![*a, *b],
            Self::MissingElseBranch(_, s) => vec![*s],
            Self::PrefixNotApplicable(p, (_, a)) => vec![p.span, *a],
            Self::PostfixNotApplicable((_, a), p) => vec![*a, p.span],
            Self::InfixNotApplicable((_, a), i, (_, b)) => vec![*a, i.span, *b],
            Self::AssignInfixNotApplicable((_, a), i, (_, b)) => vec![*a, i.span, *b],
            Self::AssignNotApplicable((_, a), (_, b)) => vec![*a, *b],

            // Eval
            Self::AddOverflow(a, b) => vec![*a, *b],
            Self::SubOverflow(a, b) => vec![*a, *b],
            Self::MulOverflow(a, b) => vec![*a, *b],
            Self::DivideByZero(a, b) => vec![*a, *b],
            Self::RemainderByZero(a, b) => vec![*a, *b],
            Self::PowOverflow(a, b) => vec![*a, *b],
            Self::NegativeIntPow(a, b) => vec![*a, *b],

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
            Self::NotIterable(_, s) => vec![*s],
            Self::RedefinedBuiltinConst(_, s) => vec![*s],
            Self::ImmutableAssign(_, a, b) => vec![*a, *b],
            Self::UndefinedFun(_, s) => vec![*s],
            Self::RedefinedFun(_, a, b) => vec![*a, *b],
            Self::RedefinedBuiltinFun(_, s) => vec![*s],
            Self::FractionEuclidDiv(a, b) => vec![a.span, b.span],
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
            Self::AssertFailed(s) => vec![*s],
            Self::AssertEqFailed(a, b) => vec![a.span, b.span],
            Self::NotImplemented(_, s) => vec![*s],
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Warning {}

impl Display for Warning {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.description(f, "", "")
    }
}

impl UserFacing for Warning {
    fn description(
        &self,
        _f: &mut fmt::Formatter<'_>,
        _line_prefix: &str,
        _line_suffix: &str,
    ) -> fmt::Result {
        todo!()
    }

    fn spans(&self) -> Vec<Span> {
        todo!()
    }
}
