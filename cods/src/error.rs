use std::error;
use std::fmt::{self, Debug, Display};

use crate::{
    BuiltinConst, DataType, FunSignature, InfixT, Kw, KwT, Op, OpSignature, OpT, Par, PctT,
    PostfixT, PrefixT, Span, Token, ValSpan,
};

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

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    NotImplemented(&'static str, Vec<Span>),

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
    EmptyCharLiteral(Span),

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
    UnexpectedToken(Token),
    UnexpectedOperator(Op),
    ExpectedExpr(Span),
    ExpectedBlock(Span),
    ExpectedFunPars(Span),
    ExpectedIdent(Span),
    ExpectedOp(OpT, Span),
    ExpectedPct(PctT, Span),
    ExpectedKw(KwT, Span),
    WrongContext(Kw),

    // Check
    UnknownType(String, Span),
    MismatchedType {
        expected: DataType,
        found: DataType,
        spans: Vec<Span>,
    },
    IfBranchIncompatibleType((DataType, Span), (DataType, Span)),
    MissingElseBranch(DataType, Span),
    NotIterable(DataType, Span),
    UndefinedVar(String, Span),
    UninitializedVar(String, Span, Span),
    RedefinedBuiltinConst(String, Span),
    UndefinedFun(String, Span),
    RedefinedFun(String, Span, Span),
    RedefinedBuiltinFun(String, Span),
    NoMatchingBuiltinFunSignature {
        name: String,
        args: Vec<DataType>,
        signatures: Vec<FunSignature>,
        span: Span,
    },
    NoMatchingInfixSignature {
        infix: InfixT,
        a: DataType,
        b: DataType,
        signatures: Vec<OpSignature<2>>,
        span: Span,
    },
    NoMatchingInfixAssignSignature {
        infix: InfixT,
        a: DataType,
        b: DataType,
        signatures: Vec<OpSignature<2>>,
        span: Span,
    },
    NoMatchingPrefixSignature {
        prefix: PrefixT,
        a: DataType,
        signatures: Vec<OpSignature<1>>,
        span: Span,
    },
    NoMatchingPostfixSignature {
        postfix: PostfixT,
        a: DataType,
        signatures: Vec<OpSignature<1>>,
        span: Span,
    },
    AssignTypeMismatch((DataType, Span), (DataType, Span)),
    InvalidAssignment(Span, Span),
    ImmutableAssign(String, Span, Span),
    ConstAssign((BuiltinConst, Span), Span),
    NotComparable((DataType, Span), (DataType, Span)),
    CastAlwaysFails((DataType, Span), (DataType, Span)),
    GlobalContextReturn(Span),

    // Eval
    Parsing(Span),
    NegOverflow(Span),
    AddOverflow(Span, Span),
    SubOverflow(Span, Span),
    MulOverflow(Span, Span),
    DivideByZero(Span, Span),
    RemainderByZero(Span, Span),
    PowOverflow(Span, Span),
    NegativeIntPow(Span, Span),
    FactorialOverflow(ValSpan),
    NegativeFactorial(ValSpan),
    CastFailed((DataType, Span), DataType),
    NegativeNcr(ValSpan),
    InvalidNcr(ValSpan, ValSpan),
    InvalidClampBounds(ValSpan, ValSpan),
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
                write!(f, "Invalid escape character: `{}`", c.escape_default())
            }
            Self::MissingEscapeChar(_) => write!(f, "Missing escape character"),
            Self::InvalidUnicodeEscapeChar(c, _) => {
                write!(
                    f,
                    "Invalid unicode escape character: `{}`",
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
                write!(f, "Invalid unicode scalar value: `{cp:x}`")
            }
            Self::MissingClosingQuote(_) => write!(f, "Missing closing quote"),
            Self::EmptyCharLiteral(_) => write!(f, "Empty character literal"),

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
            Self::UnexpectedToken(_) => write!(f, "Unexpected item"),
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
            Self::ExpectedExpr(_) => write!(f, "Expected an expression"),
            Self::ExpectedBlock(_) => write!(f, "Expected a block"),
            Self::ExpectedIdent(_) => write!(f, "Expected identifier"),
            Self::ExpectedOp(o, _) => write!(f, "Expected `{o}`"),
            Self::ExpectedKw(k, _) => write!(f, "Expected `{k}`"),
            Self::ExpectedPct(s, _) => write!(f, "Expected `{s}`"),
            Self::WrongContext(k) => write!(f, "`{k}` wasn`t expected in this context"),

            // Check
            Self::UnknownType(name, _) => write!(f, "Unknown type `{name}`"),
            Self::MismatchedType {
                expected, found, ..
            } => write!(f, "Mismatched type expected `{expected}`, found `{found}`"),
            Self::IfBranchIncompatibleType((a, _), (b, _)) => write!(
                f,
                "If and else branches have incompatible types: `{a}` and `{b}`"
            ),
            Self::MissingElseBranch(t, _) => {
                write!(f, "Missing else branch for if expression of type `{t}`")
            }
            Self::NotIterable(t, _) => write!(f, "Value of type `{t}` is not iterable"),
            Self::UndefinedVar(name, _) => write!(f, "Undefined variable `{name}`"),
            Self::UninitializedVar(name, _, _) => write!(f, "Uninitialized variable `{name}`"), // TODO separate definition and usage into hint and error
            Self::RedefinedBuiltinConst(name, _) => {
                write!(f, "Redefined builtin constant `{name}`")
            }
            Self::UndefinedFun(name, _) => write!(f, "Undefined function `{name}`"),
            Self::RedefinedFun(name, _, _) => write!(f, "Redefined function `{name}`"),
            Self::RedefinedBuiltinFun(name, _) => write!(f, "Redefined builtin function `{name}`"),
            Self::NoMatchingBuiltinFunSignature {
                name,
                args,
                signatures,
                ..
            } => {
                writeln!(
                    f,
                    "No matching signature for builtin function `{name}`:{line_suffix}"
                )?;
                for s in signatures.iter() {
                    write!(f, "{line_prefix}    {name}(")?;
                    if let Some((last, others)) = s.params.split_last() {
                        for p in others {
                            write!(f, "{p}, ")?;
                        }
                        write!(f, "{last}")?;
                    }
                    write!(f, ")")?;

                    if s.return_type != DataType::Unit {
                        write!(f, " -> {}", s.return_type)?;
                    }
                    writeln!(f, "{line_suffix}")?;
                }
                writeln!(f, "{line_prefix}{line_suffix}")?;

                writeln!(f, "{line_prefix}Called with args of type:{line_suffix}")?;
                write!(f, "{line_prefix}    {name}(")?;
                if let Some((last, others)) = args.split_last() {
                    for a in others {
                        write!(f, "{a}, ")?;
                    }
                    write!(f, "{last}")?;
                }
                writeln!(f, "){line_suffix}")?;

                Ok(())
            }
            Self::NoMatchingInfixSignature {
                infix,
                a,
                b,
                signatures,
                ..
            } => {
                writeln!(
                    f,
                    "No matching signature for infix operator `{infix}`:{line_suffix}"
                )?;
                for s in signatures.iter() {
                    writeln!(
                        f,
                        "{line_prefix}    {} {infix} {} -> {}{line_suffix}",
                        s.params[0], s.params[1], s.return_type
                    )?;
                }
                writeln!(f, "{line_prefix}{line_suffix}")?;

                writeln!(f, "{line_prefix}Found operands of type:{line_suffix}")?;
                writeln!(f, "{line_prefix}    {a} {infix} {b}{line_suffix}")?;

                Ok(())
            }
            Self::NoMatchingInfixAssignSignature {
                infix,
                a,
                b,
                signatures,
                ..
            } => {
                writeln!(
                    f,
                    "No matching signature for infix operator `{infix}`:{line_suffix}"
                )?;
                for s in signatures.iter() {
                    writeln!(
                        f,
                        "{line_prefix}    {} {infix} {}{line_suffix}",
                        s.params[0], s.params[1]
                    )?;
                }
                writeln!(f, "{line_prefix}{line_suffix}")?;

                writeln!(f, "{line_prefix}Found operands of type:{line_suffix}")?;
                writeln!(f, "{line_prefix}    {a} {infix} {b}{line_suffix}")?;

                Ok(())
            }
            Self::NoMatchingPrefixSignature {
                prefix,
                a,
                signatures,
                ..
            } => {
                writeln!(
                    f,
                    "No matching signature for prefix operator `{prefix}`:{line_suffix}"
                )?;
                for s in signatures.iter() {
                    writeln!(
                        f,
                        "{line_prefix}    {prefix}{} -> {}{line_suffix}",
                        s.params[0], s.return_type
                    )?;
                }
                writeln!(f, "{line_prefix}{line_suffix}")?;

                writeln!(f, "{line_prefix}Found operand of type:{line_suffix}")?;
                writeln!(f, "{line_prefix}    {prefix}{a}{line_suffix}")?;

                Ok(())
            }
            Self::NoMatchingPostfixSignature {
                postfix,
                a,
                signatures,
                ..
            } => {
                writeln!(
                    f,
                    "No matching signature for postfix operator `{postfix}`:{line_suffix}"
                )?;
                for s in signatures.iter() {
                    writeln!(
                        f,
                        "{line_prefix}    {}{postfix} -> {}{line_suffix}",
                        s.params[0], s.return_type
                    )?;
                }
                writeln!(f, "{line_prefix}{line_suffix}")?;

                writeln!(f, "{line_prefix}Found operand of type:{line_suffix}")?;
                writeln!(f, "{line_prefix}    {a}{postfix}{line_suffix}")?;

                Ok(())
            }
            Self::AssignTypeMismatch((a, _), (b, _)) => write!(
                f,
                "Cannot assign value of type `{b}` to variable of type `{a}`"
            ),
            Self::InvalidAssignment(_, _) => {
                write!(f, "Cannot assign to something that is not a variable")
            }
            Self::ImmutableAssign(name, _, _) => {
                write!(f, "Cannot assign twice to immutable variable `{name}`")
            }
            Self::ConstAssign((c, _), _) => {
                write!(f, "Cannot assign to builtin constant `{c}`")
            }
            Self::NotComparable((a, _), (b, _)) => {
                write!(f, "Cannot compare values of type `{a}` and `{b}`")
            }
            Self::CastAlwaysFails((a, _), (b, _)) => {
                write!(f, "Casting value of type `{a}` to `{b}` will always fail")
            }
            Self::GlobalContextReturn(_) => {
                write!(f, "Cannot return from the global context")
            }

            // Eval
            Self::Parsing(_) => write!(f, "A parsing error occured"),
            Self::NegOverflow(_) => write!(f, "Negation would overflow"),
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
            Self::NegativeIntPow(_, _) => write!(
                f,
                "Attempted to calculate the power of two `int`s with a negative exponent"
            ),
            Self::FactorialOverflow(_) => write!(f, "Factorial would overflow"),
            Self::NegativeFactorial(_) => {
                write!(
                    f,
                    "Attempted to calculate the factorial of a negative number"
                )
            }
            Self::CastFailed((a, _), b) => {
                write!(f, "Casting value of type `{a}` to `{b}` failed")
            }
            Self::NegativeNcr(r) => {
                write!(
                    f,
                    "Attempted to calculate the binomial coefficent with a negative value for r: `{r}`"
                )
            }
            Self::InvalidNcr(n, r) => {
                write!(
                    f,
                    "Attempted to calculate the binomial coefficent with n: `{n}` less than r: `{r}`"
                )
            }
            Self::InvalidClampBounds(min, max) => {
                write!(
                    f,
                    "Invalid clamp bounds min: `{min}` is greater than max: `{max}`"
                )
            }
            Self::AssertFailed(_) => {
                write!(f, "Assertion failed")
            }
            Self::AssertEqFailed(a, b) => {
                write!(
                    f,
                    "Assertion failed, values are not equal{ls}\n\
                    {lp} left: `{a}`{ls}\n\
                    {lp}right: `{b}`",
                    lp = line_prefix,
                    ls = line_suffix,
                )
            }
        }?;
        f.write_str(line_suffix)
    }

    fn spans(&self) -> Vec<Span> {
        match self {
            Self::NotImplemented(_, spans) => spans.clone(),

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
            Self::EmptyCharLiteral(s) => vec![*s],

            // Group
            Self::MissingClosingPar(p) => vec![p.span],
            Self::UnexpectedPar(p) => vec![p.span],

            // Parse
            Self::MissingOperand(s) => vec![*s],
            Self::MissingOperator(s) => vec![*s],
            Self::MissingFunArgs { span: pos, .. } => vec![*pos],
            Self::UnexpectedToken(i) => vec![i.span()],
            Self::UnexpectedFunArgs { spans, .. } => spans.clone(),
            Self::UnexpectedOperator(o) => vec![o.span],
            Self::ExpectedExpr(s) => vec![*s],
            Self::ExpectedBlock(s) => vec![*s],
            Self::ExpectedFunPars(s) => vec![*s],
            Self::ExpectedIdent(s) => vec![*s],
            Self::ExpectedOp(_, s) => vec![*s],
            Self::ExpectedKw(_, s) => vec![*s],
            Self::ExpectedPct(_, s) => vec![*s],
            Self::WrongContext(k) => vec![k.span],

            // Check
            Self::UnknownType(_, s) => vec![*s],
            Self::MismatchedType { spans, .. } => spans.clone(),
            Self::IfBranchIncompatibleType((_, a), (_, b)) => vec![*a, *b],
            Self::MissingElseBranch(_, s) => vec![*s],
            Self::NotIterable(_, s) => vec![*s],
            Self::UndefinedVar(_, s) => vec![*s],
            Self::UninitializedVar(_, a, b) => vec![*a, *b],
            Self::RedefinedBuiltinConst(_, s) => vec![*s],
            Self::UndefinedFun(_, s) => vec![*s],
            Self::RedefinedFun(_, a, b) => vec![*a, *b],
            Self::RedefinedBuiltinFun(_, s) => vec![*s],
            Self::NoMatchingBuiltinFunSignature { span, .. } => vec![*span],
            Self::NoMatchingInfixSignature { span, .. } => vec![*span],
            Self::NoMatchingInfixAssignSignature { span, .. } => vec![*span],
            Self::NoMatchingPrefixSignature { span, .. } => vec![*span],
            Self::NoMatchingPostfixSignature { span, .. } => vec![*span],
            Self::AssignTypeMismatch((_, a), (_, b)) => vec![*a, *b],
            Self::InvalidAssignment(a, b) => vec![*a, *b],
            Self::ImmutableAssign(_, a, b) => vec![*a, *b],
            Self::ConstAssign((_, a), b) => vec![*a, *b],
            Self::NotComparable((_, a), (_, b)) => vec![*a, *b],
            Self::CastAlwaysFails((_, a), (_, b)) => vec![*a, *b],
            Self::GlobalContextReturn(s) => vec![*s],

            // Eval
            Self::Parsing(s) => vec![*s],
            Self::NegOverflow(a) => vec![*a],
            Self::AddOverflow(a, b) => vec![*a, *b],
            Self::SubOverflow(a, b) => vec![*a, *b],
            Self::MulOverflow(a, b) => vec![*a, *b],
            Self::DivideByZero(a, b) => vec![*a, *b],
            Self::RemainderByZero(a, b) => vec![*a, *b],
            Self::PowOverflow(a, b) => vec![*a, *b],
            Self::NegativeIntPow(a, b) => vec![*a, *b],
            Self::FactorialOverflow(v) => vec![v.span],
            Self::NegativeFactorial(v) => vec![v.span],
            Self::CastFailed((_, a), _) => vec![*a],
            Self::NegativeNcr(a) => vec![a.span],
            Self::InvalidNcr(a, b) => vec![a.span, b.span],
            Self::InvalidClampBounds(min, max) => vec![min.span, max.span],
            Self::AssertFailed(s) => vec![*s],
            Self::AssertEqFailed(a, b) => vec![a.span, b.span],
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Warning {
    Unreachable(Span),
    UnnecesaryCast(DataType, Span),
    TypeCheckIsAlwaysTrue(DataType, Span),
}

impl Display for Warning {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.description(f, "", "")
    }
}

impl UserFacing for Warning {
    fn description(
        &self,
        f: &mut fmt::Formatter<'_>,
        line_prefix: &str,
        line_suffix: &str,
    ) -> fmt::Result {
        f.write_str(line_prefix)?;
        match self {
            Warning::Unreachable(_) => write!(f, "Unreachable code"),
            Warning::UnnecesaryCast(d, _) => {
                write!(f, "Unnecesary cast, the value is known to be of type `{d}`")
            }
            Warning::TypeCheckIsAlwaysTrue(d, _) => write!(
                f,
                "Type check is always true, the value is known to be of type `{d}`"
            ),
        }?;
        f.write_str(line_suffix)
    }

    fn spans(&self) -> Vec<Span> {
        match self {
            Warning::Unreachable(s) => vec![*s],
            Warning::UnnecesaryCast(_, s) => vec![*s],
            Warning::TypeCheckIsAlwaysTrue(_, s) => vec![*s],
        }
    }
}
