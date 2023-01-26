use std::fmt::{self, Display};
use std::ops::{Deref, DerefMut};

use cods_derive::{EnumDisplay, EnumFromStr};

use crate::{Ident, IdentSpan, Span};

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Val(ValSpan),
    Ident(IdentSpan),
    Op(Op),
    Par(Par),
    Pct(Pct),
    Kw(Kw),
}

impl Token {
    pub fn val(val: Val, span: Span) -> Self {
        Self::Val(ValSpan::new(val, span))
    }

    pub fn ident(ident: Ident, span: Span) -> Self {
        Self::Ident(IdentSpan::new(ident, span))
    }

    pub fn op(typ: OpT, span: Span) -> Self {
        Self::Op(Op::new(typ, span))
    }

    pub fn par(typ: ParT, span: Span) -> Self {
        Self::Par(Par::new(typ, span))
    }

    pub fn pct(typ: PctT, span: Span) -> Self {
        Self::Pct(Pct::new(typ, span))
    }

    pub fn kw(typ: KwT, span: Span) -> Self {
        Self::Kw(Kw::new(typ, span))
    }

    pub fn is_val(&self) -> bool {
        matches!(self, Self::Val(_))
    }

    pub fn is_ident(&self) -> bool {
        matches!(self, Self::Ident(_))
    }

    pub fn is_op(&self) -> bool {
        matches!(self, Self::Op(_))
    }

    pub fn is_par(&self) -> bool {
        matches!(self, Self::Par(_))
    }

    pub fn is_pct(&self) -> bool {
        matches!(self, Self::Pct(_))
    }

    pub fn is_kw(&self) -> bool {
        matches!(self, Self::Kw(_))
    }

    pub fn as_op(&self) -> Option<Op> {
        match self {
            Self::Op(o) => Some(*o),
            _ => None,
        }
    }

    pub fn as_par(&self) -> Option<Par> {
        match self {
            Self::Par(p) => Some(*p),
            _ => None,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Val(v) => v.span,
            Self::Ident(i) => i.span,
            Self::Op(o) => o.span,
            Self::Par(p) => p.span,
            Self::Pct(s) => s.span,
            Self::Kw(k) => k.span,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ValSpan {
    pub val: Val,
    pub span: Span,
}

impl Deref for ValSpan {
    type Target = Val;

    fn deref(&self) -> &Self::Target {
        &self.val
    }
}

impl DerefMut for ValSpan {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.val
    }
}

impl Display for ValSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.val)
    }
}

impl ValSpan {
    pub const fn new(val: Val, span: Span) -> Self {
        Self { val, span }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    Int(i128),
    Float(f64),
    Bool(bool),
    Char(char),
    Str(String),
    Range(Range),
    Unit,
}

impl Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(v) => write!(f, "{v}"),
            Self::Float(v) => write!(f, "{v}"),
            Self::Bool(v) => write!(f, "{v}"),
            Self::Char(v) => write!(f, "{v}"),
            Self::Str(v) => write!(f, "{v}"),
            Self::Range(v) => write!(f, "{v}"),
            Self::Unit => write!(f, "()"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Range {
    Exclusive(i128, i128),
    Inclusive(i128, i128),
}

impl Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Exclusive(a, b) => write!(f, "{a}..{b}"),
            Self::Inclusive(a, b) => write!(f, "{a}..={b}"),
        }
    }
}

impl Range {
    pub fn iter(&self) -> RangeIter {
        match self {
            Self::Exclusive(a, _) => RangeIter::new(*a, *self),
            Self::Inclusive(a, _) => RangeIter::new(*a, *self),
        }
    }
}

pub struct RangeIter {
    i: i128,
    range: Range,
}

impl Iterator for RangeIter {
    type Item = i128;

    fn next(&mut self) -> Option<Self::Item> {
        match self.range {
            Range::Exclusive(_, b) => {
                if self.i >= b {
                    return None;
                }
            }
            Range::Inclusive(_, b) => {
                if self.i > b {
                    return None;
                }
            }
        }
        let i = self.i;
        self.i += 1;
        Some(i)
    }
}

impl RangeIter {
    pub const fn new(i: i128, range: Range) -> Self {
        RangeIter { i, range }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Op {
    pub typ: OpT,
    pub span: Span,
}

impl Op {
    pub const fn new(typ: OpT, span: Span) -> Self {
        Op { typ, span }
    }
}

impl Deref for Op {
    type Target = OpT;

    fn deref(&self) -> &Self::Target {
        &self.typ
    }
}

impl DerefMut for Op {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.typ
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, EnumDisplay, EnumFromStr)]
pub enum OpT {
    #[cods(rename = "=")]
    Assign,
    #[cods(rename = "..")]
    RangeEx,
    #[cods(rename = "..=")]
    RangeIn,
    #[cods(rename = "+")]
    Add,
    #[cods(rename = "+=")]
    AddAssign,
    #[cods(rename = "-")]
    Sub,
    #[cods(rename = "-=")]
    SubAssign,
    #[cods(rename = "*")]
    Mul,
    #[cods(rename = "*=")]
    MulAssign,
    #[cods(rename = "/")]
    Div,
    #[cods(rename = "/=")]
    DivAssign,
    #[cods(rename = "**")]
    Pow,
    #[cods(rename = "%")]
    Rem,
    #[cods(rename = "%=")]
    RemAssign,
    #[cods(rename = "mod")]
    RemEuclid,
    #[cods(rename = "==")]
    Eq,
    #[cods(rename = "!=")]
    Ne,
    #[cods(rename = "<")]
    Lt,
    #[cods(rename = "<=")]
    Le,
    #[cods(rename = ">")]
    Gt,
    #[cods(rename = ">=")]
    Ge,
    #[cods(rename = "||")]
    Or,
    #[cods(rename = "||=")]
    OrAssign,
    #[cods(rename = "&&")]
    And,
    #[cods(rename = "&&=")]
    AndAssign,
    #[cods(rename = "|")]
    BwOr,
    #[cods(rename = "|=")]
    BwOrAssign,
    #[cods(rename = "^")]
    Xor,
    #[cods(rename = "^=")]
    XorAssign,
    #[cods(rename = "&")]
    BwAnd,
    #[cods(rename = "&=")]
    BwAndAssign,
    #[cods(rename = "<<")]
    Shl,
    #[cods(rename = "<<=")]
    ShlAssign,
    #[cods(rename = ">>")]
    Shr,
    #[cods(rename = ">>=")]
    ShrAssign,
    #[cods(rename = "!")]
    Bang,
    #[cods(rename = ".")]
    Dot,
    #[cods(rename = "as")]
    As,
    #[cods(rename = "is")]
    Is,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Par {
    pub typ: ParT,
    pub span: Span,
}

impl Deref for Par {
    type Target = ParT;

    fn deref(&self) -> &Self::Target {
        &self.typ
    }
}

impl DerefMut for Par {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.typ
    }
}

impl Par {
    pub const fn new(typ: ParT, span: Span) -> Self {
        Self { typ, span }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ParT {
    RoundOpen,
    RoundClose,
    SquareOpen,
    SquareClose,
    CurlyOpen,
    CurlyClose,
}

impl ParT {
    pub const fn is_opening(&self) -> bool {
        match self {
            Self::RoundOpen | Self::SquareOpen | Self::CurlyOpen => true,
            Self::RoundClose | Self::SquareClose | Self::CurlyClose => false,
        }
    }

    pub const fn is_closing(&self) -> bool {
        !self.is_opening()
    }

    pub const fn matches(&self, other: Self) -> bool {
        match self {
            Self::RoundOpen => matches!(other, Self::RoundClose),
            Self::RoundClose => matches!(other, Self::RoundOpen),
            Self::SquareOpen => matches!(other, Self::SquareClose),
            Self::SquareClose => matches!(other, Self::SquareOpen),
            Self::CurlyOpen => matches!(other, Self::CurlyClose),
            Self::CurlyClose => matches!(other, Self::CurlyOpen),
        }
    }

    pub const fn kind(&self) -> ParKind {
        match self {
            Self::RoundOpen | Self::RoundClose => ParKind::Round,
            Self::SquareOpen | Self::SquareClose => ParKind::Square,
            Self::CurlyOpen | Self::CurlyClose => ParKind::Curly,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ParKind {
    Round,
    Square,
    Curly,
}

impl ParKind {
    pub fn is_round(&self) -> bool {
        matches!(self, Self::Round)
    }

    pub fn is_square(&self) -> bool {
        matches!(self, Self::Square)
    }

    pub fn is_curly(&self) -> bool {
        matches!(self, Self::Curly)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Pct {
    pub typ: PctT,
    pub span: Span,
}

impl Deref for Pct {
    type Target = PctT;

    fn deref(&self) -> &Self::Target {
        &self.typ
    }
}

impl Pct {
    pub const fn new(typ: PctT, span: Span) -> Self {
        Self { typ, span }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PctT {
    Comma,
    Semi,
    Colon,
    Arrow,
    FatArrow,
    Newln,
}

impl Display for PctT {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Comma => write!(f, ","),
            Self::Semi => write!(f, ";"),
            Self::Colon => write!(f, ":"),
            Self::Arrow => write!(f, "->"),
            Self::FatArrow => write!(f, "=>"),
            Self::Newln => write!(f, "\\n"),
        }
    }
}

impl PctT {
    pub fn is_comma(&self) -> bool {
        matches!(self, Self::Comma)
    }

    pub fn is_semi(&self) -> bool {
        matches!(self, Self::Semi)
    }

    pub fn is_newln(&self) -> bool {
        matches!(self, Self::Newln)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Kw {
    pub typ: KwT,
    pub span: Span,
}

impl Display for Kw {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.typ)
    }
}

impl Deref for Kw {
    type Target = KwT;

    fn deref(&self) -> &Self::Target {
        &self.typ
    }
}

impl DerefMut for Kw {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.typ
    }
}

impl Kw {
    pub const fn new(typ: KwT, span: Span) -> Self {
        Self { typ, span }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, EnumDisplay, EnumFromStr)]
#[cods(rename_all = "snake_case")]
pub enum KwT {
    If,
    Else,
    Match,
    While,
    For,
    In,
    Fn,
    Return,
    Let,
    Mut,
}
