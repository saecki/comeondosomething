use std::f64::consts;
use std::fmt::{self, Display};
use std::ops::{Deref, DerefMut};

use crate::{CRange, Ident, IdentRange};

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Expr(Expr),
    Op(Op),
    Par(Par),
    Sep(Sep),
    Kw(Kw),
}

impl Token {
    pub fn expr(val: ExprT, range: CRange) -> Self {
        Self::Expr(Expr::new(val, range))
    }

    pub fn op(typ: OpT, range: CRange) -> Self {
        Self::Op(Op::new(typ, range))
    }

    pub fn par(typ: ParT, range: CRange) -> Self {
        Self::Par(Par::new(typ, range))
    }

    pub fn sep(typ: SepT, range: CRange) -> Self {
        Self::Sep(Sep::new(typ, range))
    }

    pub fn kw(typ: KwT, range: CRange) -> Self {
        Self::Kw(Kw::new(typ, range))
    }

    pub fn is_val(&self) -> bool {
        matches!(self, Self::Expr(_))
    }

    pub fn is_op(&self) -> bool {
        matches!(self, Self::Op(_))
    }

    pub fn is_par(&self) -> bool {
        matches!(self, Self::Par(_))
    }

    pub fn is_sep(&self) -> bool {
        matches!(self, Self::Sep(_))
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

    pub fn range(&self) -> CRange {
        match self {
            Self::Expr(n) => n.range,
            Self::Op(o) => o.range,
            Self::Par(p) => p.range,
            Self::Sep(s) => s.range,
            Self::Kw(k) => k.range,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub typ: ExprT,
    pub range: CRange,
}

impl Deref for Expr {
    type Target = ExprT;

    fn deref(&self) -> &Self::Target {
        &self.typ
    }
}

impl Expr {
    pub fn new(expr: ExprT, range: CRange) -> Self {
        Self { typ: expr, range }
    }

    pub fn as_ident(&self) -> Option<IdentRange> {
        match self.typ {
            ExprT::Ident(i) => Some(IdentRange::new(i, self.range)),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprT {
    Val(Val),
    Ident(Ident),
}

impl ExprT {
    pub const TAU: Self = Self::Val(Val::Float(consts::TAU));
    pub const PI: Self = Self::Val(Val::Float(consts::PI));
    pub const E: Self = Self::Val(Val::Float(consts::E));

    pub fn int(i: i128) -> Self {
        Self::Val(Val::Int(i))
    }

    pub fn float(f: f64) -> Self {
        Self::Val(Val::Float(f))
    }

    pub fn bool(b: bool) -> Self {
        Self::Val(Val::Bool(b))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    Int(i128),
    Float(f64),
    Bool(bool),
    Str(String),
    Range(Range),
}

#[derive(Clone, Copy, Debug, PartialEq)]
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

impl Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(v) => write!(f, "{v}"),
            Self::Float(v) => write!(f, "{v}"),
            Self::Bool(v) => write!(f, "{v}"),
            Self::Str(v) => write!(f, "{v}"),
            Self::Range(v) => write!(f, "{v}"),
        }
    }
}

impl Val {
    pub const fn type_name(&self) -> &'static str {
        match self {
            Self::Int(_) => "int",
            Self::Float(_) => "float",
            Self::Bool(_) => "bool",
            Self::Str(_) => "str",
            Self::Range(_) => "range",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Op {
    pub typ: OpT,
    pub range: CRange,
}

impl Op {
    pub const fn new(typ: OpT, range: CRange) -> Self {
        Op { typ, range }
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OpT {
    Assign,
    RangeEx,
    RangeIn,
    Add,
    AddAssign,
    Sub,
    SubAssign,
    Mul,
    MulAssign,
    Div,
    DivAssign,
    IntDiv,
    Rem,
    Pow,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Or,
    And,
    BwOr,
    BwAnd,
    /// Not or Factorial depending on position
    Bang,
    Degree,
    Radian,
    Dot,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Par {
    pub typ: ParT,
    pub range: CRange,
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
    pub const fn new(typ: ParT, range: CRange) -> Self {
        Self { typ, range }
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

#[derive(Clone, Copy, Debug, PartialEq)]
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
pub struct Sep {
    pub typ: SepT,
    pub range: CRange,
}

impl Deref for Sep {
    type Target = SepT;

    fn deref(&self) -> &Self::Target {
        &self.typ
    }
}

impl Sep {
    pub const fn new(typ: SepT, range: CRange) -> Self {
        Self { typ, range }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SepT {
    Comma,
    Semi,
    Newln,
}

impl Display for SepT {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Comma => write!(f, ","),
            Self::Semi => write!(f, ";"),
            Self::Newln => write!(f, "\\n"),
        }
    }
}

impl SepT {
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
    pub range: CRange,
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
    pub const fn new(typ: KwT, range: CRange) -> Self {
        Self { typ, range }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum KwT {
    If,
    Else,
    While,
    For,
    In,
    Fun,
}

impl KwT {
    pub fn name(&self) -> &str {
        match self {
            KwT::If => "if",
            KwT::Else => "else",
            KwT::While => "while",
            KwT::For => "for",
            KwT::In => "in",
            KwT::Fun => "fun",
        }
    }
}
