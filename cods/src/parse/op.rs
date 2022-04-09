use std::fmt::{self, Display};
use std::ops::Deref;

use crate::{OpT, Span};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Infix {
    pub typ: InfixT,
    pub span: Span,
}

impl Deref for Infix {
    type Target = InfixT;

    fn deref(&self) -> &Self::Target {
        &self.typ
    }
}

impl Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.typ)
    }
}

impl Infix {
    pub const fn new(typ: InfixT, span: Span) -> Self {
        Self { typ, span }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InfixT {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RangeEx,
    RangeIn,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    BwOr,
    BwAnd,
    Or,
    And,
    Dot,
}

impl Display for InfixT {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Assign => write!(f, "="),
            Self::AddAssign => write!(f, "+="),
            Self::SubAssign => write!(f, "-="),
            Self::MulAssign => write!(f, "*="),
            Self::DivAssign => write!(f, "/="),
            Self::RangeEx => write!(f, ".."),
            Self::RangeIn => write!(f, "..="),
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Rem => write!(f, "%"),
            Self::Eq => write!(f, "=="),
            Self::Ne => write!(f, "!="),
            Self::Lt => write!(f, "<"),
            Self::Le => write!(f, "<="),
            Self::Gt => write!(f, ">"),
            Self::Ge => write!(f, ">="),
            Self::BwOr => write!(f, "|"),
            Self::BwAnd => write!(f, "&"),
            Self::Or => write!(f, "||"),
            Self::And => write!(f, "&&"),
            Self::Dot => write!(f, "."),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Prefix {
    pub typ: PrefixT,
    pub span: Span,
}

impl Deref for Prefix {
    type Target = PrefixT;

    fn deref(&self) -> &Self::Target {
        &self.typ
    }
}

impl Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.typ)
    }
}

impl Prefix {
    pub const fn new(typ: PrefixT, span: Span) -> Self {
        Self { typ, span }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PrefixT {
    UnaryPlus,
    UnaryMinus,
    Not,
}

impl Display for PrefixT {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnaryPlus => write!(f, "+"),
            Self::UnaryMinus => write!(f, "-"),
            Self::Not => write!(f, "!"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Postfix {
    pub typ: PostfixT,
    pub span: Span,
}

impl Deref for Postfix {
    type Target = PostfixT;

    fn deref(&self) -> &Self::Target {
        &self.typ
    }
}

impl Display for Postfix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.typ)
    }
}

impl Postfix {
    pub const fn new(typ: PostfixT, span: Span) -> Self {
        Self { typ, span }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PostfixT {
    Factorial,
}

impl Display for PostfixT {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Factorial => write!(f, "!"),
        }
    }
}

impl OpT {
    pub fn infix_bp(&self) -> Option<(u8, InfixT, u8)> {
        match self {
            Self::Dot => Some((21, InfixT::Dot, 22)),
            Self::Mul => Some((17, InfixT::Mul, 18)),
            Self::Div => Some((17, InfixT::Div, 18)),
            Self::Rem => Some((17, InfixT::Rem, 18)),
            Self::Add => Some((15, InfixT::Add, 16)),
            Self::Sub => Some((15, InfixT::Sub, 16)),
            Self::BwAnd => Some((13, InfixT::BwAnd, 14)),
            Self::BwOr => Some((11, InfixT::BwOr, 12)),
            Self::Eq => Some((9, InfixT::Eq, 10)),
            Self::Ne => Some((9, InfixT::Ne, 10)),
            Self::Lt => Some((9, InfixT::Lt, 10)),
            Self::Le => Some((9, InfixT::Le, 10)),
            Self::Gt => Some((9, InfixT::Gt, 10)),
            Self::Ge => Some((9, InfixT::Ge, 10)),
            Self::And => Some((7, InfixT::And, 8)),
            Self::Or => Some((5, InfixT::Or, 6)),
            Self::RangeEx => Some((3, InfixT::RangeEx, 4)),
            Self::RangeIn => Some((3, InfixT::RangeIn, 4)),
            Self::Assign => Some((1, InfixT::Assign, 2)),
            Self::AddAssign => Some((1, InfixT::AddAssign, 2)),
            Self::SubAssign => Some((1, InfixT::SubAssign, 2)),
            Self::MulAssign => Some((1, InfixT::MulAssign, 2)),
            Self::DivAssign => Some((1, InfixT::DivAssign, 2)),
            Self::Bang => None,
        }
    }

    pub fn postfix_bp(&self) -> Option<(u8, PostfixT)> {
        match self {
            Self::Bang => Some((19, PostfixT::Factorial)),
            Self::Assign
            | Self::AddAssign
            | Self::SubAssign
            | Self::MulAssign
            | Self::DivAssign
            | Self::RangeEx
            | Self::RangeIn
            | Self::Add
            | Self::Sub
            | Self::Mul
            | Self::Div
            | Self::Rem
            | Self::Eq
            | Self::Ne
            | Self::Lt
            | Self::Le
            | Self::Gt
            | Self::Ge
            | Self::BwOr
            | Self::BwAnd
            | Self::Or
            | Self::And
            | Self::Dot => None,
        }
    }

    pub fn prefix_bp(&self) -> Option<(PrefixT, u8)> {
        match self {
            Self::Bang => Some((PrefixT::Not, 20)),
            Self::Add => Some((PrefixT::UnaryPlus, 20)),
            Self::Sub => Some((PrefixT::UnaryMinus, 20)),
            Self::Assign
            | Self::AddAssign
            | Self::SubAssign
            | Self::MulAssign
            | Self::DivAssign
            | Self::RangeEx
            | Self::RangeIn
            | Self::Mul
            | Self::Div
            | Self::Rem
            | Self::Eq
            | Self::Ne
            | Self::Lt
            | Self::Le
            | Self::Gt
            | Self::Ge
            | Self::BwOr
            | Self::BwAnd
            | Self::Or
            | Self::And
            | Self::Dot => None,
        }
    }
}
