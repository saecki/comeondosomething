use std::fmt::{self, Display};
use std::ops::Deref;

use cods_derive::{EnumDisplay, EnumFromStr};

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

#[derive(Clone, Copy, Debug, PartialEq, Eq, EnumDisplay, EnumFromStr)]
pub enum InfixT {
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
    #[cods(rename = ".")]
    Dot,
    #[cods(rename = "as")]
    As,
    #[cods(rename = "is")]
    Is,
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, EnumDisplay, EnumFromStr)]
pub enum PrefixT {
    #[cods(rename = "+")]
    UnaryPlus,
    #[cods(rename = "-")]
    UnaryMinus,
    #[cods(rename = "!")]
    Not,
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, EnumDisplay, EnumFromStr)]
pub enum PostfixT {
    #[cods(rename = "!")]
    Factorial,
}

impl OpT {
    pub fn infix_bp(&self) -> Option<(u8, InfixT, u8)> {
        match self {
            Self::Dot => Some((27, InfixT::Dot, 28)),
            Self::As => Some((23, InfixT::As, 24)),
            Self::Is => Some((23, InfixT::Is, 24)),
            Self::Mul => Some((21, InfixT::Mul, 22)),
            Self::Div => Some((21, InfixT::Div, 22)),
            Self::Pow => Some((21, InfixT::Pow, 22)),
            Self::Rem => Some((21, InfixT::Rem, 22)),
            Self::RemEuclid => Some((21, InfixT::RemEuclid, 22)),
            Self::Add => Some((19, InfixT::Add, 20)),
            Self::Sub => Some((19, InfixT::Sub, 20)),
            Self::Shl => Some((17, InfixT::Shl, 18)),
            Self::Shr => Some((17, InfixT::Shr, 18)),
            Self::BwAnd => Some((15, InfixT::BwAnd, 16)),
            Self::Xor => Some((13, InfixT::BwAnd, 14)),
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
            Self::RemAssign => Some((1, InfixT::RemAssign, 2)),
            Self::OrAssign => Some((1, InfixT::OrAssign, 2)),
            Self::AndAssign => Some((1, InfixT::AndAssign, 2)),
            Self::BwOrAssign => Some((1, InfixT::BwOrAssign, 2)),
            Self::XorAssign => Some((1, InfixT::XorAssign, 2)),
            Self::BwAndAssign => Some((1, InfixT::BwAndAssign, 2)),
            Self::ShlAssign => Some((1, InfixT::ShlAssign, 2)),
            Self::ShrAssign => Some((1, InfixT::ShrAssign, 2)),
            Self::Bang => None,
        }
    }

    pub fn postfix_bp(&self) -> Option<(u8, PostfixT)> {
        match self {
            Self::Bang => Some((25, PostfixT::Factorial)),
            Self::Assign
            | Self::AddAssign
            | Self::SubAssign
            | Self::MulAssign
            | Self::DivAssign
            | Self::RemAssign
            | Self::OrAssign
            | Self::AndAssign
            | Self::BwOrAssign
            | Self::XorAssign
            | Self::BwAndAssign
            | Self::ShlAssign
            | Self::ShrAssign
            | Self::RangeEx
            | Self::RangeIn
            | Self::Add
            | Self::Sub
            | Self::Mul
            | Self::Div
            | Self::Pow
            | Self::Rem
            | Self::RemEuclid
            | Self::Eq
            | Self::Ne
            | Self::Lt
            | Self::Le
            | Self::Gt
            | Self::Ge
            | Self::Or
            | Self::And
            | Self::BwOr
            | Self::Xor
            | Self::Shl
            | Self::Shr
            | Self::BwAnd
            | Self::Dot
            | Self::As
            | Self::Is => None,
        }
    }

    pub fn prefix_bp(&self) -> Option<(PrefixT, u8)> {
        match self {
            Self::Bang => Some((PrefixT::Not, 26)),
            Self::Add => Some((PrefixT::UnaryPlus, 26)),
            Self::Sub => Some((PrefixT::UnaryMinus, 26)),
            Self::Assign
            | Self::AddAssign
            | Self::SubAssign
            | Self::MulAssign
            | Self::DivAssign
            | Self::RemAssign
            | Self::OrAssign
            | Self::AndAssign
            | Self::BwOrAssign
            | Self::XorAssign
            | Self::BwAndAssign
            | Self::ShlAssign
            | Self::ShrAssign
            | Self::RangeEx
            | Self::RangeIn
            | Self::Mul
            | Self::Div
            | Self::Pow
            | Self::Rem
            | Self::RemEuclid
            | Self::Eq
            | Self::Ne
            | Self::Lt
            | Self::Le
            | Self::Gt
            | Self::Ge
            | Self::Or
            | Self::And
            | Self::BwOr
            | Self::Xor
            | Self::Shl
            | Self::Shr
            | Self::BwAnd
            | Self::Dot
            | Self::As
            | Self::Is => None,
        }
    }
}
