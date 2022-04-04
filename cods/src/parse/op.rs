use crate::{OpT, Span};

#[derive(Clone, Debug)]
pub struct Infix {
    pub typ: InfixT,
    pub span: Span,
}

impl Infix {
    pub const fn new(typ: InfixT, span: Span) -> Self {
        Self { typ, span }
    }
}

#[derive(Clone, Copy, Debug)]
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
    IntDiv,
    Rem,
    Pow,
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

#[derive(Clone, Debug)]
pub struct Prefix {
    pub typ: PrefixT,
    pub span: Span,
}

impl Prefix {
    pub const fn new(typ: PrefixT, span: Span) -> Self {
        Self { typ, span }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum PrefixT {
    UnaryPlus,
    UnaryMinus,
    Not,
}

#[derive(Clone, Debug)]
pub struct Postfix {
    pub typ: PostfixT,
    pub span: Span,
}

impl Postfix {
    pub const fn new(typ: PostfixT, span: Span) -> Self {
        Self { typ, span }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum PostfixT {
    Factorial,
}

impl OpT {
    pub fn infix_bp(&self) -> Option<(u8, InfixT, u8)> {
        match self {
            Self::Dot => Some((23, InfixT::Pow, 24)),
            Self::Pow => Some((19, InfixT::Pow, 20)),
            Self::Mul => Some((17, InfixT::Mul, 18)),
            Self::Div => Some((17, InfixT::Div, 18)),
            Self::IntDiv => Some((17, InfixT::IntDiv, 18)),
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
            Self::Bang => Some((21, PostfixT::Factorial)),
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
            | Self::IntDiv
            | Self::Rem
            | Self::Pow
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
            Self::Bang => Some((PrefixT::Not, 22)),
            Self::Add => Some((PrefixT::UnaryPlus, 22)),
            Self::Sub => Some((PrefixT::UnaryMinus, 22)),
            Self::Assign
            | Self::AddAssign
            | Self::SubAssign
            | Self::MulAssign
            | Self::DivAssign
            | Self::RangeEx
            | Self::RangeIn
            | Self::Mul
            | Self::Div
            | Self::IntDiv
            | Self::Rem
            | Self::Pow
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
