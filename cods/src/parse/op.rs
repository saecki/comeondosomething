use crate::OpT;

pub enum Infix {
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

pub enum Prefix {
    UnaryPlus,
    UnaryMinus,
    Not,
}

pub enum Postfix {
    Degree,
    Radian,
    Factorial,
}

impl OpT {
    pub fn infix_bp(&self) -> Option<(u8, Infix, u8)> {
        match self {
            Self::Dot => Some((23, Infix::Pow, 24)),
            Self::Pow => Some((19, Infix::Pow, 20)),
            Self::Mul => Some((17, Infix::Mul, 18)),
            Self::Div => Some((17, Infix::Div, 18)),
            Self::IntDiv => Some((17, Infix::IntDiv, 18)),
            Self::Rem => Some((17, Infix::Rem, 18)),
            Self::Add => Some((15, Infix::Add, 16)),
            Self::Sub => Some((15, Infix::Sub, 16)),
            Self::BwAnd => Some((13, Infix::BwAnd, 14)),
            Self::BwOr => Some((11, Infix::BwOr, 12)),
            Self::Eq => Some((9, Infix::Eq, 10)),
            Self::Ne => Some((9, Infix::Ne, 10)),
            Self::Lt => Some((9, Infix::Lt, 10)),
            Self::Le => Some((9, Infix::Le, 10)),
            Self::Gt => Some((9, Infix::Gt, 10)),
            Self::Ge => Some((9, Infix::Ge, 10)),
            Self::And => Some((7, Infix::And, 8)),
            Self::Or => Some((5, Infix::Or, 6)),
            Self::RangeEx => Some((3, Infix::RangeEx, 4)),
            Self::RangeIn => Some((3, Infix::RangeIn, 4)),
            Self::Assign => Some((1, Infix::Assign, 2)),
            Self::AddAssign => Some((1, Infix::AddAssign, 2)),
            Self::SubAssign => Some((1, Infix::SubAssign, 2)),
            Self::MulAssign => Some((1, Infix::MulAssign, 2)),
            Self::DivAssign => Some((1, Infix::DivAssign, 2)),
            Self::Bang | Self::Degree | Self::Radian => None,
        }
    }

    pub fn postfix_bp(&self) -> Option<(u8, Postfix)> {
        match self {
            Self::Degree => Some((21, Postfix::Degree)),
            Self::Radian => Some((21, Postfix::Radian)),
            Self::Bang => Some((21, Postfix::Factorial)),
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

    pub fn prefix_bp(&self) -> Option<(Prefix, u8)> {
        match self {
            Self::Bang => Some((Prefix::Not, 22)),
            Self::Add => Some((Prefix::UnaryPlus, 22)),
            Self::Sub => Some((Prefix::UnaryMinus, 22)),
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
            | Self::Degree
            | Self::Radian
            | Self::Dot => None,
        }
    }
}
