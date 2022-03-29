use crate::OpT;

pub enum Infix {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
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
            Self::Pow => Some((17, Infix::Pow, 18)),
            Self::Mul => Some((15, Infix::Mul, 16)),
            Self::Div => Some((15, Infix::Div, 16)),
            Self::IntDiv => Some((15, Infix::IntDiv, 16)),
            Self::Rem => Some((15, Infix::Rem, 16)),
            Self::Add => Some((13, Infix::Add, 14)),
            Self::Sub => Some((13, Infix::Sub, 14)),
            Self::BwAnd => Some((11, Infix::BwAnd, 12)),
            Self::BwOr => Some((9, Infix::BwOr, 10)),
            Self::Eq => Some((7, Infix::Eq, 8)),
            Self::Ne => Some((7, Infix::Ne, 8)),
            Self::Lt => Some((7, Infix::Lt, 8)),
            Self::Le => Some((7, Infix::Le, 8)),
            Self::Gt => Some((7, Infix::Gt, 8)),
            Self::Ge => Some((7, Infix::Ge, 8)),
            Self::And => Some((5, Infix::And, 6)),
            Self::Or => Some((3, Infix::Or, 4)),
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
            Self::Degree => Some((19, Postfix::Degree)),
            Self::Radian => Some((19, Postfix::Radian)),
            Self::Bang => Some((19, Postfix::Factorial)),
            Self::Assign
            | Self::AddAssign
            | Self::SubAssign
            | Self::MulAssign
            | Self::DivAssign
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
            | Self::And => None,
        }
    }

    pub fn prefix_bp(&self) -> Option<(Prefix, u8)> {
        match self {
            Self::Bang => Some((Prefix::Not, 20)),
            Self::Add => Some((Prefix::UnaryPlus, 20)),
            Self::Sub => Some((Prefix::UnaryMinus, 20)),
            OpT::Assign
            | Self::AddAssign
            | Self::SubAssign
            | Self::MulAssign
            | Self::DivAssign
            | OpT::Mul
            | OpT::Div
            | OpT::IntDiv
            | OpT::Rem
            | OpT::Pow
            | OpT::Eq
            | OpT::Ne
            | OpT::Lt
            | OpT::Le
            | OpT::Gt
            | OpT::Ge
            | OpT::BwOr
            | OpT::BwAnd
            | OpT::Or
            | OpT::And
            | OpT::Degree
            | OpT::Radian => None,
        }
    }
}
