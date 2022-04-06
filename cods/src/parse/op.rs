use std::fmt::{self, Display};
use std::ops::Deref;

use crate::{DataType, OpT, Span};

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
            Self::IntDiv => write!(f, "div"),
            Self::Rem => write!(f, "%"),
            Self::Pow => write!(f, "^"),
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

impl InfixT {
    pub fn resulting_type(&self, a: DataType, b: DataType) -> Option<DataType> {
        match self {
            Self::Assign => same(a, b),
            Self::AddAssign => same_num(a, b),
            Self::SubAssign => same_num(a, b),
            Self::MulAssign => same_num(a, b),
            Self::DivAssign => same_num(a, b),
            Self::RangeEx => both_int_then_range(a, b),
            Self::RangeIn => both_int_then_range(a, b),
            Self::Add => same_num(a, b),
            Self::Sub => same_num(a, b),
            Self::Mul => same_num(a, b),
            Self::Div => same_num(a, b),
            Self::IntDiv => both_int_then_range(a, b),
            Self::Rem => both_int_then_range(a, b),
            Self::Pow => same_num(a, b),
            Self::Eq => same_then_bool(a, b),
            Self::Ne => same_then_bool(a, b),
            Self::Lt => same_num_then_bool(a, b),
            Self::Le => same_num_then_bool(a, b),
            Self::Gt => same_num_then_bool(a, b),
            Self::Ge => same_num_then_bool(a, b),
            Self::BwOr => same_int_bool_then_bool(a, b),
            Self::BwAnd => same_int_bool_then_bool(a, b),
            Self::Or => both_bool(a, b),
            Self::And => both_bool(a, b),
            Self::Dot => None, // TODO
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

impl PrefixT {
    pub fn resulting_type(&self, typ: DataType) -> Option<DataType> {
        match self {
            Self::UnaryPlus => num(typ),
            Self::UnaryMinus => num(typ),
            Self::Not => bool(typ),
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

impl PostfixT {
    pub fn resulting_type(&self, typ: DataType) -> Option<DataType> {
        match self {
            PostfixT::Factorial => int(typ),
        }
    }
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

fn same_then_bool(a: DataType, b: DataType) -> Option<DataType> {
    (a == b).then(|| DataType::Bool)
}

fn same(a: DataType, b: DataType) -> Option<DataType> {
    (a == b).then(|| a)
}

fn same_num(a: DataType, b: DataType) -> Option<DataType> {
    match (a, b) {
        (DataType::Int, DataType::Int) => Some(a),
        (DataType::Float, DataType::Float) => Some(a),
        (_, _) => None,
    }
}

fn same_num_then_bool(a: DataType, b: DataType) -> Option<DataType> {
    match (a, b) {
        (DataType::Int, DataType::Int) => Some(DataType::Bool),
        (DataType::Float, DataType::Float) => Some(DataType::Bool),
        (_, _) => None,
    }
}

fn same_int_bool_then_bool(a: DataType, b: DataType) -> Option<DataType> {
    match (a, b) {
        (DataType::Int, DataType::Int) => Some(DataType::Bool),
        (DataType::Bool, DataType::Bool) => Some(DataType::Bool),
        (_, _) => None,
    }
}

fn both_int_then_range(a: DataType, b: DataType) -> Option<DataType> {
    match (a, b) {
        (DataType::Int, DataType::Int) => Some(DataType::Range),
        (_, _) => None,
    }
}

fn both_bool(a: DataType, b: DataType) -> Option<DataType> {
    match (a, b) {
        (DataType::Bool, DataType::Bool) => Some(a),
        (_, _) => None,
    }
}

fn num(typ: DataType) -> Option<DataType> {
    match typ {
        DataType::Int | DataType::Float => Some(typ),
        DataType::Range | DataType::Bool | DataType::Str | DataType::Unit => None,
    }
}

fn int(typ: DataType) -> Option<DataType> {
    match typ {
        DataType::Int => Some(typ),
        DataType::Float | DataType::Range | DataType::Bool | DataType::Str | DataType::Unit => None,
    }
}

fn bool(typ: DataType) -> Option<DataType> {
    match typ {
        DataType::Bool => Some(typ),
        DataType::Int | DataType::Float | DataType::Range | DataType::Str | DataType::Unit => None,
    }
}
