use std::fmt::{self, Display};

use crate::Val;

pub const TYPE_INT: &str = "int";
pub const TYPE_FLOAT: &str = "float";
pub const TYPE_BOOL: &str = "bool";
pub const TYPE_STR: &str = "str";
pub const TYPE_RANGE: &str = "range";

impl Val {
    pub const fn typ(&self) -> PrimitiveType {
        match self {
            Self::Int(_) => PrimitiveType::Int,
            Self::Float(_) => PrimitiveType::Float,
            Self::Bool(_) => PrimitiveType::Bool,
            Self::Str(_) => PrimitiveType::Str,
            Self::Range(_) => PrimitiveType::Range,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PrimitiveType {
    Int,
    Float,
    Range,
    Bool,
    Str,
}

impl Display for PrimitiveType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int => f.write_str(TYPE_INT),
            Self::Float => f.write_str(TYPE_FLOAT),
            Self::Bool => f.write_str(TYPE_BOOL),
            Self::Str => f.write_str(TYPE_STR),
            Self::Range => f.write_str(TYPE_RANGE),
        }
    }
}

impl PrimitiveType {
    pub fn from(name: &str) -> Option<Self> {
        let t = match name {
            TYPE_INT => Self::Int,
            TYPE_FLOAT => Self::Float,
            TYPE_BOOL => Self::Bool,
            TYPE_STR => Self::Str,
            TYPE_RANGE => Self::Range,
            _ => return None,
        };

        Some(t)
    }
}
