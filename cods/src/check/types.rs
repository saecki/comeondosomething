use std::fmt::{self, Display};

use crate::Val;

pub const TYPE_INT: &str = "int";
pub const TYPE_FLOAT: &str = "float";
pub const TYPE_BOOL: &str = "bool";
pub const TYPE_STR: &str = "str";
pub const TYPE_RANGE: &str = "range";
pub const TYPE_UNIT: &str = "()";

impl Val {
    pub const fn data_type(&self) -> DataType {
        match self {
            Self::Int(_) => DataType::Int,
            Self::Float(_) => DataType::Float,
            Self::Bool(_) => DataType::Bool,
            Self::Str(_) => DataType::Str,
            Self::Range(_) => DataType::Range,
            Self::Unit => DataType::Unit,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DataType {
    Int,
    Float,
    Bool,
    Str,
    Range,
    Unit,
}

impl Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int => f.write_str(TYPE_INT),
            Self::Float => f.write_str(TYPE_FLOAT),
            Self::Bool => f.write_str(TYPE_BOOL),
            Self::Str => f.write_str(TYPE_STR),
            Self::Range => f.write_str(TYPE_RANGE),
            Self::Unit => f.write_str("()"), // TODO
        }
    }
}

impl DataType {
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
