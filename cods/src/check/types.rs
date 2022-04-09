use std::fmt::{self, Display};

use crate::Val;

pub const TYPE_INT: &str = "int";
pub const TYPE_FLOAT: &str = "float";
pub const TYPE_BOOL: &str = "bool";
pub const TYPE_STR: &str = "str";
pub const TYPE_RANGE: &str = "range";
pub const TYPE_UNIT: &str = "unit";
pub const TYPE_ANY: &str = "any";

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
    Any,
}

impl Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int => f.write_str(TYPE_INT),
            Self::Float => f.write_str(TYPE_FLOAT),
            Self::Bool => f.write_str(TYPE_BOOL),
            Self::Str => f.write_str(TYPE_STR),
            Self::Range => f.write_str(TYPE_RANGE),
            Self::Unit => f.write_str(TYPE_UNIT),
            Self::Any => f.write_str(TYPE_ANY),
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
            TYPE_UNIT => Self::Str,
            TYPE_ANY => Self::Any,
            _ => return None,
        };

        Some(t)
    }

    pub fn is(self, requirement: Self) -> bool {
        match (self, requirement) {
            (_, DataType::Any) => true,
            (a, b) => a == b,
        }
    }

    pub fn is_not(self, requirement: Self) -> bool {
        !self.is(requirement)
    }

    pub fn is_comparable_to(self, other: Self) -> bool {
        match (self, other) {
            (DataType::Any, _) => true,
            (_, DataType::Any) => true,
            (a, b) => a == b,
        }
    }

    pub fn is_not_comparable_to(self, other: Self) -> bool {
        !self.is_comparable_to(other)
    }
}
