use strum_macros::{Display, EnumString};

use crate::Val;

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

#[derive(Clone, Copy, Debug, PartialEq, Eq, EnumString, Display)]
#[strum(serialize_all = "snake_case")]
pub enum DataType {
    Int,
    Float,
    Bool,
    Str,
    Range,
    Unit,
    Any,
}

impl DataType {
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
