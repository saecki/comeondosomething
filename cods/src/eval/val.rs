use crate::{Range, Val};

impl Val {
    pub fn to_int(&self) -> Option<i128> {
        match self {
            Self::Int(i) => Some(*i),
            _ => None,
        }
    }

    pub fn to_f64(&self) -> Option<f64> {
        match self {
            Self::Float(f) => Some(*f),
            _ => None,
        }
    }

    pub fn to_bool(&self) -> Option<bool> {
        match self {
            Self::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn to_str(&self) -> Option<&str> {
        match self {
            Self::Str(s) => Some(s),
            _ => None,
        }
    }

    pub fn into_str(self) -> Option<String> {
        match self {
            Self::Str(s) => Some(s),
            _ => None,
        }
    }

    pub fn to_range(&self) -> Option<Range> {
        match self {
            Self::Range(r) => Some(*r),
            _ => None,
        }
    }
}
