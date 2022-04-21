use crate::{Range, Val};

impl Val {
    pub fn into_int(self) -> Option<i128> {
        match self {
            Self::Int(i) => Some(i),
            _ => None,
        }
    }

    pub fn into_f64(self) -> Option<f64> {
        match self {
            Self::Float(f) => Some(f),
            _ => None,
        }
    }

    pub fn into_bool(self) -> Option<bool> {
        match self {
            Self::Bool(b) => Some(b),
            _ => None,
        }
    }

    pub fn into_str(self) -> Option<String> {
        match self {
            Self::Str(s) => Some(s),
            _ => None,
        }
    }

    pub fn into_range(self) -> Option<Range> {
        match self {
            Self::Range(r) => Some(r),
            _ => None,
        }
    }

    pub fn unwrap_int(self) -> i128 {
        match self {
            Self::Int(i) => i,
            _ => panic!("Expected val of type 'int', found '{}'", self.data_type()),
        }
    }

    pub fn unwrap_float(self) -> f64 {
        match self {
            Self::Float(f) => f,
            _ => panic!("Expected val of type 'float', found '{}'", self.data_type()),
        }
    }

    pub fn unwrap_bool(self) -> bool {
        match self {
            Self::Bool(b) => b,
            _ => panic!("Expected val of type 'bool', found '{}'", self.data_type()),
        }
    }

    pub fn unwrap_str(self) -> String {
        match self {
            Self::Str(s) => s,
            _ => panic!("Expected val of type 'str', found '{}'", self.data_type()),
        }
    }

    pub fn unwrap_range(self) -> Range {
        match self {
            Self::Range(r) => r,
            _ => panic!("Expected val of type 'range', found '{}'", self.data_type()),
        }
    }
}
