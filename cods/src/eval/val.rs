use std::fmt::Display;

use crate::{Range, Span, Val, ValSpan};

#[derive(Clone, Debug, PartialEq)]
pub enum Return {
    Val(ValSpan),
    Unit(Span),
}

impl Display for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Val(v) => write!(f, "{v}"),
            Self::Unit(_) => write!(f, "()"),
        }
    }
}

impl Return {
    pub fn span(&self) -> Span {
        match self {
            Self::Val(v) => v.span,
            Self::Unit(r) => *r,
        }
    }

    pub fn to_val(&self) -> crate::Result<&ValSpan> {
        match self {
            Self::Val(v) => Ok(v),
            Self::Unit(r) => Err(crate::Error::ExpectedValue(*r)),
        }
    }

    pub fn into_val(self) -> crate::Result<ValSpan> {
        match self {
            Self::Val(v) => Ok(v),
            Self::Unit(r) => Err(crate::Error::ExpectedValue(r)),
        }
    }

    pub fn to_int(&self) -> crate::Result<i128> {
        self.to_val()?.to_int()
    }

    pub fn to_f64(&self) -> crate::Result<f64> {
        self.to_val()?.to_f64()
    }

    pub fn to_bool(&self) -> crate::Result<bool> {
        self.to_val()?.to_bool()
    }

    pub fn to_str(&self) -> crate::Result<&str> {
        self.to_val()?.to_str()
    }

    pub fn into_str(self) -> crate::Result<String> {
        self.into_val()?.into_str()
    }

    pub fn to_range(&self) -> crate::Result<Range> {
        self.to_val()?.to_range()
    }
}

impl ValSpan {
    pub fn to_int(&self) -> crate::Result<i128> {
        self.val
            .to_int()
            .ok_or_else(|| crate::Error::ExpectedInt(self.clone()))
    }

    pub fn to_f64(&self) -> crate::Result<f64> {
        self.val
            .to_f64()
            .ok_or_else(|| crate::Error::ExpectedNumber(self.clone()))
    }

    pub fn to_bool(&self) -> crate::Result<bool> {
        self.val
            .to_bool()
            .ok_or_else(|| crate::Error::ExpectedBool(self.clone()))
    }

    pub fn to_str(&self) -> crate::Result<&str> {
        self.val
            .to_str()
            .ok_or_else(|| crate::Error::ExpectedStr(self.clone()))
    }

    pub fn into_str(self) -> crate::Result<String> {
        match self.val {
            Val::Str(s) => Ok(s),
            _ => {
                Err(crate::Error::ExpectedStr(self))
            }
        }
    }

    pub fn to_range(&self) -> crate::Result<Range> {
        self.val
            .to_range()
            .ok_or_else(|| crate::Error::ExpectedRange(self.clone()))
    }
}

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
