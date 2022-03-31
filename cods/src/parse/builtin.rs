use std::f64::consts;

use crate::Val;

pub enum BuiltinFun {
    Pow,
    Ln,
    Log,
    Sqrt,
    Ncr,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Gcd,
    Min,
    Max,
    Clamp,
    Print,
    Println,
    Spill,
    Assert,
    AssertEq,
}

impl BuiltinFun {
    pub fn from(name: &str) -> Option<Self> {
        let b = match name {
            "pow" => Self::Pow,
            "ln" => Self::Ln,
            "log" => Self::Log,
            "sqrt" => Self::Sqrt,
            "ncr" => Self::Ncr,
            "sin" => Self::Sin,
            "cos" => Self::Cos,
            "tan" => Self::Tan,
            "asin" => Self::Asin,
            "acos" => Self::Acos,
            "atan" => Self::Atan,
            "gcd" => Self::Gcd,
            "min" => Self::Min,
            "max" => Self::Max,
            "clamp" => Self::Clamp,
            "print" => Self::Print,
            "println" => Self::Println,
            "spill" => Self::Spill,
            "assert" => Self::Assert,
            "assert_eq" => Self::AssertEq,
            _ => return None,
        };
        Some(b)
    }
}

pub const PI: Val = Val::Float(consts::PI);
pub const TAU: Val = Val::Float(consts::TAU);
pub const E: Val = Val::Float(consts::E);

pub enum BuiltinConst {
    Pi,
    Tau,
    E,
}

impl BuiltinConst {
    pub fn from(name: &str) -> Option<Self> {
        let b = match name {
            "PI" => Self::Pi,
            "TAU" => Self::Tau,
            "E" => Self::E,
            _ => return None,
        };
        Some(b)
    }

    pub const fn val(&self) -> Val {
        match self {
            Self::Pi => PI,
            Self::Tau => TAU,
            Self::E => E,
        }
    }

    pub const fn val_ref(&self) -> &'static Val {
        match self {
            Self::Pi => &PI,
            Self::Tau => &TAU,
            Self::E => &E,
        }
    }
}
