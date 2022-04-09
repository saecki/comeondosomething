use std::fmt::{self, Display};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BuiltinFun {
    Pow,
    Ln,
    Log,
    Sqrt,
    Ncr,
    ToDeg,
    ToRad,
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
    SpillLocal,
    Assert,
    AssertEq,
}

impl Display for BuiltinFun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BuiltinFun::Pow => write!(f, "pow"),
            BuiltinFun::Ln => write!(f, "ln"),
            BuiltinFun::Log => write!(f, "log"),
            BuiltinFun::Sqrt => write!(f, "sqrt"),
            BuiltinFun::Ncr => write!(f, "ncr"),
            BuiltinFun::ToDeg => write!(f, "to_deg"),
            BuiltinFun::ToRad => write!(f, "to_rad"),
            BuiltinFun::Sin => write!(f, "sin"),
            BuiltinFun::Cos => write!(f, "cos"),
            BuiltinFun::Tan => write!(f, "tan"),
            BuiltinFun::Asin => write!(f, "asin"),
            BuiltinFun::Acos => write!(f, "acos"),
            BuiltinFun::Atan => write!(f, "atan"),
            BuiltinFun::Gcd => write!(f, "gcd"),
            BuiltinFun::Min => write!(f, "min"),
            BuiltinFun::Max => write!(f, "max"),
            BuiltinFun::Clamp => write!(f, "clamp"),
            BuiltinFun::Print => write!(f, "print"),
            BuiltinFun::Println => write!(f, "println"),
            BuiltinFun::Spill => write!(f, "spill"),
            BuiltinFun::SpillLocal => write!(f, "spill_local"),
            BuiltinFun::Assert => write!(f, "assert"),
            BuiltinFun::AssertEq => write!(f, "assert_eq"),
        }
    }
}

impl BuiltinFun {
    pub fn from(name: &str) -> Option<Self> {
        let b = match name {
            "pow" => Self::Pow,
            "ln" => Self::Ln,
            "log" => Self::Log,
            "sqrt" => Self::Sqrt,
            "ncr" => Self::Ncr,
            "to_deg" => Self::ToDeg,
            "to_rad" => Self::ToRad,
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
            "spill_local" => Self::SpillLocal,
            "assert" => Self::Assert,
            "assert_eq" => Self::AssertEq,
            _ => return None,
        };
        Some(b)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BuiltinConst {
    Pi,
    Tau,
    E,
}

impl Display for BuiltinConst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pi => write!(f, "PI"),
            Self::Tau => write!(f, "TAU"),
            Self::E => write!(f, "E"),
        }
    }
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
}
