use std::f64::consts;

use crate::ast::BuiltinFunCall::{self, *};
use crate::DataType::*;
use crate::{BuiltinConst, BuiltinFun, DataType, Val};

const PI: Val = Val::Float(consts::PI);
const TAU: Val = Val::Float(consts::TAU);
const E: Val = Val::Float(consts::E);

impl BuiltinConst {
    pub const fn val(&self) -> Val {
        match self {
            Self::Pi => PI,
            Self::Tau => TAU,
            Self::E => E,
        }
    }

    pub const fn data_type(&self) -> DataType {
        match self {
            Self::Pi => PI.data_type(),
            Self::Tau => TAU.data_type(),
            Self::E => E.data_type(),
        }
    }
}

pub struct Signature {
    pub params: &'static [DataType],
    pub return_type: DataType,
}

impl Signature {
    pub const fn new(params: &'static [DataType], return_type: DataType) -> Self {
        Self {
            params,
            return_type,
        }
    }
}

macro_rules! signatures {
    ($($builtin:ident($($params:ident),*) -> $return_type:ident)*) => {{
        [
            $(
                ($builtin, Signature::new(&[$($params),*], $return_type))
            ),*
        ]
    }};
}

const POW_SIGNATURES: [(BuiltinFunCall, Signature); 2] = signatures! {
    PowInt(Int, Int) -> Int
    PowFloat(Float, Float) -> Float
};
const LN_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Ln(Float) -> Float
};
const SQRT_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Sqrt(Float) -> Float
};
const LOG_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Log(Float, Float) -> Float
};
const NCR_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Ncr(Int, Int) -> Int
};
const TO_DEG_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    ToDeg(Float) -> Float
};
const TO_RAD_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    ToRad(Float) -> Float
};
const SIN_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Sin(Float) -> Float
};
const COS_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Cos(Float) -> Float
};
const TAN_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Tan(Float) -> Float
};
const ASIN_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Asin(Float) -> Float
};
const ACOS_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Acos(Float) -> Float
};
const ATAN_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Atan(Float) -> Float
};
const GCD_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Gcd(Int, Int) -> Int
};
const MIN_SIGNATURES: [(BuiltinFunCall, Signature); 2] = signatures! {
    MinInt(Int, Int) -> Int
    MinFloat(Float, Float) -> Int
};
const MAX_SIGNATURES: [(BuiltinFunCall, Signature); 2] = signatures! {
    MaxInt(Int, Int) -> Int
    MaxFloat(Float, Float) -> Int
};
const CLAMP_SIGNATURES: [(BuiltinFunCall, Signature); 2] = signatures! {
    ClampInt(Int, Int, Int) -> Int
    ClampFloat(Float, Float, Float) -> Int
};
const PRINT_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Print(Any) -> Unit
};
const PRINTLN_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Println(Any) -> Unit
};
const SPILL_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Spill() -> Unit
};
const SPILL_LOCAL_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    SpillLocal() -> Unit
};
const ASSERT_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Assert(Bool) -> Unit
};
const ASSERT_EQ_SIGNATURES: [(BuiltinFunCall, Signature); 6] = signatures! {
    AssertEq(Int, Int) -> Unit
    AssertEq(Float, Float) -> Unit
    AssertEq(Bool, Bool) -> Unit
    AssertEq(Str, Str) -> Unit
    AssertEq(Range, Range) -> Unit
    AssertEq(Unit, Unit) -> Unit
};

impl BuiltinFun {
    pub fn signatures(&self) -> &[(BuiltinFunCall, Signature)] {
        match self {
            BuiltinFun::Pow => &POW_SIGNATURES,
            BuiltinFun::Ln => &LN_SIGNATURES,
            BuiltinFun::Log => &LOG_SIGNATURES,
            BuiltinFun::Sqrt => &SQRT_SIGNATURES,
            BuiltinFun::Ncr => &NCR_SIGNATURES,
            BuiltinFun::ToDeg => &TO_DEG_SIGNATURES,
            BuiltinFun::ToRad => &TO_RAD_SIGNATURES,
            BuiltinFun::Sin => &SIN_SIGNATURES,
            BuiltinFun::Cos => &COS_SIGNATURES,
            BuiltinFun::Tan => &TAN_SIGNATURES,
            BuiltinFun::Asin => &ASIN_SIGNATURES,
            BuiltinFun::Acos => &ACOS_SIGNATURES,
            BuiltinFun::Atan => &ATAN_SIGNATURES,
            BuiltinFun::Gcd => &GCD_SIGNATURES,
            BuiltinFun::Min => &MIN_SIGNATURES, // TODO: variadic arguments
            BuiltinFun::Max => &MAX_SIGNATURES, // TODO: variadic arguments
            BuiltinFun::Clamp => &CLAMP_SIGNATURES,
            BuiltinFun::Print => &PRINT_SIGNATURES, // TODO: variadic arguments
            BuiltinFun::Println => &PRINTLN_SIGNATURES, // TODO: variadic arguments
            BuiltinFun::Spill => &SPILL_SIGNATURES,
            BuiltinFun::SpillLocal => &SPILL_LOCAL_SIGNATURES,
            BuiltinFun::Assert => &ASSERT_SIGNATURES,
            BuiltinFun::AssertEq => &ASSERT_EQ_SIGNATURES,
        }
    }
}
