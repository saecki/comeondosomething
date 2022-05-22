use std::f64::consts;

use strum::{Display, EnumString};

use crate::ast::BuiltinFunCall::{self, *};
use crate::DataType::*;
use crate::{DataType, Val};

const PI: Val = Val::Float(consts::PI);
const TAU: Val = Val::Float(consts::TAU);
const E: Val = Val::Float(consts::E);

#[derive(Clone, Copy, Debug, PartialEq, EnumString, Display)]
#[strum(serialize_all = "snake_case")]
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
    Abs,
    Print,
    Println,
    Spill,
    SpillLocal,
    Assert,
    AssertEq,
}

#[derive(Clone, Copy, Debug, PartialEq, EnumString, Display)]
#[strum(serialize_all = "SCREAMING_SNAKE_CASE")]
pub enum BuiltinConst {
    Pi,
    Tau,
    E,
}

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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Signature {
    pub params: &'static [DataType],
    /// Repetition for the last parameter
    pub repetition: Repetition,
    pub return_type: DataType,
}

impl Signature {
    pub const fn new(
        params: &'static [DataType],
        repetition: Repetition,
        return_type: DataType,
    ) -> Self {
        Self {
            params,
            repetition,
            return_type,
        }
    }

    pub const fn empty() -> Self {
        Self::new(&[], Repetition::One, DataType::Unit)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Repetition {
    /// a: int
    One,
    /// a: ..int
    ZeroOrMore,
    /// a: ...int
    OneOrMore,
}

macro_rules! signatures {
    ($($builtin:ident($($params:tt)*) -> $return_type:ident)*) => {{
        [
            $(signature!($builtin($($params)*) -> $return_type)),*
        ]
    }};
}

macro_rules! signature {
    ($builtin:ident($($params:ident),*) -> $return_type:ident) => {{
        (
            $builtin,
            Signature::new(
                &[$($params),*],
                Repetition::One,
                $return_type,
            )
        )
    }};
    ($builtin:ident($($params:ident,)* ..$last:ident) -> $return_type:ident) => {{
        (
            $builtin,
            Signature::new(
                &[$($params),* $last],
                Repetition::ZeroOrMore,
                $return_type,
            )
        )
    }};
    ($builtin:ident($($params:ident,)* ...$last:ident) -> $return_type:ident) => {{
        (
            $builtin,
            Signature::new(
                &[$($params),* $last],
                Repetition::OneOrMore,
                $return_type,
            )
        )
    }};
}

pub const POW_SIGNATURES: [(BuiltinFunCall, Signature); 2] = signatures! {
    PowInt(Int, Int) -> Int
    PowFloat(Float, Float) -> Float
};
pub const LN_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Ln(Float) -> Float
};
pub const SQRT_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Sqrt(Float) -> Float
};
pub const LOG_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Log(Float, Float) -> Float
};
pub const NCR_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Ncr(Int, Int) -> Int
};
pub const TO_DEG_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    ToDeg(Float) -> Float
};
pub const TO_RAD_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    ToRad(Float) -> Float
};
pub const SIN_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Sin(Float) -> Float
};
pub const COS_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Cos(Float) -> Float
};
pub const TAN_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Tan(Float) -> Float
};
pub const ASIN_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Asin(Float) -> Float
};
pub const ACOS_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Acos(Float) -> Float
};
pub const ATAN_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Atan(Float) -> Float
};
pub const GCD_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Gcd(Int, Int) -> Int
};
pub const MIN_SIGNATURES: [(BuiltinFunCall, Signature); 2] = signatures! {
    MinInt(...Int) -> Int
    MinFloat(...Float) -> Int
};
pub const MAX_SIGNATURES: [(BuiltinFunCall, Signature); 2] = signatures! {
    MaxInt(...Int) -> Int
    MaxFloat(...Float) -> Int
};
pub const CLAMP_SIGNATURES: [(BuiltinFunCall, Signature); 2] = signatures! {
    ClampInt(Int, Int, Int) -> Int
    ClampFloat(Float, Float, Float) -> Float
};
pub const ABS_SIGNATURES: [(BuiltinFunCall, Signature); 2] = signatures! {
    AbsInt(Int) -> Int
    AbsFloat(Float) -> Float
};
pub const PRINT_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Print(..Any) -> Unit
};
pub const PRINTLN_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Println(..Any) -> Unit
};
pub const ASSERT_SIGNATURES: [(BuiltinFunCall, Signature); 1] = signatures! {
    Assert(Bool) -> Unit
};
pub const ASSERT_EQ_SIGNATURES: [(BuiltinFunCall, Signature); 6] = signatures! {
    AssertEq(Int, Int) -> Unit
    AssertEq(Float, Float) -> Unit
    AssertEq(Bool, Bool) -> Unit
    AssertEq(Str, Str) -> Unit
    AssertEq(Range, Range) -> Unit
    AssertEq(Unit, Unit) -> Unit
};
