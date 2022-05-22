use crate::ast::Op::{self, *};
use crate::DataType::{self, *};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OpSignature<const N: usize> {
    pub params: [DataType; N],
    pub return_type: DataType,
}

impl<const N: usize> OpSignature<N> {
    pub const fn new(params: [DataType; N], return_type: DataType) -> Self {
        Self {
            params,
            return_type,
        }
    }
}

macro_rules! op_signatures {
    ($($builtin:ident($($params:tt)*) -> $return_type:ident)*) => {{
        [
            $(op_signature!($builtin($($params)*) -> $return_type)),*
        ]
    }};
}

macro_rules! op_signature {
    ($builtin:ident($($params:ident),*) -> $return_type:ident) => {{
        (
            $builtin,
            OpSignature::new(
                [$($params),*],
                $return_type,
            )
        )
    }};
}

pub const NOT_SIGNATURES: [(Op, OpSignature<1>); 1] = op_signatures! {
    Not(Bool) -> Bool
};
pub const NEG_SIGNATURES: [(Op, OpSignature<1>); 2] = op_signatures! {
    NegInt(Int) -> Int
    NegFloat(Float) -> Float
};

pub const FACTORIAL_SIGNATURES: [(Op, OpSignature<1>); 1] = op_signatures! {
    FactorialInt(Int) -> Int
};

pub const RANGE_EX_SIGNATURES: [(Op, OpSignature<2>); 1] = op_signatures! {
    RangeEx(Int, Int) -> Range
};
pub const RANGE_IN_SIGNATURES: [(Op, OpSignature<2>); 1] = op_signatures! {
    RangeIn(Int, Int) -> Range
};
pub const ADD_SIGNATURES: [(Op, OpSignature<2>); 2] = op_signatures! {
    AddInt(Int, Int) -> Int
    AddFloat(Float, Float) -> Float
};
pub const SUB_SIGNATURES: [(Op, OpSignature<2>); 2] = op_signatures! {
    SubInt(Int, Int) -> Int
    SubFloat(Float, Float) -> Float
};
pub const MUL_SIGNATURES: [(Op, OpSignature<2>); 2] = op_signatures! {
    MulInt(Int, Int) -> Int
    MulFloat(Float, Float) -> Float
};
pub const DIV_SIGNATURES: [(Op, OpSignature<2>); 2] = op_signatures! {
    DivInt(Int, Int) -> Int
    DivFloat(Float, Float) -> Float
};
pub const REM_SIGNATURES: [(Op, OpSignature<2>); 1] = op_signatures! {
    RemInt(Int, Int) -> Int
};
pub const REM_EUCLID_SIGNATURES: [(Op, OpSignature<2>); 1] = op_signatures! {
    RemEuclidInt(Int, Int) -> Int
};
pub const LT_SIGNATURES: [(Op, OpSignature<2>); 2] = op_signatures! {
    LtInt(Int, Int) -> Bool
    LtFloat(Float, Float) -> Bool
};
pub const LE_SIGNATURES: [(Op, OpSignature<2>); 2] = op_signatures! {
    LeInt(Int, Int) -> Bool
    LeFloat(Float, Float) -> Bool
};
pub const GT_SIGNATURES: [(Op, OpSignature<2>); 2] = op_signatures! {
    GtInt(Int, Int) -> Bool
    GtFloat(Float, Float) -> Bool
};
pub const GE_SIGNATURES: [(Op, OpSignature<2>); 2] = op_signatures! {
    GeInt(Int, Int) -> Bool
    GeFloat(Float, Float) -> Bool
};
pub const OR_SIGNATURES: [(Op, OpSignature<2>); 1] = op_signatures! {
    Or(Bool, Bool) -> Bool
};
pub const AND_SIGNATURES: [(Op, OpSignature<2>); 1] = op_signatures! {
    And(Bool, Bool) -> Bool
};
pub const BW_OR_SIGNATURES: [(Op, OpSignature<2>); 2] = op_signatures! {
    BwOrInt(Int, Int) -> Int
    BwOrBool(Bool, Bool) -> Bool
};
pub const BW_AND_SIGNATURES: [(Op, OpSignature<2>); 2] = op_signatures! {
    BwAndInt(Int, Int) -> Int
    BwAndBool(Bool, Bool) -> Bool
};
