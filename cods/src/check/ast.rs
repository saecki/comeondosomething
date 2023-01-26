use std::fmt::Debug;

use crate::{DataType, FunRef, Span, Val, VarRef};

#[derive(Clone, Debug, PartialEq)]
pub struct Asts {
    pub asts: Vec<Ast>,
    pub global_frame_size: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Ast {
    pub typ: AstT,
    pub data_type: AstDatatype,
    pub returns: bool,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AstDatatype {
    Expr(DataType),
    Statement,
}

impl AstDatatype {
    pub fn as_expr(&self) -> Option<DataType> {
        if let Self::Expr(v) = self {
            Some(*v)
        } else {
            None
        }
    }
}

impl Ast {
    pub const fn expr(typ: AstT, data_type: DataType, returns: bool, span: Span) -> Self {
        Self {
            typ,
            data_type: AstDatatype::Expr(data_type),
            returns,
            span,
        }
    }

    pub const fn statement(typ: AstT, returns: bool, span: Span) -> Self {
        Self {
            typ,
            data_type: AstDatatype::Statement,
            returns,
            span,
        }
    }

    pub fn val(val: Val, span: Span) -> Self {
        let data_type = val.data_type();
        Self::expr(AstT::Val(val), data_type, false, span)
    }

    pub fn var(var: VarRef, data_type: DataType, returns: bool, span: Span) -> Self {
        Self::expr(AstT::Var(var), data_type, returns, span)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstT {
    Error,
    Var(VarRef),
    Val(Val),
    Op(Op, Vec<Ast>),
    Is(Box<Ast>, DataType),
    Cast(Box<Ast>, DataType),
    Unit,
    Block(Vec<Ast>),
    IfExpr(IfExpr),
    MatchExpr(MatchExpr),
    WhileLoop(WhileLoop),
    ForLoop(ForLoop),
    VarAssign(VarRef, Box<Ast>),
    FunCall(FunRef, Vec<Ast>),
    Return(Box<Ast>),
    BuiltinFunCall(BuiltinFunCall, Vec<Ast>),
    Spill(Vec<(String, VarRef)>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Op {
    Not,
    NegInt,
    NegFloat,
    RangeIn,
    RangeEx,
    AddInt,
    AddFloat,
    SubInt,
    SubFloat,
    MulInt,
    MulFloat,
    DivInt,
    DivFloat,
    PowInt,
    PowFloat,
    PowFloatInt,
    RemInt,
    RemFloat,
    RemEuclidInt,
    FactorialInt,
    Eq,
    Ne,
    LtInt,
    LtFloat,
    LeInt,
    LeFloat,
    GtInt,
    GtFloat,
    GeInt,
    GeFloat,
    Or,
    And,
    BwOrInt,
    BwOrBool,
    XorInt,
    XorBool,
    BwAndInt,
    BwAndBool,
    ShlInt,
    ShrInt,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfExpr {
    pub cases: Vec<CondBlock>,
    pub else_block: Option<Vec<Ast>>,
}

impl IfExpr {
    pub const fn new(cases: Vec<CondBlock>, else_block: Option<Vec<Ast>>) -> Self {
        Self { cases, else_block }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CondBlock {
    pub cond: Ast,
    pub block: Vec<Ast>,
}

impl CondBlock {
    pub const fn new(cond: Ast, block: Vec<Ast>) -> Self {
        Self { cond, block }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchExpr {
    pub value: Box<Ast>,
    pub arms: Vec<MatchArm>,
    pub else_arm: Option<Box<Ast>>,
}

impl MatchExpr {
    pub fn new(value: Box<Ast>, arms: Vec<MatchArm>, else_arm: Option<Box<Ast>>) -> Self {
        Self {
            value,
            arms,
            else_arm,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchArm {
    pub cond: Ast,
    pub expr: Ast,
}

impl MatchArm {
    pub fn new(cond: Ast, expr: Ast) -> Self {
        Self { cond, expr }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhileLoop {
    pub cond: Box<Ast>,
    pub block: Vec<Ast>,
}

impl WhileLoop {
    pub fn new(cond: Box<Ast>, block: Vec<Ast>) -> Self {
        Self { cond, block }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ForLoop {
    pub var: VarRef,
    pub iter: Box<Ast>,
    pub block: Vec<Ast>,
}

impl ForLoop {
    pub const fn new(var: VarRef, iter: Box<Ast>, block: Vec<Ast>) -> Self {
        Self { var, iter, block }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Fun {
    pub params: Vec<VarRef>,
    pub block: Vec<Ast>,
    pub frame_size: usize,
}

impl Fun {
    pub fn new(params: Vec<VarRef>, block: Vec<Ast>, frame_size: usize) -> Self {
        Self {
            params,
            block,
            frame_size,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BuiltinFunCall {
    PowInt,
    PowFloat,
    PowFloatInt,
    Ln,
    Log,
    Sqrt,
    Ncr,
    ToDeg,
    ToRad,
    Sin,
    Cos,
    Tan,
    Sinh,
    Cosh,
    Tanh,
    Asin,
    Acos,
    Atan,
    Asinh,
    Acosh,
    Atanh,
    Gcd,
    MinInt,
    MinFloat,
    MaxInt,
    MaxFloat,
    ClampInt,
    ClampFloat,
    AbsInt,
    AbsFloat,
    Print,
    Println,
    Assert,
    AssertEq,
}
