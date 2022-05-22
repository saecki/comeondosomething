use std::cell::{Ref, RefCell};
use std::fmt::Debug;
use std::rc::Rc;

use crate::{DataType, Span, Val, VarRef};

#[derive(Clone, Debug, PartialEq)]
pub struct Asts {
    pub asts: Vec<Ast>,
    pub global_frame_size: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Ast {
    pub typ: AstT,
    pub data_type: Option<DataType>,
    pub returns: bool,
    pub span: Span,
}

impl Ast {
    pub const fn expr(typ: AstT, data_type: DataType, returns: bool, span: Span) -> Self {
        Self {
            typ,
            data_type: Some(data_type),
            returns,
            span,
        }
    }

    pub const fn statement(typ: AstT, returns: bool, span: Span) -> Self {
        Self {
            typ,
            data_type: None,
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
    WhileLoop(WhileLoop),
    ForLoop(ForLoop),
    Assign(VarRef, Box<Ast>),
    VarDef(VarRef, Box<Ast>),
    FunCall(Rc<Fun>, Vec<Ast>),
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
    RemInt,
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
    BwAndInt,
    BwAndBool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum IntExpr {
    Cast(Box<Ast>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum FloatExpr {
    Cast(Box<Ast>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum BoolExpr {
    Cast(Box<Ast>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum CharExpr {
    Cast(Box<Ast>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum StrExpr {
    Cast(Box<Ast>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum RangeExpr {
    Cast(Box<Ast>),
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
pub struct Fun(RefCell<Option<InnerFun>>);

#[derive(Clone, Debug, PartialEq)]
struct InnerFun {
    params: Vec<VarRef>,
    block: Vec<Ast>,
    frame_size: usize,
}

impl Default for Fun {
    fn default() -> Self {
        Self(RefCell::new(None))
    }
}

impl Fun {
    pub fn init(&self, params: Vec<VarRef>, block: Vec<Ast>, frame_size: usize) {
        self.0.replace(Some(InnerFun {
            params,
            block,
            frame_size,
        }));
    }

    pub fn borrow(&self) -> FunRef<'_> {
        FunRef {
            inner: self.0.borrow(),
        }
    }

    pub fn frame_size(&self) -> usize {
        self.0
            .borrow()
            .as_ref()
            .expect("Expected function to be initialized")
            .frame_size
    }
}

pub struct FunRef<'a> {
    inner: Ref<'a, Option<InnerFun>>,
}

impl FunRef<'_> {
    pub fn params(&self) -> &[VarRef] {
        &self
            .inner
            .as_ref()
            .expect("Expected function to be initialized")
            .params
    }

    pub fn block(&self) -> &[Ast] {
        &self
            .inner
            .as_ref()
            .expect("Expected function to be initialized")
            .block
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BuiltinFunCall {
    PowInt,
    PowFloat,
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
