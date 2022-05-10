use std::cell::{Ref, RefCell};
use std::fmt::Debug;
use std::rc::Rc;

use crate::{DataType, Range, Span, Val, ValSpan, VarRef};

#[derive(Clone, Debug, PartialEq)]
pub struct Asts {
    pub asts: Vec<Ast>,
    pub global_frame_size: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Ast {
    pub typ: AstT,
    pub data_type: Option<DataType>,
    pub span: Span,
}

impl Ast {
    pub const fn expr(typ: AstT, data_type: DataType, span: Span) -> Self {
        Self {
            typ,
            data_type: Some(data_type),
            span,
        }
    }

    pub const fn statement(typ: AstT, span: Span) -> Self {
        Self {
            typ,
            data_type: None,
            span,
        }
    }

    pub fn val(val: ValSpan) -> Self {
        match val.val {
            Val::Int(i) => Self::expr(AstT::Int(IntExpr::Val(i)), DataType::Int, val.span),
            Val::Float(f) => Self::expr(AstT::Float(FloatExpr::Val(f)), DataType::Float, val.span),
            Val::Bool(b) => Self::expr(AstT::Bool(BoolExpr::Val(b)), DataType::Bool, val.span),
            Val::Str(s) => Self::expr(AstT::Str(StrExpr::Val(s)), DataType::Str, val.span),
            Val::Range(r) => Self::expr(AstT::Range(RangeExpr::Val(r)), DataType::Range, val.span),
            Val::Unit => Self::expr(AstT::Unit, DataType::Unit, val.span),
        }
    }

    pub fn var(var: VarRef, data_type: DataType, span: Span) -> Self {
        match data_type {
            DataType::Int => Self::expr(AstT::Var(var), data_type, span),
            DataType::Float => Self::expr(AstT::Var(var), data_type, span),
            DataType::Bool => Self::expr(AstT::Var(var), data_type, span),
            DataType::Str => Self::expr(AstT::Var(var), data_type, span),
            DataType::Range => Self::expr(AstT::Var(var), data_type, span),
            DataType::Unit => Self::expr(AstT::Unit, data_type, span),
            DataType::Any => Self::expr(AstT::Var(var), data_type, span),
        }
    }

    pub fn int(expr: IntExpr, span: Span) -> Self {
        Self::expr(AstT::Int(expr), DataType::Int, span)
    }

    pub fn float(expr: FloatExpr, span: Span) -> Self {
        Self::expr(AstT::Float(expr), DataType::Float, span)
    }

    pub fn bool(expr: BoolExpr, span: Span) -> Self {
        Self::expr(AstT::Bool(expr), DataType::Bool, span)
    }

    pub fn str(expr: StrExpr, span: Span) -> Self {
        Self::expr(AstT::Str(expr), DataType::Str, span)
    }

    pub fn range(expr: RangeExpr, span: Span) -> Self {
        Self::expr(AstT::Range(expr), DataType::Range, span)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstT {
    Error,
    Var(VarRef),
    Int(IntExpr),
    Float(FloatExpr),
    Bool(BoolExpr),
    Str(StrExpr),
    Range(RangeExpr),
    Unit,
    Block(Vec<Ast>),
    IfExpr(IfExpr),
    WhileLoop(WhileLoop),
    ForLoop(ForLoop),
    Assign(VarRef, Box<Ast>),
    VarDef(VarRef, Box<Ast>),
    FunCall(Rc<Fun>, Vec<Ast>),
    BuiltinFunCall(BuiltinFunCall, Vec<Ast>),
    Spill(Vec<(String, VarRef)>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum IntExpr {
    Val(i128),
    Cast(Box<Ast>),
    Neg(Box<Ast>),
    Add(Box<Ast>, Box<Ast>),
    Sub(Box<Ast>, Box<Ast>),
    Mul(Box<Ast>, Box<Ast>),
    Div(Box<Ast>, Box<Ast>),
    Rem(Box<Ast>, Box<Ast>),
    RemEuclid(Box<Ast>, Box<Ast>),
    Factorial(Box<Ast>),
    BwOr(Box<Ast>, Box<Ast>),
    BwAnd(Box<Ast>, Box<Ast>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum FloatExpr {
    Val(f64),
    Cast(Box<Ast>),
    Neg(Box<Ast>),
    Add(Box<Ast>, Box<Ast>),
    Sub(Box<Ast>, Box<Ast>),
    Mul(Box<Ast>, Box<Ast>),
    Div(Box<Ast>, Box<Ast>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum BoolExpr {
    Val(bool),
    Cast(Box<Ast>),
    Not(Box<Ast>),
    Eq(Box<Ast>, Box<Ast>),
    Ne(Box<Ast>, Box<Ast>),
    LtInt(Box<Ast>, Box<Ast>),
    LtFloat(Box<Ast>, Box<Ast>),
    LeInt(Box<Ast>, Box<Ast>),
    LeFloat(Box<Ast>, Box<Ast>),
    GtInt(Box<Ast>, Box<Ast>),
    GtFloat(Box<Ast>, Box<Ast>),
    GeInt(Box<Ast>, Box<Ast>),
    GeFloat(Box<Ast>, Box<Ast>),
    BwOr(Box<Ast>, Box<Ast>),
    BwAnd(Box<Ast>, Box<Ast>),
    Or(Box<Ast>, Box<Ast>),
    And(Box<Ast>, Box<Ast>),
    Is(Box<Ast>, DataType),
}

#[derive(Clone, Debug, PartialEq)]
pub enum StrExpr {
    Val(String),
    Cast(Box<Ast>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum RangeExpr {
    Val(Range),
    Cast(Box<Ast>),
    Ex(Box<Ast>, Box<Ast>),
    In(Box<Ast>, Box<Ast>),
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
    Spill,
    SpillLocal,
    Assert,
    AssertEq,
}
