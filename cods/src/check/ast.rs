use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

use crate::{DataType, Range, Span, Val, ValSpan};

#[derive(Clone, Debug, PartialEq)]
pub struct Ast {
    pub typ: AstT,
    pub data_type: DataType,
    pub span: Span,
}

impl Ast {
    pub const fn new(typ: AstT, data_type: DataType, span: Span) -> Self {
        Self {
            typ,
            data_type,
            span,
        }
    }

    pub fn val(val: ValSpan) -> Self {
        match val.val {
            Val::Int(i) => Self::new(AstT::Int(IntExpr::Val(i)), DataType::Int, val.span),
            Val::Float(f) => Self::new(AstT::Float(FloatExpr::Val(f)), DataType::Float, val.span),
            Val::Bool(b) => Self::new(AstT::Bool(BoolExpr::Val(b)), DataType::Bool, val.span),
            Val::Str(s) => Self::new(AstT::Str(StrExpr::Val(s)), DataType::Str, val.span),
            Val::Range(r) => Self::new(AstT::Range(RangeExpr::Val(r)), DataType::Range, val.span),
            Val::Unit => Self::new(AstT::Unit, DataType::Unit, val.span),
        }
    }

    pub fn var(var: Rc<Var>, data_type: DataType, span: Span) -> Self {
        match data_type {
            DataType::Int => Self::new(AstT::Var(var), data_type, span),
            DataType::Float => Self::new(AstT::Var(var), data_type, span),
            DataType::Bool => Self::new(AstT::Var(var), data_type, span),
            DataType::Str => Self::new(AstT::Var(var), data_type, span),
            DataType::Range => Self::new(AstT::Var(var), data_type, span),
            DataType::Unit => Self::new(AstT::Unit, data_type, span),
            DataType::Any => Self::new(AstT::Var(var), data_type, span),
        }
    }

    pub fn int(expr: IntExpr, span: Span) -> Self {
        Self::new(AstT::Int(expr), DataType::Int, span)
    }

    pub fn float(expr: FloatExpr, span: Span) -> Self {
        Self::new(AstT::Float(expr), DataType::Float, span)
    }

    pub fn bool(expr: BoolExpr, span: Span) -> Self {
        Self::new(AstT::Bool(expr), DataType::Bool, span)
    }

    pub fn str(expr: StrExpr, span: Span) -> Self {
        Self::new(AstT::Str(expr), DataType::Str, span)
    }

    pub fn range(expr: RangeExpr, span: Span) -> Self {
        Self::new(AstT::Range(expr), DataType::Range, span)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstT {
    Error,
    Var(Rc<Var>),
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
    Assign(Rc<Var>, Box<Ast>),
    VarDef(Rc<Var>, Box<Ast>),
    FunCall(Rc<Fun>, Vec<Ast>),
    BuiltinFunCall(BuiltinFunCall, Vec<Ast>),
    Spill(Vec<(String, Rc<Var>)>),
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
    pub var: Rc<Var>,
    pub iter: Box<Ast>,
    pub block: Vec<Ast>,
}

impl ForLoop {
    pub const fn new(var: Rc<Var>, iter: Box<Ast>, block: Vec<Ast>) -> Self {
        Self { var, iter, block }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Var {
    value: RefCell<Option<Val>>,
}

impl Var {
    pub fn new(value: Option<Val>) -> Self {
        Self {
            value: RefCell::new(value),
        }
    }

    pub fn set(&self, value: Val) {
        self.value.replace(Some(value));
    }

    // TODO: allocate values using bumpalo and only store references
    // -> Use cell instead of refcell
    pub fn get(&self) -> Val {
        self.value
            .borrow()
            .as_ref()
            .expect("Expected variable to be initialized")
            .clone()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Fun {
    params: Vec<Rc<Var>>,
    block: Vec<Ast>,
}

impl Fun {
    pub fn new(params: Vec<Rc<Var>>, block: Vec<Ast>) -> Self {
        Self { params, block }
    }

    pub fn params(&self) -> &[Rc<Var>] {
        &self.params
    }

    pub fn block(&self) -> &[Ast] {
        &self.block
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
    Print,
    Println,
    Spill,
    SpillLocal,
    Assert,
    AssertEq,
}
