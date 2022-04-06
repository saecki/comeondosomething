use std::cell::RefCell;
use std::fmt::Debug;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use crate::{DataType, Infix, Postfix, Prefix, Span, Val, ValSpan};

#[derive(Clone, Debug, PartialEq)]
pub struct Ast {
    pub typ: AstT,
    pub data_type: DataType,
    pub span: Span,
}

impl Deref for Ast {
    type Target = AstT;

    fn deref(&self) -> &Self::Target {
        &self.typ
    }
}

impl DerefMut for Ast {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.typ
    }
}

impl Ast {
    pub fn new(typ: AstT, data_type: DataType, span: Span) -> Self {
        Self {
            typ,
            data_type,
            span,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstT {
    Empty,
    Error,
    Val(ValSpan),
    Block(Vec<Ast>),
    IfExpr(IfExpr),
    WhileLoop(WhileLoop),
    ForLoop(ForLoop),
    Prefix(Prefix, Box<Ast>),
    Postfix(Box<Ast>, Postfix),
    Infix(Box<Ast>, Infix, Box<Ast>),
    InfixAssign(Rc<Var>, Infix, Box<Ast>),
    Assign(Rc<Var>, Box<Ast>),
    VarDef(Rc<Var>, Box<Ast>),
    VarRef(Rc<Var>),
    FunCall(Rc<Fun>, Vec<Ast>),
}

impl AstT {
    pub fn is_empty(&self) -> bool {
        matches!(self, Self::Empty)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfExpr {
    pub cases: Vec<CondBlock>,
    pub else_block: Option<Block>,
}

impl IfExpr {
    pub const fn new(cases: Vec<CondBlock>, else_block: Option<Block>) -> Self {
        Self { cases, else_block }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CondBlock {
    pub cond: Ast,
    pub block: Vec<Ast>,
    pub span: Span,
}

impl CondBlock {
    pub const fn new(cond: Ast, block: Vec<Ast>, span: Span) -> Self {
        Self { cond, block, span }
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
pub struct Block {
    pub asts: Vec<Ast>,
    pub span: Span,
}

impl Block {
    pub const fn new(asts: Vec<Ast>, span: Span) -> Self {
        Self { asts, span }
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
