use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use crate::{DataType, IdentSpan, Infix, Postfix, Prefix, Span, ValSpan};

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
    Ident(IdentSpan),
    Block(Vec<Ast>),
    IfExpr(IfExpr),
    WhileLoop(WhileLoop),
    ForLoop(ForLoop),
    FunDef(Rc<Fun>),
    FunCall(Rc<Fun>, Vec<Ast>),
    VarDef(IdentSpan, Box<Ast>, bool),
    Prefix(Prefix, Box<Ast>),
    Postfix(Box<Ast>, Postfix),
    Infix(Box<Ast>, Infix, Box<Ast>),
    InfixAssign(IdentSpan, Infix, Box<Ast>),
    Assign(IdentSpan, Infix, Box<Ast>),
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
    pub ident: IdentSpan,
    pub iter: Box<Ast>,
    pub block: Vec<Ast>,
}

impl ForLoop {
    pub const fn new(ident: IdentSpan, iter: Box<Ast>, block: Vec<Ast>) -> Self {
        Self { ident, iter, block }
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

// TODO: define fun before parsing body to support recursive calls
#[derive(Clone, Debug, PartialEq)]
pub struct Fun {
    pub ident: IdentSpan,
    pub params: Vec<FunParam>,
    pub return_type: Option<DataType>,
    pub block: Vec<Ast>,
}

impl Fun {
    pub const fn new(
        ident: IdentSpan,
        params: Vec<FunParam>,
        return_type: Option<DataType>,
        block: Vec<Ast>,
    ) -> Self {
        Self {
            ident,
            params,
            return_type,
            block,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunParam {
    pub ident: IdentSpan,
    pub typ: DataType,
}

impl FunParam {
    pub const fn new(ident: IdentSpan, typ: DataType) -> Self {
        Self { ident, typ }
    }
}

// TODO: move to scope
#[derive(Clone, Debug, PartialEq)]
pub struct Var {
    pub ident: IdentSpan,
    pub data_type: DataType,
    pub assigned: bool,
    pub mutable: bool,
}

impl Var {
    pub fn new(ident: IdentSpan, data_type: DataType, assigned: bool, mutable: bool) -> Self {
        Self {
            ident,
            data_type,
            assigned,
            mutable,
        }
    }
}
