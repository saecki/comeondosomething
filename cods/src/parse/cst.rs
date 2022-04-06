use crate::{IdentSpan, Infix, Kw, Op, Par, Pct, Postfix, Prefix, Span, ValSpan};

#[derive(Clone, Debug, PartialEq)]
pub enum Cst {
    Empty(Span),
    Error(Span),
    Val(ValSpan),
    Ident(IdentSpan),
    Par(Par, Box<Cst>, Par),
    Block(Block),
    IfExpr(IfExpr),
    WhileLoop(WhileLoop),
    ForLoop(ForLoop),
    FunDef(FunDef),
    FunCall(FunCall),
    VarDef(VarDef),
    Prefix(Prefix, Box<Cst>),
    Postfix(Box<Cst>, Postfix),
    Infix(Box<Cst>, Infix, Box<Cst>),
    InfixAssign(IdentSpan, Infix, Box<Cst>),
    Assign(IdentSpan, Infix, Box<Cst>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfExpr {
    pub if_block: IfBlock,
    pub else_if_blocks: Vec<ElseIfBlock>,
    pub else_block: Option<ElseBlock>,
    /// If true, all branches are required to have the same return type
    pub is_expr: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfBlock {
    pub kw: Kw,
    pub cond: Box<Cst>,
    pub block: Block,
}

impl IfBlock {
    pub fn new(kw: Kw, cond: Box<Cst>, block: Block) -> Self {
        Self { kw, cond, block }
    }

    pub fn span(&self) -> Span {
        Span::across(self.kw.span, self.block.r_par.span)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ElseIfBlock {
    pub else_kw: Kw,
    pub if_kw: Kw,
    pub cond: Cst,
    pub block: Block,
}

impl ElseIfBlock {
    pub fn new(else_kw: Kw, if_kw: Kw, cond: Cst, block: Block) -> Self {
        Self {
            else_kw,
            if_kw,
            cond,
            block,
        }
    }

    pub fn span(&self) -> Span {
        Span::across(self.else_kw.span, self.block.r_par.span)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ElseBlock {
    pub kw: Kw,
    pub block: Block,
}

impl ElseBlock {
    pub fn new(kw: Kw, block: Block) -> Self {
        Self { kw, block }
    }

    pub fn span(&self) -> Span {
        Span::across(self.kw.span, self.block.r_par.span)
    }
}

impl IfExpr {
    pub fn new(
        if_block: IfBlock,
        else_if_blocks: Vec<ElseIfBlock>,
        else_block: Option<ElseBlock>,
        is_expr: bool,
    ) -> Self {
        Self {
            if_block,
            else_if_blocks,
            else_block,
            is_expr,
        }
    }

    pub fn span(&self) -> Span {
        if let Some(e) = &self.else_block {
            Span::across(self.if_block.kw.span, e.block.r_par.span)
        } else if let Some(e) = self.else_if_blocks.last() {
            Span::across(self.if_block.kw.span, e.block.r_par.span)
        } else {
            Span::across(self.if_block.kw.span, self.if_block.block.r_par.span)
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhileLoop {
    pub kw: Kw,
    pub cond: Box<Cst>,
    pub block: Block,
}

impl WhileLoop {
    pub fn new(while_kw: Kw, cond: Box<Cst>, block: Block) -> Self {
        Self {
            kw: while_kw,
            cond,
            block,
        }
    }

    pub fn span(&self) -> Span {
        Span::across(self.kw.span, self.block.r_par.span)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ForLoop {
    pub for_kw: Kw,
    pub ident: IdentSpan,
    pub in_kw: Kw,
    pub iter: Box<Cst>,
    pub block: Block,
}

impl ForLoop {
    pub fn new(for_kw: Kw, ident: IdentSpan, in_kw: Kw, iter: Box<Cst>, block: Block) -> Self {
        Self {
            for_kw,
            ident,
            in_kw,
            iter,
            block,
        }
    }

    pub fn span(&self) -> Span {
        Span::across(self.for_kw.span, self.block.r_par.span)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunDef {
    pub fn_kw: Kw,
    pub ident: IdentSpan,
    pub params: FunParams,
    pub return_type: Option<ReturnType>,
    pub block: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunParams {
    pub l_par: Par,
    pub r_par: Par,
    pub items: Vec<FunParam>,
}

impl FunParams {
    pub fn new(l_par: Par, r_par: Par, items: Vec<FunParam>) -> Self {
        Self {
            l_par,
            r_par,
            items,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunParam {
    pub ident: IdentSpan,
    pub colon: Pct,
    pub typ: IdentSpan,
}

impl FunParam {
    pub fn new(ident: IdentSpan, colon: Pct, typ: IdentSpan) -> Self {
        Self { ident, colon, typ }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ReturnType {
    pub arrow: Pct,
    pub typ: IdentSpan,
}

impl ReturnType {
    pub fn new(arrow: Pct, typ: IdentSpan) -> Self {
        Self { arrow, typ }
    }
}

impl FunDef {
    pub fn new(
        fn_kw: Kw,
        ident: IdentSpan,
        params: FunParams,
        return_typ: Option<ReturnType>,
        block: Block,
    ) -> Self {
        Self {
            fn_kw,
            ident,
            params,
            return_type: return_typ,
            block,
        }
    }

    pub fn span(&self) -> Span {
        Span::across(self.fn_kw.span, self.block.r_par.span)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunCall {
    pub ident: IdentSpan,
    pub args: FunArgs,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunArgs {
    pub l_par: Par,
    pub r_par: Par,
    pub items: Vec<Cst>,
}

impl FunArgs {
    pub fn new(l_par: Par, r_par: Par, items: Vec<Cst>) -> Self {
        Self {
            l_par,
            r_par,
            items,
        }
    }
}

impl FunCall {
    pub const fn new(ident: IdentSpan, args: FunArgs) -> Self {
        Self { ident, args }
    }

    pub const fn span(&self) -> Span {
        Span::across(self.ident.span, self.args.r_par.span)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub l_par: Par,
    pub r_par: Par,
    pub csts: Vec<Cst>,
}

impl Block {
    pub fn new(l_par: Par, r_par: Par, csts: Vec<Cst>) -> Self {
        Self { l_par, r_par, csts }
    }

    pub fn span(&self) -> Span {
        Span::across(self.l_par.span, self.r_par.span)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDef {
    pub kw: Kw,
    pub ident: IdentSpan,
    pub type_hint: Option<(Pct, IdentSpan)>,
    pub value: (Op, Box<Cst>),
}

impl VarDef {
    pub fn new(
        kw: Kw,
        ident: IdentSpan,
        type_hint: Option<(Pct, IdentSpan)>,
        value: (Op, Box<Cst>),
    ) -> Self {
        Self {
            kw,
            ident,
            type_hint,
            value,
        }
    }

    pub fn span(&self) -> Span {
        Span::across(self.kw.span, self.value.1.span())
    }
}

impl Cst {
    pub fn span(&self) -> Span {
        match self {
            Self::Empty(s) => *s,
            Self::Error(s) => *s,
            Self::Val(v) => v.span,
            Self::Ident(i) => i.span,
            Self::Par(l, _, r) => Span::across(l.span, r.span),
            Self::Block(g) => g.span(),
            Self::IfExpr(i) => i.span(),
            Self::WhileLoop(w) => w.span(),
            Self::ForLoop(f) => f.span(),
            Self::VarDef(v) => v.span(),
            Self::FunDef(f) => f.span(),
            Self::FunCall(f) => f.span(),
            Self::Prefix(p, a) => Span::across(p.span, a.span()),
            Self::Postfix(a, p) => Span::across(a.span(), p.span),
            Self::Infix(a, _, b) => Span::across(a.span(), b.span()),
            Self::InfixAssign(a, _, b) => Span::across(a.span, b.span()),
            Self::Assign(a, _, b) => Span::across(a.span, b.span()),
        }
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, Self::Empty(_))
    }
}
