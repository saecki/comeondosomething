use crate::{IdentSpan, Infix, Kw, Op, Par, Pct, Postfix, Prefix, Span, ValSpan};

#[derive(Clone, Debug)]
pub enum Cst {
    Empty(Span),
    Error(Span),
    Val(ValSpan),
    Ident(IdentSpan),
    Par(Par, Box<Cst>, Par),
    Block(Block),
    Infix(Box<Cst>, Infix, Box<Cst>),
    AssignInfix(IdentSpan, Infix, Box<Cst>),
    Prefix(Prefix, Box<Cst>),
    Postfix(Box<Cst>, Postfix),
    IfExpr(IfExpr),
    WhileLoop(WhileLoop),
    ForLoop(ForLoop),
    VarDef(VarDef),
    VarAssign(VarAssign),
    FunDef(FunDef),
    FunCall(FunCall),
}

#[derive(Clone, Debug)]
pub struct IfExpr {
    pub if_block: (Kw, Box<Cst>, Block),
    pub else_if_blocks: Vec<(Kw, Kw, Cst, Block)>,
    pub else_block: Option<(Kw, Block)>,
}

impl IfExpr {
    pub fn new(
        if_block: (Kw, Box<Cst>, Block),
        else_if_blocks: Vec<(Kw, Kw, Cst, Block)>,
        else_block: Option<(Kw, Block)>,
    ) -> Self {
        Self {
            if_block,
            else_if_blocks,
            else_block,
        }
    }

    pub fn span(&self) -> Span {
        if let Some((_, b)) = &self.else_block {
            Span::across(self.if_block.0.span, b.r_par.span)
        } else if let Some((_, _, _, b)) = self.else_if_blocks.last() {
            Span::across(self.if_block.0.span, b.r_par.span)
        } else {
            Span::across(self.if_block.0.span, self.if_block.2.r_par.span)
        }
    }
}

#[derive(Clone, Debug)]
pub struct WhileLoop {
    pub while_kw: Kw,
    pub cond: Box<Cst>,
    pub block: Block,
}

impl WhileLoop {
    pub fn new(while_kw: Kw, cond: Box<Cst>, block: Block) -> Self {
        Self {
            while_kw,
            cond,
            block,
        }
    }

    pub fn span(&self) -> Span {
        Span::across(self.while_kw.span, self.block.r_par.span)
    }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct VarDef {
    pub var_kw: Kw,
    pub ident: IdentSpan,
    pub type_hint: Option<(Pct, IdentSpan)>,
    pub assign: Op,
    pub value: Box<Cst>,
}

impl VarDef {
    pub fn new(
        var_kw: Kw,
        ident: IdentSpan,
        type_hint: Option<(Pct, IdentSpan)>,
        assign: Op,
        value: Box<Cst>,
    ) -> Self {
        Self {
            var_kw,
            ident,
            type_hint,
            assign,
            value,
        }
    }

    pub fn span(&self) -> Span {
        Span::across(self.var_kw.span, self.value.span())
    }
}

#[derive(Clone, Debug)]
pub struct VarAssign {
    pub ident: IdentSpan,
    pub assign: Infix,
    pub value: Box<Cst>,
}

impl VarAssign {
    pub fn span(&self) -> Span {
        Span::across(self.ident.span, self.value.span())
    }
}

#[derive(Clone, Debug)]
pub struct FunDef {
    pub fn_kw: Kw,
    pub ident: IdentSpan,
    pub args: (Par, Vec<(IdentSpan, Pct, IdentSpan)>, Par),
    pub return_typ: Option<(Pct, IdentSpan)>,
    pub block: Block,
}

impl FunDef {
    pub fn new(
        fn_kw: Kw,
        ident: IdentSpan,
        args: (Par, Vec<(IdentSpan, Pct, IdentSpan)>, Par),
        return_typ: Option<(Pct, IdentSpan)>,
        block: Block,
    ) -> Self {
        Self {
            fn_kw,
            ident,
            args,
            return_typ,
            block,
        }
    }

    pub fn span(&self) -> Span {
        Span::across(self.fn_kw.span, self.block.r_par.span)
    }
}

#[derive(Clone, Debug)]
pub struct FunCall {
    pub ident: IdentSpan,
    pub args: (Par, Vec<Cst>, Par),
}

impl FunCall {
    pub const fn new(ident: IdentSpan, args: (Par, Vec<Cst>, Par)) -> Self {
        Self { ident, args }
    }

    pub const fn span(&self) -> Span {
        Span::across(self.ident.span, self.args.2.span)
    }
}

#[derive(Clone, Debug)]
pub struct Block {
    pub l_par: Par,
    pub r_par: Par,
    pub block: Vec<Cst>,
}

impl Block {
    pub fn new(l_par: Par, r_par: Par, block: Vec<Cst>) -> Self {
        Self {
            l_par,
            r_par,
            block,
        }
    }

    pub fn span(&self) -> Span {
        Span::across(self.l_par.span, self.r_par.span)
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
            Self::Infix(a, _, b) => Span::across(a.span(), b.span()),
            Self::AssignInfix(a, _, b) => Span::across(a.span, b.span()),
            Self::Prefix(p, a) => Span::across(p.span, a.span()),
            Self::Postfix(a, p) => Span::across(a.span(), p.span),
            Self::IfExpr(i) => i.span(),
            Self::WhileLoop(w) => w.span(),
            Self::ForLoop(f) => f.span(),
            Self::VarDef(v) => v.span(),
            Self::VarAssign(v) => v.span(),
            Self::FunDef(f) => f.span(),
            Self::FunCall(f) => f.span(),
        }
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, Self::Empty(_))
    }
}
