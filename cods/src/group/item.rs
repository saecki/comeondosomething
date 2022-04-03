use crate::{Ident, IdentSpan, Kw, KwT, Op, OpT, Par, ParKind, Pct, PctT, Span, Val, ValSpan};

#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    Group(Group),
    Val(ValSpan),
    Ident(IdentSpan),

    Op(Op),
    Pct(Pct),
    Kw(Kw),
}

impl Item {
    pub fn val(val: Val, span: Span) -> Self {
        Self::Val(ValSpan::new(val, span))
    }

    pub fn ident(ident: Ident, span: Span) -> Self {
        Self::Ident(IdentSpan::new(ident, span))
    }

    pub fn op(typ: OpT, span: Span) -> Self {
        Self::Op(Op::new(typ, span))
    }

    pub fn pct(typ: PctT, span: Span) -> Self {
        Self::Pct(Pct::new(typ, span))
    }

    pub fn kw(typ: KwT, span: Span) -> Self {
        Self::Kw(Kw::new(typ, span))
    }

    pub fn into_group(self) -> Option<Group> {
        match self {
            Self::Group(g) => Some(g),
            _ => None,
        }
    }

    pub fn into_val(self) -> Option<ValSpan> {
        match self {
            Self::Val(v) => Some(v),
            _ => None,
        }
    }

    pub fn into_ident(self) -> Option<IdentSpan> {
        match self {
            Self::Ident(i) => Some(i),
            _ => None,
        }
    }

    pub fn as_op(&self) -> Option<Op> {
        match self {
            Self::Op(o) => Some(*o),
            _ => None,
        }
    }

    pub fn as_pct(&self) -> Option<Pct> {
        match self {
            Self::Pct(c) => Some(*c),
            _ => None,
        }
    }

    pub fn is_group(&self) -> bool {
        matches!(self, Self::Group(_))
    }

    pub fn is_op(&self) -> bool {
        matches!(self, Self::Op(_))
    }

    pub fn is_pct(&self) -> bool {
        matches!(self, Self::Pct(_))
    }

    pub fn is_comma(&self) -> bool {
        match self {
            Self::Pct(p) => p.is_comma(),
            _ => false,
        }
    }

    pub fn is_semi(&self) -> bool {
        match self {
            Self::Pct(p) => p.is_semi(),
            _ => false,
        }
    }

    pub fn is_newln(&self) -> bool {
        match self {
            Self::Pct(p) => p.is_newln(),
            _ => false,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Group(g) => g.span(),
            Self::Val(v) => v.span,
            Self::Ident(i) => i.span,
            Self::Op(o) => o.span,
            Self::Pct(p) => p.span,
            Self::Kw(k) => k.span,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Group {
    pub l_par: Par,
    pub r_par: Par,
    pub items: Vec<Item>,
}

impl Group {
    pub const fn new(l_par: Par, r_par: Par, items: Vec<Item>) -> Self {
        Self {
            l_par,
            r_par,
            items,
        }
    }

    pub fn span(&self) -> Span {
        Span::across(self.l_par.span, self.r_par.span)
    }

    pub fn inner_span(&self) -> Span {
        Span::between(self.l_par.span, self.r_par.span)
    }

    pub fn par_kind(&self) -> ParKind {
        self.l_par.kind()
    }
}
