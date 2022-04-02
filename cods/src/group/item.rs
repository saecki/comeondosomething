use crate::{Span, Expr, Kw, Op, ParKind, Pct, Token};

#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    Group(Group),
    Expr(Expr),

    Op(Op),
    Pct(Pct),
    Kw(Kw),
}

impl Item {
    pub fn try_from(token: Token) -> Option<Self> {
        match token {
            Token::Expr(n) => Some(Self::Expr(n)),
            Token::Op(o) => Some(Self::Op(o)),
            Token::Par(_) => None,
            Token::Pct(s) => Some(Self::Pct(s)),
            Token::Kw(k) => Some(Self::Kw(k)),
        }
    }

    pub fn into_group(self) -> Option<Group> {
        match self {
            Self::Group(g) => Some(g),
            _ => None,
        }
    }

    pub fn into_expr(self) -> Option<Expr> {
        match self {
            Self::Expr(e) => Some(e),
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
            Self::Group(g) => g.span,
            Self::Expr(n) => n.span,
            Self::Op(o) => o.span,
            Self::Pct(p) => p.span,
            Self::Kw(k) => k.span,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Group {
    pub items: Vec<Item>,
    pub span: Span,
    pub par_kind: ParKind,
}

impl Group {
    pub fn new(items: Vec<Item>, span: Span, par: ParKind) -> Self {
        Self {
            items,
            span,
            par_kind: par,
        }
    }
}
