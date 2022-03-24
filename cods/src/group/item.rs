use crate::{Expr, Fun, Kw, Op, ParKind, Range, Sep, Token};

#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    Group(Group),
    Expr(Expr),
    Fun(Fun),

    Op(Op),
    Sep(Sep),
    Kw(Kw),
}

impl Item {
    pub fn try_from(token: Token) -> Option<Self> {
        match token {
            Token::Expr(n) => Some(Self::Expr(n)),
            Token::Op(o) => Some(Self::Op(o)),
            Token::Fun(c) => Some(Self::Fun(c)),
            Token::Par(_) => None,
            Token::Sep(s) => Some(Self::Sep(s)),
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

    pub fn as_fun(&self) -> Option<Fun> {
        match self {
            Self::Fun(c) => Some(*c),
            _ => None,
        }
    }

    pub fn as_op(&self) -> Option<Op> {
        match self {
            Self::Op(o) => Some(*o),
            _ => None,
        }
    }

    pub fn as_sep(&self) -> Option<Sep> {
        match self {
            Self::Sep(c) => Some(*c),
            _ => None,
        }
    }

    pub fn is_group(&self) -> bool {
        matches!(self, Self::Group(_))
    }

    pub fn is_op(&self) -> bool {
        matches!(self, Self::Op(_))
    }

    pub fn is_sep(&self) -> bool {
        matches!(self, Self::Sep(_))
    }

    pub fn is_comma(&self) -> bool {
        match self {
            Self::Sep(s) => s.is_comma(),
            _ => false,
        }
    }

    pub fn is_semi(&self) -> bool {
        match self {
            Self::Sep(s) => s.is_semi(),
            _ => false,
        }
    }

    pub fn is_newln(&self) -> bool {
        match self {
            Self::Sep(s) => s.is_newln(),
            _ => false,
        }
    }

    pub fn range(&self) -> Range {
        match self {
            Self::Group(g) => g.range,
            Self::Expr(n) => n.range,
            Self::Op(o) => o.range,
            Self::Fun(c) => c.range,
            Self::Sep(s) => s.range,
            Self::Kw(k) => k.range,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Group {
    pub items: Vec<Item>,
    pub range: Range,
    pub par_kind: ParKind,
}

impl Group {
    pub fn new(items: Vec<Item>, range: Range, par: ParKind) -> Self {
        Self {
            items,
            range,
            par_kind: par,
        }
    }
}
