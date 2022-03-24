use std::collections::HashMap;
use std::fmt::Display;
use std::ops::Deref;

use crate::{Context, Expr, ExprT, Ident, Range, Val};

impl Val {
    pub fn to_int(&self) -> Option<i128> {
        match self {
            Self::Int(i) => Some(*i),
            Self::Float(f) => {
                let i = *f as i128;
                #[allow(clippy::float_cmp)]
                if i as f64 == *f {
                    Some(i)
                } else {
                    None
                }
            }
            Self::Bool(_) | Self::Str(_) => None,
        }
    }

    pub fn to_f64(&self) -> Option<f64> {
        match self {
            Self::Int(i) => Some(*i as f64),
            Self::Float(f) => Some(*f),
            Self::Bool(_) | Self::Str(_) => None,
        }
    }
}

impl Context {
    pub fn to_val<'a>(&'a self, expr: &'a Expr) -> crate::Result<&'a Val> {
        match &expr.typ {
            ExprT::Val(p) => Ok(p),
            ExprT::Var(id) => match self.var_val(*id) {
                Some(d) => Ok(d),
                None => {
                    let name = self.ident_name(*id);
                    Err(crate::Error::UndefinedVar(name.to_owned(), expr.range))
                }
            },
        }
    }

    pub fn var(&self, id: Ident) -> Option<&Var> {
        self.scope.var(id)
    }

    pub fn var_mut(&mut self, id: Ident) -> Option<&mut Var> {
        self.scope.var_mut(id)
    }

    pub fn var_val(&self, id: Ident) -> Option<&Val> {
        self.scope.val(id)
    }

    pub fn var_val_mut(&mut self, id: Ident) -> Option<&mut Val> {
        self.scope.val_mut(id)
    }

    pub fn set_var(&mut self, id: Ident, val: Option<Val>) {
        // TODO const vars
        self.scope.set_var(id, val);
    }
}

#[derive(Debug, Default)]
pub struct Scope {
    pub vars: HashMap<Ident, Var>,
}

impl Scope {
    pub fn clear(&mut self) {
        self.vars.clear();
    }

    pub fn var(&self, id: Ident) -> Option<&Var> {
        self.vars.get(&id)
    }

    pub fn var_mut(&mut self, id: Ident) -> Option<&mut Var> {
        self.vars.get_mut(&id)
    }

    pub fn val(&self, id: Ident) -> Option<&Val> {
        self.var(id).and_then(|v| v.value.as_ref())
    }

    pub fn val_mut(&mut self, id: Ident) -> Option<&mut Val> {
        self.var_mut(id).and_then(|v| v.value.as_mut())
    }

    pub fn set_var(&mut self, id: Ident, val: Option<Val>) {
        match self.vars.get_mut(&id) {
            Some(v) => v.value = val,
            None => {
                self.vars.insert(id, Var::new(val));
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Var {
    pub value: Option<Val>,
}

impl Var {
    pub fn new(value: Option<Val>) -> Self {
        Self { value }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Return {
    Val(ValRange),
    Unit(Range),
}

impl Display for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Val(v) => write!(f, "{v}"),
            Self::Unit(_) => write!(f, "()"),
        }
    }
}

impl Return {
    pub fn range(&self) -> Range {
        match self {
            Self::Val(v) => v.range,
            Self::Unit(r) => *r,
        }
    }

    pub fn to_val(&self) -> crate::Result<&ValRange> {
        match self {
            Self::Val(v) => Ok(v),
            Self::Unit(r) => Err(crate::Error::ExpectedValue(*r)),
        }
    }

    pub fn into_val(self) -> crate::Result<ValRange> {
        match self {
            Self::Val(v) => Ok(v),
            Self::Unit(r) => Err(crate::Error::ExpectedValue(r)),
        }
    }

    pub fn to_f64(&self) -> crate::Result<f64> {
        self.to_val()?.to_f64()
    }

    pub fn to_bool(&self) -> crate::Result<bool> {
        self.to_val()?.to_bool()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ValRange {
    pub val: Val,
    pub range: Range,
}

impl Deref for ValRange {
    type Target = Val;

    fn deref(&self) -> &Self::Target {
        &self.val
    }
}

impl std::ops::DerefMut for ValRange {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.val
    }
}

impl Display for ValRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.val)
    }
}

impl ValRange {
    pub const fn new(val: Val, range: Range) -> Self {
        Self { val, range }
    }

    pub fn to_f64(&self) -> crate::Result<f64> {
        match self.val {
            Val::Int(i) => Ok(i as f64),
            Val::Float(f) => Ok(f),
            Val::Bool(_) | Val::Str(_) => Err(crate::Error::ExpectedNumber(self.clone())),
        }
    }

    pub fn to_bool(&self) -> crate::Result<bool> {
        match self.val {
            Val::Bool(b) => Ok(b),
            Val::Int(_) | Val::Float(_) | Val::Str(_) => {
                Err(crate::Error::ExpectedBool(self.clone()))
            }
        }
    }
}
