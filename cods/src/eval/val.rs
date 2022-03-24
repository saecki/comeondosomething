use std::collections::HashMap;

use crate::{Context, Expr, ExprT, Ident, Val};

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
