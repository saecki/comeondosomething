use std::collections::HashMap;

use crate::{CRange, Context, Ident, Val, ValRange};

impl Context {
    pub fn resolve_var(&self, id: &IdentRange) -> crate::Result<ValRange> {
        match self.var_val(id.ident) {
            Some(v) => Ok(ValRange::new(v.clone(), id.range)),
            None => {
                let name = self.ident_name(id.ident);
                Err(crate::Error::UndefinedVar(name.to_owned(), id.range))
            }
        }
    }

    pub fn var(&self, id: Ident) -> Option<&Var> {
        for s in self.scopes.iter().rev() {
            if let Some(v) = s.var(id) {
                return Some(v);
            }
        }
        None
    }

    pub fn var_mut(&mut self, id: Ident) -> Option<&mut Var> {
        for s in self.scopes.iter_mut().rev() {
            if let Some(v) = s.var_mut(id) {
                return Some(v);
            }
        }
        None
    }

    pub fn var_val(&self, id: Ident) -> Option<&Val> {
        for s in self.scopes.iter().rev() {
            if let Some(v) = s.val(id) {
                return Some(v);
            }
        }
        None
    }

    pub fn var_val_mut(&mut self, id: Ident) -> Option<&mut Val> {
        for s in self.scopes.iter_mut().rev() {
            if let Some(v) = s.val_mut(id) {
                return Some(v);
            }
        }
        None
    }

    pub fn set_var(&mut self, id: Ident, val: Option<Val>) {
        match self.var_mut(id) {
            Some(v) => v.value = val,
            None => {
                let s = self
                    .scopes
                    .last_mut()
                    .expect("Expected at least the global scope");
                s.set_var(id, val);
            }
        }
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
pub struct IdentRange {
    pub ident: Ident,
    pub range: CRange,
}

impl IdentRange {
    pub const fn new(ident: Ident, range: CRange) -> Self {
        Self { ident, range }
    }
}
