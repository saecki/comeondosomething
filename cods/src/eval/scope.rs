use std::collections::HashMap;

use crate::{Block, Context, Ident, IdentRange, Val, ValRange};

impl Context {
    pub fn scopes(&self) -> impl Iterator<Item = &Scope> {
        self.scopes.iter().rev()
    }

    pub fn scopes_mut(&mut self) -> impl Iterator<Item = &mut Scope> {
        self.scopes.iter_mut().rev()
    }

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
        for s in self.scopes() {
            if let Some(v) = s.var(id) {
                return Some(v);
            }
        }
        None
    }

    pub fn var_mut(&mut self, id: Ident) -> Option<&mut Var> {
        for s in self.scopes_mut() {
            if let Some(v) = s.var_mut(id) {
                return Some(v);
            }
        }
        None
    }

    pub fn var_val(&self, id: Ident) -> Option<&Val> {
        for s in self.scopes() {
            if let Some(v) = s.val(id) {
                return Some(v);
            }
        }
        None
    }

    pub fn var_val_mut(&mut self, id: Ident) -> Option<&mut Val> {
        for s in self.scopes_mut() {
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
                let s = self.scopes.last_mut().expect("At least the global scope");
                s.set_var(id, val);
            }
        }
    }

    pub fn resolve_fun(&self, id: &IdentRange) -> crate::Result<&Fun> {
        match self.fun(id.ident) {
            Some(f) => Ok(f),
            None => {
                let name = self.ident_name(id.ident);
                Err(crate::Error::UndefinedFun(name.to_owned(), id.range))
            }
        }
    }

    pub fn fun(&self, id: Ident) -> Option<&Fun> {
        for s in self.scopes() {
            if let Some(f) = s.fun(id) {
                return Some(f);
            }
        }
        None
    }

    pub fn def_fun(
        &mut self,
        id: &IdentRange,
        params: &[IdentRange],
        block: &Block,
    ) -> crate::Result<()> {
        let s = self.scopes.last_mut().expect("At least the global scope");
        if let Some(f) = s.funs.get(&id.ident) {
            let i_r = f.ident.range;
            let name = self.ident_name(id.ident);
            return Err(crate::Error::RedefinedFun(name.into(), i_r, id.range));
        }

        let fun = Fun::new(*id, params.to_owned(), block.to_owned());
        s.funs.insert(id.ident, fun);

        Ok(())
    }
}

#[derive(Debug, Default)]
pub struct Scope {
    pub vars: HashMap<Ident, Var>,
    pub funs: HashMap<Ident, Fun>,
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

    pub fn fun(&self, id: Ident) -> Option<&Fun> {
        self.funs.get(&id)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Var {
    pub value: Option<Val>,
}

impl Var {
    pub const fn new(value: Option<Val>) -> Self {
        Self { value }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Fun {
    pub ident: IdentRange,
    pub params: Vec<IdentRange>,
    pub block: Block,
}

impl Fun {
    pub const fn new(ident: IdentRange, params: Vec<IdentRange>, block: Block) -> Self {
        Self {
            ident,
            params,
            block,
        }
    }
}
