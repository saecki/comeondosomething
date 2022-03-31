use std::collections::HashMap;

use crate::{Block, BuiltinConst, CRange, Context, Ident, IdentRange, Val, ValRange};

impl Context {
    fn scopes(&self) -> impl Iterator<Item = &Scope> {
        self.scopes.iter().rev()
    }

    fn scopes_mut(&mut self) -> impl Iterator<Item = &mut Scope> {
        self.scopes.iter_mut().rev()
    }

    fn var(&self, id: Ident) -> Option<&Var> {
        for s in self.scopes() {
            if let Some(v) = s.var(id) {
                return Some(v);
            }
        }
        None
    }

    fn var_mut(&mut self, id: Ident) -> Option<&mut Var> {
        for s in self.scopes_mut() {
            if let Some(v) = s.var_mut(id) {
                return Some(v);
            }
        }
        None
    }

    pub fn resolve_var(&self, id: &IdentRange) -> crate::Result<ValRange> {
        let name = self.ident_name(id.ident);
        if let Some(b) = BuiltinConst::from(name) {
            return Ok(ValRange::new(b.val(), id.range));
        }

        let var = match self.var(id.ident) {
            Some(v) => v,
            None => return Err(crate::Error::UndefinedVar(name.to_owned(), id.range)),
        };

        match &var.value {
            Some(v) => Ok(ValRange::new(v.clone(), id.range)),
            None => Err(crate::Error::UninitializedVar(
                name.to_owned(),
                var.ident.range,
                id.range,
            )),
        }
    }

    pub fn def_var(&mut self, id: IdentRange, val: Option<Val>, mutable: bool) {
        let s = self.scopes.last_mut().expect("At least the global scope");
        let var = Var::new(id, val, mutable);
        s.vars.insert(id.ident, var);
    }

    pub fn set_var(
        &mut self,
        id: &IdentRange,
        val: Option<Val>,
        val_r: CRange,
    ) -> crate::Result<()> {
        match self.var_mut(id.ident) {
            Some(v) => {
                if !v.mutable && v.value.is_some() {
                    let name = self.ident_name(id.ident);
                    return Err(crate::Error::ImmutableAssign(name.into(), id.range, val_r));
                }
                v.value = val;
                Ok(())
            }
            None => {
                let name = self.ident_name(id.ident);
                Err(crate::Error::UndefinedVar(name.to_owned(), id.range))
            }
        }
    }

    fn fun(&self, id: Ident) -> Option<&Fun> {
        for s in self.scopes() {
            if let Some(f) = s.fun(id) {
                return Some(f);
            }
        }
        None
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

    pub fn def_fun(
        &mut self,
        id: IdentRange,
        params: Vec<IdentRange>,
        block: Block,
    ) -> crate::Result<()> {
        let s = self.scopes.last_mut().expect("At least the global scope");
        if let Some(f) = s.fun(id.ident) {
            let i_r = f.ident.range;
            let name = self.ident_name(id.ident);
            return Err(crate::Error::RedefinedFun(name.to_owned(), i_r, id.range));
        }

        let fun = Fun::new(id, params, block);
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

    pub fn def_var(&mut self, id: IdentRange, val: Option<Val>, mutable: bool) {
        let var = Var::new(id, val, mutable);
        self.vars.insert(id.ident, var);
    }

    pub fn fun(&self, id: Ident) -> Option<&Fun> {
        self.funs.get(&id)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Var {
    pub ident: IdentRange,
    pub value: Option<Val>,
    pub mutable: bool,
}

impl Var {
    pub const fn new(ident: IdentRange, value: Option<Val>, mutable: bool) -> Self {
        Self {
            ident,
            value,
            mutable,
        }
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
