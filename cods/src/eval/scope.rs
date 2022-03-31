use std::collections::HashMap;
use std::rc::Rc;

use crate::{Block, BuiltinConst, CRange, Context, Ident, IdentRange, Val, ValRange};

impl Context {
    pub fn resolve_var(&self, id: &IdentRange) -> crate::Result<ValRange> {
        let name = self.idents.name(id.ident);
        if let Some(b) = BuiltinConst::from(name) {
            return Ok(ValRange::new(b.val(), id.range));
        }

        let var = match self.scopes.var(id.ident) {
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

    pub fn def_var(&mut self, var: Var) {
        let s = self.scopes.current_mut();
        s.def_var(var);
    }

    pub fn set_var(
        &mut self,
        id: &IdentRange,
        val: Option<Val>,
        val_r: CRange,
    ) -> crate::Result<()> {
        match self.scopes.var_mut(id.ident) {
            Some(v) => {
                if !v.mutable && v.value.is_some() {
                    let name = self.idents.name(id.ident);
                    return Err(crate::Error::ImmutableAssign(name.into(), id.range, val_r));
                }
                v.value = val;
                Ok(())
            }
            None => {
                let name = self.idents.name(id.ident);
                Err(crate::Error::UndefinedVar(name.to_owned(), id.range))
            }
        }
    }

    pub fn resolve_fun(&self, id: &IdentRange) -> crate::Result<Rc<Fun>> {
        match self.scopes.fun(id.ident) {
            Some(f) => Ok(f),
            None => {
                let name = self.idents.name(id.ident);
                Err(crate::Error::UndefinedFun(name.to_owned(), id.range))
            }
        }
    }

    pub fn def_fun(&mut self, fun: Fun) -> crate::Result<()> {
        let s = self.scopes.current_mut();
        let id = fun.ident;
        if let Some(f) = s.fun(id.ident) {
            let i_r = f.ident.range;
            let name = self.idents.name(id.ident);
            return Err(crate::Error::RedefinedFun(name.to_owned(), i_r, id.range));
        }

        s.funs.insert(id.ident, Rc::new(fun));

        Ok(())
    }
}

#[derive(Debug)]
pub struct Scopes(Vec<Scope>);

impl Default for Scopes {
    fn default() -> Self {
        Self(vec![Scope::default()])
    }
}

impl Scopes {
    pub fn current(&self) -> &Scope {
        self.0.last().expect("Expected at least the global scope")
    }

    pub fn current_mut(&mut self) -> &mut Scope {
        self.0
            .last_mut()
            .expect("Expected at least the global scope")
    }

    pub fn iter(&self) -> impl Iterator<Item = &Scope> {
        self.0.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Scope> {
        self.0.iter_mut()
    }

    pub fn rev(&self) -> impl Iterator<Item = &Scope> {
        self.0.iter().rev()
    }

    pub fn rev_mut(&mut self) -> impl Iterator<Item = &mut Scope> {
        self.0.iter_mut().rev()
    }

    pub fn push(&mut self, value: Scope) {
        self.0.push(value)
    }

    pub fn pop(&mut self) -> Option<Scope> {
        self.0.pop()
    }

    fn var(&self, id: Ident) -> Option<&Var> {
        for s in self.rev() {
            if let Some(v) = s.var(id) {
                return Some(v);
            }
        }
        None
    }

    fn var_mut(&mut self, id: Ident) -> Option<&mut Var> {
        for s in self.rev_mut() {
            if let Some(v) = s.var_mut(id) {
                return Some(v);
            }
        }
        None
    }

    fn fun(&self, id: Ident) -> Option<Rc<Fun>> {
        for s in self.rev() {
            if let Some(f) = s.fun(id) {
                return Some(f);
            }
        }
        None
    }
}

#[derive(Debug, Default)]
pub struct Scope {
    vars: HashMap<Ident, Var>,
    funs: HashMap<Ident, Rc<Fun>>,
}

impl Scope {
    pub fn clear(&mut self) {
        self.vars.clear();
        self.funs.clear();
    }

    pub fn vars(&self) -> impl Iterator<Item = &Var> {
        self.vars.values()
    }

    pub fn funs(&self) -> impl Iterator<Item = &Rc<Fun>> {
        self.funs.values()
    }

    pub fn var(&self, id: Ident) -> Option<&Var> {
        self.vars.get(&id)
    }

    pub fn var_mut(&mut self, id: Ident) -> Option<&mut Var> {
        self.vars.get_mut(&id)
    }

    pub fn def_var(&mut self, var: Var) {
        self.vars.insert(var.ident.ident, var);
    }

    pub fn fun(&self, id: Ident) -> Option<Rc<Fun>> {
        self.funs.get(&id).map(|f| Rc::clone(f))
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
