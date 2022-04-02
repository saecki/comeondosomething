use std::collections::HashMap;
use std::rc::Rc;

use crate::{Block, BuiltinConst, Context, Ident, IdentSpan, Span, Val, ValSpan};

impl Context {
    pub fn resolve_var(&self, id: &IdentSpan) -> crate::Result<ValSpan> {
        let name = self.idents.name(id.ident);
        if let Some(b) = BuiltinConst::from(name) {
            return Ok(ValSpan::new(b.val(), id.span));
        }

        let var = match self.scopes.var(id.ident) {
            Some(v) => v,
            None => return Err(crate::Error::UndefinedVar(name.to_owned(), id.span)),
        };

        match &var.value {
            Some(v) => Ok(ValSpan::new(v.clone(), id.span)),
            None => Err(crate::Error::UninitializedVar(
                name.to_owned(),
                var.ident.span,
                id.span,
            )),
        }
    }

    pub fn def_var(&mut self, var: Var) {
        let s = self.scopes.current_mut();
        s.def_var(var);
    }

    pub fn set_var(&mut self, id: &IdentSpan, val: Option<Val>, val_s: Span) -> crate::Result<()> {
        match self.scopes.var_mut(id.ident) {
            Some(v) => {
                if !v.mutable && v.value.is_some() {
                    let name = self.idents.name(id.ident);
                    return Err(crate::Error::ImmutableAssign(name.into(), id.span, val_s));
                }
                v.value = val;
                Ok(())
            }
            None => {
                let name = self.idents.name(id.ident);
                Err(crate::Error::UndefinedVar(name.to_owned(), id.span))
            }
        }
    }

    pub fn resolve_fun(&self, id: &IdentSpan) -> crate::Result<Rc<Fun>> {
        match self.scopes.fun(id.ident) {
            Some(f) => Ok(f),
            None => {
                let name = self.idents.name(id.ident);
                Err(crate::Error::UndefinedFun(name.to_owned(), id.span))
            }
        }
    }

    pub fn def_fun(&mut self, fun: Rc<Fun>) -> crate::Result<()> {
        let s = self.scopes.current_mut();
        let id = fun.ident;
        if let Some(f) = s.fun(id.ident) {
            let i_s = f.ident.span;
            let name = self.idents.name(id.ident);
            return Err(crate::Error::RedefinedFun(name.to_owned(), i_s, id.span));
        }

        s.funs.insert(id.ident, fun);

        Ok(())
    }
}

#[derive(Debug)]
pub struct Scopes {
    scopes: Vec<Scope>,
    len: usize,
}

impl Default for Scopes {
    fn default() -> Self {
        Self {
            scopes: vec![Scope::default()],
            len: 1,
        }
    }
}

impl Scopes {
    pub fn current(&self) -> &Scope {
        self.scopes
            .get(self.len - 1)
            .expect("Expected at least the global scope")
    }

    pub fn current_mut(&mut self) -> &mut Scope {
        self.scopes
            .get_mut(self.len - 1)
            .expect("Expected at least the global scope")
    }

    pub fn iter(&self) -> impl Iterator<Item = &Scope> {
        self.scopes[0..(self.len)].iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Scope> {
        self.scopes[0..(self.len)].iter_mut()
    }

    pub fn rev(&self) -> impl Iterator<Item = &Scope> {
        self.scopes[0..(self.len)].iter().rev()
    }

    pub fn rev_mut(&mut self) -> impl Iterator<Item = &mut Scope> {
        self.scopes[0..(self.len)].iter_mut().rev()
    }

    pub fn push(&mut self) {
        if self.len >= self.scopes.len() {
            self.scopes.push(Scope::default())
        }
        self.len += 1;
    }

    pub fn pop(&mut self) {
        self.current_mut().clear();
        self.len -= 1;
    }

    pub fn clear(&mut self) {
        for s in self.iter_mut() {
            s.clear();
        }
        self.len = 0;
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
        self.funs.get(&id).map(Rc::clone)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Var {
    pub ident: IdentSpan,
    pub value: Option<Val>,
    pub mutable: bool,
}

impl Var {
    pub const fn new(ident: IdentSpan, value: Option<Val>, mutable: bool) -> Self {
        Self {
            ident,
            value,
            mutable,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Fun {
    pub ident: IdentSpan,
    pub params: Vec<Param>,
    pub return_type: Option<IdentSpan>,
    pub block: Block,
}

impl Fun {
    pub const fn new(
        ident: IdentSpan,
        params: Vec<Param>,
        return_type: Option<IdentSpan>,
        block: Block,
    ) -> Self {
        Self {
            ident,
            params,
            return_type,
            block,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Param {
    pub ident: IdentSpan,
    pub typ: IdentSpan,
}

impl Param {
    pub const fn new(ident: IdentSpan, typ: IdentSpan) -> Self {
        Self { ident, typ }
    }
}
