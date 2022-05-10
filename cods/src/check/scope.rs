use std::collections::HashMap;
use std::rc::Rc;

use crate::{
    ast, Ast, BuiltinConst, BuiltinFun, Context, DataType, Ident, IdentSpan, Span, VarRef,
};

pub enum ResolvedFun {
    Fun(Rc<Fun>),
    Builtin(BuiltinFun),
}

pub enum ResolvedVar<'a> {
    Var(&'a Var),
    Const(BuiltinConst),
}

impl Context {
    pub fn resolve_fun(&self, scopes: &Scopes, id: &IdentSpan) -> crate::Result<ResolvedFun> {
        let name = self.idents.name(id.ident);
        if let Ok(b) = name.parse::<BuiltinFun>() {
            return Ok(ResolvedFun::Builtin(b));
        }

        match scopes.fun(id.ident) {
            Some(f) => Ok(ResolvedFun::Fun(f)),
            None => {
                let name = self.idents.name(id.ident);
                Err(crate::Error::UndefinedFun(name.to_owned(), id.span))
            }
        }
    }

    pub fn def_fun(&mut self, scopes: &mut Scopes, fun: Fun) -> crate::Result<()> {
        let s = scopes.current_mut();
        let id = fun.ident;
        if let Some(f) = s.fun(id.ident) {
            let i_s = f.ident.span;
            let name = self.idents.name(id.ident);
            return Err(crate::Error::RedefinedFun(name.to_owned(), i_s, id.span));
        }

        s.funs.insert(id.ident, Rc::new(fun));

        Ok(())
    }

    /// Resolve the var belonging to the identifier.
    pub fn resolve_var<'a>(
        &self,
        scopes: &'a Scopes,
        id: &IdentSpan,
    ) -> crate::Result<ResolvedVar<'a>> {
        let name = self.idents.name(id.ident);
        if let Ok(b) = name.parse::<BuiltinConst>() {
            return Ok(ResolvedVar::Const(b));
        }

        match scopes.var(id.ident) {
            Some(v) => Ok(ResolvedVar::Var(v)),
            None => Err(crate::Error::UndefinedVar(name.to_owned(), id.span)),
        }
    }

    /// Resolve the var and make sure it is initialized
    pub fn get_var<'a>(
        &self,
        scopes: &'a Scopes,
        id: &IdentSpan,
    ) -> crate::Result<ResolvedVar<'a>> {
        let var = match self.resolve_var(scopes, id)? {
            c @ ResolvedVar::Const(_) => return Ok(c),
            ResolvedVar::Var(v) => v,
        };

        if !var.assigned {
            let name = self.idents.name(id.ident);
            return Err(crate::Error::UninitializedVar(
                name.into(),
                var.ident.span,
                id.span,
            ));
        }

        Ok(ResolvedVar::Var(var))
    }

    pub fn def_var(
        &self,
        scopes: &mut Scopes,
        ident: IdentSpan,
        data_type: DataType,
        assigned: bool,
        mutable: bool,
    ) -> VarRef {
        let inner = VarRef::Local(scopes.frame_size());
        let var = Var::new(ident, data_type, assigned, mutable, inner);
        scopes.extend_frame(1);
        scopes.current_mut().def_var(var);
        inner
    }

    pub fn set_var(&self, scopes: &mut Scopes, id: &IdentSpan, val: &Ast) -> crate::Result<()> {
        match scopes.var_mut(id.ident) {
            Some(v) => {
                if !v.mutable && v.assigned {
                    let name = self.idents.name(id.ident);
                    return Err(crate::Error::ImmutableAssign(
                        name.into(),
                        id.span,
                        val.span,
                    ));
                }

                v.assigned = true;

                Ok(())
            }
            None => {
                let name = self.idents.name(id.ident);
                Err(crate::Error::UndefinedVar(name.to_owned(), id.span))
            }
        }
    }
}

#[derive(Debug)]
pub struct Scopes {
    scopes: Vec<Scope>,
    frames: Vec<Frame>,
    len: usize,
}

#[derive(Debug)]
struct Frame {
    scope_index: usize,
    size: usize,
}

impl Frame {
    fn new(scope_index: usize, size: usize) -> Self {
        Self { scope_index, size }
    }
}

impl Default for Scopes {
    fn default() -> Self {
        Self {
            scopes: vec![Scope::default()],
            frames: vec![Frame::new(0, 0)],
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

    pub fn next_mut(&mut self) -> &mut Scope {
        if self.len >= self.scopes.len() {
            self.scopes.push(Scope::default())
        }
        self.scopes
            .get_mut(self.len)
            .expect("Expected to find next scope")
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

    fn push(&mut self) {
        if self.len >= self.scopes.len() {
            self.scopes.push(Scope::default())
        }
        self.len += 1;
    }

    fn pop(&mut self) {
        self.current_mut().clear();
        self.len -= 1;
    }

    pub fn with_new_frame<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.push_frame();
        let r = self.with_new(f);
        self.pop_frame();
        r
    }

    pub fn with_new<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.push();
        let r = f(self);
        self.pop();
        r
    }

    pub fn clear(&mut self) {
        for s in self.iter_mut() {
            s.clear();
        }
        self.len = 0;
    }

    fn fun(&self, id: Ident) -> Option<Rc<Fun>> {
        for s in self.rev() {
            if let Some(f) = s.fun(id) {
                return Some(f);
            }
        }
        None
    }

    fn var(&self, id: Ident) -> Option<&Var> {
        // XXX
        let scope_index = self.current_frame().scope_index;
        for s in self.scopes[scope_index..].iter().rev() {
            if let Some(v) = s.var(id) {
                return Some(v);
            }
        }
        None
    }

    fn var_mut(&mut self, id: Ident) -> Option<&mut Var> {
        // XXX
        let scope_index = self.current_frame().scope_index;
        for s in self.scopes[scope_index..].iter_mut().rev() {
            if let Some(v) = s.var_mut(id) {
                return Some(v);
            }
        }
        None
    }

    fn push_frame(&mut self) {
        self.frames.push(Frame::new(self.len, 0));
    }

    fn pop_frame(&mut self) -> usize {
        self.frames
            .pop()
            .expect("Expected stack frames to be non empty")
            .size
    }

    pub fn frame_size(&self) -> usize {
        self.current_frame().size
    }

    pub fn set_frame_size(&mut self, size: usize) {
        self.current_frame_mut().size = size;
    }

    pub fn extend_frame(&mut self, size: usize) {
        self.current_frame_mut().size += size;
    }

    fn current_frame(&self) -> &Frame {
        self.frames
            .last()
            .expect("Expected stack frames to be non empty")
    }

    fn current_frame_mut(&mut self) -> &mut Frame {
        self.frames
            .last_mut()
            .expect("Expected stack frames to be non empty")
    }
}

#[derive(Debug, Default)]
pub struct Scope {
    funs: HashMap<Ident, Rc<Fun>>,
    vars: HashMap<Ident, Var>,
}

impl Scope {
    pub fn clear(&mut self) {
        self.vars.clear();
        self.funs.clear();
    }

    pub fn fun(&self, id: Ident) -> Option<Rc<Fun>> {
        self.funs.get(&id).map(Rc::clone)
    }

    pub fn var(&self, id: Ident) -> Option<&Var> {
        self.vars.get(&id)
    }

    pub fn var_mut(&mut self, id: Ident) -> Option<&mut Var> {
        self.vars.get_mut(&id)
    }

    fn def_var(&mut self, var: Var) {
        self.vars.insert(var.ident.ident, var);
    }

    pub fn vars(&self) -> impl Iterator<Item = &Var> {
        self.vars.values()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Fun {
    pub ident: IdentSpan,
    pub params: Vec<FunParam>,
    pub return_type: DataType,
    pub inner: Rc<ast::Fun>,
}

impl Fun {
    pub const fn new(
        ident: IdentSpan,
        params: Vec<FunParam>,
        return_type: DataType,
        inner: Rc<ast::Fun>,
    ) -> Self {
        Self {
            ident,
            params,
            return_type,
            inner,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunParam {
    pub ident: IdentSpan,
    pub data_type: DataType,
    pub span: Span,
}

impl FunParam {
    pub const fn new(ident: IdentSpan, data_type: DataType, span: Span) -> Self {
        Self {
            ident,
            data_type,
            span,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Var {
    pub ident: IdentSpan,
    pub data_type: DataType,
    pub assigned: bool,
    pub mutable: bool,
    pub inner: VarRef,
}

impl Var {
    pub fn new(
        ident: IdentSpan,
        data_type: DataType,
        assigned: bool,
        mutable: bool,
        inner: VarRef,
    ) -> Self {
        Self {
            ident,
            data_type,
            assigned,
            mutable,
            inner,
        }
    }
}
