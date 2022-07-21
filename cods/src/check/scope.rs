use std::cell::Cell;
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

pub enum ResolveError {
    NotFound,
    DynCapture(Span),
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
        let current = scopes.current_funs();
        let id = fun.ident;
        for (i, f) in current {
            if *i == id.ident {
                let i_s = f.ident.span;
                let name = self.idents.name(id.ident);
                return Err(crate::Error::RedefinedFun(name.to_owned(), i_s, id.span));
            }
        }

        scopes.funs.push((id.ident, Rc::new(fun)));

        Ok(())
    }

    /// Resolve the var belonging to the identifier.
    pub fn resolve_var<'a>(
        &self,
        scopes: &'a mut Scopes,
        id: &IdentSpan,
    ) -> crate::Result<ResolvedVar<'a>> {
        let name = self.idents.name(id.ident);
        if let Ok(b) = name.parse::<BuiltinConst>() {
            return Ok(ResolvedVar::Const(b));
        }

        match scopes.var_mut(id.ident) {
            Ok(v) => {
                v.uses += 1;
                Ok(ResolvedVar::Var(v))
            }
            Err(ResolveError::DynCapture(s)) => Err(crate::Error::NotImplemented(
                "Capturing variables from a dynamic scope is not yet implemented",
                vec![s, id.span],
            )),
            Err(ResolveError::NotFound) => {
                Err(crate::Error::UndefinedVar(name.to_owned(), id.span))
            }
        }
    }

    /// Resolve the var and make sure it is initialized
    pub fn get_var<'a>(
        &self,
        scopes: &'a mut Scopes,
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
        let inner = scopes.var_ref();
        let var = Var::new(ident, data_type, assigned, mutable, inner);
        scopes.extend_frame(1);
        scopes.vars.push(var);
        inner
    }

    pub fn set_var(&self, scopes: &mut Scopes, id: &IdentSpan, val: &Ast) -> crate::Result<()> {
        match scopes.var_mut(id.ident) {
            Ok(v) => {
                if !v.mutable && v.assigned {
                    let name = self.idents.name(id.ident);
                    return Err(crate::Error::ImmutableAssign(
                        name.into(),
                        id.span,
                        val.span,
                    ));
                }

                v.assigned = true;
                v.uses += 1;

                Ok(())
            }
            Err(ResolveError::DynCapture(s)) => Err(crate::Error::NotImplemented(
                "Capturing variables from a dynamic scope is not yet implemented",
                vec![s, id.span],
            )),
            Err(ResolveError::NotFound) => {
                let name = self.idents.name(id.ident);
                Err(crate::Error::UndefinedVar(name.to_owned(), id.span))
            }
        }
    }

    pub fn with_new_frame<T>(
        &mut self,
        scopes: &mut Scopes,
        fun: Rc<Fun>,
        f: impl FnOnce(&mut Self, &mut Scopes) -> T,
    ) -> T {
        scopes.push_frame(fun);
        let r = self.with_new(scopes, f);
        scopes.pop_frame();
        r
    }

    pub fn with_new<T>(
        &mut self,
        scopes: &mut Scopes,
        f: impl FnOnce(&mut Self, &mut Scopes) -> T,
    ) -> T {
        scopes.push();
        let r = f(self, scopes);
        self.check_unused(scopes);
        scopes.pop();
        r
    }

    pub fn check_unused(&mut self, scopes: &Scopes) {
        for v in scopes.current_vars() {
            if v.uses == 0 {
                let name = self.idents.name(v.ident.ident);
                if name == "_" {
                    continue;
                }

                self.warnings
                    .push(crate::Warning::UnusedVar(name.to_owned(), v.ident.span));
            }
        }

        for (i, f) in scopes.current_funs() {
            if f.uses.get() == 0 {
                let name = self.idents.name(*i);
                self.warnings
                    .push(crate::Warning::UnusedFun(name.to_owned(), f.ident.span));
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Scopes {
    vars: Vec<Var>,
    funs: Vec<(Ident, Rc<Fun>)>,
    scopes: Vec<Scope>,
    /// Frames can span multiple scopes.
    frames: Vec<Frame>,
}

/// Starting indices of var and fun scopes.
#[derive(Clone, Debug)]
struct Scope {
    var: usize,
    fun: usize,
}

impl Scope {
    fn new(var: usize, fun: usize) -> Self {
        Self { var, fun }
    }

    fn global() -> Self {
        Self { var: 0, fun: 0 }
    }
}

#[derive(Clone, Debug)]
struct Frame {
    fun: Option<Rc<Fun>>,
    scope_index: usize,
    size: usize,
}

impl Frame {
    fn new(fun: Option<Rc<Fun>>, scope_index: usize, size: usize) -> Self {
        Self {
            fun,
            scope_index,
            size,
        }
    }
}

impl Default for Scopes {
    fn default() -> Self {
        Self {
            vars: vec![],
            funs: vec![],
            scopes: vec![Scope::global()],
            frames: vec![Frame::new(None, 0, 0)],
        }
    }
}

impl Scopes {
    pub fn current_vars(&self) -> &[Var] {
        let start = self
            .scopes
            .last()
            .expect("Expected at least the global scope")
            .var;
        &self.vars[start..]
    }

    pub fn current_funs(&self) -> &[(Ident, Rc<Fun>)] {
        let start = self
            .scopes
            .last()
            .expect("Expected at least the global scope")
            .fun;
        &self.funs[start..]
    }

    fn push(&mut self) {
        self.scopes
            .push(Scope::new(self.vars.len(), self.funs.len()));
    }

    fn pop(&mut self) {
        let indices = self.scopes.pop().expect("Expected at least a second scope");
        self.vars.truncate(indices.var);
        self.funs.truncate(indices.fun);
    }

    fn fun(&self, id: Ident) -> Option<Rc<Fun>> {
        for (i, f) in self.funs.iter().rev() {
            if *i == id {
                return Some(Rc::clone(f));
            }
        }
        None
    }

    fn var_mut(&mut self, id: Ident) -> Result<&mut Var, ResolveError> {
        let current_scope = self.scopes[self.current_frame().scope_index].var;
        for v in self.vars[current_scope..].iter_mut().rev() {
            if v.ident.ident == id {
                // workaround for lifetime issue: https://github.com/rust-lang/rust/issues/54663
                return Ok(unsafe { std::mem::transmute::<_, _>(v) });
            }
        }
        if let Some(f) = self.frames.get(1) {
            let second_scope = self.scopes[f.scope_index].var;
            for v in self.vars[second_scope..current_scope].iter_mut().rev() {
                if v.ident.ident == id {
                    return Err(ResolveError::DynCapture(v.ident.span));
                }
            }
            for v in self.vars[..second_scope].iter_mut().rev() {
                if v.ident.ident == id {
                    return Ok(v);
                }
            }
        }
        Err(ResolveError::NotFound)
    }

    fn push_frame(&mut self, fun: Rc<Fun>) {
        self.frames
            .push(Frame::new(Some(fun), self.scopes.len(), 0));
    }

    fn pop_frame(&mut self) -> Frame {
        self.frames
            .pop()
            .expect("Expected stack frames to be non empty")
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

    pub fn var_ref(&self) -> VarRef {
        match self.frames.len() {
            1 => VarRef::Global(self.frame_size()),
            _ => VarRef::Local(self.frame_size()),
        }
    }

    pub fn fun_context(&self) -> Option<Rc<Fun>> {
        self.current_frame().fun.clone()
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

#[derive(Clone, Debug, PartialEq)]
pub struct Fun {
    pub ident: IdentSpan,
    pub params: Vec<FunParam>,
    pub return_type: ReturnType,
    pub uses: Cell<u32>,
    pub inner: Rc<ast::Fun>,
}

impl Fun {
    pub const fn new(
        ident: IdentSpan,
        params: Vec<FunParam>,
        return_type: ReturnType,
        inner: Rc<ast::Fun>,
    ) -> Self {
        Self {
            ident,
            params,
            return_type,
            uses: Cell::new(0),
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReturnType {
    pub data_type: DataType,
    pub span: Option<Span>,
}

impl ReturnType {
    pub fn new(data_type: DataType, span: Option<Span>) -> Self {
        Self { data_type, span }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Var {
    pub ident: IdentSpan,
    pub data_type: DataType,
    pub assigned: bool,
    pub mutable: bool,
    pub uses: u32,
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
            uses: 0,
            inner,
        }
    }
}
