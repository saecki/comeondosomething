use std::rc::Rc;

use crate::ast::{Ast, AstT, ForLoop, Fun, IfExpr, WhileLoop};
use crate::{Context, IdentSpan, Infix, Postfix, Prefix, Range, Span, Val, ValSpan};

pub use val::*;

#[cfg(test)]
mod test;
mod val;

impl Context {
    /// Evaluate all ast's and return the last value.
    pub fn eval_all(&mut self, asts: &[Ast]) -> crate::Result<Option<Val>> {
        match asts.split_last() {
            Some((last, others)) => {
                for c in others {
                    self.eval_ast(c)?;
                }
                self.eval(last)
            }
            None => Err(crate::Error::MissingExpr),
        }
    }

    pub fn eval(&mut self, ast: &Ast) -> crate::Result<Option<Val>> {
        match self.eval_ast(ast)? {
            Return::Val(v) => Ok(Some(v.val)),
            Return::Unit(_) => Ok(None),
        }
    }

    pub fn eval_to_vals(&mut self, args: &[Ast]) -> crate::Result<Vec<ValSpan>> {
        let mut vals = Vec::with_capacity(args.len());
        for a in args {
            vals.push(self.eval_to_val(a)?);
        }
        Ok(vals)
    }

    pub fn eval_to_val(&mut self, ast: &Ast) -> crate::Result<ValSpan> {
        self.eval_ast(ast)?.into_val()
    }

    pub fn eval_to_int(&mut self, ast: &Ast) -> crate::Result<i128> {
        self.eval_ast(ast)?.to_int()
    }

    pub fn eval_to_f64(&mut self, ast: &Ast) -> crate::Result<f64> {
        self.eval_ast(ast)?.to_f64()
    }

    pub fn eval_to_bool(&mut self, ast: &Ast) -> crate::Result<bool> {
        self.eval_ast(ast)?.to_bool()
    }

    pub fn eval_to_range(&mut self, ast: &Ast) -> crate::Result<Range> {
        self.eval_ast(ast)?.to_range()
    }

    pub fn eval_ast(&mut self, ast: &Ast) -> crate::Result<Return> {
        let s = ast.span;
        match &ast.typ {
            AstT::Empty => Ok(Return::Unit(s)),
            AstT::Error => Err(crate::Error::Parsing(s)),
            AstT::Val(v) => Ok(Return::Val(v.clone())),
            AstT::Ident(i) => todo!(),
            AstT::Block(a) => self.eval_block(a, s),
            AstT::IfExpr(a) => self.eval_if_expr(a, s),
            AstT::WhileLoop(a) => self.eval_while_loop(a, s),
            AstT::ForLoop(a) => self.eval_for_loop(a, s),
            AstT::FunDef(a) => self.eval_fun_def(a, s),
            AstT::FunCall(a, b) => self.eval_fun_call(a, b, s),
            AstT::VarDef(a, b, c) => self.eval_var_def(a, b, *c, s),
            AstT::Prefix(p, a) => self.eval_prefix(p, a, s),
            AstT::Postfix(a, p) => self.eval_postfix(a, p, s),
            AstT::Infix(a, i, b) => self.eval_infix(a, i, b, s),
            AstT::InfixAssign(a, i, b) => self.eval_infix_assign(a, i, b, s),
            AstT::Assign(a, i, b) => self.eval_assign(a, i, b, s),
        }
        .map(|mut r| {
            if let Return::Val(v) = &mut r {
                if let Some(i) = v.convert_to_int() {
                    v.val = Val::Int(i);
                }
            }
            r
        })
    }

    fn eval_block(&mut self, asts: &[Ast], span: Span) -> crate::Result<Return> {
        self.scopes.push();
        let r = match asts.split_last() {
            Some((last, others)) => {
                for c in others {
                    self.eval_ast(c)?;
                }
                self.eval_ast(last)
            }
            None => Ok(Return::Unit(span)),
        };
        self.scopes.pop();
        r
    }

    fn eval_if_expr(&mut self, if_expr: &IfExpr, span: Span) -> crate::Result<Return> {
        for c in if_expr.cases.iter() {
            if self.eval_to_bool(&c.cond)? {
                return self.eval_block(&c.block, span);
            }
        }
        if let Some(b) = &if_expr.else_block {
            return self.eval_block(&b.asts, b.span);
        }
        Ok(Return::Unit(span))
    }

    fn eval_while_loop(&mut self, whl_loop: &WhileLoop, span: Span) -> crate::Result<Return> {
        while self.eval_to_bool(&whl_loop.cond)? {
            self.scopes.push();
            for a in whl_loop.block.iter() {
                self.eval_ast(a)?;
            }

            self.scopes.pop();
        }
        Ok(Return::Unit(span))
    }

    fn eval_for_loop(&mut self, for_loop: &ForLoop, span: Span) -> crate::Result<Return> {
        let iter = self.eval_to_range(&for_loop.iter)?;

        for i in iter.iter() {
            self.scopes.push();
            todo!("define var");

            for c in for_loop.block.iter() {
                self.eval_ast(c)?;
            }

            self.scopes.pop();
        }

        Ok(Return::Unit(span))
    }

    fn eval_fun_def(&mut self, fun: &Rc<Fun>, span: Span) -> crate::Result<Return> {
        // TODO: consider removing this
        Ok(Return::Unit(span))
    }

    fn eval_fun_call(&mut self, fun: &Rc<Fun>, args: &[Ast], span: Span) -> crate::Result<Return> {
        if args.len() < fun.params.len() {
            return Err(crate::Error::MissingFunArgs {
                expected: fun.params.len(),
                found: args.len(),
                span: Span::pos(span.end - 1),
            });
        }

        if args.len() > fun.params.len() {
            let spans = args.iter().skip(fun.params.len()).map(|a| a.span).collect();
            return Err(crate::Error::UnexpectedFunArgs {
                expected: fun.params.len(),
                found: args.len(),
                spans,
            });
        }

        let mut arg_vals = Vec::new();
        for a in args.iter() {
            let val = self.eval_to_val(a)?;
            arg_vals.push(val);
        }

        self.scopes.push();
        let scope = self.scopes.current_mut();
        for (p, a) in fun.params.iter().zip(arg_vals) {
            todo!("define var")
        }

        // TODO: type checking
        let r = match fun.block.split_last() {
            Some((last, others)) => {
                for c in others {
                    self.eval_ast(c)?;
                }
                self.eval_ast(last)
            }
            None => Ok(Return::Unit(span)),
        };

        self.scopes.pop();

        r
    }

    fn eval_var_def(
        &mut self,
        _id: &IdentSpan,
        _b: &Ast,
        _mutable: bool,
        _span: Span,
    ) -> crate::Result<Return> {
        todo!()
    }

    fn eval_prefix(&mut self, _p: &Prefix, _a: &Ast, _span: Span) -> crate::Result<Return> {
        todo!()
    }

    fn eval_postfix(&mut self, _a: &Ast, _p: &Postfix, _span: Span) -> crate::Result<Return> {
        todo!()
    }

    fn eval_infix(&mut self, _a: &Ast, _i: &Infix, _b: &Ast, _span: Span) -> crate::Result<Return> {
        todo!()
    }

    fn eval_infix_assign(
        &mut self,
        _id: &IdentSpan,
        _i: &Infix,
        _b: &Ast,
        _span: Span,
    ) -> crate::Result<Return> {
        todo!()
    }

    fn eval_assign(
        &mut self,
        _id: &IdentSpan,
        _i: &Infix,
        _b: &Ast,
        _span: Span,
    ) -> crate::Result<Return> {
        todo!("set var");
    }
}
