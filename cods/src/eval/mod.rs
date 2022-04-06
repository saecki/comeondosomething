use std::rc::Rc;

use crate::ast::{self, Ast, AstT, ForLoop, Fun, IfExpr, WhileLoop};
use crate::{Context, Infix, Postfix, Prefix, Range, Span, Val, ValSpan};

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
            AstT::Block(a) => self.eval_block(a, s),
            AstT::IfExpr(a) => self.eval_if_expr(a, s),
            AstT::WhileLoop(a) => self.eval_while_loop(a, s),
            AstT::ForLoop(a) => self.eval_for_loop(a, s),
            AstT::Prefix(p, a) => self.eval_prefix(p, a, s),
            AstT::Postfix(a, p) => self.eval_postfix(a, p, s),
            AstT::Infix(a, i, b) => self.eval_infix(a, i, b, s),
            AstT::InfixAssign(a, i, b) => self.eval_infix_assign(a, i, b, s),
            AstT::Assign(v, b) => self.eval_assign(v, b, s),
            AstT::VarDef(v, b) => self.eval_var_def(v, b, s),
            AstT::VarRef(v) => self.eval_var_ref(v, s),
            AstT::FunCall(a, b) => self.eval_fun_call(a, b, s),
        }
    }

    fn eval_block(&mut self, asts: &[Ast], span: Span) -> crate::Result<Return> {
        let r = match asts.split_last() {
            Some((last, others)) => {
                for c in others {
                    self.eval_ast(c)?;
                }
                self.eval_ast(last)
            }
            None => Ok(Return::Unit(span)),
        };
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
            for a in whl_loop.block.iter() {
                self.eval_ast(a)?;
            }
        }
        Ok(Return::Unit(span))
    }

    fn eval_for_loop(&mut self, for_loop: &ForLoop, span: Span) -> crate::Result<Return> {
        let iter = self.eval_to_range(&for_loop.iter)?;

        for i in iter.iter() {
            for_loop.var.set(Val::Int(i));

            for c in for_loop.block.iter() {
                self.eval_ast(c)?;
            }
        }

        Ok(Return::Unit(span))
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
        _var: &Rc<ast::Var>,
        _i: &Infix,
        _b: &Ast,
        _span: Span,
    ) -> crate::Result<Return> {
        todo!()
    }

    fn eval_assign(&mut self, var: &Rc<ast::Var>, b: &Ast, span: Span) -> crate::Result<Return> {
        let val = self.eval_to_val(b)?;
        var.set(val.val);
        Ok(Return::Unit(span))
    }

    fn eval_var_def(&mut self, var: &Rc<ast::Var>, b: &Ast, span: Span) -> crate::Result<Return> {
        let val = self.eval_to_val(b)?;
        var.set(val.val);
        Ok(Return::Unit(span))
    }

    fn eval_var_ref(&mut self, var: &Rc<ast::Var>, span: Span) -> crate::Result<Return> {
        Ok(Return::Val(ValSpan::new(var.get(), span)))
    }

    fn eval_fun_call(&mut self, fun: &Rc<Fun>, args: &[Ast], span: Span) -> crate::Result<Return> {
        for (p, a) in fun.params().iter().zip(args) {
            let val = self.eval_to_val(a)?;
            p.set(val.val);
        }

        let r = match fun.block().split_last() {
            Some((last, others)) => {
                for c in others {
                    self.eval_ast(c)?;
                }
                self.eval_ast(last)
            }
            None => Ok(Return::Unit(span)),
        };

        r
    }
}
