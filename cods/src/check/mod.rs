use std::cmp::max;
use std::rc::Rc;

use crate::cst::{self, Cst};
use crate::{Context, Infix, InfixT, Postfix, PostfixT, Prefix, PrefixT, Span, VarRef};

pub use ast::{Ast, AstT, Asts, BuiltinFunCall};
pub use builtin::{BuiltinConst, BuiltinFun, FunSignature, Repetition, SignatureKind, SpillKind};
pub use funs::*;
pub use op::OpSignature;
pub use scope::*;
pub use types::*;

pub mod ast;
mod builtin;
mod funs;
mod op;
mod scope;
#[cfg(test)]
mod test;
mod types;

#[derive(Default)]
pub struct Checker {
    pub scopes: Scopes,
    pub funs: Funs,
}

impl Checker {
    pub fn clear(&mut self) {
        self.scopes.clear();
        self.funs.clear();
    }
}

impl Context {
    pub fn check(&mut self, csts: Vec<Cst>) -> crate::Result<Asts> {
        let mut checker = Checker::default();
        self.check_with(&mut checker, csts)
    }

    pub fn check_with(&mut self, checker: &mut Checker, csts: Vec<Cst>) -> crate::Result<Asts> {
        let (asts, _) = self.check_types(checker, csts, true)?;
        let global_frame_size = checker.scopes.frame_size();

        self.check_unused(&checker.scopes);

        Ok(Asts {
            asts,
            global_frame_size,
        })
    }

    fn check_types(
        &mut self,
        checker: &mut Checker,
        mut csts: Vec<Cst>,
        is_expr: bool,
    ) -> crate::Result<(Vec<Ast>, bool)> {
        for c in csts.iter_mut() {
            if let Cst::FunDef(f) = c {
                if !f.defined {
                    self.check_fun_def_signature(checker, f)?;
                    f.defined = true;
                }
            }
        }

        let mut asts = Vec::with_capacity(csts.len());
        let mut returns = false;
        if let Some(last) = csts.pop() {
            let mut iter = csts.into_iter();
            for c in iter.by_ref() {
                let ast = self.check_type(checker, c, false)?;
                let ast_returns = ast.returns;
                asts.push(ast);
                if ast_returns {
                    returns = true;
                    break;
                }
            }

            if returns {
                let s = match iter.next() {
                    Some(c) => Span::across(c.span(), last.span()),
                    None => last.span(),
                };
                self.warnings.push(crate::Warning::Unreachable(s));
            } else {
                let ast = self.check_type(checker, last, is_expr)?;
                if ast.returns {
                    returns = true;
                }
                asts.push(ast);
            }
        }

        Ok((asts, returns))
    }

    fn check_type(&mut self, checker: &mut Checker, cst: Cst, is_expr: bool) -> crate::Result<Ast> {
        let span = cst.span();
        let ast = match cst {
            Cst::Empty(s) => Ast::expr(AstT::Unit, DataType::Unit, false, s),
            Cst::Error(s) => Ast::expr(AstT::Error, DataType::Never, false, s),
            Cst::Val(v) => Ast::val(v.val, v.span),
            Cst::Ident(i) => match self.get_var(&mut checker.scopes, &i)? {
                ResolvedVar::Const(c) => Ast::val(c.val(), span),
                ResolvedVar::Var(var) => Ast::var(var.inner, var.data_type, false, i.span),
            },
            Cst::Par(_, c, _) => self.check_par(checker, *c, is_expr)?,
            Cst::Block(b) => self.check_block(checker, b, is_expr)?,
            Cst::IfExpr(i) => self.check_if_expr(checker, i, is_expr)?,
            Cst::MatchExpr(m) => self.check_match_expr(checker, m, is_expr)?,
            Cst::WhileLoop(w) => self.check_while_loop(checker, w)?,
            Cst::ForLoop(f) => self.check_for_loop(checker, f)?,
            Cst::FunDef(mut f) => {
                if !f.defined {
                    self.check_fun_def_signature(checker, &f)?;
                    f.defined = true;
                }
                self.check_fun_def_block(checker, f)?
            }
            Cst::FunCall(f) => self.check_fun_call(checker, f)?,
            Cst::Return(r) => self.check_return(checker, r)?,
            Cst::VarDef(v) => self.check_var_def(checker, v)?,
            Cst::Prefix(p, a) => self.check_prefix(checker, p, *a, span)?,
            Cst::Postfix(a, p) => self.check_postfix(checker, *a, p, span)?,
            Cst::Infix(a, i, b) => self.check_infix(checker, *a, i, *b, span)?,
        };
        Ok(ast)
    }

    fn check_par(&mut self, checker: &mut Checker, cst: Cst, is_expr: bool) -> crate::Result<Ast> {
        let ast = self.check_type(checker, cst, is_expr)?;
        expect_expr(&ast)?;
        Ok(ast)
    }

    fn check_block(
        &mut self,
        checker: &mut Checker,
        block: cst::Block,
        is_expr: bool,
    ) -> crate::Result<Ast> {
        let span = block.span();

        let (asts, returns) = self.with_new_scope(checker, ExecPolicy::Once, |ctx, checker| {
            ctx.check_types(checker, block.csts, is_expr)
        })?;

        let data_type = asts
            .last()
            .and_then(|a| a.data_type.as_expr())
            .unwrap_or(DataType::Unit);

        Ok(Ast::expr(AstT::Block(asts), data_type, returns, span))
    }

    fn check_if_expr(
        &mut self,
        checker: &mut Checker,
        i: cst::IfExpr,
        is_expr: bool,
    ) -> crate::Result<Ast> {
        let mut cases = Vec::new();
        let mut data_type = DataType::Unit;
        let mut returns = true;
        let span = i.span();

        let init_frame_size = checker.scopes.frame_size();
        let mut max_frame_size = init_frame_size;
        let mut uninitialized_vars = checker.scopes.uninitialized_vars();
        let if_block_span = i.if_block.block.span();
        {
            let cond = self.check_cond(checker, *i.if_block.cond)?;
            if cond.returns {
                let s = Span::across(i.if_block.block.span(), span);
                self.warnings.push(crate::Warning::Unreachable(s));

                data_type = DataType::Never;

                cases.push(ast::CondBlock::new(cond, Vec::new()));
            } else {
                let (block, block_returns) =
                    self.with_new_scope(checker, ExecPolicy::Once, |ctx, checker| {
                        ctx.check_types(checker, i.if_block.block.csts, is_expr)
                    })?;
                max_frame_size = checker.scopes.frame_size();
                checker.scopes.set_frame_size(init_frame_size);

                if is_expr {
                    data_type = block
                        .last()
                        .and_then(|a| a.data_type.as_expr())
                        .unwrap_or(DataType::Unit);
                }

                if !block_returns {
                    returns = false
                }

                cases.push(ast::CondBlock::new(cond, block));
            }

            // check if variables have been initialized in this branch
            for uninit_var in uninitialized_vars.iter_mut() {
                let var = checker
                    .scopes
                    .var_mut(uninit_var.ident.ident)
                    .expect("This variable should exist");

                // set the initial resultant state of initialization for this if statement
                uninit_var.result = var.initialized;
                var.initialized = uninit_var.prev;
            }
        }

        for e in i.else_if_blocks {
            let e_span = e.span();
            let cond = self.check_cond(checker, e.cond)?;
            if cond.returns {
                self.warnings.push(crate::Warning::Unreachable(e_span));

                cases.push(ast::CondBlock::new(cond, Vec::new()));
            } else {
                let block_span = e.block.span();
                let (block, block_returns) =
                    self.with_new_scope(checker, ExecPolicy::Once, |ctx, checker| {
                        ctx.check_types(checker, e.block.csts, is_expr)
                    })?;
                max_frame_size = max(max_frame_size, checker.scopes.frame_size());
                checker.scopes.set_frame_size(init_frame_size);

                if is_expr {
                    let d = block
                        .last()
                        .and_then(|a| a.data_type.as_expr())
                        .unwrap_or(DataType::Unit);

                    if data_type == DataType::Never {
                        data_type = d;
                    } else if d.is_not(data_type) {
                        let if_expr_span = cases[0].block.last().map_or(if_block_span, |a| a.span);
                        let else_expr_span = block.last().map_or(block_span, |a| a.span);
                        return Err(crate::Error::IfBranchIncompatibleType(
                            (data_type, if_expr_span),
                            (d, else_expr_span),
                        ));
                    }
                }

                if !block_returns {
                    returns = false;
                }

                cases.push(ast::CondBlock::new(cond, block));
            }

            // check if variables have been initialized in this branch
            for uninit_var in uninitialized_vars.iter_mut() {
                let var = checker
                    .scopes
                    .var_mut(uninit_var.ident.ident)
                    .expect("This variable should exist");

                if var.initialized != uninit_var.result {
                    uninit_var.result = Initialized::Maybe;
                }
                var.initialized = uninit_var.prev;
            }
        }

        let else_block = if let Some(e) = i.else_block {
            let block_span = e.block.span();
            let (block, block_returns) =
                self.with_new_scope(checker, ExecPolicy::Once, |ctx, checker| {
                    let b = ctx.check_types(checker, e.block.csts, is_expr)?;
                    Ok(b)
                })?;
            max_frame_size = max(max_frame_size, checker.scopes.frame_size());

            if is_expr {
                let d = block
                    .last()
                    .and_then(|a| a.data_type.as_expr())
                    .unwrap_or(DataType::Unit);

                if data_type == DataType::Never {
                    data_type = d;
                } else if d.is_not(data_type) {
                    let if_expr_span = cases[0].block.last().map_or(if_block_span, |a| a.span);
                    let else_expr_span = block.last().map_or(block_span, |a| a.span);
                    return Err(crate::Error::IfBranchIncompatibleType(
                        (data_type, if_expr_span),
                        (d, else_expr_span),
                    ));
                }
            }

            if !block_returns {
                returns = false;
            }

            Some(block)
        } else {
            returns = false;

            if is_expr && data_type.is_not(DataType::Unit) {
                return Err(crate::Error::MissingElseBranch(data_type, span));
            } else {
                None
            }
        };

        checker.scopes.set_frame_size(max_frame_size);

        // check if variables have been initialized in the else branch, even if there is none
        // explicitly declared
        for uninit_var in uninitialized_vars.iter_mut() {
            let var = checker
                .scopes
                .var_mut(uninit_var.ident.ident)
                .expect("This variable should exist");

            if var.initialized != uninit_var.result {
                uninit_var.result = Initialized::Maybe;
            }
            var.initialized = uninit_var.prev;
        }

        // Set variables initialization to the resultant state of all branches
        for uninit_var in uninitialized_vars.iter_mut() {
            let var = checker
                .scopes
                .var_mut(uninit_var.ident.ident)
                .expect("This variable should exist");
            var.initialized = uninit_var.result;
        }

        let if_expr = ast::IfExpr::new(cases, else_block);
        if is_expr {
            Ok(Ast::expr(AstT::IfExpr(if_expr), data_type, returns, span))
        } else {
            Ok(Ast::statement(AstT::IfExpr(if_expr), returns, span))
        }
    }

    fn check_match_expr(
        &mut self,
        checker: &mut Checker,
        m: cst::MatchExpr,
        is_expr: bool,
    ) -> crate::Result<Ast> {
        let mut uninitialized_vars = checker.scopes.uninitialized_vars();
        let mut arms: Vec<ast::MatchArm> = Vec::with_capacity(m.arms.len());
        let init_frame_size = checker.scopes.frame_size();
        let mut max_frame_size = init_frame_size;
        let mut default_arm = None;
        let mut data_type = DataType::Unit;
        let mut first = true;
        let mut returns = true;
        let mut exaustive = false;
        let span = m.span();

        let value = self.check_type(checker, *m.value, true)?;
        let value_t = expect_expr(&value)?;

        if value.returns {
            let s = Span::across(m.l_par.span, m.r_par.span);
            self.warnings.push(crate::Warning::Unreachable(s));

            data_type = DataType::Never;
        } else {
            let mut arms_iter = m.arms.into_iter();
            while let Some(a) = arms_iter.next() {
                // check for default arm
                if let Cst::Ident(i) = a.cond {
                    if self.idents.name(i.ident) == "_" {
                        let expr = self.check_type(checker, a.expr, is_expr)?;
                        let expr_data_type = expr.data_type.as_expr().unwrap_or(DataType::Unit);

                        max_frame_size = max(max_frame_size, checker.scopes.frame_size());

                        if !expr.returns {
                            returns = false;
                        }

                        if is_expr {
                            if first {
                                data_type = expr_data_type;
                            } else if expr_data_type.is_not(data_type) {
                                return Err(crate::Error::MatchArmIncompatibleType(
                                    (data_type, arms[0].expr.span),
                                    (expr_data_type, expr.span),
                                ));
                            }
                        }

                        // check if variables have been initialized in the default arm
                        for uninit_var in uninitialized_vars.iter_mut() {
                            let var = checker
                                .scopes
                                .var_mut(uninit_var.ident.ident)
                                .expect("This variable should exist");

                            if first {
                                uninit_var.result = var.initialized;
                            } else if var.initialized != uninit_var.result {
                                uninit_var.result = Initialized::Maybe;
                            }
                        }

                        if let Some(next_arm) = arms_iter.next() {
                            let start_span = next_arm.span();
                            let last = arms_iter.next_back().unwrap_or(next_arm);
                            let s = Span::across(start_span, last.span());
                            self.warnings.push(crate::Warning::Unreachable(s));
                        }

                        default_arm = Some(Box::new(expr));
                        exaustive = true;
                        break;
                    }
                }

                // other match arms
                let cond = self.check_type(checker, a.cond, true)?;
                let cond_t = expect_expr(&cond)?;

                if value_t.is_not_comparable_to(cond_t) {
                    return Err(crate::Error::NotComparable(
                        (value_t, value.span),
                        (cond_t, cond.span),
                    ));
                }

                if cond.returns {
                    self.warnings
                        .push(crate::Warning::Unreachable(a.expr.span()));
                } else {
                    let expr = self.check_type(checker, a.expr, is_expr)?;
                    let expr_data_type = expr.data_type.as_expr().unwrap_or(DataType::Unit);

                    max_frame_size = max(max_frame_size, checker.scopes.frame_size());
                    checker.scopes.set_frame_size(init_frame_size);

                    if !expr.returns {
                        returns = false;
                    }

                    if is_expr {
                        if first {
                            data_type = expr_data_type;
                        } else if expr_data_type.is_not(data_type) {
                            self.errors.push(crate::Error::MatchArmIncompatibleType(
                                (data_type, arms[0].expr.span),
                                (expr_data_type, expr.span),
                            ))
                        }
                    }

                    arms.push(ast::MatchArm::new(cond, expr));
                }

                // check if variables have been initialized in this arm
                for uninit_var in uninitialized_vars.iter_mut() {
                    let var = checker
                        .scopes
                        .var_mut(uninit_var.ident.ident)
                        .expect("This variable should exist");

                    if first {
                        uninit_var.result = var.initialized;
                    } else if var.initialized != uninit_var.result {
                        uninit_var.result = Initialized::Maybe;
                    }
                    var.initialized = uninit_var.prev;
                }

                first = false;
            }

            if !exaustive {
                // TODO: recognize exhaustive matches without catch all `_` arm
                return Err(crate::Error::MissingMatchArm(value.span));
            }
        }

        checker.scopes.set_frame_size(max_frame_size);

        // Set variables initialization to the resultant state of all arms
        for uninit_var in uninitialized_vars.iter_mut() {
            let var = checker
                .scopes
                .var_mut(uninit_var.ident.ident)
                .expect("This variable should exist");
            var.initialized = uninit_var.result
        }

        let match_expr = ast::MatchExpr::new(Box::new(value), arms, default_arm);
        if is_expr {
            Ok(Ast::expr(
                AstT::MatchExpr(match_expr),
                data_type,
                returns,
                span,
            ))
        } else {
            Ok(Ast::statement(AstT::MatchExpr(match_expr), returns, span))
        }
    }

    fn check_while_loop(&mut self, checker: &mut Checker, w: cst::WhileLoop) -> crate::Result<Ast> {
        let span = w.span();

        let cond = self.check_cond(checker, *w.cond)?;
        let cond_returns = cond.returns;
        let block = if cond.returns {
            let s = w.block.span();
            self.warnings.push(crate::Warning::Unreachable(s));

            Vec::new()
        } else {
            let uninitialized_vars = checker.scopes.uninitialized_vars();
            let (block, _) =
                self.with_new_scope(checker, ExecPolicy::MultipleTimes, |ctx, checker| {
                    ctx.check_types(checker, w.block.csts, false)
                })?;

            // Mark variables that have been initialized as possibly initialized.
            for uninit_var in uninitialized_vars.iter() {
                let var = checker
                    .scopes
                    .var_mut(uninit_var.ident.ident)
                    .expect("variable should exist");
                if uninit_var.prev != var.initialized {
                    var.initialized = Initialized::Maybe;
                }
            }

            block
        };

        let whl_loop = ast::WhileLoop::new(Box::new(cond), block);
        Ok(Ast::statement(
            AstT::WhileLoop(whl_loop),
            cond_returns,
            span,
        ))
    }

    fn check_cond(&mut self, checker: &mut Checker, c: Cst) -> crate::Result<Ast> {
        let cond = self.check_type(checker, c, true)?;
        let cond_data_type = expect_expr(&cond)?;
        if cond_data_type.is_not(DataType::Bool) {
            return Err(crate::Error::MismatchedType {
                expected: DataType::Bool,
                found: cond_data_type,
                spans: vec![cond.span],
            });
        }
        Ok(cond)
    }

    fn check_for_loop(&mut self, checker: &mut Checker, f: cst::ForLoop) -> crate::Result<Ast> {
        let span = f.span();

        let iter = self.check_type(checker, *f.iter, true)?;
        let iter_data_type = expect_expr(&iter)?;
        if iter_data_type.is_not(DataType::Range) {
            return Err(crate::Error::NotIterable(iter_data_type, iter.span));
        }
        let iter_type = DataType::Int;

        let uninitialized_vars = checker.scopes.uninitialized_vars();
        let (inner, block) =
            self.with_new_scope(checker, ExecPolicy::MultipleTimes, |ctx, checker| {
                let inner = ctx.def_var(&mut checker.scopes, f.ident, iter_type, true, false);
                let (block, _) = ctx.check_types(checker, f.block.csts, false)?;
                Ok((inner, block))
            })?;

        // Mark variables that have been initialized as possibly initialized.
        for uninit_var in uninitialized_vars.iter() {
            let var = checker
                .scopes
                .var_mut(uninit_var.ident.ident)
                .expect("variable should exist");
            if uninit_var.prev != var.initialized {
                var.initialized = Initialized::Maybe;
            }
        }

        let for_loop = ast::ForLoop::new(inner, Box::new(iter), block);
        Ok(Ast::statement(AstT::ForLoop(for_loop), false, span))
    }

    fn check_fun_def_signature(
        &mut self,
        checker: &mut Checker,
        f: &cst::FunDef,
    ) -> crate::Result<()> {
        let mut params = Vec::with_capacity(f.params.items.len());
        for p in f.params.items.iter() {
            let typ = self.resolve_data_type(&p.typ)?;
            let span = Span::across(p.ident.span, p.typ.span());
            params.push(FunParam::new(p.ident, typ, span));
        }

        let return_type = f
            .return_type
            .as_ref()
            .map_or(Ok(DataType::Unit), |r| self.resolve_data_type(&r.typ))?;

        // Define function before checking block to support recursive calls
        let inner = checker.funs.push();
        let ret = ReturnType::new(return_type, f.return_type.as_ref().map(|r| r.typ.span()));
        let fun = Fun::new(f.ident, params, ret, inner);
        self.def_fun(&mut checker.scopes, fun)?;

        Ok(())
    }

    fn check_fun_def_block(&mut self, checker: &mut Checker, f: cst::FunDef) -> crate::Result<Ast> {
        let span = f.span();
        let block_span = f.block.span();

        let fun = match self.resolve_fun(&checker.scopes, &f.ident)? {
            ResolvedFun::Fun(f) => f,
            ResolvedFun::Builtin(_) => return Ok(Ast::statement(AstT::Unit, false, span)),
        };

        // TODO: store variables initialized by this function block and set their initialization
        // state after calling this function
        let uninitialized_vars = checker.scopes.uninitialized_vars();
        self.with_new_scope_and_frame(
            checker,
            ExecPolicy::MultipleTimes,
            Rc::clone(&fun),
            |ctx, checker| {
                let mut inner_params = Vec::new();
                for p in fun.params.iter() {
                    let param = ctx.def_var(&mut checker.scopes, p.ident, p.data_type, true, false);
                    inner_params.push(param);
                }

                // Check function block
                let is_expr = f.return_type.is_some();
                let (block, _) = ctx.check_types(checker, f.block.csts, is_expr)?;

                let block_type = block
                    .last()
                    .and_then(|a| a.data_type.as_expr())
                    .unwrap_or(DataType::Unit);

                if let Some(r) = f.return_type {
                    if block_type.is_not(fun.return_type.data_type) {
                        let span = block.last().map_or(block_span, |a| a.span);
                        return Err(crate::Error::MismatchedType {
                            expected: fun.return_type.data_type,
                            found: block_type,
                            spans: vec![r.typ.span(), span],
                        });
                    }
                }

                // Initialize function block data
                checker.funs.init(
                    fun.inner,
                    ast::Fun::new(inner_params, block, checker.scopes.frame_size()),
                );

                Ok(())
            },
        )?;
        // Restore intialization state of variables
        for uninit_var in uninitialized_vars.iter() {
            let var = checker
                .scopes
                .var_mut(uninit_var.ident.ident)
                .expect("variable should exist");
            var.initialized = uninit_var.prev;
        }

        Ok(Ast::statement(AstT::Unit, false, span))
    }

    fn check_fun_call(&mut self, checker: &mut Checker, f: cst::FunCall) -> crate::Result<Ast> {
        let span = f.span();

        let fun = match self.resolve_fun(&checker.scopes, &f.ident)? {
            ResolvedFun::Fun(f) => f,
            ResolvedFun::Builtin(b) => {
                return self.check_builtin_fun_call(checker, b, f.args, span)
            }
        };

        fun.uses.set(fun.uses.get() + 1);

        {
            let found = f.args.items.len();
            let expected = fun.params.len();
            if found < expected {
                return Err(crate::Error::MissingFunArgs {
                    expected,
                    found,
                    span: f.args.r_par.span.before(),
                });
            }
            if found > expected {
                let spans = f
                    .args
                    .items
                    .iter()
                    .skip(expected)
                    .map(|a| a.span())
                    .collect();
                return Err(crate::Error::UnexpectedFunArgs {
                    expected,
                    found,
                    spans,
                });
            }
        }
        let mut args = Vec::with_capacity(f.args.items.len());
        for (p, a) in fun.params.iter().zip(f.args.items.into_iter()) {
            let val = self.check_type(checker, a, true)?;
            let expected = p.data_type;
            let found = expect_expr(&val)?;
            if found.is_not(expected) {
                return Err(crate::Error::MismatchedType {
                    expected,
                    found,
                    spans: vec![p.span, val.span],
                });
            }
            args.push(val);
        }

        Ok(Ast::expr(
            AstT::FunCall(fun.inner, args),
            fun.return_type.data_type,
            false,
            span,
        ))
    }

    fn check_builtin_fun_call(
        &mut self,
        checker: &mut Checker,
        b: BuiltinFun,
        f_args: cst::FunArgs,
        span: Span,
    ) -> crate::Result<Ast> {
        let mut args = Vec::with_capacity(f_args.items.len());
        for a in f_args.items {
            args.push(self.check_type(checker, a, true)?);
        }

        let signatures: &[_] = match b.signatures() {
            SignatureKind::Normal(s) => s,
            SignatureKind::Spill(SpillKind::Global) => {
                // TODO: support non local references
                if !args.is_empty() {
                    return Err(crate::Error::NoMatchingBuiltinFunSignature {
                        name: b.to_string(),
                        args: args
                            .iter()
                            .map(expect_expr)
                            .collect::<crate::Result<Vec<DataType>>>()?,
                        signatures: vec![FunSignature::empty()],
                        span,
                    });
                }
                let vars = self.collect_spill_vars(self.resolve_current_vars(&mut checker.scopes));
                return Ok(Ast::expr(AstT::Spill(vars), DataType::Unit, false, span));
            }
            SignatureKind::Spill(SpillKind::Local) => {
                if !args.is_empty() {
                    return Err(crate::Error::NoMatchingBuiltinFunSignature {
                        name: b.to_string(),
                        args: args
                            .iter()
                            .map(expect_expr)
                            .collect::<crate::Result<Vec<DataType>>>()?,
                        signatures: vec![FunSignature::empty()],
                        span,
                    });
                }
                let vars = self.collect_spill_vars(self.resolve_current_vars(&mut checker.scopes));
                return Ok(Ast::expr(AstT::Spill(vars), DataType::Unit, false, span));
            }
        };

        let mut fun = None;
        'signatures: for (c, s) in signatures {
            let (last, others) = match s.params.split_last() {
                Some(params) => params,
                None if args.is_empty() => {
                    fun = Some((c, s));
                    break 'signatures;
                }
                None => {
                    continue 'signatures;
                }
            };

            if args.len() < others.len() {
                continue 'signatures;
            }
            for (&p, a) in others.iter().zip(args.iter()) {
                let a_data_type = expect_expr(a)?;
                if a_data_type.is_not(p) {
                    continue 'signatures;
                }
            }

            let mut args_iter = args[others.len()..].iter();
            match s.repetition {
                Repetition::One => {
                    if let Some(a) = args_iter.next() {
                        let a_data_type = expect_expr(a)?;
                        if a_data_type.is_not(*last) {
                            continue 'signatures;
                        }
                    } else {
                        continue 'signatures;
                    }
                }
                Repetition::ZeroOrMore => {
                    for a in args_iter {
                        let a_data_type = expect_expr(a)?;
                        if a_data_type.is_not(*last) {
                            continue 'signatures;
                        }
                    }
                }
                Repetition::OneOrMore => {
                    if let Some(a) = args_iter.next() {
                        let a_data_type = expect_expr(a)?;
                        if a_data_type.is_not(*last) {
                            continue 'signatures;
                        }
                    } else {
                        continue 'signatures;
                    }
                    for a in args_iter {
                        let a_data_type = expect_expr(a)?;
                        if a_data_type.is_not(*last) {
                            continue 'signatures;
                        }
                    }
                }
            }

            fun = Some((c, s));
            break 'signatures;
        }
        let (fun, signature) = match fun {
            Some(s) => s,
            None => {
                return Err(crate::Error::NoMatchingBuiltinFunSignature {
                    name: b.to_string(),
                    args: args
                        .iter()
                        .map(expect_expr)
                        .collect::<crate::Result<Vec<DataType>>>()?,
                    signatures: signatures.iter().map(|(_, s)| s.clone()).collect(),
                    span,
                });
            }
        };

        let return_type = signature.return_type;
        Ok(Ast::expr(
            AstT::BuiltinFunCall(*fun, args),
            return_type,
            false,
            span,
        ))
    }

    fn check_return(&mut self, checker: &mut Checker, r: cst::Return) -> crate::Result<Ast> {
        let fun = match checker.scopes.fun_context() {
            Some(f) => f,
            None => return Err(crate::Error::GlobalContextReturn(r.kw.span)),
        };

        let span = r.span();
        let val = match r.val {
            Some(val) => self.check_type(checker, *val, true)?,
            None => Ast::expr(AstT::Unit, DataType::Unit, true, span),
        };

        let data_type = expect_expr(&val)?;
        if data_type.is_not(fun.return_type.data_type) {
            let mut spans = vec![val.span];
            if let Some(s) = fun.return_type.span {
                spans.push(s);
            }
            return Err(crate::Error::MismatchedType {
                expected: fun.return_type.data_type,
                found: data_type,
                spans,
            });
        }

        Ok(Ast::expr(
            AstT::Return(Box::new(val)),
            DataType::Never,
            true,
            span,
        ))
    }

    fn check_var_def(&mut self, checker: &mut Checker, v: cst::VarDef) -> crate::Result<Ast> {
        let span = v.span();

        let mutable = v.mutable.is_some();

        match v.inner {
            cst::VarDefInner::ExplicitAssign { type_hint, value } => {
                let val = self.check_type(checker, *value.1, true)?;
                let val_data_type = expect_expr(&val)?;

                if val.returns {
                    let s = Span::across(v.kw.span, value.0.span);
                    self.warnings.push(crate::Warning::Unreachable(s));
                }

                let data_type = self.resolve_data_type(&type_hint.1)?;
                if val_data_type.is_not(data_type) {
                    return Err(crate::Error::MismatchedType {
                        expected: data_type,
                        found: val_data_type,
                        spans: vec![type_hint.1.span(), val.span],
                    });
                }

                let val_returns = val.returns;
                let var_ref = self.def_var(&mut checker.scopes, v.ident, data_type, true, mutable);
                Ok(Ast::statement(
                    AstT::VarAssign(var_ref, Box::new(val)),
                    val_returns,
                    span,
                ))
            }
            cst::VarDefInner::ImplicitAssign { value } => {
                let val = self.check_type(checker, *value.1, true)?;
                let data_type = expect_expr(&val)?;

                if val.returns {
                    let s = Span::across(v.kw.span, value.0.span);
                    self.warnings.push(crate::Warning::Unreachable(s));
                }

                let val_returns = val.returns;
                let var_ref = self.def_var(&mut checker.scopes, v.ident, data_type, true, mutable);
                Ok(Ast::statement(
                    AstT::VarAssign(var_ref, Box::new(val)),
                    val_returns,
                    span,
                ))
            }
            cst::VarDefInner::Declaration { type_hint } => {
                let data_type = self.resolve_data_type(&type_hint.1)?;
                self.def_var(&mut checker.scopes, v.ident, data_type, false, mutable);

                Ok(Ast::statement(AstT::Unit, false, span))
            }
        }
    }

    fn check_prefix(
        &mut self,
        checker: &mut Checker,
        p: Prefix,
        a: Cst,
        span: Span,
    ) -> crate::Result<Ast> {
        let ast = match p.typ {
            PrefixT::UnaryPlus => {
                let a = self.check_type(checker, a, true)?;
                let a_data_type = expect_expr(&a)?;
                for (_, s) in &op::NEG_SIGNATURES {
                    if s.params[0].is(a_data_type) {
                        return Ok(a);
                    }
                }
                return Err(crate::Error::NoMatchingPrefixSignature {
                    prefix: p.typ,
                    a: a_data_type,
                    signatures: op::NEG_SIGNATURES.iter().map(|(_, s)| s.clone()).collect(),
                    span,
                });
            }
            PrefixT::UnaryMinus => {
                self.check_prefix_signatures(checker, p, a, &op::NEG_SIGNATURES, span)?
            }
            PrefixT::Not => {
                self.check_prefix_signatures(checker, p, a, &op::NOT_SIGNATURES, span)?
            }
        };

        Ok(ast)
    }

    fn check_prefix_signatures(
        &mut self,
        checker: &mut Checker,
        prefix: Prefix,
        arg: Cst,
        signatures: &[(ast::Op, OpSignature<1>)],
        span: Span,
    ) -> crate::Result<Ast> {
        let a = self.check_type(checker, arg, true)?;
        let a_data_type = expect_expr(&a)?;

        let mut op = None;
        for (o, s) in signatures {
            if s.params[0].is(a_data_type) {
                op = Some((o, s));
                break;
            }
        }

        let (op, signature) = match op {
            Some(o) => o,
            None => {
                return Err(crate::Error::NoMatchingPrefixSignature {
                    prefix: prefix.typ,
                    a: a_data_type,
                    signatures: signatures.iter().map(|(_, s)| s.clone()).collect(),
                    span,
                });
            }
        };

        let returns = a.returns;
        Ok(Ast::expr(
            AstT::Op(*op, vec![a]),
            signature.return_type,
            returns,
            span,
        ))
    }

    fn check_postfix(
        &mut self,
        checker: &mut Checker,
        a: Cst,
        p: Postfix,
        span: Span,
    ) -> crate::Result<Ast> {
        let ast = match p.typ {
            PostfixT::Factorial => {
                self.check_postfix_signatures(checker, p, a, &op::FACTORIAL_SIGNATURES, span)?
            }
        };

        Ok(ast)
    }

    fn check_postfix_signatures(
        &mut self,
        checker: &mut Checker,
        postfix: Postfix,
        arg: Cst,
        signatures: &[(ast::Op, OpSignature<1>)],
        span: Span,
    ) -> crate::Result<Ast> {
        let a = self.check_type(checker, arg, true)?;
        let a_data_type = expect_expr(&a)?;

        let mut op = None;
        for (o, s) in signatures {
            if s.params[0].is(a_data_type) {
                op = Some((o, s));
                break;
            }
        }

        let (op, signature) = match op {
            Some(o) => o,
            None => {
                return Err(crate::Error::NoMatchingPostfixSignature {
                    postfix: postfix.typ,
                    a: a_data_type,
                    signatures: signatures.iter().map(|(_, s)| s.clone()).collect(),
                    span,
                });
            }
        };

        let returns = a.returns;
        Ok(Ast::expr(
            AstT::Op(*op, vec![a]),
            signature.return_type,
            returns,
            span,
        ))
    }

    fn check_infix(
        &mut self,
        checker: &mut Checker,
        a: Cst,
        i: Infix,
        b: Cst,
        span: Span,
    ) -> crate::Result<Ast> {
        let ast = match i.typ {
            InfixT::Assign => {
                let ident = match a {
                    Cst::Ident(i) => i,
                    _ => return Err(crate::Error::InvalidAssignment(a.span(), i.span)),
                };

                let expr = self.check_type(checker, b, true)?;
                let expr_data_type = expect_expr(&expr)?;
                let returns = expr.returns;
                let var = match self.resolve_var(&mut checker.scopes, &ident)? {
                    ResolvedVar::Var(v) => v,
                    ResolvedVar::Const(c) => {
                        return Err(crate::Error::ConstAssign((c, ident.span), i.span))
                    }
                };

                if expr_data_type.is_not(var.data_type) {
                    return Err(crate::Error::AssignTypeMismatch(
                        (var.data_type, ident.span),
                        (expr_data_type, expr.span),
                    ));
                }

                let inner = var.inner;
                self.set_var(&mut checker.scopes, &ident, &expr)?;

                Ast::statement(AstT::VarAssign(inner, Box::new(expr)), returns, span)
            }
            InfixT::AddAssign => {
                self.check_infix_assign_signatures(checker, i, (a, b), &op::ADD_SIGNATURES, span)?
            }
            InfixT::SubAssign => {
                self.check_infix_assign_signatures(checker, i, (a, b), &op::SUB_SIGNATURES, span)?
            }
            InfixT::MulAssign => {
                self.check_infix_assign_signatures(checker, i, (a, b), &op::MUL_SIGNATURES, span)?
            }
            InfixT::DivAssign => {
                self.check_infix_assign_signatures(checker, i, (a, b), &op::DIV_SIGNATURES, span)?
            }
            InfixT::RemAssign => {
                self.check_infix_assign_signatures(checker, i, (a, b), &op::REM_SIGNATURES, span)?
            }
            InfixT::OrAssign => {
                self.check_infix_assign_signatures(checker, i, (a, b), &op::OR_SIGNATURES, span)?
            }
            InfixT::AndAssign => {
                self.check_infix_assign_signatures(checker, i, (a, b), &op::AND_SIGNATURES, span)?
            }
            InfixT::BwOrAssign => {
                self.check_infix_assign_signatures(checker, i, (a, b), &op::BW_OR_SIGNATURES, span)?
            }
            InfixT::XorAssign => {
                self.check_infix_assign_signatures(checker, i, (a, b), &op::XOR_SIGNATURES, span)?
            }
            InfixT::BwAndAssign => self.check_infix_assign_signatures(
                checker,
                i,
                (a, b),
                &op::BW_AND_SIGNATURES,
                span,
            )?,
            InfixT::ShlAssign => {
                self.check_infix_assign_signatures(checker, i, (a, b), &op::SHL_SIGNATURES, span)?
            }
            InfixT::ShrAssign => {
                self.check_infix_assign_signatures(checker, i, (a, b), &op::SHR_SIGNATURES, span)?
            }
            InfixT::RangeEx => {
                self.check_infix_signatures(checker, i, (a, b), &op::RANGE_EX_SIGNATURES, span)?
            }
            InfixT::RangeIn => {
                self.check_infix_signatures(checker, i, (a, b), &op::RANGE_IN_SIGNATURES, span)?
            }
            InfixT::Add => {
                self.check_infix_signatures(checker, i, (a, b), &op::ADD_SIGNATURES, span)?
            }
            InfixT::Sub => {
                self.check_infix_signatures(checker, i, (a, b), &op::SUB_SIGNATURES, span)?
            }
            InfixT::Mul => {
                self.check_infix_signatures(checker, i, (a, b), &op::MUL_SIGNATURES, span)?
            }
            InfixT::Div => {
                self.check_infix_signatures(checker, i, (a, b), &op::DIV_SIGNATURES, span)?
            }
            InfixT::Pow => {
                self.check_infix_signatures(checker, i, (a, b), &op::POW_SIGNATURES, span)?
            }
            InfixT::Rem => {
                self.check_infix_signatures(checker, i, (a, b), &op::REM_SIGNATURES, span)?
            }
            InfixT::RemEuclid => {
                self.check_infix_signatures(checker, i, (a, b), &op::REM_EUCLID_SIGNATURES, span)?
            }
            InfixT::Eq => {
                let a = self.check_type(checker, a, true)?;
                let a_t = expect_expr(&a)?;
                let b = self.check_type(checker, b, true)?;
                let b_t = expect_expr(&b)?;
                let returns = a.returns || b.returns;

                if a_t.is_not_comparable_to(b_t) {
                    return Err(crate::Error::NotComparable((a_t, a.span), (b_t, b.span)));
                }
                Ast::expr(
                    AstT::Op(ast::Op::Eq, vec![a, b]),
                    DataType::Bool,
                    returns,
                    span,
                )
            }
            InfixT::Ne => {
                let a = self.check_type(checker, a, true)?;
                let a_t = expect_expr(&a)?;
                let b = self.check_type(checker, b, true)?;
                let b_t = expect_expr(&b)?;
                let returns = a.returns || b.returns;

                if a_t.is_not_comparable_to(b_t) {
                    return Err(crate::Error::NotComparable((a_t, a.span), (b_t, b.span)));
                }
                Ast::expr(
                    AstT::Op(ast::Op::Ne, vec![a, b]),
                    DataType::Bool,
                    returns,
                    span,
                )
            }
            InfixT::Lt => {
                self.check_infix_signatures(checker, i, (a, b), &op::LT_SIGNATURES, span)?
            }
            InfixT::Le => {
                self.check_infix_signatures(checker, i, (a, b), &op::LE_SIGNATURES, span)?
            }
            InfixT::Gt => {
                self.check_infix_signatures(checker, i, (a, b), &op::GT_SIGNATURES, span)?
            }
            InfixT::Ge => {
                self.check_infix_signatures(checker, i, (a, b), &op::GE_SIGNATURES, span)?
            }
            InfixT::Or => {
                self.check_infix_signatures(checker, i, (a, b), &op::OR_SIGNATURES, span)?
            }
            InfixT::And => {
                self.check_infix_signatures(checker, i, (a, b), &op::AND_SIGNATURES, span)?
            }
            InfixT::BwOr => {
                self.check_infix_signatures(checker, i, (a, b), &op::BW_OR_SIGNATURES, span)?
            }
            InfixT::Xor => {
                self.check_infix_signatures(checker, i, (a, b), &op::XOR_SIGNATURES, span)?
            }
            InfixT::BwAnd => {
                self.check_infix_signatures(checker, i, (a, b), &op::BW_AND_SIGNATURES, span)?
            }
            InfixT::Shl => {
                self.check_infix_signatures(checker, i, (a, b), &op::SHL_SIGNATURES, span)?
            }
            InfixT::Shr => {
                self.check_infix_signatures(checker, i, (a, b), &op::SHR_SIGNATURES, span)?
            }
            InfixT::Dot => {
                return Err(crate::Error::NotImplemented(
                    "Field access is not yet implemented",
                    vec![span],
                ))
            }
            InfixT::As => {
                let data_type = self.resolve_data_type(&b)?;

                let a = self.check_type(checker, a, true)?;
                let a_data_type = expect_expr(&a)?;

                if a_data_type == data_type {
                    let s = Span::across(i.span, b.span());
                    self.warnings
                        .push(crate::Warning::UnnecesaryCast(data_type, s));
                    return Ok(a);
                }

                let cast_err = || {
                    Err(crate::Error::CastAlwaysFails(
                        (a_data_type, a.span),
                        (data_type, b.span()),
                    ))
                };

                // This has to be kept in sync with the implementation in `cods/eval/mod.rs`
                let a = match data_type {
                    DataType::Int => match a_data_type {
                        DataType::Int => a,
                        DataType::Float => a,
                        DataType::Char => a,
                        DataType::Any => a,
                        _ => return cast_err(),
                    },
                    DataType::Float => match a_data_type {
                        DataType::Float => a,
                        DataType::Int => a,
                        DataType::Any => a,
                        _ => return cast_err(),
                    },
                    DataType::Bool => match a_data_type {
                        DataType::Bool => a,
                        DataType::Int => a,
                        DataType::Any => a,
                        _ => return cast_err(),
                    },
                    DataType::Char => match a_data_type {
                        DataType::Char => a,
                        DataType::Int => a,
                        DataType::Any => a,
                        _ => return cast_err(),
                    },
                    DataType::Str => match a_data_type {
                        DataType::Str => a,
                        DataType::Any => a,
                        _ => return cast_err(),
                    },
                    DataType::Range => match a_data_type {
                        DataType::Range => a,
                        DataType::Any => a,
                        _ => return cast_err(),
                    },
                    DataType::Unit => match a_data_type {
                        DataType::Unit => a,
                        DataType::Any => a,
                        _ => return cast_err(),
                    },
                    DataType::Any => match a_data_type {
                        DataType::Any => a,
                        _ => a,
                    },
                    DataType::Never => return cast_err(),
                };

                let returns = a.returns;
                Ast::expr(AstT::Cast(Box::new(a), data_type), data_type, returns, span)
            }
            InfixT::Is => {
                let data_type = self.resolve_data_type(&b)?;
                let a = self.check_type(checker, a, true)?;
                let a_data_type = expect_expr(&a)?;

                if a_data_type == data_type {
                    let s = Span::across(i.span, b.span());
                    self.warnings
                        .push(crate::Warning::TypeCheckIsAlwaysTrue(data_type, s));
                }

                let returns = a.returns;

                Ast::expr(
                    AstT::Is(Box::new(a), data_type),
                    DataType::Bool,
                    returns,
                    span,
                )
            }
        };

        Ok(ast)
    }

    fn check_infix_signatures(
        &mut self,
        checker: &mut Checker,
        infix: Infix,
        args: (Cst, Cst),
        signatures: &[(ast::Op, OpSignature<2>)],
        span: Span,
    ) -> crate::Result<Ast> {
        let a = self.check_type(checker, args.0, true)?;
        let a_data_type = expect_expr(&a)?;
        let b = self.check_type(checker, args.1, true)?;
        let b_data_type = expect_expr(&b)?;

        let mut op = None;
        for (o, s) in signatures {
            if a_data_type.is_not(s.params[0]) {
                continue;
            }
            if b_data_type.is_not(s.params[1]) {
                continue;
            }

            op = Some((o, s));
            break;
        }

        let (op, signature) = match op {
            Some(o) => o,
            None => {
                return Err(crate::Error::NoMatchingInfixSignature {
                    infix: infix.typ,
                    a: a_data_type,
                    b: b_data_type,
                    signatures: signatures.iter().map(|(_, s)| s.clone()).collect(),
                    span,
                });
            }
        };

        let returns = a.returns | b.returns;
        Ok(Ast::expr(
            AstT::Op(*op, vec![a, b]),
            signature.return_type,
            returns,
            span,
        ))
    }

    fn check_infix_assign_signatures(
        &mut self,
        checker: &mut Checker,
        infix: Infix,
        args: (Cst, Cst),
        signatures: &[(ast::Op, OpSignature<2>)],
        span: Span,
    ) -> crate::Result<Ast> {
        let ident = match args.0 {
            Cst::Ident(i) => i,
            _ => return Err(crate::Error::InvalidAssignment(args.0.span(), infix.span)),
        };

        let b = self.check_type(checker, args.1, true)?;
        let b_data_type = expect_expr(&b)?;
        let returns = b.returns;
        let var = match self.get_var(&mut checker.scopes, &ident)? {
            ResolvedVar::Var(v) => v,
            ResolvedVar::Const(c) => {
                return Err(crate::Error::ConstAssign((c, ident.span), infix.span))
            }
        };

        let mut op = None;
        for (o, s) in signatures {
            if var.data_type.is_not(s.params[0]) {
                continue;
            }
            if b_data_type.is_not(s.params[1]) {
                continue;
            }

            op = Some((o, s));
        }

        // TODO: add ast::Op::AddAssignInt etc.
        let expr = match op {
            Some((o, s)) => {
                let returns = b.returns;
                let var_expr = Ast::var(var.inner, var.data_type, returns, ident.span);
                Ast::expr(
                    AstT::Op(*o, vec![var_expr, b]),
                    s.return_type,
                    returns,
                    span,
                )
            }
            None => {
                return Err(crate::Error::NoMatchingInfixAssignSignature {
                    infix: infix.typ,
                    a: var.data_type,
                    b: b_data_type,
                    signatures: signatures.iter().map(|(_, s)| s.clone()).collect(),
                    span,
                });
            }
        };

        let inner = var.inner;

        self.set_var(&mut checker.scopes, &ident, &expr)?;

        Ok(Ast::statement(
            AstT::VarAssign(inner, Box::new(expr)),
            returns,
            span,
        ))
    }

    fn resolve_data_type(&self, cst: &Cst) -> crate::Result<DataType> {
        match cst {
            Cst::Ident(ident) => {
                let name = self.idents.name(ident.ident);
                name.parse::<DataType>()
                    .map_err(|_| crate::Error::UnknownType(name.into(), ident.span))
            }
            Cst::Par(_, val, _) if val.is_empty() => Ok(DataType::Unit),
            _ => Err(crate::Error::ExpectedType(cst.span())),
        }
    }

    fn collect_spill_vars(&self, vars: &[Var]) -> Vec<(String, VarRef)> {
        vars.iter()
            .filter(|v| v.initialized == Initialized::Yes)
            .map(|v| {
                let name = self.idents.name(v.ident.ident).to_owned();
                (name, v.inner)
            })
            .collect()
    }
}

fn expect_expr(ast: &Ast) -> crate::Result<DataType> {
    ast.data_type
        .as_expr()
        .ok_or(crate::Error::ExpectedExpr(ast.span))
}
