use std::cmp::max;
use std::rc::Rc;

use crate::cst::{self, Cst};
use crate::{
    Context, IdentSpan, Infix, InfixT, KwT, Postfix, PostfixT, Prefix, PrefixT, Span, VarRef,
};

pub use ast::{Ast, AstT, Asts, BuiltinFunCall};
pub use builtin::*;
pub use op::*;
pub use scope::*;
pub use types::*;

pub mod ast;
mod builtin;
mod op;
mod scope;
#[cfg(test)]
mod test;
mod types;

impl Context {
    pub fn check(&mut self, csts: Vec<Cst>) -> crate::Result<Asts> {
        let mut scopes = Scopes::default();
        self.check_with(&mut scopes, csts)
    }

    pub fn check_with(&mut self, scopes: &mut Scopes, csts: Vec<Cst>) -> crate::Result<Asts> {
        let (asts, _) = self.check_types(scopes, csts, true)?;
        let global_frame_size = scopes.frame_size();

        self.check_unused(scopes.current());

        Ok(Asts {
            asts,
            global_frame_size,
        })
    }

    fn check_types(
        &mut self,
        scopes: &mut Scopes,
        mut csts: Vec<Cst>,
        is_expr: bool,
    ) -> crate::Result<(Vec<Ast>, bool)> {
        for c in csts.iter_mut() {
            if let Cst::FunDef(f) = c {
                if !f.defined {
                    self.check_fun_def_signature(scopes, f)?;
                    f.defined = true;
                }
            }
        }

        let mut asts = Vec::with_capacity(csts.len());
        let mut returns = false;
        if let Some(last) = csts.pop() {
            let mut iter = csts.into_iter();
            for c in iter.by_ref() {
                let ast = self.check_type(scopes, c, false)?;
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
                let ast = self.check_type(scopes, last, is_expr)?;
                if ast.returns {
                    returns = true;
                }
                asts.push(ast);
            }
        }

        Ok((asts, returns))
    }

    fn check_type(&mut self, scopes: &mut Scopes, cst: Cst, is_expr: bool) -> crate::Result<Ast> {
        let span = cst.span();
        let ast = match cst {
            Cst::Empty(s) => Ast::expr(AstT::Unit, DataType::Unit, false, s),
            Cst::Error(s) => Ast::expr(AstT::Error, DataType::Never, false, s),
            Cst::Val(v) => Ast::val(v.val, v.span),
            Cst::Ident(i) => match self.get_var(scopes, &i)? {
                ResolvedVar::Const(c) => Ast::val(c.val(), span),
                ResolvedVar::Var(var) => Ast::var(var.inner, var.data_type, false, i.span),
            },
            Cst::Par(_, c, _) => self.check_par(scopes, *c, is_expr)?,
            Cst::Block(b) => self.check_block(scopes, b, is_expr)?,
            Cst::IfExpr(i) => self.check_if_expr(scopes, i, is_expr)?,
            Cst::WhileLoop(w) => self.check_while_loop(scopes, w)?,
            Cst::ForLoop(f) => self.check_for_loop(scopes, f)?,
            Cst::FunDef(mut f) => {
                if !f.defined {
                    self.check_fun_def_signature(scopes, &f)?;
                    f.defined = true;
                }
                self.check_fun_def_block(scopes, f)?
            }
            Cst::FunCall(f) => self.check_fun_call(scopes, f)?,
            Cst::Return(r) => self.check_return(scopes, r)?,
            Cst::VarDef(v) => self.check_var_def(scopes, v)?,
            Cst::Prefix(p, a) => self.check_prefix(scopes, p, *a, span)?,
            Cst::Postfix(a, p) => self.check_postfix(scopes, *a, p, span)?,
            Cst::Infix(a, i, b) => self.check_infix(scopes, *a, i, *b, span)?,
        };
        Ok(ast)
    }

    fn check_par(&mut self, scopes: &mut Scopes, cst: Cst, is_expr: bool) -> crate::Result<Ast> {
        let ast = self.check_type(scopes, cst, is_expr)?;
        expect_expr(&ast)?;
        Ok(ast)
    }

    fn check_block(
        &mut self,
        scopes: &mut Scopes,
        block: cst::Block,
        is_expr: bool,
    ) -> crate::Result<Ast> {
        let span = block.span();

        let (asts, returns) = self.with_new(scopes, |ctx, scopes| {
            ctx.check_types(scopes, block.csts, is_expr) //
        })?;

        let data_type = asts
            .last()
            .and_then(|a| a.data_type)
            .unwrap_or(DataType::Unit);

        Ok(Ast::expr(AstT::Block(asts), data_type, returns, span))
    }

    fn check_if_expr(
        &mut self,
        scopes: &mut Scopes,
        i: cst::IfExpr,
        is_expr: bool,
    ) -> crate::Result<Ast> {
        let mut cases = Vec::new();
        let mut data_type = DataType::Unit;
        let mut returns = true;
        let span = i.span();

        let init_frame_size = scopes.frame_size();
        let mut max_branch_frame_size = 0;
        let if_block_span = i.if_block.block.span();
        {
            let cond = self.check_cond(scopes, *i.if_block.cond)?;
            if cond.returns {
                let s = Span::across(i.if_block.block.span(), span);
                self.warnings.push(crate::Warning::Unreachable(s));

                data_type = DataType::Never;

                cases.push(ast::CondBlock::new(cond, Vec::new()));
            } else {
                let (block, block_returns) = self.with_new(scopes, |ctx, scopes| {
                    let b = ctx.check_types(scopes, i.if_block.block.csts, is_expr)?;
                    max_branch_frame_size = scopes.frame_size();
                    Ok(b)
                })?;
                scopes.set_frame_size(init_frame_size);

                if is_expr {
                    data_type = block
                        .last()
                        .and_then(|a| a.data_type)
                        .unwrap_or(DataType::Unit);
                }

                if !block_returns {
                    returns = false
                }

                cases.push(ast::CondBlock::new(cond, block));
            }
        }

        for e in i.else_if_blocks {
            let e_span = e.span();
            let cond = self.check_cond(scopes, e.cond)?;
            if cond.returns {
                self.warnings.push(crate::Warning::Unreachable(e_span));

                cases.push(ast::CondBlock::new(cond, Vec::new()));
            } else {
                let block_span = e.block.span();
                let (block, block_returns) = self.with_new(scopes, |ctx, scopes| {
                    let b = ctx.check_types(scopes, e.block.csts, is_expr)?;
                    max_branch_frame_size = max(max_branch_frame_size, scopes.frame_size());
                    Ok(b)
                })?;
                scopes.set_frame_size(init_frame_size);

                if is_expr {
                    let d = block
                        .last()
                        .and_then(|a| a.data_type)
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
        }

        let else_block = if let Some(e) = i.else_block {
            let block_span = e.block.span();
            let (block, block_returns) = self.with_new(scopes, |ctx, scopes| {
                let b = ctx.check_types(scopes, e.block.csts, is_expr)?;
                max_branch_frame_size = max(max_branch_frame_size, scopes.frame_size());
                Ok(b)
            })?;

            if is_expr {
                let d = block
                    .last()
                    .and_then(|a| a.data_type)
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

        scopes.set_frame_size(max_branch_frame_size);

        let if_expr = ast::IfExpr::new(cases, else_block);
        if is_expr {
            Ok(Ast::expr(AstT::IfExpr(if_expr), data_type, returns, span))
        } else {
            Ok(Ast::statement(AstT::IfExpr(if_expr), returns, span))
        }
    }

    fn check_while_loop(&mut self, scopes: &mut Scopes, w: cst::WhileLoop) -> crate::Result<Ast> {
        let span = w.span();

        let cond = self.check_cond(scopes, *w.cond)?;
        let cond_returns = cond.returns;
        let block = if cond.returns {
            let s = w.block.span();
            self.warnings.push(crate::Warning::Unreachable(s));

            Vec::new()
        } else {
            let (block, _) = self.with_new(scopes, |ctx, scopes| {
                ctx.check_types(scopes, w.block.csts, false) //
            })?;
            block
        };

        let whl_loop = ast::WhileLoop::new(Box::new(cond), block);
        Ok(Ast::statement(
            AstT::WhileLoop(whl_loop),
            cond_returns,
            span,
        ))
    }

    fn check_cond(&mut self, scopes: &mut Scopes, c: Cst) -> crate::Result<Ast> {
        let cond = self.check_type(scopes, c, true)?;
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

    fn check_for_loop(&mut self, scopes: &mut Scopes, f: cst::ForLoop) -> crate::Result<Ast> {
        let span = f.span();

        let iter = self.check_type(scopes, *f.iter, true)?;
        let iter_data_type = expect_expr(&iter)?;
        if iter_data_type.is_not(DataType::Range) {
            return Err(crate::Error::NotIterable(iter_data_type, iter.span));
        }
        let iter_type = DataType::Int;

        let (inner, block) = self.with_new(scopes, |ctx, scopes| {
            let inner = ctx.def_var(scopes, f.ident, iter_type, true, false);
            let (block, _) = ctx.check_types(scopes, f.block.csts, false)?;
            Ok((inner, block))
        })?;

        let for_loop = ast::ForLoop::new(inner, Box::new(iter), block);
        Ok(Ast::statement(AstT::ForLoop(for_loop), false, span))
    }

    fn check_fun_def_signature(
        &mut self,
        scopes: &mut Scopes,
        f: &cst::FunDef,
    ) -> crate::Result<()> {
        let mut params = Vec::with_capacity(f.params.items.len());
        for p in f.params.items.iter() {
            let typ = self.resolve_data_type(&p.typ)?;
            let span = Span::across(p.ident.span, p.typ.span);
            params.push(FunParam::new(p.ident, typ, span));
        }

        let return_type = f
            .return_type
            .as_ref()
            .map_or(Ok(DataType::Unit), |r| self.resolve_data_type(&r.typ))?;

        // Define fun before checking block to support recursive calls
        let inner = Rc::new(ast::Fun::default());
        let ret = ReturnType::new(return_type, f.return_type.as_ref().map(|r| r.typ.span));
        let fun = Fun::new(f.ident, params, ret, Rc::clone(&inner));
        self.def_fun(scopes, fun)?;

        Ok(())
    }

    fn check_fun_def_block(&mut self, scopes: &mut Scopes, f: cst::FunDef) -> crate::Result<Ast> {
        let span = f.span();
        let block_span = f.block.span();

        let fun = match self.resolve_fun(scopes, &f.ident)? {
            ResolvedFun::Fun(f) => f,
            ResolvedFun::Builtin(_) => return Ok(Ast::statement(AstT::Unit, false, span)),
        };

        self.with_new_frame(scopes, Rc::clone(&fun), |ctx, scopes| {
            let mut inner_params = Vec::new();
            for p in fun.params.iter() {
                let param = ctx.def_var(scopes, p.ident, p.data_type, true, false);
                inner_params.push(param);
            }

            // Check function block
            let is_expr = f.return_type.is_some();
            let (block, _) = ctx.check_types(scopes, f.block.csts, is_expr)?;

            let block_type = block
                .last()
                .and_then(|a| a.data_type)
                .unwrap_or(DataType::Unit);

            if let Some(r) = f.return_type {
                if block_type.is_not(fun.return_type.data_type) {
                    let span = block.last().map_or(block_span, |a| a.span);
                    return Err(crate::Error::MismatchedType {
                        expected: fun.return_type.data_type,
                        found: block_type,
                        spans: vec![r.typ.span, span],
                    });
                }
            }

            // Initialize function
            fun.inner.init(inner_params, block, scopes.frame_size());

            Ok(())
        })?;

        Ok(Ast::statement(AstT::Unit, false, span))
    }

    fn check_fun_call(&mut self, scopes: &mut Scopes, f: cst::FunCall) -> crate::Result<Ast> {
        let span = f.span();

        let fun = match self.resolve_fun(scopes, &f.ident)? {
            ResolvedFun::Fun(f) => f,
            ResolvedFun::Builtin(b) => return self.check_builtin_fun_call(scopes, b, f.args, span),
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
            let val = self.check_type(scopes, a, true)?;
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
            AstT::FunCall(Rc::clone(&fun.inner), args),
            fun.return_type.data_type,
            false,
            span,
        ))
    }

    fn check_builtin_fun_call(
        &mut self,
        scopes: &mut Scopes,
        b: BuiltinFun,
        f_args: cst::FunArgs,
        span: Span,
    ) -> crate::Result<Ast> {
        let mut args = Vec::with_capacity(f_args.items.len());
        for a in f_args.items {
            args.push(self.check_type(scopes, a, true)?);
        }

        let signatures: &[_] = match b {
            BuiltinFun::Pow => &builtin::POW_SIGNATURES,
            BuiltinFun::Ln => &builtin::LN_SIGNATURES,
            BuiltinFun::Log => &builtin::LOG_SIGNATURES,
            BuiltinFun::Sqrt => &builtin::SQRT_SIGNATURES,
            BuiltinFun::Ncr => &builtin::NCR_SIGNATURES,
            BuiltinFun::ToDeg => &builtin::TO_DEG_SIGNATURES,
            BuiltinFun::ToRad => &builtin::TO_RAD_SIGNATURES,
            BuiltinFun::Sin => &builtin::SIN_SIGNATURES,
            BuiltinFun::Cos => &builtin::COS_SIGNATURES,
            BuiltinFun::Tan => &builtin::TAN_SIGNATURES,
            BuiltinFun::Asin => &builtin::ASIN_SIGNATURES,
            BuiltinFun::Acos => &builtin::ACOS_SIGNATURES,
            BuiltinFun::Atan => &builtin::ATAN_SIGNATURES,
            BuiltinFun::Gcd => &builtin::GCD_SIGNATURES,
            BuiltinFun::Min => &builtin::MIN_SIGNATURES,
            BuiltinFun::Max => &builtin::MAX_SIGNATURES,
            BuiltinFun::Clamp => &builtin::CLAMP_SIGNATURES,
            BuiltinFun::Abs => &builtin::ABS_SIGNATURES,
            BuiltinFun::Print => &builtin::PRINT_SIGNATURES,
            BuiltinFun::Println => &builtin::PRINTLN_SIGNATURES,
            BuiltinFun::Assert => &builtin::ASSERT_SIGNATURES,
            BuiltinFun::AssertEq => &builtin::ASSERT_EQ_SIGNATURES,
            BuiltinFun::Spill => {
                // XXX
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
                let vars = self.collect_spill_vars(scopes.current().vars());
                return Ok(Ast::expr(AstT::Spill(vars), DataType::Unit, false, span));
            }
            BuiltinFun::SpillLocal => {
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
                let vars = self.collect_spill_vars(scopes.current().vars());
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

    fn check_return(&mut self, scopes: &mut Scopes, r: cst::Return) -> crate::Result<Ast> {
        let fun = match scopes.fun_context() {
            Some(f) => f,
            None => return Err(crate::Error::GlobalContextReturn(r.kw.span)),
        };

        let span = r.span();
        let val = match r.val {
            Some(val) => self.check_type(scopes, *val, true)?,
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

    fn check_var_def(&mut self, scopes: &mut Scopes, v: cst::VarDef) -> crate::Result<Ast> {
        let span = v.span();
        let val = self.check_type(scopes, *v.value.1, true)?;
        let val_data_type = expect_expr(&val)?;

        if val.returns {
            let s = Span::across(v.kw.span, v.value.0.span);
            self.warnings.push(crate::Warning::Unreachable(s));
        }

        let data_type = match v.type_hint {
            Some((_, t)) => {
                let data_type = self.resolve_data_type(&t)?;
                if val_data_type.is_not(data_type) {
                    return Err(crate::Error::MismatchedType {
                        expected: data_type,
                        found: val_data_type,
                        spans: vec![t.span, val.span],
                    });
                }
                data_type
            }
            None => val_data_type,
        };

        let mutable = v.kw.typ == KwT::Var;
        let inner = self.def_var(scopes, v.ident, data_type, true, mutable);

        let val_returns = val.returns;
        Ok(Ast::statement(
            AstT::VarAssign(inner, Box::new(val)),
            val_returns,
            span,
        ))
    }

    fn check_prefix(
        &mut self,
        scopes: &mut Scopes,
        p: Prefix,
        a: Cst,
        span: Span,
    ) -> crate::Result<Ast> {
        let ast = match p.typ {
            PrefixT::UnaryPlus => {
                let a = self.check_type(scopes, a, true)?;
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
                self.check_prefix_signatures(scopes, p, a, &op::NEG_SIGNATURES, span)?
            }
            PrefixT::Not => {
                self.check_prefix_signatures(scopes, p, a, &op::NOT_SIGNATURES, span)?
            }
        };

        Ok(ast)
    }

    fn check_prefix_signatures(
        &mut self,
        scopes: &mut Scopes,
        prefix: Prefix,
        arg: Cst,
        signatures: &[(ast::Op, OpSignature<1>)],
        span: Span,
    ) -> crate::Result<Ast> {
        let a = self.check_type(scopes, arg, true)?;
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
        scopes: &mut Scopes,
        a: Cst,
        p: Postfix,
        span: Span,
    ) -> crate::Result<Ast> {
        let ast = match p.typ {
            PostfixT::Factorial => {
                self.check_postfix_signatures(scopes, p, a, &op::FACTORIAL_SIGNATURES, span)?
            }
        };

        Ok(ast)
    }

    fn check_postfix_signatures(
        &mut self,
        scopes: &mut Scopes,
        postfix: Postfix,
        arg: Cst,
        signatures: &[(ast::Op, OpSignature<1>)],
        span: Span,
    ) -> crate::Result<Ast> {
        let a = self.check_type(scopes, arg, true)?;
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
        scopes: &mut Scopes,
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

                let expr = self.check_type(scopes, b, true)?;
                let expr_data_type = expect_expr(&expr)?;
                let returns = expr.returns;
                let var = match self.resolve_var(scopes, &ident)? {
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
                self.set_var(scopes, &ident, &expr)?;

                Ast::statement(AstT::VarAssign(inner, Box::new(expr)), returns, span)
            }
            InfixT::AddAssign => {
                self.check_infix_assign_signatures(scopes, i, (a, b), &op::ADD_SIGNATURES, span)?
            }
            InfixT::SubAssign => {
                self.check_infix_assign_signatures(scopes, i, (a, b), &op::SUB_SIGNATURES, span)?
            }
            InfixT::MulAssign => {
                self.check_infix_assign_signatures(scopes, i, (a, b), &op::MUL_SIGNATURES, span)?
            }
            InfixT::DivAssign => {
                self.check_infix_assign_signatures(scopes, i, (a, b), &op::DIV_SIGNATURES, span)?
            }
            InfixT::RangeEx => {
                self.check_infix_signatures(scopes, i, (a, b), &op::RANGE_EX_SIGNATURES, span)?
            }
            InfixT::RangeIn => {
                self.check_infix_signatures(scopes, i, (a, b), &op::RANGE_IN_SIGNATURES, span)?
            }
            InfixT::Add => {
                self.check_infix_signatures(scopes, i, (a, b), &op::ADD_SIGNATURES, span)?
            }
            InfixT::Sub => {
                self.check_infix_signatures(scopes, i, (a, b), &op::SUB_SIGNATURES, span)?
            }
            InfixT::Mul => {
                self.check_infix_signatures(scopes, i, (a, b), &op::MUL_SIGNATURES, span)?
            }
            InfixT::Div => {
                self.check_infix_signatures(scopes, i, (a, b), &op::DIV_SIGNATURES, span)?
            }
            InfixT::Rem => {
                self.check_infix_signatures(scopes, i, (a, b), &op::REM_SIGNATURES, span)?
            }
            InfixT::RemEuclid => {
                self.check_infix_signatures(scopes, i, (a, b), &op::REM_EUCLID_SIGNATURES, span)?
            }
            InfixT::Eq => {
                let a = self.check_type(scopes, a, true)?;
                let a_t = expect_expr(&a)?;
                let b = self.check_type(scopes, b, true)?;
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
                let a = self.check_type(scopes, a, true)?;
                let a_t = expect_expr(&a)?;
                let b = self.check_type(scopes, b, true)?;
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
                self.check_infix_signatures(scopes, i, (a, b), &op::LT_SIGNATURES, span)?
            }
            InfixT::Le => {
                self.check_infix_signatures(scopes, i, (a, b), &op::LE_SIGNATURES, span)?
            }
            InfixT::Gt => {
                self.check_infix_signatures(scopes, i, (a, b), &op::GT_SIGNATURES, span)?
            }
            InfixT::Ge => {
                self.check_infix_signatures(scopes, i, (a, b), &op::GE_SIGNATURES, span)?
            }
            InfixT::Or => {
                self.check_infix_signatures(scopes, i, (a, b), &op::OR_SIGNATURES, span)?
            }
            InfixT::And => {
                self.check_infix_signatures(scopes, i, (a, b), &op::AND_SIGNATURES, span)?
            }
            InfixT::BwOr => {
                self.check_infix_signatures(scopes, i, (a, b), &op::BW_OR_SIGNATURES, span)?
            }
            InfixT::BwAnd => {
                self.check_infix_signatures(scopes, i, (a, b), &op::BW_AND_SIGNATURES, span)?
            }
            InfixT::Dot => {
                return Err(crate::Error::NotImplemented(
                    "Field access is not yet implemented",
                    vec![span],
                ))
            }
            InfixT::As => {
                let ident = match b {
                    Cst::Ident(i) => i,
                    _ => return Err(crate::Error::ExpectedIdent(b.span())),
                };
                let data_type = self.resolve_data_type(&ident)?;
                let a = self.check_type(scopes, a, true)?;
                let a_data_type = expect_expr(&a)?;

                if a_data_type == data_type {
                    let s = Span::across(i.span, ident.span);
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

                let a = match data_type {
                    DataType::Int => match a_data_type {
                        DataType::Int => a,
                        DataType::Float => a,
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
                Ast::expr(
                    AstT::Cast(Box::new(a), data_type),
                    data_type,
                    returns,
                    span,
                )
            }
            InfixT::Is => {
                let ident = match b {
                    Cst::Ident(i) => i,
                    _ => return Err(crate::Error::ExpectedIdent(b.span())),
                };
                let data_type = self.resolve_data_type(&ident)?;
                let a = self.check_type(scopes, a, true)?;
                let a_data_type = expect_expr(&a)?;

                if a_data_type == data_type {
                    let s = Span::across(i.span, ident.span);
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
        scopes: &mut Scopes,
        infix: Infix,
        args: (Cst, Cst),
        signatures: &[(ast::Op, OpSignature<2>)],
        span: Span,
    ) -> crate::Result<Ast> {
        let a = self.check_type(scopes, args.0, true)?;
        let a_data_type = expect_expr(&a)?;
        let b = self.check_type(scopes, args.1, true)?;
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
        scopes: &mut Scopes,
        infix: Infix,
        args: (Cst, Cst),
        signatures: &[(ast::Op, OpSignature<2>)],
        span: Span,
    ) -> crate::Result<Ast> {
        let ident = match args.0 {
            Cst::Ident(i) => i,
            _ => return Err(crate::Error::InvalidAssignment(args.0.span(), infix.span)),
        };

        let b = self.check_type(scopes, args.1, true)?;
        let b_data_type = expect_expr(&b)?;
        let returns = b.returns;
        let var = match self.get_var(scopes, &ident)? {
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

        self.set_var(scopes, &ident, &expr)?;

        Ok(Ast::statement(
            AstT::VarAssign(inner, Box::new(expr)),
            returns,
            span,
        ))
    }

    fn resolve_data_type(&self, typ: &IdentSpan) -> crate::Result<DataType> {
        let name = self.idents.name(typ.ident);
        name.parse::<DataType>()
            .map_err(|_| crate::Error::UnknownType(name.into(), typ.span))
    }

    fn collect_spill_vars<'a>(
        &self,
        var_iter: impl Iterator<Item = &'a Var>,
    ) -> Vec<(String, VarRef)> {
        let mut vars: Vec<_> = var_iter
            .filter(|v| v.assigned)
            .map(|v| {
                let name = self.idents.name(v.ident.ident).to_owned();
                (name, v.inner)
            })
            .collect();

        vars.sort();
        vars
    }
}

fn expect_expr(ast: &Ast) -> crate::Result<DataType> {
    ast.data_type.ok_or(crate::Error::ExpectedExpr(ast.span))
}
