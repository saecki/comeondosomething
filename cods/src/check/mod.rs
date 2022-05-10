use std::cmp::max;
use std::rc::Rc;

use crate::cst::{self, Cst};
use crate::{
    BuiltinFun, Context, IdentSpan, Infix, InfixT, KwT, Postfix, PostfixT, Prefix, PrefixT, Span,
    ValSpan, VarRef,
};

pub use ast::{Ast, AstT, Asts, BoolExpr, BuiltinFunCall, FloatExpr, IntExpr, RangeExpr, StrExpr};
use builtin::*;
pub use scope::*;
pub use types::*;

pub mod ast;
mod builtin;
mod scope;
#[cfg(test)]
mod test;
mod types;

impl Context {
    pub fn check(&mut self, csts: Vec<Cst>) -> crate::Result<Asts> {
        let mut scopes = Scopes::default();
        self.check_with(csts, &mut scopes)
    }

    pub fn check_with(&mut self, csts: Vec<Cst>, scopes: &mut Scopes) -> crate::Result<Asts> {
        let asts = self.check_types(scopes, csts, true)?;
        let global_frame_size = scopes.frame_size();
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
    ) -> crate::Result<Vec<Ast>> {
        for c in csts.iter_mut() {
            if let Cst::FunDef(f) = c {
                if !f.defined {
                    self.check_fun_def_signature(scopes, f)?;
                    f.defined = true;
                }
            }
        }

        let mut asts = Vec::with_capacity(csts.len());
        if let Some(last) = csts.pop() {
            for c in csts {
                let ast = self.check_type(scopes, c, false)?;
                asts.push(ast);
            }

            let ast = self.check_type(scopes, last, is_expr)?;
            asts.push(ast);
        }

        Ok(asts)
    }

    fn check_type(&mut self, scopes: &mut Scopes, cst: Cst, is_expr: bool) -> crate::Result<Ast> {
        let span = cst.span();
        let ast = match cst {
            Cst::Empty(s) => Ast::expr(AstT::Unit, DataType::Unit, s),
            Cst::Error(s) => Ast::expr(AstT::Error, DataType::Unit, s),
            Cst::Val(v) => Ast::val(v),
            Cst::Ident(i) => match self.get_var(scopes, &i)? {
                ResolvedVar::Const(c) => Ast::val(ValSpan::new(c.val(), span)),
                ResolvedVar::Var(var) => Ast::var(var.inner, var.data_type, i.span),
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

        let asts = scopes.with_new(|scopes| {
            self.check_types(scopes, block.csts, is_expr) //
        })?;

        let data_type = asts
            .last()
            .and_then(|a| a.data_type)
            .unwrap_or(DataType::Unit);

        Ok(Ast::expr(AstT::Block(asts), data_type, span))
    }

    fn check_if_expr(
        &mut self,
        scopes: &mut Scopes,
        i: cst::IfExpr,
        is_expr: bool,
    ) -> crate::Result<Ast> {
        let mut cases = Vec::new();
        let mut data_type = DataType::Unit;
        let span = i.span();

        let init_frame_size = scopes.frame_size();
        let mut max_branch_frame_size = 0;
        let if_block_span = i.if_block.block.span();
        {
            let cond = self.check_cond(scopes, *i.if_block.cond)?;
            let block = scopes.with_new(|scopes| {
                let b = self.check_types(scopes, i.if_block.block.csts, is_expr)?;
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

            cases.push(ast::CondBlock::new(cond, block));
        }

        for e in i.else_if_blocks {
            let cond = self.check_cond(scopes, e.cond)?;

            let block_span = e.block.span();
            let block = scopes.with_new(|scopes| {
                let b = self.check_types(scopes, e.block.csts, is_expr)?;
                max_branch_frame_size = max(max_branch_frame_size, scopes.frame_size());
                Ok(b)
            })?;
            scopes.set_frame_size(init_frame_size);

            if is_expr {
                let d = block
                    .last()
                    .and_then(|a| a.data_type)
                    .unwrap_or(DataType::Unit);
                if d.is_not(data_type) {
                    let if_expr_span = cases[0].block.last().map_or(if_block_span, |a| a.span);
                    let else_expr_span = block.last().map_or(block_span, |a| a.span);
                    return Err(crate::Error::IfBranchIncompatibleType(
                        (data_type, if_expr_span),
                        (d, else_expr_span),
                    ));
                }
            }

            cases.push(ast::CondBlock::new(cond, block));
        }

        let else_block = if let Some(e) = i.else_block {
            let block_span = e.block.span();
            let block = scopes.with_new(|scopes| {
                let b = self.check_types(scopes, e.block.csts, is_expr)?;
                max_branch_frame_size = max(max_branch_frame_size, scopes.frame_size());
                Ok(b)
            })?;

            if is_expr {
                let d = block
                    .last()
                    .and_then(|a| a.data_type)
                    .unwrap_or(DataType::Unit);
                if d.is_not(data_type) {
                    let if_expr_span = cases[0].block.last().map_or(if_block_span, |a| a.span);
                    let else_expr_span = block.last().map_or(block_span, |a| a.span);
                    return Err(crate::Error::IfBranchIncompatibleType(
                        (data_type, if_expr_span),
                        (d, else_expr_span),
                    ));
                }
            }

            Some(block)
        } else if is_expr && data_type.is_not(DataType::Unit) {
            return Err(crate::Error::MissingElseBranch(data_type, span));
        } else {
            None
        };

        scopes.set_frame_size(max_branch_frame_size);

        let if_expr = ast::IfExpr::new(cases, else_block);
        if is_expr {
            Ok(Ast::expr(AstT::IfExpr(if_expr), data_type, span))
        } else {
            Ok(Ast::statement(AstT::IfExpr(if_expr), span))
        }
    }

    fn check_while_loop(&mut self, scopes: &mut Scopes, w: cst::WhileLoop) -> crate::Result<Ast> {
        let span = w.span();

        let cond = self.check_cond(scopes, *w.cond)?;
        let block = scopes.with_new(|scopes| {
            self.check_types(scopes, w.block.csts, false) //
        })?;

        let whl_loop = ast::WhileLoop::new(Box::new(cond), block);
        Ok(Ast::statement(AstT::WhileLoop(whl_loop), span))
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

        let (inner, block) = scopes.with_new(|scopes| {
            let inner = self.def_var(scopes, f.ident, iter_type, true, false);
            let block = self.check_types(scopes, f.block.csts, false)?;
            Ok((inner, block))
        })?;

        let for_loop = ast::ForLoop::new(inner, Box::new(iter), block);
        Ok(Ast::statement(AstT::ForLoop(for_loop), span))
    }

    fn check_fun_def_signature(
        &mut self,
        scopes: &mut Scopes,
        f: &cst::FunDef,
    ) -> crate::Result<()> {
        // Function signature
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
        let inner = Rc::new(ast::Fun::new());
        let fun = Fun::new(f.ident, params, return_type, Rc::clone(&inner));
        self.def_fun(scopes, fun)?;

        Ok(())
    }

    fn check_fun_def_block(&mut self, scopes: &mut Scopes, f: cst::FunDef) -> crate::Result<Ast> {
        let span = f.span();
        let block_span = f.block.span();

        let fun = match self.resolve_fun(scopes, &f.ident)? {
            ResolvedFun::Fun(f) => f,
            ResolvedFun::Builtin(_) => panic!("Expected user defined function"),
        };

        scopes.with_new_frame(|scopes| {
            let mut inner_params = Vec::new();
            for p in fun.params.iter() {
                let param = self.def_var(scopes, p.ident, p.data_type, true, false);
                inner_params.push(param);
            }

            // Check function block
            let is_expr = f.return_type.is_some();
            let block = self.check_types(scopes, f.block.csts, is_expr)?;

            let block_type = block
                .last()
                .and_then(|a| a.data_type)
                .unwrap_or(DataType::Unit);

            if let Some(r) = f.return_type {
                if block_type.is_not(fun.return_type) {
                    let span = block.last().map_or(block_span, |a| a.span);
                    return Err(crate::Error::MismatchedType {
                        expected: fun.return_type,
                        found: block_type,
                        spans: vec![r.typ.span, span],
                    });
                }
            }

            // Initialize function
            fun.inner.init(inner_params, block, scopes.frame_size());

            Ok(())
        })?;

        Ok(Ast::statement(AstT::Unit, span))
    }

    fn check_fun_call(&mut self, scopes: &mut Scopes, f: cst::FunCall) -> crate::Result<Ast> {
        let span = f.span();

        let fun = match self.resolve_fun(scopes, &f.ident)? {
            ResolvedFun::Fun(f) => f,
            ResolvedFun::Builtin(b) => return self.check_builtin_fun_call(scopes, b, f.args, span),
        };

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
            fun.return_type,
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

        let mut fun = None;
        'signatures: for (c, s) in b.signatures() {
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
            None => return Err(crate::Error::NoMatchingSignature(b, span)),
        };

        match fun {
            BuiltinFunCall::Spill => {
                // XXX
                // TODO: support non local references
                let vars = self.collect_spill_vars(scopes.current().vars());
                return Ok(Ast::expr(AstT::Spill(vars), signature.return_type, span));
            }
            BuiltinFunCall::SpillLocal => {
                let vars = self.collect_spill_vars(scopes.current().vars());
                return Ok(Ast::expr(AstT::Spill(vars), signature.return_type, span));
            }
            _ => (),
        }

        let return_type = signature.return_type;
        Ok(Ast::expr(
            AstT::BuiltinFunCall(*fun, args),
            return_type,
            span,
        ))
    }

    fn check_var_def(&mut self, scopes: &mut Scopes, v: cst::VarDef) -> crate::Result<Ast> {
        let span = v.span();
        let val = self.check_type(scopes, *v.value.1, true)?;
        let val_data_type = expect_expr(&val)?;

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

        Ok(Ast::statement(AstT::VarDef(inner, Box::new(val)), span))
    }

    fn check_prefix(
        &mut self,
        scopes: &mut Scopes,
        p: Prefix,
        a: Cst,
        span: Span,
    ) -> crate::Result<Ast> {
        let a = self.check_type(scopes, a, true)?;
        let a_data_type = expect_expr(&a)?;

        let ast = match (p.typ, a_data_type) {
            (PrefixT::UnaryPlus, DataType::Int) => a,
            (PrefixT::UnaryPlus, DataType::Float) => a,
            (PrefixT::UnaryMinus, DataType::Int) => Ast::int(IntExpr::Neg(Box::new(a)), span),
            (PrefixT::UnaryMinus, DataType::Float) => Ast::float(FloatExpr::Neg(Box::new(a)), span),
            (PrefixT::Not, DataType::Bool) => Ast::bool(BoolExpr::Not(Box::new(a)), span),
            _ => return Err(crate::Error::PrefixNotApplicable(p, (a_data_type, a.span))),
        };

        Ok(ast)
    }

    fn check_postfix(
        &mut self,
        scopes: &mut Scopes,
        a: Cst,
        p: Postfix,
        span: Span,
    ) -> crate::Result<Ast> {
        let a = self.check_type(scopes, a, true)?;
        let a_data_type = expect_expr(&a)?;

        let ast = match (p.typ, a_data_type) {
            (PostfixT::Factorial, DataType::Int) => Ast::int(IntExpr::Factorial(Box::new(a)), span),
            _ => return Err(crate::Error::PostfixNotApplicable((a_data_type, a.span), p)),
        };

        Ok(ast)
    }

    fn check_infix(
        &mut self,
        scopes: &mut Scopes,
        a: Cst,
        i: Infix,
        b: Cst,
        s: Span,
    ) -> crate::Result<Ast> {
        fn infix_error(
            a_t: DataType,
            a: Ast,
            i: Infix,
            b_t: DataType,
            b: Ast,
        ) -> crate::Result<Ast> {
            Err(crate::Error::InfixNotApplicable(
                (a_t, a.span),
                i,
                (b_t, b.span),
            ))
        }

        let ast = match i.typ {
            InfixT::Assign => {
                let ident = match a {
                    Cst::Ident(i) => i,
                    _ => return Err(crate::Error::InvalidAssignment(a.span(), i.span)),
                };

                let expr = self.check_type(scopes, b, true)?;
                let expr_data_type = expect_expr(&expr)?;
                let var = match self.resolve_var(scopes, &ident)? {
                    ResolvedVar::Var(v) => v,
                    ResolvedVar::Const(c) => {
                        return Err(crate::Error::ConstAssign((c, ident.span), i.span))
                    }
                };

                if expr_data_type.is_not(var.data_type) {
                    return Err(crate::Error::AssignNotApplicable(
                        (var.data_type, ident.span),
                        (expr_data_type, expr.span),
                    ));
                }

                let inner = var.inner;
                self.set_var(scopes, &ident, &expr)?;

                Ast::statement(AstT::Assign(inner, Box::new(expr)), s)
            }
            InfixT::AddAssign => {
                self.check_infix_assign(scopes, a, i, b, IntExpr::Add, FloatExpr::Add, s)?
            }
            InfixT::SubAssign => {
                self.check_infix_assign(scopes, a, i, b, IntExpr::Sub, FloatExpr::Sub, s)?
            }
            InfixT::MulAssign => {
                self.check_infix_assign(scopes, a, i, b, IntExpr::Mul, FloatExpr::Mul, s)?
            }
            InfixT::DivAssign => {
                self.check_infix_assign(scopes, a, i, b, IntExpr::Div, FloatExpr::Div, s)?
            }
            InfixT::RangeEx => {
                let a = self.check_type(scopes, a, true)?;
                let b = self.check_type(scopes, b, true)?;
                match (expect_expr(&a)?, expect_expr(&b)?) {
                    (DataType::Int, DataType::Int) => {
                        Ast::range(RangeExpr::Ex(Box::new(a), Box::new(b)), s)
                    }
                    (a_t, b_t) => return infix_error(a_t, a, i, b_t, b),
                }
            }
            InfixT::RangeIn => {
                let a = self.check_type(scopes, a, true)?;
                let b = self.check_type(scopes, b, true)?;
                match (expect_expr(&a)?, expect_expr(&b)?) {
                    (DataType::Int, DataType::Int) => {
                        Ast::range(RangeExpr::In(Box::new(a), Box::new(b)), s)
                    }
                    (a_t, b_t) => return infix_error(a_t, a, i, b_t, b),
                }
            }
            InfixT::Add => {
                let a = self.check_type(scopes, a, true)?;
                let b = self.check_type(scopes, b, true)?;
                match (expect_expr(&a)?, expect_expr(&b)?) {
                    (DataType::Int, DataType::Int) => {
                        Ast::int(IntExpr::Add(Box::new(a), Box::new(b)), s)
                    }
                    (DataType::Float, DataType::Float) => {
                        Ast::float(FloatExpr::Add(Box::new(a), Box::new(b)), s)
                    }
                    (a_t, b_t) => return infix_error(a_t, a, i, b_t, b),
                }
            }
            InfixT::Sub => {
                let a = self.check_type(scopes, a, true)?;
                let b = self.check_type(scopes, b, true)?;
                match (expect_expr(&a)?, expect_expr(&b)?) {
                    (DataType::Int, DataType::Int) => {
                        Ast::int(IntExpr::Sub(Box::new(a), Box::new(b)), s)
                    }
                    (DataType::Float, DataType::Float) => {
                        Ast::float(FloatExpr::Sub(Box::new(a), Box::new(b)), s)
                    }
                    (a_t, b_t) => return infix_error(a_t, a, i, b_t, b),
                }
            }
            InfixT::Mul => {
                let a = self.check_type(scopes, a, true)?;
                let b = self.check_type(scopes, b, true)?;
                match (expect_expr(&a)?, expect_expr(&b)?) {
                    (DataType::Int, DataType::Int) => {
                        Ast::int(IntExpr::Mul(Box::new(a), Box::new(b)), s)
                    }
                    (DataType::Float, DataType::Float) => {
                        Ast::float(FloatExpr::Mul(Box::new(a), Box::new(b)), s)
                    }
                    (a_t, b_t) => return infix_error(a_t, a, i, b_t, b),
                }
            }
            InfixT::Div => {
                let a = self.check_type(scopes, a, true)?;
                let b = self.check_type(scopes, b, true)?;
                match (expect_expr(&a)?, expect_expr(&b)?) {
                    (DataType::Int, DataType::Int) => {
                        Ast::int(IntExpr::Div(Box::new(a), Box::new(b)), s)
                    }
                    (DataType::Float, DataType::Float) => {
                        Ast::float(FloatExpr::Div(Box::new(a), Box::new(b)), s)
                    }
                    (a_t, b_t) => return infix_error(a_t, a, i, b_t, b),
                }
            }
            InfixT::Rem => {
                let a = self.check_type(scopes, a, true)?;
                let b = self.check_type(scopes, b, true)?;
                match (expect_expr(&a)?, expect_expr(&b)?) {
                    (DataType::Int, DataType::Int) => {
                        Ast::int(IntExpr::Rem(Box::new(a), Box::new(b)), s)
                    }
                    (a_t, b_t) => return infix_error(a_t, a, i, b_t, b),
                }
            }
            InfixT::RemEuclid => {
                let a = self.check_type(scopes, a, true)?;
                let b = self.check_type(scopes, b, true)?;
                match (expect_expr(&a)?, expect_expr(&b)?) {
                    (DataType::Int, DataType::Int) => {
                        Ast::int(IntExpr::RemEuclid(Box::new(a), Box::new(b)), s)
                    }
                    (a_t, b_t) => return infix_error(a_t, a, i, b_t, b),
                }
            }
            InfixT::Eq => {
                let a = self.check_type(scopes, a, true)?;
                let a_t = expect_expr(&a)?;
                let b = self.check_type(scopes, b, true)?;
                let b_t = expect_expr(&b)?;

                if a_t.is_not_comparable_to(b_t) {
                    return infix_error(a_t, a, i, b_t, b);
                }
                Ast::bool(BoolExpr::Eq(Box::new(a), Box::new(b)), s)
            }
            InfixT::Ne => {
                let a = self.check_type(scopes, a, true)?;
                let a_t = expect_expr(&a)?;
                let b = self.check_type(scopes, b, true)?;
                let b_t = expect_expr(&b)?;

                if a_t.is_not_comparable_to(b_t) {
                    return infix_error(a_t, a, i, b_t, b);
                }
                Ast::bool(BoolExpr::Ne(Box::new(a), Box::new(b)), s)
            }
            InfixT::Lt => {
                let a = self.check_type(scopes, a, true)?;
                let b = self.check_type(scopes, b, true)?;
                match (expect_expr(&a)?, expect_expr(&b)?) {
                    (DataType::Int, DataType::Int) => {
                        Ast::bool(BoolExpr::LtInt(Box::new(a), Box::new(b)), s)
                    }
                    (DataType::Float, DataType::Float) => {
                        Ast::bool(BoolExpr::LtFloat(Box::new(a), Box::new(b)), s)
                    }
                    (a_t, b_t) => return infix_error(a_t, a, i, b_t, b),
                }
            }
            InfixT::Le => {
                let a = self.check_type(scopes, a, true)?;
                let b = self.check_type(scopes, b, true)?;
                match (expect_expr(&a)?, expect_expr(&b)?) {
                    (DataType::Int, DataType::Int) => {
                        Ast::bool(BoolExpr::LeInt(Box::new(a), Box::new(b)), s)
                    }
                    (DataType::Float, DataType::Float) => {
                        Ast::bool(BoolExpr::LeFloat(Box::new(a), Box::new(b)), s)
                    }
                    (a_t, b_t) => return infix_error(a_t, a, i, b_t, b),
                }
            }
            InfixT::Gt => {
                let a = self.check_type(scopes, a, true)?;
                let b = self.check_type(scopes, b, true)?;
                match (expect_expr(&a)?, expect_expr(&b)?) {
                    (DataType::Int, DataType::Int) => {
                        Ast::bool(BoolExpr::GtInt(Box::new(a), Box::new(b)), s)
                    }
                    (DataType::Float, DataType::Float) => {
                        Ast::bool(BoolExpr::GtFloat(Box::new(a), Box::new(b)), s)
                    }
                    (a_t, b_t) => return infix_error(a_t, a, i, b_t, b),
                }
            }
            InfixT::Ge => {
                let a = self.check_type(scopes, a, true)?;
                let b = self.check_type(scopes, b, true)?;
                match (expect_expr(&a)?, expect_expr(&b)?) {
                    (DataType::Int, DataType::Int) => {
                        Ast::bool(BoolExpr::GeInt(Box::new(a), Box::new(b)), s)
                    }
                    (DataType::Float, DataType::Float) => {
                        Ast::bool(BoolExpr::GeFloat(Box::new(a), Box::new(b)), s)
                    }
                    (a_t, b_t) => return infix_error(a_t, a, i, b_t, b),
                }
            }
            InfixT::BwOr => {
                let a = self.check_type(scopes, a, true)?;
                let b = self.check_type(scopes, b, true)?;
                match (expect_expr(&a)?, expect_expr(&b)?) {
                    (DataType::Int, DataType::Int) => {
                        Ast::int(IntExpr::BwOr(Box::new(a), Box::new(b)), s)
                    }
                    (DataType::Bool, DataType::Bool) => {
                        Ast::bool(BoolExpr::BwOr(Box::new(a), Box::new(b)), s)
                    }
                    (a_t, b_t) => return infix_error(a_t, a, i, b_t, b),
                }
            }
            InfixT::BwAnd => {
                let a = self.check_type(scopes, a, true)?;
                let b = self.check_type(scopes, b, true)?;
                match (expect_expr(&a)?, expect_expr(&b)?) {
                    (DataType::Int, DataType::Int) => {
                        Ast::int(IntExpr::BwAnd(Box::new(a), Box::new(b)), s)
                    }
                    (DataType::Bool, DataType::Bool) => {
                        Ast::bool(BoolExpr::BwAnd(Box::new(a), Box::new(b)), s)
                    }
                    (a_t, b_t) => return infix_error(a_t, a, i, b_t, b),
                }
            }
            InfixT::Or => {
                let a = self.check_type(scopes, a, true)?;
                let b = self.check_type(scopes, b, true)?;
                match (expect_expr(&a)?, expect_expr(&b)?) {
                    (DataType::Bool, DataType::Bool) => {
                        Ast::bool(BoolExpr::Or(Box::new(a), Box::new(b)), s)
                    }
                    (a_t, b_t) => return infix_error(a_t, a, i, b_t, b),
                }
            }
            InfixT::And => {
                let a = self.check_type(scopes, a, true)?;
                let b = self.check_type(scopes, b, true)?;
                match (expect_expr(&a)?, expect_expr(&b)?) {
                    (DataType::Bool, DataType::Bool) => {
                        Ast::bool(BoolExpr::And(Box::new(a), Box::new(b)), s)
                    }
                    (a_t, b_t) => return infix_error(a_t, a, i, b_t, b),
                }
            }
            InfixT::Dot => {
                return Err(crate::Error::NotImplemented(
                    "Field access is not yet implemented",
                    s,
                ))
            }
            InfixT::As => {
                let ident = match b {
                    Cst::Ident(i) => i,
                    _ => return Err(crate::Error::ExpectedIdent(b.span())),
                };
                let data_type = self.resolve_data_type(&ident)?;
                let mut a = self.check_type(scopes, a, true)?;
                let a_data_type = expect_expr(&a)?;

                if a_data_type == data_type {
                    return Ok(a);
                }

                match (a_data_type, data_type) {
                    (DataType::Float, DataType::Int) => Ast::int(IntExpr::Cast(Box::new(a)), s),
                    (DataType::Any, DataType::Int) => Ast::int(IntExpr::Cast(Box::new(a)), s),
                    (DataType::Int, DataType::Float) => Ast::float(FloatExpr::Cast(Box::new(a)), s),
                    (DataType::Any, DataType::Float) => Ast::float(FloatExpr::Cast(Box::new(a)), s),
                    (DataType::Any, DataType::Bool) => Ast::bool(BoolExpr::Cast(Box::new(a)), s),
                    (DataType::Any, DataType::Str) => Ast::str(StrExpr::Cast(Box::new(a)), s),
                    (DataType::Any, DataType::Range) => Ast::range(RangeExpr::Cast(Box::new(a)), s),
                    (_, DataType::Any) => {
                        a.data_type = Some(DataType::Any);
                        a
                    }
                    (_, _) => {
                        return Err(crate::Error::CastAlwaysFails(
                            (a_data_type, a.span),
                            (data_type, b.span()),
                        ));
                    }
                }
            }
            InfixT::Is => {
                let ident = match b {
                    Cst::Ident(i) => i,
                    _ => return Err(crate::Error::ExpectedIdent(b.span())),
                };
                let data_type = self.resolve_data_type(&ident)?;
                let a = self.check_type(scopes, a, true)?;

                Ast::bool(BoolExpr::Is(Box::new(a), data_type), s)
            }
        };

        Ok(ast)
    }

    fn check_infix_assign(
        &mut self,
        scopes: &mut Scopes,
        a: Cst,
        i: Infix,
        b: Cst,
        int: fn(Box<Ast>, Box<Ast>) -> IntExpr,
        float: fn(Box<Ast>, Box<Ast>) -> FloatExpr,
        s: Span,
    ) -> crate::Result<Ast> {
        let ident = match a {
            Cst::Ident(i) => i,
            _ => return Err(crate::Error::InvalidAssignment(a.span(), i.span)),
        };

        let b = self.check_type(scopes, b, true)?;
        let b_data_type = expect_expr(&b)?;
        let var = match self.get_var(scopes, &ident)? {
            ResolvedVar::Var(v) => v,
            ResolvedVar::Const(c) => {
                return Err(crate::Error::ConstAssign((c, ident.span), i.span))
            }
        };
        let var_expr = Ast::var(var.inner, var.data_type, ident.span);

        let expr = match (var.data_type, b_data_type) {
            (DataType::Int, DataType::Int) => Ast::int(int(Box::new(var_expr), Box::new(b)), s),
            (DataType::Float, DataType::Float) => {
                Ast::float(float(Box::new(var_expr), Box::new(b)), s)
            }
            (_, _) => {
                return Err(crate::Error::AssignInfixNotApplicable(
                    (var.data_type, a.span()),
                    i,
                    (b_data_type, b.span),
                ));
            }
        };

        let inner = var.inner;

        self.set_var(scopes, &ident, &expr)?;

        Ok(Ast::statement(AstT::Assign(inner, Box::new(expr)), s))
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
