use std::rc::Rc;

use crate::cst::{self, Cst};
use crate::{
    BuiltinFun, Context, IdentSpan, Infix, InfixT, KwT, Postfix, PostfixT, Prefix, PrefixT, Span,
    ValSpan,
};

pub use ast::{Ast, AstT, BoolExpr, FloatExpr, IntExpr, RangeExpr, StrExpr};
use scope::*;
pub use types::*;

use self::ast::BuiltinFunCall;

pub mod ast;
mod builtin;
mod scope;
#[cfg(test)]
mod test;
mod types;

impl Context {
    pub fn check(&mut self, csts: Vec<Cst>) -> crate::Result<Vec<Ast>> {
        let mut scopes = Scopes::default();
        self.check_types(&mut scopes, csts)
    }

    fn check_types(&mut self, scopes: &mut Scopes, csts: Vec<Cst>) -> crate::Result<Vec<Ast>> {
        let mut asts = Vec::with_capacity(csts.len());
        for c in csts {
            let ast = self.check_type(scopes, c)?;
            asts.push(ast);
        }

        Ok(asts)
    }

    fn check_type(&mut self, scopes: &mut Scopes, cst: Cst) -> crate::Result<Ast> {
        let span = cst.span();
        let ast = match cst {
            Cst::Empty(s) => Ast::new(AstT::Unit, DataType::Unit, s),
            Cst::Error(s) => Ast::new(AstT::Error, DataType::Unit, s),
            Cst::Val(v) => Ast::val(v),
            Cst::Ident(i) => match self.get_var(scopes, &i)? {
                ResolvedVar::Const(c) => Ast::val(ValSpan::new(c.val(), span)),
                ResolvedVar::Var(var) => Ast::var(Rc::clone(&var.inner), var.data_type, i.span),
            },
            Cst::Par(_, c, _) => self.check_type(scopes, *c)?,
            Cst::Block(b) => self.check_block(scopes, b)?,
            Cst::IfExpr(i) => self.check_if_expr(scopes, i)?,
            Cst::WhileLoop(w) => self.check_while_loop(scopes, w)?,
            Cst::ForLoop(f) => self.check_for_loop(scopes, f)?,
            Cst::FunDef(f) => self.check_fun_def(scopes, f)?,
            Cst::FunCall(f) => self.check_fun_call(scopes, f)?,
            Cst::VarDef(v) => self.check_var_def(scopes, v)?,
            Cst::Prefix(p, a) => self.check_prefix(scopes, p, *a, span)?,
            Cst::Postfix(a, p) => self.check_postfix(scopes, *a, p, span)?,
            Cst::Infix(a, i, b) => self.check_infix(scopes, *a, i, *b, span)?,
        };
        Ok(ast)
    }

    fn check_block(&mut self, scopes: &mut Scopes, block: cst::Block) -> crate::Result<Ast> {
        let span = block.span();

        scopes.push();
        let asts = self.check_types(scopes, block.csts)?;
        scopes.pop();

        let data_type = match asts.last() {
            Some(a) => a.data_type,
            None => DataType::Unit,
        };

        Ok(Ast::new(AstT::Block(asts), data_type, span))
    }

    fn check_if_expr(&mut self, scopes: &mut Scopes, i: cst::IfExpr) -> crate::Result<Ast> {
        let mut cases = Vec::new();
        let mut data_type = DataType::Unit;
        let span = i.span();

        let if_block_span = i.if_block.block.span();
        {
            let cond = self.check_type(scopes, *i.if_block.cond)?;
            if cond.data_type.is_not(DataType::Bool) {
                return Err(crate::Error::MismatchedType {
                    expected: DataType::Bool,
                    found: cond.data_type,
                    spans: vec![cond.span],
                });
            }

            scopes.push();
            let block = self.check_types(scopes, i.if_block.block.csts)?;
            scopes.pop();

            if i.is_expr {
                data_type = match block.last() {
                    Some(a) => a.data_type,
                    None => DataType::Unit,
                };
            }

            cases.push(ast::CondBlock::new(cond, block));
        }

        for e in i.else_if_blocks {
            let cond = self.check_type(scopes, e.cond)?;
            if cond.data_type.is_not(DataType::Bool) {
                return Err(crate::Error::MismatchedType {
                    expected: DataType::Bool,
                    found: cond.data_type,
                    spans: vec![cond.span],
                });
            }

            scopes.push();
            let block_span = e.block.span();
            let block = self.check_types(scopes, e.block.csts)?;
            scopes.pop();

            if i.is_expr {
                let d = block.last().map_or(DataType::Unit, |a| a.data_type);
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
            scopes.push();
            let block_span = e.block.span();
            let block = self.check_types(scopes, e.block.csts)?;
            scopes.pop();

            if i.is_expr {
                let d = block.last().map_or(DataType::Unit, |a| a.data_type);
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
        } else if i.is_expr && data_type.is_not(DataType::Unit) {
            return Err(crate::Error::MissingElseBranch(data_type, span));
        } else {
            None
        };

        let if_expr = ast::IfExpr::new(cases, else_block);
        Ok(Ast::new(AstT::IfExpr(if_expr), data_type, span))
    }

    fn check_while_loop(&mut self, scopes: &mut Scopes, w: cst::WhileLoop) -> crate::Result<Ast> {
        let span = w.span();

        let cond = self.check_type(scopes, *w.cond)?;

        scopes.push();
        let block = self.check_types(scopes, w.block.csts)?;
        scopes.pop();

        let whl_loop = ast::WhileLoop::new(Box::new(cond), block);
        Ok(Ast::new(AstT::WhileLoop(whl_loop), DataType::Unit, span))
    }

    fn check_for_loop(&mut self, scopes: &mut Scopes, f: cst::ForLoop) -> crate::Result<Ast> {
        let span = f.span();

        let iter = self.check_type(scopes, *f.iter)?;
        if iter.data_type.is_not(DataType::Range) {
            return Err(crate::Error::NotIterable(iter.data_type, iter.span));
        }
        let iter_type = DataType::Int;

        scopes.push();
        let inner = Rc::new(ast::Var::new(None));
        let var = Var::new(f.ident, iter_type, true, false, Rc::clone(&inner));
        self.def_var(scopes, var);
        let block = self.check_types(scopes, f.block.csts)?;
        scopes.pop();

        let for_loop = ast::ForLoop::new(inner, Box::new(iter), block);
        Ok(Ast::new(AstT::ForLoop(for_loop), DataType::Unit, span))
    }

    fn check_fun_def(&mut self, scopes: &mut Scopes, f: cst::FunDef) -> crate::Result<Ast> {
        let span = f.span();

        let mut params = Vec::with_capacity(f.params.items.len());
        for p in f.params.items {
            let typ = self.resolve_data_type(&p.typ)?;
            let span = Span::across(p.ident.span, p.typ.span);
            params.push(FunParam::new(p.ident, typ, span));
        }

        scopes.push();
        let mut inner_params = Vec::new();
        for p in params.iter() {
            let param = Rc::new(ast::Var::new(None));
            self.def_var(
                scopes,
                Var::new(p.ident, p.data_type, true, false, Rc::clone(&param)),
            );
            inner_params.push(param);
        }
        let block_span = f.block.span();
        let block = self.check_types(scopes, f.block.csts)?;
        scopes.pop();

        let block_type = block.last().map_or(DataType::Unit, |a| a.data_type);
        let return_type = if let Some(r) = f.return_type {
            let return_type = self.resolve_data_type(&r.typ)?;

            if block_type.is_not(return_type) {
                let span = block.last().map_or(block_span, |a| a.span);
                return Err(crate::Error::MismatchedType {
                    expected: return_type,
                    found: block_type,
                    spans: vec![r.typ.span, span],
                });
            }

            Some(return_type)
        } else {
            None
        };

        let inner = ast::Fun::new(inner_params, block);
        let fun = Fun::new(f.ident, params, return_type, Rc::new(inner));
        self.def_fun(scopes, fun)?;

        Ok(Ast::new(AstT::Unit, DataType::Unit, span))
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
            let val = self.check_type(scopes, a)?;
            let expected = p.data_type;
            let found = val.data_type;
            if found.is_not(expected) {
                return Err(crate::Error::MismatchedType {
                    expected,
                    found,
                    spans: vec![p.span, val.span],
                });
            }
            args.push(val);
        }

        let return_type = match fun.return_type {
            Some(t) => t,
            None => DataType::Unit,
        };

        Ok(Ast::new(
            AstT::FunCall(Rc::clone(&fun.inner), args),
            return_type,
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
            args.push(self.check_type(scopes, a)?);
        }

        let mut fun = None;
        'outer: for (c, s) in b.signatures() {
            if s.params.len() != args.len() {
                continue;
            }
            for (&p, a) in s.params.iter().zip(args.iter()) {
                if a.data_type.is_not(p) {
                    continue 'outer;
                }
            }

            fun = Some((c, s));
            break;
        }
        let (fun, signature) = match fun {
            Some(s) => s,
            None => return Err(crate::Error::NoMatchingSignature(b, span)),
        };

        match fun {
            BuiltinFunCall::Spill => {
                let vars = self.collect_spill_vars(scopes.iter().flat_map(Scope::vars));
                return Ok(Ast::new(AstT::Spill(vars), signature.return_type, span));
            }
            BuiltinFunCall::SpillLocal => {
                let vars = self.collect_spill_vars(scopes.current().vars());
                return Ok(Ast::new(AstT::Spill(vars), signature.return_type, span));
            }
            _ => (),
        }

        let return_type = signature.return_type;
        Ok(Ast::new(
            AstT::BuiltinFunCall(*fun, args),
            return_type,
            span,
        ))
    }

    fn check_var_def(&mut self, scopes: &mut Scopes, v: cst::VarDef) -> crate::Result<Ast> {
        let span = v.span();
        let val = self.check_type(scopes, *v.value.1)?;

        let data_type = match v.type_hint {
            Some((_, t)) => {
                let data_type = self.resolve_data_type(&t)?;
                if val.data_type.is_not(data_type) {
                    return Err(crate::Error::MismatchedType {
                        expected: data_type,
                        found: val.data_type,
                        spans: vec![t.span, val.span],
                    });
                }

                data_type
            }
            None => val.data_type,
        };

        let mutable = v.kw.typ == KwT::Var;
        let inner = Rc::new(ast::Var::new(None));
        let var = Var::new(v.ident, data_type, true, mutable, Rc::clone(&inner));
        self.def_var(scopes, var);

        Ok(Ast::new(
            AstT::VarDef(inner, Box::new(val)),
            DataType::Unit,
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
        let a = self.check_type(scopes, a)?;

        let ast = match (p.typ, a.data_type) {
            (PrefixT::UnaryPlus, DataType::Int) => a,
            (PrefixT::UnaryPlus, DataType::Float) => a,
            (PrefixT::UnaryMinus, DataType::Int) => Ast::int(IntExpr::Neg(Box::new(a)), span),
            (PrefixT::UnaryMinus, DataType::Float) => Ast::float(FloatExpr::Neg(Box::new(a)), span),
            (PrefixT::Not, DataType::Bool) => Ast::bool(BoolExpr::Not(Box::new(a)), span),
            _ => return Err(crate::Error::PrefixNotApplicable(p, (a.data_type, a.span))),
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
        let a = self.check_type(scopes, a)?;

        let ast = match (p.typ, a.data_type) {
            (PostfixT::Factorial, DataType::Int) => Ast::int(IntExpr::Factorial(Box::new(a)), span),
            _ => return Err(crate::Error::PostfixNotApplicable((a.data_type, a.span), p)),
        };

        Ok(ast)
    }

    fn check_infix(
        &mut self,
        scopes: &mut Scopes,
        a: Cst,
        i: Infix,
        b: Cst,
        span: Span,
    ) -> crate::Result<Ast> {
        fn infix_assign_error(a: (DataType, Span), i: Infix, b: Ast) -> crate::Result<Ast> {
            Err(crate::Error::AssignInfixNotApplicable(
                a,
                i,
                (b.data_type, b.span),
            ))
        }

        fn infix_error(a: Ast, i: Infix, b: Ast) -> crate::Result<Ast> {
            Err(crate::Error::InfixNotApplicable(
                (a.data_type, a.span),
                i,
                (b.data_type, b.span),
            ))
        }

        let ast = match i.typ {
            InfixT::Assign => {
                let ident = match a {
                    Cst::Ident(i) => i,
                    _ => return Err(crate::Error::InvalidAssignment(a.span(), i.span)),
                };

                let expr = self.check_type(scopes, b)?;
                let var = match self.resolve_var(scopes, &ident)? {
                    ResolvedVar::Var(v) => v,
                    ResolvedVar::Const(c) => {
                        return Err(crate::Error::ConstAssign((c, ident.span), i.span))
                    }
                };

                if expr.data_type.is_not(var.data_type) {
                    return Err(crate::Error::AssignNotApplicable(
                        (var.data_type, ident.span),
                        (expr.data_type, expr.span),
                    ));
                }

                let inner = Rc::clone(&var.inner);

                self.set_var(scopes, &ident, &expr)?;

                // TODO: return some sort of statement type that can't be used as an expression
                Ast::new(AstT::Assign(inner, Box::new(expr)), DataType::Unit, span)
            }
            InfixT::AddAssign => {
                let ident = match a {
                    Cst::Ident(i) => i,
                    _ => return Err(crate::Error::InvalidAssignment(a.span(), i.span)),
                };

                let b = self.check_type(scopes, b)?;
                let var = match self.get_var(scopes, &ident)? {
                    ResolvedVar::Var(v) => v,
                    ResolvedVar::Const(c) => {
                        return Err(crate::Error::ConstAssign((c, ident.span), i.span))
                    }
                };
                let var_expr = Ast::var(Rc::clone(&var.inner), var.data_type, ident.span);

                let expr = match (var.data_type, b.data_type) {
                    (DataType::Int, DataType::Int) => {
                        Ast::int(IntExpr::Add(Box::new(var_expr), Box::new(b)), span)
                    }
                    (DataType::Float, DataType::Float) => {
                        Ast::float(FloatExpr::Add(Box::new(var_expr), Box::new(b)), span)
                    }
                    (_, _) => return infix_assign_error((var.data_type, ident.span), i, b),
                };

                let inner = Rc::clone(&var.inner);

                self.set_var(scopes, &ident, &expr)?;

                Ast::new(AstT::Assign(inner, Box::new(expr)), DataType::Unit, span)
            }
            InfixT::SubAssign => {
                let ident = match a {
                    Cst::Ident(i) => i,
                    _ => return Err(crate::Error::InvalidAssignment(a.span(), i.span)),
                };

                let b = self.check_type(scopes, b)?;
                let var = match self.get_var(scopes, &ident)? {
                    ResolvedVar::Var(v) => v,
                    ResolvedVar::Const(c) => {
                        return Err(crate::Error::ConstAssign((c, ident.span), i.span))
                    }
                };
                let var_expr = Ast::var(Rc::clone(&var.inner), var.data_type, ident.span);

                let expr = match (var.data_type, b.data_type) {
                    (DataType::Int, DataType::Int) => {
                        Ast::int(IntExpr::Sub(Box::new(var_expr), Box::new(b)), span)
                    }
                    (DataType::Float, DataType::Float) => {
                        Ast::float(FloatExpr::Sub(Box::new(var_expr), Box::new(b)), span)
                    }
                    (_, _) => return infix_assign_error((var.data_type, ident.span), i, b),
                };

                Ast::new(
                    AstT::Assign(Rc::clone(&var.inner), Box::new(expr)),
                    DataType::Unit,
                    span,
                )
            }
            InfixT::MulAssign => {
                let ident = match a {
                    Cst::Ident(i) => i,
                    _ => return Err(crate::Error::InvalidAssignment(a.span(), i.span)),
                };

                let b = self.check_type(scopes, b)?;
                let var = match self.get_var(scopes, &ident)? {
                    ResolvedVar::Var(v) => v,
                    ResolvedVar::Const(c) => {
                        return Err(crate::Error::ConstAssign((c, ident.span), i.span))
                    }
                };
                let var_expr = Ast::var(Rc::clone(&var.inner), var.data_type, ident.span);

                let expr = match (var.data_type, b.data_type) {
                    (DataType::Int, DataType::Int) => {
                        Ast::int(IntExpr::Mul(Box::new(var_expr), Box::new(b)), span)
                    }
                    (DataType::Float, DataType::Float) => {
                        Ast::float(FloatExpr::Mul(Box::new(var_expr), Box::new(b)), span)
                    }
                    (_, _) => return infix_assign_error((var.data_type, ident.span), i, b),
                };

                Ast::new(
                    AstT::Assign(Rc::clone(&var.inner), Box::new(expr)),
                    DataType::Unit,
                    span,
                )
            }
            InfixT::DivAssign => {
                let ident = match a {
                    Cst::Ident(i) => i,
                    _ => return Err(crate::Error::InvalidAssignment(a.span(), i.span)),
                };

                let b = self.check_type(scopes, b)?;
                let var = match self.get_var(scopes, &ident)? {
                    ResolvedVar::Var(v) => v,
                    ResolvedVar::Const(c) => {
                        return Err(crate::Error::ConstAssign((c, ident.span), i.span))
                    }
                };
                let var_expr = Ast::var(Rc::clone(&var.inner), var.data_type, ident.span);

                let expr = match (var.data_type, b.data_type) {
                    (DataType::Int, DataType::Int) => {
                        Ast::int(IntExpr::Div(Box::new(var_expr), Box::new(b)), span)
                    }
                    (DataType::Float, DataType::Float) => {
                        Ast::float(FloatExpr::Div(Box::new(var_expr), Box::new(b)), span)
                    }
                    (_, _) => return infix_assign_error((var.data_type, ident.span), i, b),
                };

                Ast::new(
                    AstT::Assign(Rc::clone(&var.inner), Box::new(expr)),
                    DataType::Unit,
                    span,
                )
            }
            InfixT::RangeEx => {
                let a = self.check_type(scopes, a)?;
                let b = self.check_type(scopes, b)?;
                match (a.data_type, b.data_type) {
                    (DataType::Int, DataType::Int) => {
                        Ast::range(RangeExpr::Ex(Box::new(a), Box::new(b)), span)
                    }
                    (_, _) => return infix_error(a, i, b),
                }
            }
            InfixT::RangeIn => {
                let a = self.check_type(scopes, a)?;
                let b = self.check_type(scopes, b)?;
                match (a.data_type, b.data_type) {
                    (DataType::Int, DataType::Int) => {
                        Ast::range(RangeExpr::In(Box::new(a), Box::new(b)), span)
                    }
                    (_, _) => return infix_error(a, i, b),
                }
            }
            InfixT::Add => {
                let a = self.check_type(scopes, a)?;
                let b = self.check_type(scopes, b)?;
                match (a.data_type, b.data_type) {
                    (DataType::Int, DataType::Int) => {
                        Ast::int(IntExpr::Add(Box::new(a), Box::new(b)), span)
                    }
                    (DataType::Float, DataType::Float) => {
                        Ast::float(FloatExpr::Add(Box::new(a), Box::new(b)), span)
                    }
                    (_, _) => return infix_error(a, i, b),
                }
            }
            InfixT::Sub => {
                let a = self.check_type(scopes, a)?;
                let b = self.check_type(scopes, b)?;
                match (a.data_type, b.data_type) {
                    (DataType::Int, DataType::Int) => {
                        Ast::int(IntExpr::Sub(Box::new(a), Box::new(b)), span)
                    }
                    (DataType::Float, DataType::Float) => {
                        Ast::float(FloatExpr::Sub(Box::new(a), Box::new(b)), span)
                    }
                    (_, _) => return infix_error(a, i, b),
                }
            }
            InfixT::Mul => {
                let a = self.check_type(scopes, a)?;
                let b = self.check_type(scopes, b)?;
                match (a.data_type, b.data_type) {
                    (DataType::Int, DataType::Int) => {
                        Ast::int(IntExpr::Mul(Box::new(a), Box::new(b)), span)
                    }
                    (DataType::Float, DataType::Float) => {
                        Ast::float(FloatExpr::Mul(Box::new(a), Box::new(b)), span)
                    }
                    (_, _) => return infix_error(a, i, b),
                }
            }
            InfixT::Div => {
                let a = self.check_type(scopes, a)?;
                let b = self.check_type(scopes, b)?;
                match (a.data_type, b.data_type) {
                    (DataType::Int, DataType::Int) => {
                        Ast::int(IntExpr::Div(Box::new(a), Box::new(b)), span)
                    }
                    (DataType::Float, DataType::Float) => {
                        Ast::float(FloatExpr::Div(Box::new(a), Box::new(b)), span)
                    }
                    (_, _) => return infix_error(a, i, b),
                }
            }
            InfixT::Rem => {
                let a = self.check_type(scopes, a)?;
                let b = self.check_type(scopes, b)?;
                match (a.data_type, b.data_type) {
                    (DataType::Int, DataType::Int) => {
                        Ast::int(IntExpr::Rem(Box::new(a), Box::new(b)), span)
                    }
                    (_, _) => return infix_error(a, i, b),
                }
            }
            InfixT::Eq => {
                let a = self.check_type(scopes, a)?;
                let b = self.check_type(scopes, b)?;
                if a.data_type.is_not_comparable_to(b.data_type) {
                    return infix_error(a, i, b);
                }
                Ast::bool(BoolExpr::Eq(Box::new(a), Box::new(b)), span)
            }
            InfixT::Ne => {
                let a = self.check_type(scopes, a)?;
                let b = self.check_type(scopes, b)?;
                if a.data_type.is_not_comparable_to(b.data_type) {
                    return infix_error(a, i, b);
                }
                Ast::bool(BoolExpr::Ne(Box::new(a), Box::new(b)), span)
            }
            InfixT::Lt => {
                let a = self.check_type(scopes, a)?;
                let b = self.check_type(scopes, b)?;
                match (a.data_type, b.data_type) {
                    (DataType::Int, DataType::Int) => {
                        Ast::bool(BoolExpr::LtInt(Box::new(a), Box::new(b)), span)
                    }
                    (DataType::Float, DataType::Float) => {
                        Ast::bool(BoolExpr::LtFloat(Box::new(a), Box::new(b)), span)
                    }
                    (_, _) => return infix_error(a, i, b),
                }
            }
            InfixT::Le => {
                let a = self.check_type(scopes, a)?;
                let b = self.check_type(scopes, b)?;
                match (a.data_type, b.data_type) {
                    (DataType::Int, DataType::Int) => {
                        Ast::bool(BoolExpr::LeInt(Box::new(a), Box::new(b)), span)
                    }
                    (DataType::Float, DataType::Float) => {
                        Ast::bool(BoolExpr::LeFloat(Box::new(a), Box::new(b)), span)
                    }
                    (_, _) => return infix_error(a, i, b),
                }
            }
            InfixT::Gt => {
                let a = self.check_type(scopes, a)?;
                let b = self.check_type(scopes, b)?;
                match (a.data_type, b.data_type) {
                    (DataType::Int, DataType::Int) => {
                        Ast::bool(BoolExpr::GtInt(Box::new(a), Box::new(b)), span)
                    }
                    (DataType::Float, DataType::Float) => {
                        Ast::bool(BoolExpr::GtFloat(Box::new(a), Box::new(b)), span)
                    }
                    (_, _) => return infix_error(a, i, b),
                }
            }
            InfixT::Ge => {
                let a = self.check_type(scopes, a)?;
                let b = self.check_type(scopes, b)?;
                match (a.data_type, b.data_type) {
                    (DataType::Int, DataType::Int) => {
                        Ast::bool(BoolExpr::GeInt(Box::new(a), Box::new(b)), span)
                    }
                    (DataType::Float, DataType::Float) => {
                        Ast::bool(BoolExpr::GeFloat(Box::new(a), Box::new(b)), span)
                    }
                    (_, _) => return infix_error(a, i, b),
                }
            }
            InfixT::BwOr => {
                let a = self.check_type(scopes, a)?;
                let b = self.check_type(scopes, b)?;
                match (a.data_type, b.data_type) {
                    (DataType::Int, DataType::Int) => {
                        Ast::int(IntExpr::BwOr(Box::new(a), Box::new(b)), span)
                    }
                    (DataType::Bool, DataType::Bool) => {
                        Ast::bool(BoolExpr::BwOr(Box::new(a), Box::new(b)), span)
                    }
                    (_, _) => return infix_error(a, i, b),
                }
            }
            InfixT::BwAnd => {
                let a = self.check_type(scopes, a)?;
                let b = self.check_type(scopes, b)?;
                match (a.data_type, b.data_type) {
                    (DataType::Int, DataType::Int) => {
                        Ast::int(IntExpr::BwAnd(Box::new(a), Box::new(b)), span)
                    }
                    (DataType::Bool, DataType::Bool) => {
                        Ast::bool(BoolExpr::BwAnd(Box::new(a), Box::new(b)), span)
                    }
                    (_, _) => return infix_error(a, i, b),
                }
            }
            InfixT::Or => {
                let a = self.check_type(scopes, a)?;
                let b = self.check_type(scopes, b)?;
                match (a.data_type, b.data_type) {
                    (DataType::Bool, DataType::Bool) => {
                        Ast::bool(BoolExpr::Or(Box::new(a), Box::new(b)), span)
                    }
                    (_, _) => return infix_error(a, i, b),
                }
            }
            InfixT::And => {
                let a = self.check_type(scopes, a)?;
                let b = self.check_type(scopes, b)?;
                match (a.data_type, b.data_type) {
                    (DataType::Bool, DataType::Bool) => {
                        Ast::bool(BoolExpr::And(Box::new(a), Box::new(b)), span)
                    }
                    (_, _) => return infix_error(a, i, b),
                }
            }
            InfixT::Dot => {
                return Err(crate::Error::NotImplemented(
                    "Field access is not yet implemented",
                    span,
                ))
            }
        };

        Ok(ast)
    }

    fn resolve_data_type(&self, typ: &IdentSpan) -> crate::Result<DataType> {
        let name = self.idents.name(typ.ident);
        match DataType::from(name) {
            Some(d) => Ok(d),
            None => Err(crate::Error::UnknownType(name.into(), typ.span)),
        }
    }

    fn collect_spill_vars<'a>(
        &self,
        var_iter: impl Iterator<Item = &'a Var>,
    ) -> Vec<(String, Rc<ast::Var>)> {
        var_iter
            .filter(|v| v.assigned)
            .map(|v| {
                let name = self.idents.name(v.ident.ident).to_owned();
                (name, Rc::clone(&v.inner))
            })
            .collect()
    }
}
