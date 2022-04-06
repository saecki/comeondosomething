use std::rc::Rc;

use crate::cst::{self, Cst};
use crate::{Context, IdentSpan, Infix, KwT, Postfix, Prefix, Span};

pub use ast::{Ast, AstT};
use scope::*;
pub use types::*;

pub mod ast;
mod scope;
mod types;

// TODO: type checker struct that holds function and var definitions (currently in context)
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
            Cst::Empty(s) => Ast::new(AstT::Empty, DataType::Unit, s),
            Cst::Error(s) => Ast::new(AstT::Error, DataType::Unit, s),
            Cst::Val(v) => {
                let data_type = v.data_type();
                Ast::new(AstT::Val(v), data_type, span)
            }
            Cst::Ident(i) => {
                let var = self.resolve_var(scopes, &i)?;
                Ast::new(AstT::VarRef(Rc::clone(&var.inner)), var.data_type, span)
            }
            Cst::Par(_, c, _) => {
                let mut ast = self.check_type(scopes, *c)?;
                ast.span = span;
                ast
            }
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
            Cst::InfixAssign(a, i, b) => self.check_infix_assign(scopes, a, i, *b, span)?,
            Cst::Assign(a, i, b) => self.check_assign(scopes, a, i, *b, span)?,
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
            let span = i.if_block.span();

            let cond = self.check_type(scopes, *i.if_block.cond)?;
            if cond.data_type != DataType::Bool {
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

            cases.push(ast::CondBlock::new(cond, block, span));
        }

        for e in i.else_if_blocks {
            let span = e.span();

            let cond = self.check_type(scopes, e.cond)?;
            if cond.data_type != DataType::Bool {
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
                if data_type != d {
                    let if_expr_span = cases[0].block.last().map_or(if_block_span, |a| a.span);
                    let else_expr_span = block.last().map_or(block_span, |a| a.span);
                    return Err(crate::Error::IfBranchIncompatibleType(
                        (data_type, if_expr_span),
                        (d, else_expr_span),
                    ));
                }
            }

            cases.push(ast::CondBlock::new(cond, block, span));
        }

        let else_block = if let Some(e) = i.else_block {
            let span = e.span();

            scopes.push();
            let block_span = e.block.span();
            let block = self.check_types(scopes, e.block.csts)?;
            scopes.pop();

            if i.is_expr {
                let d = block.last().map_or(DataType::Unit, |a| a.data_type);
                if data_type != d {
                    let if_expr_span = cases[0].block.last().map_or(if_block_span, |a| a.span);
                    let else_expr_span = block.last().map_or(block_span, |a| a.span);
                    return Err(crate::Error::IfBranchIncompatibleType(
                        (data_type, if_expr_span),
                        (d, else_expr_span),
                    ));
                }
            }

            Some(ast::Block::new(block, span))
        } else if i.is_expr && data_type != DataType::Unit {
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
        if iter.data_type != DataType::Range {
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
            params.push(FunParam::new(p.ident, typ));
        }

        scopes.push();
        let mut inner_params = Vec::new();
        for p in params.iter() {
            let param = Rc::new(ast::Var::new(None));
            self.def_var(
                scopes,
                Var::new(p.ident, p.typ, true, false, Rc::clone(&param)),
            );
            inner_params.push(param);
        }
        let block = self.check_types(scopes, f.block.csts)?;
        scopes.pop();

        let return_type = if let Some(r) = f.return_type {
            Some(self.resolve_data_type(&r.typ)?)
        } else {
            None
        };

        let block_type = block.last().map_or(DataType::Unit, |a| a.data_type);
        if let Some(t) = return_type {
            if t == block_type {
                todo!("error")
            }
        }

        let inner = ast::Fun::new(inner_params, block);
        let fun = Fun::new(f.ident, params, return_type, Rc::new(inner));
        self.def_fun(scopes, fun)?;

        Ok(Ast::new(AstT::Empty, DataType::Unit, span))
    }

    fn check_fun_call(&mut self, scopes: &mut Scopes, f: cst::FunCall) -> crate::Result<Ast> {
        let span = f.span();

        let fun = self.resolve_fun(scopes, &f.ident)?;

        {
            let expected = fun.params.len();
            let found = f.args.items.len();
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
        for a in f.args.items {
            args.push(self.check_type(scopes, a)?);
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

    fn check_var_def(&mut self, scopes: &mut Scopes, v: cst::VarDef) -> crate::Result<Ast> {
        let span = v.span();
        let val = self.check_type(scopes, *v.value.1)?;

        let typ = match v.type_hint {
            Some((_, t)) => {
                let typ = self.resolve_data_type(&t)?;
                if val.data_type != typ {
                    return Err(crate::Error::MismatchedType {
                        expected: typ,
                        found: val.data_type,
                        spans: vec![t.span, val.span],
                    });
                }

                typ
            }
            None => val.data_type,
        };

        let mutable = v.kw.typ == KwT::Var;
        let inner = Rc::new(ast::Var::new(None));
        let var = Var::new(v.ident, typ, true, mutable, Rc::clone(&inner));
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
        let data_type = match p.resulting_type(a.data_type) {
            Some(t) => t,
            None => return Err(crate::Error::PrefixNotApplicable(p, a)),
        };

        Ok(Ast::new(AstT::Prefix(p, Box::new(a)), data_type, span))
    }

    fn check_postfix(
        &mut self,
        scopes: &mut Scopes,
        a: Cst,
        p: Postfix,
        span: Span,
    ) -> crate::Result<Ast> {
        let a = self.check_type(scopes, a)?;
        let data_type = match p.resulting_type(a.data_type) {
            Some(t) => t,
            None => return Err(crate::Error::PostfixNotApplicable(a, p)),
        };

        Ok(Ast::new(AstT::Postfix(Box::new(a), p), data_type, span))
    }

    fn check_infix(
        &mut self,
        scopes: &mut Scopes,
        a: Cst,
        i: Infix,
        b: Cst,
        span: Span,
    ) -> crate::Result<Ast> {
        let a = self.check_type(scopes, a)?;
        let b = self.check_type(scopes, b)?;

        let data_type = match i.resulting_type(a.data_type, b.data_type) {
            Some(t) => t,
            None => return Err(crate::Error::InfixNotApplicable(a, i, b)),
        };

        Ok(Ast::new(
            AstT::Infix(Box::new(a), i, Box::new(b)),
            data_type,
            span,
        ))
    }

    fn check_infix_assign(
        &mut self,
        scopes: &mut Scopes,
        a: IdentSpan,
        i: Infix,
        b: Cst,
        span: Span,
    ) -> crate::Result<Ast> {
        let b = self.check_type(scopes, b)?;
        let var = self.resolve_var(scopes, &a)?;

        let data_type = match i.resulting_type(var.data_type, b.data_type) {
            Some(t) => t,
            None => {
                return Err(crate::Error::AssignInfixNotApplicable(
                    (a, var.data_type),
                    i,
                    b,
                ))
            }
        };

        let inner = Rc::clone(&var.inner);
        self.set_var(scopes, &a, &b)?;

        Ok(Ast::new(
            AstT::InfixAssign(inner, i, Box::new(b)),
            data_type,
            span,
        ))
    }

    fn check_assign(
        &mut self,
        scopes: &mut Scopes,
        a: IdentSpan,
        i: Infix,
        b: Cst,
        span: Span,
    ) -> crate::Result<Ast> {
        let b = self.check_type(scopes, b)?;
        let var = self.resolve_var(scopes, &a)?;

        let data_type = match i.resulting_type(var.data_type, b.data_type) {
            Some(t) => t,
            None => return Err(crate::Error::AssignNotApplicable((a, var.data_type), i, b)),
        };

        let inner = Rc::clone(&var.inner);
        self.set_var(scopes, &a, &b)?;

        Ok(Ast::new(AstT::Assign(inner, Box::new(b)), data_type, span))
    }

    fn resolve_data_type(&self, typ: &IdentSpan) -> crate::Result<DataType> {
        let name = self.idents.name(typ.ident);
        match DataType::from(name) {
            Some(d) => Ok(d),
            None => Err(crate::Error::UnknownType(name.into(), typ.span)),
        }
    }
}
