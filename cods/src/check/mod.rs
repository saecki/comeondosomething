use std::rc::Rc;

use crate::ast::Var;
use crate::cst::{self, Cst};
use crate::{Context, IdentSpan, Infix, KwT, Postfix, Prefix, Span};

pub use ast::{Ast, AstT};
pub use scope::*;
pub use types::*;

pub mod ast;
mod scope;
mod types;

// TODO: type checker struct that holds function and var definitions (currently in context)
impl Context {
    pub fn check(&mut self, csts: Vec<Cst>) -> crate::Result<Vec<Ast>> {
        let mut asts = Vec::with_capacity(csts.len());
        for c in csts {
            let ast = self.check_type(c)?;
            asts.push(ast);
        }

        Ok(asts)
    }

    fn check_type(&mut self, cst: Cst) -> crate::Result<Ast> {
        let span = cst.span();
        let ast = match cst {
            Cst::Empty(s) => Ast::new(AstT::Empty, DataType::Unit, s),
            Cst::Error(s) => Ast::new(AstT::Error, DataType::Unit, s),
            Cst::Val(v) => {
                let data_type = v.data_type();
                Ast::new(AstT::Val(v), data_type, span)
            }
            Cst::Ident(i) => {
                let data_type = self.resolve_var(&i)?;
                Ast::new(AstT::Ident(i), data_type, span)
            }
            Cst::Par(_, c, _) => {
                let mut ast = self.check_type(*c)?;
                ast.span = span;
                ast
            }
            Cst::Block(b) => self.check_block(b)?,
            Cst::IfExpr(i) => self.check_if_expr(i)?,
            Cst::WhileLoop(w) => self.check_while_loop(w)?,
            Cst::ForLoop(f) => self.check_for_loop(f)?,
            Cst::FunDef(f) => self.check_fun_def(f)?,
            Cst::FunCall(f) => self.check_fun_call(f)?,
            Cst::VarDef(v) => self.check_var_def(v)?,
            Cst::Prefix(p, a) => self.check_prefix(p, *a, span)?,
            Cst::Postfix(a, p) => self.check_postfix(*a, p, span)?,
            Cst::Infix(a, i, b) => self.check_infix(*a, i, *b, span)?,
            Cst::InfixAssign(a, i, b) => self.check_infix_assign(a, i, *b, span)?,
            Cst::Assign(a, i, b) => self.check_assign(a, i, *b, span)?,
        };
        Ok(ast)
    }

    fn check_block(&mut self, block: cst::Block) -> crate::Result<Ast> {
        let span = block.span();

        self.scopes.push();
        let asts = self.check(block.csts)?;
        self.scopes.pop();

        let data_type = match asts.last() {
            Some(a) => a.data_type,
            None => DataType::Unit,
        };

        Ok(Ast::new(AstT::Block(asts), data_type, span))
    }

    fn check_if_expr(&mut self, i: cst::IfExpr) -> crate::Result<Ast> {
        let mut cases = Vec::new();
        let mut data_type = DataType::Unit;
        let span = i.span();

        let if_block_span = i.if_block.block.span();
        {
            let span = i.if_block.span();

            let cond = self.check_type(*i.if_block.cond)?;
            if cond.data_type != DataType::Bool {
                return Err(crate::Error::MismatchedType {
                    expected: DataType::Bool,
                    found: cond.data_type,
                    spans: vec![cond.span],
                });
            }

            self.scopes.push();
            let block = self.check(i.if_block.block.csts)?;
            self.scopes.pop();

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

            let cond = self.check_type(e.cond)?;
            if cond.data_type != DataType::Bool {
                return Err(crate::Error::MismatchedType {
                    expected: DataType::Bool,
                    found: cond.data_type,
                    spans: vec![cond.span],
                });
            }

            self.scopes.push();
            let block_span = e.block.span();
            let block = self.check(e.block.csts)?;
            self.scopes.pop();

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

            self.scopes.push();
            let block_span = e.block.span();
            let block = self.check(e.block.csts)?;
            self.scopes.pop();

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

    fn check_while_loop(&mut self, w: cst::WhileLoop) -> crate::Result<Ast> {
        let span = w.span();

        let cond = self.check_type(*w.cond)?;

        self.scopes.push();
        let block = self.check(w.block.csts)?;
        self.scopes.pop();

        let whl_loop = ast::WhileLoop::new(Box::new(cond), block);
        Ok(Ast::new(AstT::WhileLoop(whl_loop), DataType::Unit, span))
    }

    fn check_for_loop(&mut self, f: cst::ForLoop) -> crate::Result<Ast> {
        let span = f.span();

        let iter = self.check_type(*f.iter)?;
        if iter.data_type != DataType::Range {
            return Err(crate::Error::NotIterable(iter.data_type, iter.span));
        }
        let iter_type = DataType::Int;

        self.scopes.push();
        self.def_var(Var::new(f.ident, iter_type, true, false));
        let block = self.check(f.block.csts)?;
        self.scopes.pop();

        let for_loop = ast::ForLoop::new(f.ident, Box::new(iter), block);
        Ok(Ast::new(AstT::ForLoop(for_loop), DataType::Unit, span))
    }

    fn check_fun_def(&mut self, f: cst::FunDef) -> crate::Result<Ast> {
        let span = f.span();

        let mut params = Vec::with_capacity(f.params.items.len());
        for p in f.params.items {
            let typ = self.resolve_data_type(&p.typ)?;
            params.push(ast::FunParam::new(p.ident, typ));
        }

        let return_type = if let Some(r) = f.return_type {
            Some(self.resolve_data_type(&r.typ)?)
        } else {
            None
        };

        self.scopes.push();
        for p in params.iter() {
            self.def_var(Var::new(p.ident, p.typ, true, false))
        }
        let block = self.check(f.block.csts)?;
        self.scopes.pop();

        let block_type = block.last().map_or(DataType::Unit, |a| a.data_type);
        if let Some(t) = return_type {
            if t == block_type {
                todo!("error")
            }
        }

        let fun = Rc::new(ast::Fun::new(f.ident, params, return_type, block));
        self.def_fun(Rc::clone(&fun))?;

        Ok(Ast::new(AstT::FunDef(fun), DataType::Unit, span)) // TODO: consider just returning empty ast
    }

    fn check_fun_call(&mut self, f: cst::FunCall) -> crate::Result<Ast> {
        let span = f.span();

        let fun = self.resolve_fun(&f.ident)?;

        let mut args = Vec::with_capacity(f.args.items.len());
        for a in f.args.items {
            args.push(self.check_type(a)?);
        }

        let return_type = match fun.return_type {
            Some(t) => t,
            None => DataType::Unit,
        };

        Ok(Ast::new(AstT::FunCall(fun, args), return_type, span))
    }

    fn check_var_def(&mut self, v: cst::VarDef) -> crate::Result<Ast> {
        let span = v.span();
        let val = self.check_type(*v.value.1)?;

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

        self.def_var(Var::new(v.ident, typ, true, mutable));

        Ok(Ast::new(
            AstT::VarDef(v.ident, Box::new(val), mutable),
            DataType::Unit,
            span,
        ))
    }

    fn check_prefix(&mut self, p: Prefix, a: Cst, span: Span) -> crate::Result<Ast> {
        let a = self.check_type(a)?;
        let data_type = match p.resulting_type(a.data_type) {
            Some(t) => t,
            None => return Err(crate::Error::PrefixNotApplicable(p, a)),
        };

        Ok(Ast::new(AstT::Prefix(p, Box::new(a)), data_type, span))
    }

    fn check_postfix(&mut self, a: Cst, p: Postfix, span: Span) -> crate::Result<Ast> {
        let a = self.check_type(a)?;
        let data_type = match p.resulting_type(a.data_type) {
            Some(t) => t,
            None => return Err(crate::Error::PostfixNotApplicable(a, p)),
        };

        Ok(Ast::new(AstT::Postfix(Box::new(a), p), data_type, span))
    }

    fn check_infix(&mut self, a: Cst, i: Infix, b: Cst, span: Span) -> crate::Result<Ast> {
        let a = self.check_type(a)?;
        let b = self.check_type(b)?;

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
        a: IdentSpan,
        i: Infix,
        b: Cst,
        span: Span,
    ) -> crate::Result<Ast> {
        let a_type = self.resolve_var(&a)?;
        let b = self.check_type(b)?;

        let data_type = match i.resulting_type(a_type, b.data_type) {
            Some(t) => t,
            None => return Err(crate::Error::AssignInfixNotApplicable((a, a_type), i, b)),
        };

        self.set_var(&a, &b)?;

        Ok(Ast::new(
            AstT::InfixAssign(a, i, Box::new(b)),
            data_type,
            span,
        ))
    }

    fn check_assign(&mut self, a: IdentSpan, i: Infix, b: Cst, span: Span) -> crate::Result<Ast> {
        let a_type = self.resolve_var(&a)?;
        let b = self.check_type(b)?;

        let data_type = match i.resulting_type(a_type, b.data_type) {
            Some(t) => t,
            None => return Err(crate::Error::AssignNotApplicable((a, a_type), i, b)),
        };

        self.set_var(&a, &b)?;

        Ok(Ast::new(AstT::Assign(a, i, Box::new(b)), data_type, span))
    }

    fn resolve_data_type(&self, typ: &IdentSpan) -> crate::Result<DataType> {
        let name = self.idents.name(typ.ident);
        match DataType::from(name) {
            Some(d) => Ok(d),
            None => Err(crate::Error::UnknownType(name.into(), typ.span)),
        }
    }
}
