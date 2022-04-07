use std::rc::Rc;

use crate::ast::{ForLoop, Fun, IfExpr, Var, WhileLoop};
use crate::{Ast, BoolExpr, FloatExpr, IntExpr, Range, RangeExpr, StrExpr, Val, ValSpan};

pub use val::*;

mod val;

pub fn eval_all(asts: &[Ast]) -> crate::Result<Val> {
    match asts.split_last() {
        Some((last, others)) => {
            for a in others {
                eval_ast(a)?;
            }

            eval_ast(last)
        }
        None => Ok(Val::Unit),
    }
}

pub fn eval_ast(ast: &Ast) -> crate::Result<Val> {
    match &ast.typ {
        crate::AstT::Error => Err(crate::Error::Parsing(ast.span)),
        crate::AstT::Var(v) => Ok(v.get()),
        crate::AstT::Int(i) => eval_int_expr(i),
        crate::AstT::Float(f) => eval_float_expr(f),
        crate::AstT::Bool(b) => eval_bool_expr(b),
        crate::AstT::Str(s) => eval_str_expr(s),
        crate::AstT::Range(r) => eval_range_expr(r),
        crate::AstT::Unit => Ok(Val::Unit),
        crate::AstT::Block(b) => eval_all(b),
        crate::AstT::IfExpr(i) => eval_if_expr(i),
        crate::AstT::WhileLoop(w) => eval_while_loop(w),
        crate::AstT::ForLoop(f) => eval_for_loop(f),
        crate::AstT::Assign(v, e) => eval_assign(v, e),
        crate::AstT::VarDef(v, e) => eval_var_def(v, e),
        crate::AstT::FunCall(f, a) => eval_fun_call(f, a),
    }
}

fn eval_int_expr(expr: &IntExpr) -> crate::Result<Val> {
    let int = match expr {
        IntExpr::Val(i) => *i,
        IntExpr::CastFloat(_) => todo!(),
        IntExpr::Neg(a) => eval_ast(a)?
            .unwrap_int()
            .checked_neg()
            .ok_or(crate::Error::NegOverflow(a.span))?,
        IntExpr::Add(a, b) => {
            let va = eval_ast(a)?.unwrap_int();
            let vb = eval_ast(b)?.unwrap_int();
            va.checked_add(vb)
                .ok_or(crate::Error::AddOverflow(a.span, b.span))?
        }
        IntExpr::Sub(a, b) => {
            let va = eval_ast(a)?.unwrap_int();
            let vb = eval_ast(b)?.unwrap_int();
            va.checked_sub(vb)
                .ok_or(crate::Error::SubOverflow(a.span, b.span))?
        }
        IntExpr::Mul(a, b) => {
            let va = eval_ast(a)?.unwrap_int();
            let vb = eval_ast(b)?.unwrap_int();
            va.checked_mul(vb)
                .ok_or(crate::Error::MulOverflow(a.span, b.span))?
        }
        IntExpr::Div(a, b) => {
            let va = eval_ast(a)?.unwrap_int();
            let vb = eval_ast(b)?.unwrap_int();
            va.checked_div(vb)
                .ok_or(crate::Error::DivideByZero(a.span, b.span))?
        }
        IntExpr::Rem(a, b) => {
            let va = eval_ast(a)?.unwrap_int();
            let vb = eval_ast(b)?.unwrap_int();
            va.checked_rem(vb)
                .ok_or(crate::Error::RemainderByZero(a.span, b.span))?
        }
        IntExpr::Pow(a, b) => {
            let va = eval_ast(a)?.unwrap_int();
            let vb = eval_ast(b)?.unwrap_int();
            if vb < 0 {
                return Err(crate::Error::NegativeIntPow(a.span, b.span));
            }
            if vb > u32::MAX as i128 {
                return Err(crate::Error::PowOverflow(a.span, b.span));
            }
            va.checked_pow(vb as u32)
                .ok_or(crate::Error::PowOverflow(a.span, b.span))?
        }
        IntExpr::Factorial(a) => {
            let va = eval_ast(a)?.unwrap_int();

            if va < 0 {
                return Err(crate::Error::NegativeFactorial(ValSpan::new(
                    Val::Int(va),
                    a.span,
                )));
            }

            let mut f: i128 = 1;
            for i in 2..=va {
                f = f
                    .checked_mul(i)
                    .ok_or(crate::Error::FactorialOverflow(ValSpan::new(
                        Val::Int(va),
                        a.span,
                    )))?;
            }
            f
        }
        IntExpr::BwOr(a, b) => {
            let va = eval_ast(a)?.unwrap_int();
            let vb = eval_ast(b)?.unwrap_int();
            va | vb
        }
        IntExpr::BwAnd(a, b) => {
            let va = eval_ast(a)?.unwrap_int();
            let vb = eval_ast(b)?.unwrap_int();
            va & vb
        }
    };

    Ok(Val::Int(int))
}

fn eval_float_expr(expr: &FloatExpr) -> crate::Result<Val> {
    let float = match expr {
        FloatExpr::Val(f) => *f,
        FloatExpr::CastInt(a) => eval_ast(a)?.unwrap_int() as f64,
        FloatExpr::Neg(a) => -eval_ast(a)?.unwrap_float(),
        FloatExpr::Add(a, b) => {
            let va = eval_ast(a)?.unwrap_float();
            let vb = eval_ast(b)?.unwrap_float();
            va + vb
        }
        FloatExpr::Sub(a, b) => {
            let va = eval_ast(a)?.unwrap_float();
            let vb = eval_ast(b)?.unwrap_float();
            va - vb
        }
        FloatExpr::Mul(a, b) => {
            let va = eval_ast(a)?.unwrap_float();
            let vb = eval_ast(b)?.unwrap_float();
            va * vb
        }
        FloatExpr::Div(a, b) => {
            let va = eval_ast(a)?.unwrap_float();
            let vb = eval_ast(b)?.unwrap_float();
            va / vb
        }
        FloatExpr::Pow(a, b) => {
            let va = eval_ast(a)?.unwrap_float();
            let vb = eval_ast(b)?.unwrap_float();
            va.powf(vb)
        }
    };

    Ok(Val::Float(float))
}

fn eval_bool_expr(expr: &BoolExpr) -> crate::Result<Val> {
    let bool = match expr {
        BoolExpr::Val(b) => *b,
        BoolExpr::Not(a) => !eval_ast(a)?.unwrap_bool(),
        BoolExpr::Eq(a, b) => {
            let va = eval_ast(a)?;
            let vb = eval_ast(b)?;
            va == vb
        }
        BoolExpr::Ne(a, b) => {
            let va = eval_ast(a)?;
            let vb = eval_ast(b)?;
            va != vb
        }
        BoolExpr::LtInt(a, b) => {
            let va = eval_ast(a)?.unwrap_int();
            let vb = eval_ast(b)?.unwrap_int();
            va < vb
        }
        BoolExpr::LtFloat(a, b) => {
            let va = eval_ast(a)?.unwrap_float();
            let vb = eval_ast(b)?.unwrap_float();
            va < vb
        }
        BoolExpr::LeInt(a, b) => {
            let va = eval_ast(a)?.unwrap_int();
            let vb = eval_ast(b)?.unwrap_int();
            va <= vb
        }
        BoolExpr::LeFloat(a, b) => {
            let va = eval_ast(a)?.unwrap_float();
            let vb = eval_ast(b)?.unwrap_float();
            va <= vb
        }
        BoolExpr::GtInt(a, b) => {
            let va = eval_ast(a)?.unwrap_int();
            let vb = eval_ast(b)?.unwrap_int();
            va > vb
        }
        BoolExpr::GtFloat(a, b) => {
            let va = eval_ast(a)?.unwrap_float();
            let vb = eval_ast(b)?.unwrap_float();
            va > vb
        }
        BoolExpr::GeInt(a, b) => {
            let va = eval_ast(a)?.unwrap_int();
            let vb = eval_ast(b)?.unwrap_int();
            va >= vb
        }
        BoolExpr::GeFloat(a, b) => {
            let va = eval_ast(a)?.unwrap_float();
            let vb = eval_ast(b)?.unwrap_float();
            va > vb
        }
        BoolExpr::BwOr(a, b) => {
            let va = eval_ast(a)?.unwrap_bool();
            let vb = eval_ast(b)?.unwrap_bool();
            va | vb
        }
        BoolExpr::BwAnd(a, b) => {
            let va = eval_ast(a)?.unwrap_bool();
            let vb = eval_ast(b)?.unwrap_bool();
            va & vb
        }
        BoolExpr::Or(a, b) => {
            let va = eval_ast(a)?.unwrap_bool();
            let vb = eval_ast(b)?.unwrap_bool();
            va || vb
        }
        BoolExpr::And(a, b) => {
            let va = eval_ast(a)?.unwrap_bool();
            let vb = eval_ast(b)?.unwrap_bool();
            va && vb
        }
    };

    Ok(Val::Bool(bool))
}

fn eval_str_expr(expr: &StrExpr) -> crate::Result<Val> {
    let string = match expr {
        StrExpr::Val(s) => s.clone(),
    };

    Ok(Val::Str(string))
}

fn eval_range_expr(expr: &RangeExpr) -> crate::Result<Val> {
    let range = match expr {
        RangeExpr::Val(r) => *r,
        RangeExpr::Ex(a, b) => {
            let va = eval_ast(a)?.unwrap_int();
            let vb = eval_ast(b)?.unwrap_int();
            Range::Exclusive(va, vb)
        }
        RangeExpr::In(a, b) => {
            let va = eval_ast(a)?.unwrap_int();
            let vb = eval_ast(b)?.unwrap_int();
            Range::Inclusive(va, vb)
        }
    };

    Ok(Val::Range(range))
}

fn eval_if_expr(if_expr: &IfExpr) -> crate::Result<Val> {
    for c in if_expr.cases.iter() {
        if eval_ast(&c.cond)?.unwrap_bool() {
            return eval_all(&c.block);
        }
    }

    match &if_expr.else_block {
        Some(b) => eval_all(b),
        None => Ok(Val::Unit),
    }
}

fn eval_while_loop(whl_loop: &WhileLoop) -> crate::Result<Val> {
    while eval_ast(&whl_loop.cond)?.unwrap_bool() {
        eval_all(&whl_loop.block)?;
    }

    Ok(Val::Unit)
}

fn eval_for_loop(for_loop: &ForLoop) -> crate::Result<Val> {
    let iter = eval_ast(&for_loop.iter)?.unwrap_range();

    for i in iter.iter() {
        for_loop.var.set(Val::Int(i));
        eval_all(&for_loop.block)?;
    }

    Ok(Val::Unit)
}

fn eval_assign(var: &Rc<Var>, expr: &Ast) -> crate::Result<Val> {
    let val = eval_ast(expr)?;
    var.set(val);
    Ok(Val::Unit)
}

fn eval_var_def(var: &Rc<Var>, expr: &Ast) -> crate::Result<Val> {
    let val = eval_ast(expr)?;
    var.set(val);
    Ok(Val::Unit)
}

fn eval_fun_call(fun: &Rc<Fun>, args: &[Ast]) -> crate::Result<Val> {
    let mut arg_vals = Vec::with_capacity(args.len());
    for a in args {
        arg_vals.push(eval_ast(a)?);
    }
    for (p, v) in fun.params().iter().zip(arg_vals.into_iter()) {
        p.set(v)
    }

    eval_all(fun.block())
}
