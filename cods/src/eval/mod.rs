use std::iter::Peekable;
use std::rc::Rc;

use crate::ast::{BuiltinFunCall, ForLoop, Fun, IfExpr, Var, WhileLoop};
use crate::{
    Ast, AstT, BoolExpr, DataType, FloatExpr, IntExpr, Range, RangeExpr, StrExpr, Val, ValSpan,
};

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

pub fn eval_iter(asts: &[Ast]) -> Peekable<impl Iterator<Item = crate::Result<Val>> + '_> {
    asts.iter().map(eval_ast).peekable()
}

pub fn eval_ast(ast: &Ast) -> crate::Result<Val> {
    match &ast.typ {
        AstT::Error => Err(crate::Error::Parsing(ast.span)),
        AstT::Var(v) => Ok(v.get()),
        AstT::Int(i) => eval_int_expr(i),
        AstT::Float(f) => eval_float_expr(f),
        AstT::Bool(b) => eval_bool_expr(b),
        AstT::Str(s) => eval_str_expr(s),
        AstT::Range(r) => eval_range_expr(r),
        AstT::Unit => Ok(Val::Unit),
        AstT::Block(b) => eval_all(b),
        AstT::IfExpr(i) => eval_if_expr(i),
        AstT::WhileLoop(w) => eval_while_loop(w),
        AstT::ForLoop(f) => eval_for_loop(f),
        AstT::Assign(v, e) => eval_assign(v, e),
        AstT::VarDef(v, e) => eval_var_def(v, e),
        AstT::FunCall(f, a) => eval_fun_call(f, a),
        AstT::BuiltinFunCall(f, a) => eval_builtin_fun_call(*f, a),
        AstT::Spill(v) => eval_spill(v),
    }
}

fn eval_int_expr(expr: &IntExpr) -> crate::Result<Val> {
    let int = match expr {
        IntExpr::Val(i) => *i,
        IntExpr::Cast(a) => match eval_ast(a)? {
            Val::Int(i) => i,
            Val::Float(f) => f as i128,
            v => {
                return Err(crate::Error::CastFailed(
                    (v.data_type(), a.span),
                    DataType::Int,
                ));
            }
        },
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
        IntExpr::RemEuclid(a, b) => {
            let va = eval_ast(a)?.unwrap_int();
            let vb = eval_ast(b)?.unwrap_int();
            if vb == 0 {
                return Err(crate::Error::RemainderByZero(a.span, b.span));
            }
            let r = va % vb;
            if (r > 0 && vb < 0) || (r < 0 && vb > 0) {
                r + vb
            } else {
                r
            }
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
                f = f.checked_mul(i).ok_or_else(|| {
                    crate::Error::FactorialOverflow(ValSpan::new(Val::Int(va), a.span))
                })?;
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
        FloatExpr::Cast(a) => match eval_ast(a)? {
            Val::Float(f) => f,
            Val::Int(i) => i as f64,
            v => {
                return Err(crate::Error::CastFailed(
                    (v.data_type(), a.span),
                    DataType::Float,
                ));
            }
        },
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
    };

    Ok(Val::Float(float))
}

fn eval_bool_expr(expr: &BoolExpr) -> crate::Result<Val> {
    let bool = match expr {
        BoolExpr::Val(b) => *b,
        BoolExpr::Cast(a) => match eval_ast(a)? {
            Val::Bool(b) => b,
            v => {
                return Err(crate::Error::CastFailed(
                    (v.data_type(), a.span),
                    DataType::Bool,
                ));
            }
        },
        BoolExpr::Is(a, d) => {
            let v = eval_ast(a)?;
            v.data_type().is(*d)
        }
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
        StrExpr::Cast(a) => match eval_ast(a)? {
            Val::Str(s) => s,
            v => {
                return Err(crate::Error::CastFailed(
                    (v.data_type(), a.span),
                    DataType::Str,
                ));
            }
        },
    };

    Ok(Val::Str(string))
}

fn eval_range_expr(expr: &RangeExpr) -> crate::Result<Val> {
    let range = match expr {
        RangeExpr::Val(r) => *r,
        RangeExpr::Cast(a) => match eval_ast(a)? {
            Val::Range(r) => r,
            v => {
                return Err(crate::Error::CastFailed(
                    (v.data_type(), a.span),
                    DataType::Range,
                ));
            }
        },
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

fn eval_builtin_fun_call(fun: BuiltinFunCall, args: &[Ast]) -> crate::Result<Val> {
    let val = match fun {
        BuiltinFunCall::PowInt => {
            let a = &args[0];
            let b = &args[1];
            let base = eval_ast(a)?.unwrap_int();
            let exp = eval_ast(b)?.unwrap_int();
            if exp < 0 {
                return Err(crate::Error::NegativeIntPow(a.span, b.span));
            }
            if exp > u32::MAX as i128 {
                return Err(crate::Error::PowOverflow(a.span, b.span));
            }
            let val = base
                .checked_pow(exp as u32)
                .ok_or(crate::Error::PowOverflow(a.span, b.span))?;
            Val::Int(val)
        }
        BuiltinFunCall::PowFloat => {
            let base = eval_ast(&args[0])?.unwrap_float();
            let exp = eval_ast(&args[1])?.unwrap_float();
            Val::Float(base.powf(exp))
        }
        BuiltinFunCall::Ln => {
            let num = eval_ast(&args[0])?.unwrap_float();
            Val::Float(num.ln())
        }
        BuiltinFunCall::Log => {
            let base = eval_ast(&args[0])?.unwrap_float();
            let num = eval_ast(&args[1])?.unwrap_float();
            Val::Float(num.log(base))
        }
        BuiltinFunCall::Sqrt => {
            let num = eval_ast(&args[0])?.unwrap_float();
            Val::Float(num.sqrt())
        }
        BuiltinFunCall::Ncr => {
            let n = eval_ast(&args[0])?.unwrap_int();
            let mut r = eval_ast(&args[1])?.unwrap_int();
            if r < 0 {
                return Err(crate::Error::NegativeNcr(ValSpan::new(
                    Val::Int(r),
                    args[1].span,
                )));
            }
            if n < r {
                return Err(crate::Error::InvalidNcr(
                    ValSpan::new(Val::Int(n), args[0].span),
                    ValSpan::new(Val::Int(r), args[1].span),
                ));
            }

            // symmetrical: nCr(9, 2) == nCr(9, 7)
            if r > n - r {
                r = n - r;
            }

            let mut val = 1;
            for i in 1..=r {
                val *= n - r + i;
                val /= i;
            }

            Val::Int(val)
        }
        BuiltinFunCall::ToDeg => {
            let num = eval_ast(&args[0])?.unwrap_float();
            Val::Float(num.to_degrees())
        }
        BuiltinFunCall::ToRad => {
            let num = eval_ast(&args[0])?.unwrap_float();
            Val::Float(num.to_radians())
        }
        BuiltinFunCall::Sin => {
            let num = eval_ast(&args[0])?.unwrap_float();
            Val::Float(num.sin())
        }
        BuiltinFunCall::Cos => {
            let num = eval_ast(&args[0])?.unwrap_float();
            Val::Float(num.cos())
        }
        BuiltinFunCall::Tan => {
            let num = eval_ast(&args[0])?.unwrap_float();
            Val::Float(num.tan())
        }
        BuiltinFunCall::Asin => {
            let num = eval_ast(&args[0])?.unwrap_float();
            Val::Float(num.asin())
        }
        BuiltinFunCall::Acos => {
            let num = eval_ast(&args[0])?.unwrap_float();
            Val::Float(num.acos())
        }
        BuiltinFunCall::Atan => {
            let num = eval_ast(&args[0])?.unwrap_float();
            Val::Float(num.atan())
        }
        BuiltinFunCall::Gcd => {
            let mut a = eval_ast(&args[0])?.unwrap_int();
            let mut b = eval_ast(&args[1])?.unwrap_int();
            while b != 0 {
                let t = b;
                b = a % b;
                a = t;
            }
            Val::Int(a)
        }
        BuiltinFunCall::MinInt => Val::Int(fold_eval_int(args, i128::min)?),
        BuiltinFunCall::MinFloat => Val::Float(fold_eval_float(args, f64::min)?),
        BuiltinFunCall::MaxInt => Val::Int(fold_eval_int(args, i128::max)?),
        BuiltinFunCall::MaxFloat => Val::Float(fold_eval_float(args, f64::max)?),
        BuiltinFunCall::ClampInt => {
            let num = eval_ast(&args[0])?.unwrap_int();
            let min = eval_ast(&args[1])?.unwrap_int();
            let max = eval_ast(&args[2])?.unwrap_int();
            if min > max {
                return Err(crate::Error::InvalidClampBounds(
                    ValSpan::new(Val::Int(min), args[1].span),
                    ValSpan::new(Val::Int(max), args[2].span),
                ));
            }
            Val::Int(num.clamp(min, max))
        }
        BuiltinFunCall::ClampFloat => {
            let num = eval_ast(&args[0])?.unwrap_float();
            let min = eval_ast(&args[1])?.unwrap_float();
            let max = eval_ast(&args[2])?.unwrap_float();
            // floating point weirdness, negated assertion of stdlib
            #[allow(clippy::neg_cmp_op_on_partial_ord)]
            if !(min <= max) {
                return Err(crate::Error::InvalidClampBounds(
                    ValSpan::new(Val::Float(min), args[1].span),
                    ValSpan::new(Val::Float(max), args[2].span),
                ));
            }
            Val::Float(num.clamp(min, max))
        }
        BuiltinFunCall::Print => {
            eval_print(args)?;
            Val::Unit
        }
        BuiltinFunCall::Println => {
            eval_print(args)?;
            println!();
            Val::Unit
        }
        BuiltinFunCall::Spill => unreachable!(), // TODO: cleanup
        BuiltinFunCall::SpillLocal => unreachable!(),
        BuiltinFunCall::Assert => {
            let va = eval_ast(&args[0])?.unwrap_bool();
            if !va {
                return Err(crate::Error::AssertFailed(args[0].span));
            }
            Val::Unit
        }
        BuiltinFunCall::AssertEq => {
            let a = eval_ast(&args[0])?;
            let b = eval_ast(&args[1])?;
            if a != b {
                return Err(crate::Error::AssertEqFailed(
                    ValSpan::new(a, args[0].span),
                    ValSpan::new(b, args[1].span),
                ));
            }
            Val::Unit
        }
    };
    Ok(val)
}

fn eval_print(args: &[Ast]) -> crate::Result<()> {
    let mut vals = eval_iter(args);
    if let Some(first) = vals.next() {
        print!("{}", first?); // TODO: Evaluator struct with stdio
    }

    for v in vals {
        print!(" {}", v?);
    }

    Ok(())
}

fn eval_spill(vars: &[(String, Rc<Var>)]) -> crate::Result<Val> {
    for (n, v) in vars {
        println!("{n} = {}", v.get());
    }
    Ok(Val::Unit)
}

fn fold_eval_int(args: &[Ast], fold: fn(i128, i128) -> i128) -> crate::Result<i128> {
    let mut current = eval_ast(&args[0])?.unwrap_int();
    for a in &args[1..] {
        let val = eval_ast(a)?.unwrap_int();
        current = fold(current, val);
    }
    Ok(current)
}

fn fold_eval_float(args: &[Ast], fold: fn(f64, f64) -> f64) -> crate::Result<f64> {
    let mut current = eval_ast(&args[0])?.unwrap_float();
    for a in &args[1..] {
        let val = eval_ast(a)?.unwrap_float();
        current = fold(current, val);
    }
    Ok(current)
}
