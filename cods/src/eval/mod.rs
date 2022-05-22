use std::rc::Rc;

use crate::ast::{BuiltinFunCall, CharExpr, ForLoop, Fun, IfExpr, WhileLoop};
use crate::{
    Ast, AstT, Asts, BoolExpr, DataType, FloatExpr, IntExpr, Range, RangeExpr, StrExpr, Val,
    ValSpan,
};

pub use stack::*;
pub use val::*;

mod stack;
mod val;

pub fn eval(asts: &Asts) -> crate::Result<Val> {
    let mut stack = Stack::default();
    let val = eval_with(&mut stack, asts)?;
    Ok(val)
}

pub fn eval_with(stack: &mut Stack, asts: &Asts) -> crate::Result<Val> {
    stack.extend_to(asts.global_frame_size);
    let val = eval_asts(stack, &asts.asts)?;
    Ok(val)
}

fn eval_asts(stack: &mut Stack, asts: &[Ast]) -> crate::Result<Val> {
    match asts.split_last() {
        Some((last, others)) => {
            for a in others {
                eval_ast(stack, a)?;
            }

            eval_ast(stack, last)
        }
        None => Ok(Val::Unit),
    }
}

fn eval_iter<'a>(
    stack: &'a mut Stack,
    asts: &'a [Ast],
) -> impl Iterator<Item = crate::Result<Val>> + 'a {
    asts.iter().map(|a| eval_ast(stack, a))
}

fn eval_ast(stack: &mut Stack, ast: &Ast) -> crate::Result<Val> {
    match &ast.typ {
        AstT::Error => Err(crate::Error::Parsing(ast.span)),
        AstT::Var(v) => Ok(stack.get(v)),
        AstT::Int(i) => eval_int_expr(stack, i),
        AstT::Float(f) => eval_float_expr(stack, f),
        AstT::Bool(b) => eval_bool_expr(stack, b),
        AstT::Char(c) => eval_char_expr(stack, c),
        AstT::Str(s) => eval_str_expr(stack, s),
        AstT::Range(r) => eval_range_expr(stack, r),
        AstT::Unit => Ok(Val::Unit),
        AstT::Block(b) => eval_asts(stack, b),
        AstT::IfExpr(i) => eval_if_expr(stack, i),
        AstT::WhileLoop(w) => eval_while_loop(stack, w),
        AstT::ForLoop(f) => eval_for_loop(stack, f),
        AstT::Assign(v, e) => eval_assign(stack, v, e),
        AstT::VarDef(v, e) => eval_var_def(stack, v, e),
        AstT::FunCall(f, a) => eval_fun_call(stack, f, a),
        AstT::Return(v) => eval_return(stack, v),
        AstT::BuiltinFunCall(f, a) => eval_builtin_fun_call(stack, *f, a),
        AstT::Spill(v) => eval_spill(stack, v),
    }
}

fn eval_int_expr(stack: &mut Stack, expr: &IntExpr) -> crate::Result<Val> {
    let int = match expr {
        IntExpr::Val(i) => *i,
        IntExpr::Cast(a) => match eval_ast(stack, a)? {
            Val::Int(i) => i,
            Val::Float(f) => f as i128,
            v => {
                return Err(crate::Error::CastFailed(
                    (v.data_type(), a.span),
                    DataType::Int,
                ));
            }
        },
        IntExpr::Neg(a) => eval_ast(stack, a)?
            .unwrap_int()
            .checked_neg()
            .ok_or(crate::Error::NegOverflow(a.span))?,
        IntExpr::Add(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_int();
            let vb = eval_ast(stack, b)?.unwrap_int();
            va.checked_add(vb)
                .ok_or(crate::Error::AddOverflow(a.span, b.span))?
        }
        IntExpr::Sub(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_int();
            let vb = eval_ast(stack, b)?.unwrap_int();
            va.checked_sub(vb)
                .ok_or(crate::Error::SubOverflow(a.span, b.span))?
        }
        IntExpr::Mul(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_int();
            let vb = eval_ast(stack, b)?.unwrap_int();
            va.checked_mul(vb)
                .ok_or(crate::Error::MulOverflow(a.span, b.span))?
        }
        IntExpr::Div(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_int();
            let vb = eval_ast(stack, b)?.unwrap_int();
            va.checked_div(vb)
                .ok_or(crate::Error::DivideByZero(a.span, b.span))?
        }
        IntExpr::Rem(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_int();
            let vb = eval_ast(stack, b)?.unwrap_int();
            va.checked_rem(vb)
                .ok_or(crate::Error::RemainderByZero(a.span, b.span))?
        }
        IntExpr::RemEuclid(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_int();
            let vb = eval_ast(stack, b)?.unwrap_int();
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
            let va = eval_ast(stack, a)?.unwrap_int();

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
            let va = eval_ast(stack, a)?.unwrap_int();
            let vb = eval_ast(stack, b)?.unwrap_int();
            va | vb
        }
        IntExpr::BwAnd(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_int();
            let vb = eval_ast(stack, b)?.unwrap_int();
            va & vb
        }
    };

    Ok(Val::Int(int))
}

fn eval_float_expr(stack: &mut Stack, expr: &FloatExpr) -> crate::Result<Val> {
    let float = match expr {
        FloatExpr::Val(f) => *f,
        FloatExpr::Cast(a) => match eval_ast(stack, a)? {
            Val::Float(f) => f,
            Val::Int(i) => i as f64,
            v => {
                return Err(crate::Error::CastFailed(
                    (v.data_type(), a.span),
                    DataType::Float,
                ));
            }
        },
        FloatExpr::Neg(a) => -eval_ast(stack, a)?.unwrap_float(),
        FloatExpr::Add(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_float();
            let vb = eval_ast(stack, b)?.unwrap_float();
            va + vb
        }
        FloatExpr::Sub(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_float();
            let vb = eval_ast(stack, b)?.unwrap_float();
            va - vb
        }
        FloatExpr::Mul(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_float();
            let vb = eval_ast(stack, b)?.unwrap_float();
            va * vb
        }
        FloatExpr::Div(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_float();
            let vb = eval_ast(stack, b)?.unwrap_float();
            va / vb
        }
    };

    Ok(Val::Float(float))
}

fn eval_bool_expr(stack: &mut Stack, expr: &BoolExpr) -> crate::Result<Val> {
    let bool = match expr {
        BoolExpr::Val(b) => *b,
        BoolExpr::Cast(a) => match eval_ast(stack, a)? {
            Val::Bool(b) => b,
            v => {
                return Err(crate::Error::CastFailed(
                    (v.data_type(), a.span),
                    DataType::Bool,
                ));
            }
        },
        BoolExpr::Is(a, d) => {
            let v = eval_ast(stack, a)?;
            v.data_type().is(*d)
        }
        BoolExpr::Not(a) => !eval_ast(stack, a)?.unwrap_bool(),
        BoolExpr::Eq(a, b) => {
            let va = eval_ast(stack, a)?;
            let vb = eval_ast(stack, b)?;
            va == vb
        }
        BoolExpr::Ne(a, b) => {
            let va = eval_ast(stack, a)?;
            let vb = eval_ast(stack, b)?;
            va != vb
        }
        BoolExpr::LtInt(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_int();
            let vb = eval_ast(stack, b)?.unwrap_int();
            va < vb
        }
        BoolExpr::LtFloat(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_float();
            let vb = eval_ast(stack, b)?.unwrap_float();
            va < vb
        }
        BoolExpr::LeInt(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_int();
            let vb = eval_ast(stack, b)?.unwrap_int();
            va <= vb
        }
        BoolExpr::LeFloat(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_float();
            let vb = eval_ast(stack, b)?.unwrap_float();
            va <= vb
        }
        BoolExpr::GtInt(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_int();
            let vb = eval_ast(stack, b)?.unwrap_int();
            va > vb
        }
        BoolExpr::GtFloat(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_float();
            let vb = eval_ast(stack, b)?.unwrap_float();
            va > vb
        }
        BoolExpr::GeInt(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_int();
            let vb = eval_ast(stack, b)?.unwrap_int();
            va >= vb
        }
        BoolExpr::GeFloat(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_float();
            let vb = eval_ast(stack, b)?.unwrap_float();
            va > vb
        }
        BoolExpr::BwOr(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_bool();
            let vb = eval_ast(stack, b)?.unwrap_bool();
            va | vb
        }
        BoolExpr::BwAnd(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_bool();
            let vb = eval_ast(stack, b)?.unwrap_bool();
            va & vb
        }
        BoolExpr::Or(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_bool();
            let vb = eval_ast(stack, b)?.unwrap_bool();
            va || vb
        }
        BoolExpr::And(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_bool();
            let vb = eval_ast(stack, b)?.unwrap_bool();
            va && vb
        }
    };

    Ok(Val::Bool(bool))
}

fn eval_char_expr(stack: &mut Stack, expr: &CharExpr) -> crate::Result<Val> {
    let char = match expr {
        CharExpr::Val(s) => *s,
        CharExpr::Cast(a) => match eval_ast(stack, a)? {
            Val::Char(c) => c,
            v => {
                return Err(crate::Error::CastFailed(
                    (v.data_type(), a.span),
                    DataType::Char,
                ));
            }
        },
    };

    Ok(Val::Char(char))
}

fn eval_str_expr(stack: &mut Stack, expr: &StrExpr) -> crate::Result<Val> {
    let string = match expr {
        StrExpr::Val(s) => s.clone(),
        StrExpr::Cast(a) => match eval_ast(stack, a)? {
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

fn eval_range_expr(stack: &mut Stack, expr: &RangeExpr) -> crate::Result<Val> {
    let range = match expr {
        RangeExpr::Val(r) => *r,
        RangeExpr::Cast(a) => match eval_ast(stack, a)? {
            Val::Range(r) => r,
            v => {
                return Err(crate::Error::CastFailed(
                    (v.data_type(), a.span),
                    DataType::Range,
                ));
            }
        },
        RangeExpr::Ex(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_int();
            let vb = eval_ast(stack, b)?.unwrap_int();
            Range::Exclusive(va, vb)
        }
        RangeExpr::In(a, b) => {
            let va = eval_ast(stack, a)?.unwrap_int();
            let vb = eval_ast(stack, b)?.unwrap_int();
            Range::Inclusive(va, vb)
        }
    };

    Ok(Val::Range(range))
}

fn eval_if_expr(stack: &mut Stack, if_expr: &IfExpr) -> crate::Result<Val> {
    for c in if_expr.cases.iter() {
        if eval_ast(stack, &c.cond)?.unwrap_bool() {
            return eval_asts(stack, &c.block);
        }
    }

    match &if_expr.else_block {
        Some(b) => eval_asts(stack, b),
        None => Ok(Val::Unit),
    }
}

fn eval_while_loop(stack: &mut Stack, whl_loop: &WhileLoop) -> crate::Result<Val> {
    while eval_ast(stack, &whl_loop.cond)?.unwrap_bool() {
        eval_asts(stack, &whl_loop.block)?;
    }

    Ok(Val::Unit)
}

fn eval_for_loop(stack: &mut Stack, for_loop: &ForLoop) -> crate::Result<Val> {
    let iter = eval_ast(stack, &for_loop.iter)?.unwrap_range();

    for i in iter.iter() {
        stack.set(&for_loop.var, Val::Int(i));
        eval_asts(stack, &for_loop.block)?;
    }

    Ok(Val::Unit)
}

fn eval_assign(stack: &mut Stack, var: &VarRef, expr: &Ast) -> crate::Result<Val> {
    let val = eval_ast(stack, expr)?;
    stack.set(var, val);
    Ok(Val::Unit)
}

fn eval_var_def(stack: &mut Stack, var: &VarRef, expr: &Ast) -> crate::Result<Val> {
    let val = eval_ast(stack, expr)?;
    stack.set(var, val);
    Ok(Val::Unit)
}

fn eval_fun_call(stack: &mut Stack, fun: &Rc<Fun>, args: &[Ast]) -> crate::Result<Val> {
    let fun_ref = fun.borrow();
    let mut arg_vals = Vec::with_capacity(fun_ref.params().len());
    for (p, a) in fun_ref.params().iter().zip(args.iter()) {
        let val = eval_ast(stack, a)?;
        arg_vals.push((p, val));
    }

    stack.push(fun.frame_size());
    for (p, a) in arg_vals {
        stack.set(p, a);
    }
    let block = fun_ref.block();
    let val = match eval_asts(stack, block) {
        Err(crate::Error::Return(v)) => Ok(v),
        r => r,
    };
    stack.pop();
    val
}

fn eval_return(stack: &mut Stack, val: &Ast) -> crate::Result<Val> {
    let val = eval_ast(stack, val)?;
    Err(crate::Error::Return(val))
}

fn eval_builtin_fun_call(
    stack: &mut Stack,
    fun: BuiltinFunCall,
    args: &[Ast],
) -> crate::Result<Val> {
    let val = match fun {
        BuiltinFunCall::PowInt => {
            let a = &args[0];
            let b = &args[1];
            let base = eval_ast(stack, a)?.unwrap_int();
            let exp = eval_ast(stack, b)?.unwrap_int();
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
            let base = eval_ast(stack, &args[0])?.unwrap_float();
            let exp = eval_ast(stack, &args[1])?.unwrap_float();
            Val::Float(base.powf(exp))
        }
        BuiltinFunCall::Ln => {
            let num = eval_ast(stack, &args[0])?.unwrap_float();
            Val::Float(num.ln())
        }
        BuiltinFunCall::Log => {
            let base = eval_ast(stack, &args[0])?.unwrap_float();
            let num = eval_ast(stack, &args[1])?.unwrap_float();
            Val::Float(num.log(base))
        }
        BuiltinFunCall::Sqrt => {
            let num = eval_ast(stack, &args[0])?.unwrap_float();
            Val::Float(num.sqrt())
        }
        BuiltinFunCall::Ncr => {
            let n = eval_ast(stack, &args[0])?.unwrap_int();
            let mut r = eval_ast(stack, &args[1])?.unwrap_int();
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
            let num = eval_ast(stack, &args[0])?.unwrap_float();
            Val::Float(num.to_degrees())
        }
        BuiltinFunCall::ToRad => {
            let num = eval_ast(stack, &args[0])?.unwrap_float();
            Val::Float(num.to_radians())
        }
        BuiltinFunCall::Sin => {
            let num = eval_ast(stack, &args[0])?.unwrap_float();
            Val::Float(num.sin())
        }
        BuiltinFunCall::Cos => {
            let num = eval_ast(stack, &args[0])?.unwrap_float();
            Val::Float(num.cos())
        }
        BuiltinFunCall::Tan => {
            let num = eval_ast(stack, &args[0])?.unwrap_float();
            Val::Float(num.tan())
        }
        BuiltinFunCall::Asin => {
            let num = eval_ast(stack, &args[0])?.unwrap_float();
            Val::Float(num.asin())
        }
        BuiltinFunCall::Acos => {
            let num = eval_ast(stack, &args[0])?.unwrap_float();
            Val::Float(num.acos())
        }
        BuiltinFunCall::Atan => {
            let num = eval_ast(stack, &args[0])?.unwrap_float();
            Val::Float(num.atan())
        }
        BuiltinFunCall::Gcd => {
            let mut a = eval_ast(stack, &args[0])?.unwrap_int();
            let mut b = eval_ast(stack, &args[1])?.unwrap_int();
            while b != 0 {
                let t = b;
                b = a % b;
                a = t;
            }
            Val::Int(a)
        }
        BuiltinFunCall::MinInt => Val::Int(fold_eval_int(stack, args, i128::min)?),
        BuiltinFunCall::MinFloat => Val::Float(fold_eval_float(stack, args, f64::min)?),
        BuiltinFunCall::MaxInt => Val::Int(fold_eval_int(stack, args, i128::max)?),
        BuiltinFunCall::MaxFloat => Val::Float(fold_eval_float(stack, args, f64::max)?),
        BuiltinFunCall::ClampInt => {
            let num = eval_ast(stack, &args[0])?.unwrap_int();
            let min = eval_ast(stack, &args[1])?.unwrap_int();
            let max = eval_ast(stack, &args[2])?.unwrap_int();
            if min > max {
                return Err(crate::Error::InvalidClampBounds(
                    ValSpan::new(Val::Int(min), args[1].span),
                    ValSpan::new(Val::Int(max), args[2].span),
                ));
            }
            Val::Int(num.clamp(min, max))
        }
        BuiltinFunCall::ClampFloat => {
            let num = eval_ast(stack, &args[0])?.unwrap_float();
            let min = eval_ast(stack, &args[1])?.unwrap_float();
            let max = eval_ast(stack, &args[2])?.unwrap_float();
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
        BuiltinFunCall::AbsInt => {
            let num = eval_ast(stack, &args[0])?.unwrap_int();
            Val::Int(num.abs())
        }
        BuiltinFunCall::AbsFloat => {
            let num = eval_ast(stack, &args[0])?.unwrap_float();
            Val::Float(num.abs())
        }
        BuiltinFunCall::Print => {
            eval_print(stack, args)?;
            Val::Unit
        }
        BuiltinFunCall::Println => {
            eval_print(stack, args)?;
            println!();
            Val::Unit
        }
        BuiltinFunCall::Assert => {
            let va = eval_ast(stack, &args[0])?.unwrap_bool();
            if !va {
                return Err(crate::Error::AssertFailed(args[0].span));
            }
            Val::Unit
        }
        BuiltinFunCall::AssertEq => {
            let a = eval_ast(stack, &args[0])?;
            let b = eval_ast(stack, &args[1])?;
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

fn eval_print(stack: &mut Stack, args: &[Ast]) -> crate::Result<()> {
    let mut vals = eval_iter(stack, args);
    if let Some(first) = vals.next() {
        print!("{}", first?); // TODO: Evaluator struct with stdio
    }

    for v in vals {
        print!(" {}", v?);
    }

    Ok(())
}

fn eval_spill(stack: &mut Stack, vars: &[(String, VarRef)]) -> crate::Result<Val> {
    for (n, v) in vars {
        println!("{n} = {}", stack.get(v));
    }
    Ok(Val::Unit)
}

fn fold_eval_int(
    stack: &mut Stack,
    args: &[Ast],
    fold: fn(i128, i128) -> i128,
) -> crate::Result<i128> {
    let mut current = eval_ast(stack, &args[0])?.unwrap_int();
    for a in &args[1..] {
        let val = eval_ast(stack, a)?.unwrap_int();
        current = fold(current, val);
    }
    Ok(current)
}

fn fold_eval_float(
    stack: &mut Stack,
    args: &[Ast],
    fold: fn(f64, f64) -> f64,
) -> crate::Result<f64> {
    let mut current = eval_ast(stack, &args[0])?.unwrap_float();
    for a in &args[1..] {
        let val = eval_ast(stack, a)?.unwrap_float();
        current = fold(current, val);
    }
    Ok(current)
}
