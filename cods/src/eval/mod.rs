use std::rc::Rc;

use crate::ast::{BuiltinFunCall, ForLoop, Fun, IfExpr, Op, WhileLoop};
use crate::{Ast, AstT, Asts, DataType, Range, Span, Val, ValSpan};

pub use stack::*;
pub use val::*;

mod stack;
mod val;

type EvalResult<T> = std::result::Result<T, EvalError>;

enum EvalError {
    Return(Val),
    Error(crate::Error),
}

pub fn eval(asts: &Asts) -> crate::Result<Val> {
    let mut stack = Stack::default();
    eval_with(&mut stack, asts)
}

pub fn eval_with(stack: &mut Stack, asts: &Asts) -> crate::Result<Val> {
    stack.resize(asts.global_frame_size);
    match eval_asts(stack, &asts.asts) {
        Ok(v) => Ok(v),
        Err(EvalError::Error(e)) => return Err(e),
        Err(_) => unreachable!(),
    }
}

fn eval_asts(stack: &mut Stack, asts: &[Ast]) -> EvalResult<Val> {
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

fn eval_ast(stack: &mut Stack, ast: &Ast) -> EvalResult<Val> {
    match &ast.typ {
        AstT::Error => err(crate::Error::Parsing(ast.span)),
        AstT::Var(v) => Ok(stack.get(v)),
        AstT::Val(v) => Ok(v.clone()),
        AstT::Op(o, a) => eval_op(stack, o, a),
        AstT::Is(a, t) => eval_is(stack, a, *t),
        AstT::Cast(a, t) => eval_cast(stack, a, *t),
        AstT::Unit => Ok(Val::Unit),
        AstT::Block(b) => eval_asts(stack, b),
        AstT::IfExpr(i) => eval_if_expr(stack, i),
        AstT::WhileLoop(w) => eval_while_loop(stack, w),
        AstT::ForLoop(f) => eval_for_loop(stack, f),
        AstT::VarAssign(v, e) => eval_var_assign(stack, v, e),
        AstT::FunCall(f, a) => eval_fun_call(stack, f, a),
        AstT::Return(v) => eval_return(stack, v),
        AstT::BuiltinFunCall(f, a) => eval_builtin_fun_call(stack, *f, a),
        AstT::Spill(v) => eval_spill(stack, v),
    }
}

fn eval_op(stack: &mut Stack, op: &Op, args: &[Ast]) -> EvalResult<Val> {
    let val = match op {
        Op::Not => {
            let va = eval_ast(stack, &args[0])?.unwrap_bool();
            Val::Bool(!va)
        }
        Op::NegInt => {
            let va = eval_ast(stack, &args[0])?.unwrap_int();
            Val::Int(-va)
        }
        Op::NegFloat => {
            let va = eval_ast(stack, &args[0])?.unwrap_float();
            Val::Float(-va)
        }
        Op::RangeEx => {
            let va = eval_ast(stack, &args[0])?.unwrap_int();
            let vb = eval_ast(stack, &args[1])?.unwrap_int();
            Val::Range(Range::Exclusive(va, vb))
        }
        Op::RangeIn => {
            let va = eval_ast(stack, &args[0])?.unwrap_int();
            let vb = eval_ast(stack, &args[1])?.unwrap_int();
            Val::Range(Range::Inclusive(va, vb))
        }
        Op::AddInt => {
            let va = eval_ast(stack, &args[0])?.unwrap_int();
            let vb = eval_ast(stack, &args[1])?.unwrap_int();
            match va.checked_add(vb) {
                Some(i) => Val::Int(i),
                None => return err(crate::Error::AddOverflow(args[0].span, args[1].span)),
            }
        }
        Op::AddFloat => {
            let va = eval_ast(stack, &args[0])?.unwrap_float();
            let vb = eval_ast(stack, &args[1])?.unwrap_float();
            Val::Float(va + vb)
        }
        Op::SubInt => {
            let va = eval_ast(stack, &args[0])?.unwrap_int();
            let vb = eval_ast(stack, &args[1])?.unwrap_int();
            match va.checked_sub(vb) {
                Some(i) => Val::Int(i),
                None => return err(crate::Error::SubOverflow(args[0].span, args[1].span)),
            }
        }
        Op::SubFloat => {
            let va = eval_ast(stack, &args[0])?.unwrap_float();
            let vb = eval_ast(stack, &args[1])?.unwrap_float();
            Val::Float(va - vb)
        }
        Op::MulInt => {
            let va = eval_ast(stack, &args[0])?.unwrap_int();
            let vb = eval_ast(stack, &args[1])?.unwrap_int();
            match va.checked_mul(vb) {
                Some(i) => Val::Int(i),
                None => return err(crate::Error::MulOverflow(args[0].span, args[1].span)),
            }
        }
        Op::MulFloat => {
            let va = eval_ast(stack, &args[0])?.unwrap_float();
            let vb = eval_ast(stack, &args[1])?.unwrap_float();
            Val::Float(va * vb)
        }
        Op::DivInt => {
            let va = eval_ast(stack, &args[0])?.unwrap_int();
            let vb = eval_ast(stack, &args[1])?.unwrap_int();
            match va.checked_div(vb) {
                Some(i) => Val::Int(i),
                None => return err(crate::Error::DivideByZero(args[0].span, args[1].span)),
            }
        }
        Op::DivFloat => {
            let va = eval_ast(stack, &args[0])?.unwrap_float();
            let vb = eval_ast(stack, &args[1])?.unwrap_float();
            Val::Float(va / vb)
        }
        Op::RemInt => {
            let va = eval_ast(stack, &args[0])?.unwrap_int();
            let vb = eval_ast(stack, &args[1])?.unwrap_int();
            va.checked_rem(vb)
                .map(Val::Int)
                .ok_or(crate::Error::RemainderByZero(args[0].span, args[1].span))
                .map_err(EvalError::Error)?
        }
        Op::RemEuclidInt => {
            let va = eval_ast(stack, &args[0])?.unwrap_int();
            let vb = eval_ast(stack, &args[1])?.unwrap_int();
            if vb == 0 {
                return err(crate::Error::RemainderByZero(args[0].span, args[1].span));
            }
            let r = va % vb;
            if (r > 0 && vb < 0) || (r < 0 && vb > 0) {
                Val::Int(r + vb)
            } else {
                Val::Int(r)
            }
        }
        Op::FactorialInt => {
            let va = eval_ast(stack, &args[0])?.unwrap_int();

            if va < 0 {
                return err(crate::Error::NegativeFactorial(ValSpan::new(
                    Val::Int(va),
                    args[0].span,
                )));
            }

            let mut f: i128 = 1;
            for i in 2..=va {
                f = match f.checked_mul(i) {
                    Some(v) => v,
                    None => {
                        return err(crate::Error::FactorialOverflow(ValSpan::new(
                            Val::Int(va),
                            args[0].span,
                        )))
                    }
                };
            }
            Val::Int(f)
        }
        Op::Eq => {
            let va = eval_ast(stack, &args[0])?;
            let vb = eval_ast(stack, &args[1])?;
            Val::Bool(va == vb)
        }
        Op::Ne => {
            let va = eval_ast(stack, &args[0])?;
            let vb = eval_ast(stack, &args[1])?;
            Val::Bool(va != vb)
        }
        Op::LtInt => {
            let va = eval_ast(stack, &args[0])?.unwrap_int();
            let vb = eval_ast(stack, &args[1])?.unwrap_int();
            Val::Bool(va < vb)
        }
        Op::LtFloat => {
            let va = eval_ast(stack, &args[0])?.unwrap_float();
            let vb = eval_ast(stack, &args[1])?.unwrap_float();
            Val::Bool(va < vb)
        }
        Op::LeInt => {
            let va = eval_ast(stack, &args[0])?.unwrap_int();
            let vb = eval_ast(stack, &args[1])?.unwrap_int();
            Val::Bool(va <= vb)
        }
        Op::LeFloat => {
            let va = eval_ast(stack, &args[0])?.unwrap_float();
            let vb = eval_ast(stack, &args[1])?.unwrap_float();
            Val::Bool(va <= vb)
        }
        Op::GtInt => {
            let va = eval_ast(stack, &args[0])?.unwrap_int();
            let vb = eval_ast(stack, &args[1])?.unwrap_int();
            Val::Bool(va > vb)
        }
        Op::GtFloat => {
            let va = eval_ast(stack, &args[0])?.unwrap_float();
            let vb = eval_ast(stack, &args[1])?.unwrap_float();
            Val::Bool(va > vb)
        }
        Op::GeInt => {
            let va = eval_ast(stack, &args[0])?.unwrap_int();
            let vb = eval_ast(stack, &args[1])?.unwrap_int();
            Val::Bool(va >= vb)
        }
        Op::GeFloat => {
            let va = eval_ast(stack, &args[0])?.unwrap_float();
            let vb = eval_ast(stack, &args[1])?.unwrap_float();
            Val::Bool(va > vb)
        }
        Op::BwOrInt => {
            let va = eval_ast(stack, &args[0])?.unwrap_int();
            let vb = eval_ast(stack, &args[1])?.unwrap_int();
            Val::Int(va | vb)
        }
        Op::BwOrBool => {
            let va = eval_ast(stack, &args[0])?.unwrap_bool();
            let vb = eval_ast(stack, &args[1])?.unwrap_bool();
            Val::Bool(va | vb)
        }
        Op::BwAndInt => {
            let va = eval_ast(stack, &args[0])?.unwrap_int();
            let vb = eval_ast(stack, &args[1])?.unwrap_int();
            Val::Int(va & vb)
        }
        Op::BwAndBool => {
            let va = eval_ast(stack, &args[0])?.unwrap_bool();
            let vb = eval_ast(stack, &args[1])?.unwrap_bool();
            Val::Bool(va & vb)
        }
        Op::Or => {
            let va = eval_ast(stack, &args[0])?.unwrap_bool();
            let vb = eval_ast(stack, &args[1])?.unwrap_bool();
            Val::Bool(va || vb)
        }
        Op::And => {
            let va = eval_ast(stack, &args[0])?.unwrap_bool();
            let vb = eval_ast(stack, &args[1])?.unwrap_bool();
            Val::Bool(va && vb)
        }
    };

    Ok(val)
}

fn eval_is(stack: &mut Stack, a: &Ast, t: DataType) -> EvalResult<Val> {
    let va = eval_ast(stack, a)?;
    Ok(Val::Bool(va.data_type().is(t)))
}

fn eval_cast(stack: &mut Stack, a: &Ast, t: DataType) -> EvalResult<Val> {
    fn cast_err(val: Val, data_type: DataType, span: Span) -> EvalResult<Val> {
        err(crate::Error::CastFailed((val.data_type(), span), data_type))
    }

    let va = eval_ast(stack, a)?;
    let val = match t {
        DataType::Int => Val::Int(match va {
            Val::Int(i) => i,
            Val::Float(f) => f as i128,
            v => return cast_err(v, t, a.span),
        }),
        DataType::Float => Val::Float(match va {
            Val::Float(f) => f,
            Val::Int(i) => i as f64,
            v => return cast_err(v, t, a.span),
        }),
        DataType::Bool => Val::Bool(match va {
            Val::Bool(b) => b,
            Val::Int(i) => i != 0,
            v => return cast_err(v, t, a.span),
        }),
        DataType::Char => Val::Char(match va {
            Val::Char(c) => c,
            v => return cast_err(v, t, a.span),
        }),
        DataType::Str => Val::Str(match va {
            Val::Str(s) => s,
            v => return cast_err(v, t, a.span),
        }),
        DataType::Range => Val::Range(match va {
            Val::Range(r) => r,
            v => return cast_err(v, t, a.span),
        }),
        DataType::Unit => match va {
            Val::Unit => Val::Unit,
            v => return cast_err(v, t, a.span),
        },
        DataType::Any => va,
        DataType::Never => unreachable!("Never has no instances"),
    };
    Ok(val)
}

fn eval_if_expr(stack: &mut Stack, if_expr: &IfExpr) -> EvalResult<Val> {
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

fn eval_while_loop(stack: &mut Stack, whl_loop: &WhileLoop) -> EvalResult<Val> {
    while eval_ast(stack, &whl_loop.cond)?.unwrap_bool() {
        eval_asts(stack, &whl_loop.block)?;
    }

    Ok(Val::Unit)
}

fn eval_for_loop(stack: &mut Stack, for_loop: &ForLoop) -> EvalResult<Val> {
    let iter = eval_ast(stack, &for_loop.iter)?.unwrap_range();

    for i in iter.iter() {
        stack.set(&for_loop.var, Val::Int(i));
        eval_asts(stack, &for_loop.block)?;
    }

    Ok(Val::Unit)
}

fn eval_var_assign(stack: &mut Stack, var: &VarRef, expr: &Ast) -> EvalResult<Val> {
    let val = eval_ast(stack, expr)?;
    stack.set(var, val);
    Ok(Val::Unit)
}

fn eval_fun_call(stack: &mut Stack, fun: &Rc<Fun>, args: &[Ast]) -> EvalResult<Val> {
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
        Err(EvalError::Return(v)) => Ok(v),
        r => r,
    };
    stack.pop();
    val
}

fn eval_return(stack: &mut Stack, val: &Ast) -> EvalResult<Val> {
    let val = eval_ast(stack, val)?;
    Err(EvalError::Return(val))
}

fn eval_builtin_fun_call(stack: &mut Stack, fun: BuiltinFunCall, args: &[Ast]) -> EvalResult<Val> {
    let val = match fun {
        BuiltinFunCall::PowInt => {
            let a = &args[0];
            let b = &args[1];
            let base = eval_ast(stack, a)?.unwrap_int();
            let exp = eval_ast(stack, b)?.unwrap_int();
            if exp < 0 {
                return err(crate::Error::NegativeIntPow(a.span, b.span));
            }
            if exp > u32::MAX as i128 {
                return err(crate::Error::PowOverflow(a.span, b.span));
            }
            match base.checked_pow(exp as u32) {
                Some(i) => Val::Int(i),
                None => return err(crate::Error::PowOverflow(a.span, b.span)),
            }
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
                return err(crate::Error::NegativeNcr(ValSpan::new(
                    Val::Int(r),
                    args[1].span,
                )));
            }
            if n < r {
                return err(crate::Error::InvalidNcr(
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
                return err(crate::Error::InvalidClampBounds(
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
                return err(crate::Error::InvalidClampBounds(
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
                return err(crate::Error::AssertFailed(args[0].span));
            }
            Val::Unit
        }
        BuiltinFunCall::AssertEq => {
            let a = eval_ast(stack, &args[0])?;
            let b = eval_ast(stack, &args[1])?;
            if a != b {
                return err(crate::Error::AssertEqFailed(
                    ValSpan::new(a, args[0].span),
                    ValSpan::new(b, args[1].span),
                ));
            }
            Val::Unit
        }
    };
    Ok(val)
}

fn eval_print(stack: &mut Stack, args: &[Ast]) -> EvalResult<()> {
    if let Some((first, others)) = args.split_first() {
        let f = eval_ast(stack, first)?;
        print!("{}", f); // TODO: Evaluator struct with stdio

        for a in others {
            let v = eval_ast(stack, a)?;
            print!(" {}", v);
        }
    }

    Ok(())
}

fn eval_spill(stack: &mut Stack, vars: &[(String, VarRef)]) -> EvalResult<Val> {
    for (n, v) in vars {
        println!("{n} = {}", stack.get(v));
    }
    Ok(Val::Unit)
}

fn fold_eval_int(
    stack: &mut Stack,
    args: &[Ast],
    fold: fn(i128, i128) -> i128,
) -> EvalResult<i128> {
    let mut current = eval_ast(stack, &args[0])?.unwrap_int();
    for a in &args[1..] {
        let val = eval_ast(stack, a)?.unwrap_int();
        current = fold(current, val);
    }
    Ok(current)
}

fn fold_eval_float(stack: &mut Stack, args: &[Ast], fold: fn(f64, f64) -> f64) -> EvalResult<f64> {
    let mut current = eval_ast(stack, &args[0])?.unwrap_float();
    for a in &args[1..] {
        let val = eval_ast(stack, a)?.unwrap_float();
        current = fold(current, val);
    }
    Ok(current)
}

#[inline(always)]
fn err<T>(error: crate::Error) -> EvalResult<T> {
    Err(EvalError::Error(error))
}
