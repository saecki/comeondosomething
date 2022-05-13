use std::f64::consts;

use crate::{BuiltinConst, Context, DataType, Span, Val};

#[test]
fn undefined_var() {
    let input = "print(x)";
    let mut ctx = Context::default();
    let error = ctx.parse_and_eval(input).unwrap_err();
    assert_eq!(error, crate::Error::UndefinedVar("x".into(), Span::pos(6)));
}

#[test]
fn undefined_outside_scope() {
    let input = "{ val x = 7 }; println(x);";
    let mut ctx = Context::default();
    let error = ctx.parse_and_eval(input).unwrap_err();
    assert_eq!(error, crate::Error::UndefinedVar("x".into(), Span::pos(23)));
}

#[test]
fn defined_inside_scope() {
    let input = "{ val x = 7; println(x); x }";
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap();
    assert_eq!(val, Val::Int(7));
}

#[test]
fn can_assign_to_var_in_outer_scope() {
    let input = "var x = 2; { x = 7 }; x";
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap();
    assert_eq!(val, Val::Int(7));
}

#[test]
fn cannot_assign_twice_to_immutable_var() {
    let input = "val x = 2; x = 4";
    let mut ctx = Context::default();
    let error = ctx.parse_and_eval(input).unwrap_err();
    assert_eq!(
        error,
        crate::Error::ImmutableAssign("x".into(), Span::pos(11), Span::pos(15)),
    );
}

#[test]
fn cannot_assign_twice_to_builtin_const() {
    let input = "TAU += 4";
    let mut ctx = Context::default();
    let error = ctx.parse_and_eval(input).unwrap_err();
    assert_eq!(
        error,
        crate::Error::ConstAssign((BuiltinConst::Tau, Span::of(0, 3)), Span::of(4, 6)),
    );
}

#[test]
fn can_resolve_builtin_const() {
    let input = "val x = PI; x";
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap();
    assert_eq!(val, Val::Float(consts::PI));
}

#[test]
fn cannot_redefine_function() {
    let input = "fun a(i: int) { }; fun a() { }";
    let mut ctx = Context::default();
    let error = ctx.parse_and_eval(input).unwrap_err();
    assert_eq!(
        error,
        crate::Error::RedefinedFun("a".into(), Span::pos(4), Span::pos(23)),
    );
}

#[test]
fn coerce_types() {
    let input = "val a: any = 12; a";
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap();
    assert_eq!(val, Val::Int(12),);
}

#[test]
fn cast_to_any() {
    let input = "val a = 12; a as any";
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap();
    assert_eq!(val, Val::Int(12),);
}

#[test]
fn cast_from_any_fails_at_runtime() {
    let input = "val a: any = 12; a as str";
    let mut ctx = Context::default();
    let error = ctx.parse_and_eval(input).unwrap_err();
    assert_eq!(
        error,
        crate::Error::CastFailed((DataType::Int, Span::pos(17)), DataType::Str)
    );
}

#[test]
fn cast_int_to_str_fails_while_type_checking() {
    let input = "val a: int = 12; a as str";
    let mut ctx = Context::default();
    let error = ctx.parse_and_eval(input).unwrap_err();
    assert_eq!(
        error,
        crate::Error::CastAlwaysFails(
            (DataType::Int, Span::pos(17)),
            (DataType::Str, Span::of(22, 25))
        )
    );
}

#[test]
fn cast_int_coerced_as_any_to_str_fails_at_runtime() {
    let input = "val a: any = 12; a as str";
    let mut ctx = Context::default();
    let error = ctx.parse_and_eval(input).unwrap_err();
    assert_eq!(
        error,
        crate::Error::CastFailed((DataType::Int, Span::pos(17)), DataType::Str)
    );
}

#[test]
fn if_expr_branch_types_are_equal() {
    let input = "
        if 3 >= 4 {
            9
        } else if false {
            12
        } else {
            43
        }
    ";
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap();
    assert_eq!(val, Val::Int(43));
}

#[test]
fn if_expr_else_branch_not_needed_when_unit() {
    let input = "
        var a = 3
        val b = if 3 >= 4 {
            spill()
        } else if false {
            a = 7
        }
        b
    ";
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap();
    assert_eq!(val, Val::Unit);
}

#[test]
fn if_expr_branch_types_are_enforced() {
    let input = r#"
        if 3 >= 4 {
            println("hi")
            9
        } else if false {
            12.0
        } else {
            false
        }
    "#;
    let mut ctx = Context::default();
    let error = ctx.parse_and_eval(input).unwrap_err();
    assert_eq!(
        error,
        crate::Error::IfBranchIncompatibleType(
            (DataType::Int, Span::pos(59)),
            (DataType::Float, Span::of(99, 103))
        ),
    );
}

#[test]
fn if_statement_branch_types_can_differ() {
    let input = r#"
        if 3 >= 4 {
            println("hi")
            9
        } else if false {
            12.0
        } else {
            false
        }
        3 * 7
    "#;
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap();
    assert_eq!(val, Val::Int(21));
}

#[test]
fn statement_cannot_be_used_as_expr() {
    let input = "val a = (val b = 12)";
    let mut ctx = Context::default();
    let error = ctx.parse_and_eval(input).unwrap_err();
    assert_eq!(error, crate::Error::ExpectedExpr(Span::of(9, 19)));
}

#[test]
fn recursive_function_calls() {
    let input = "
        fun fib(i: int) -> int {
            if i == 0 {
                0
            } else if i == 1 {
                1
            } else {
                fib(i - 1) + fib(i - 2)
            }
        }
        fib(8)
    ";
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap();
    assert_eq!(val, Val::Int(21));
}

#[test]
fn recursive_function_calls2() {
    let input = "
        val a = first(23)
        fun first(a: int) -> int {
            if a < 0 {
                0
            } else {
                second(a)
            }
        }
        fun second(b: int) -> int {
            first(b - 1)
        }
        a
    ";
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap();
    assert_eq!(val, Val::Int(0));
}

#[test]
fn function_call_before_definition() {
    let input = "
        val a = test(23)
        fun test(i: int) -> int {
            i * 3
        }
        a
    ";
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap();
    assert_eq!(val, Val::Int(69));
}

#[test]
fn can_capture_global_var() {
    let input = "
        val a = 3
        fun captures() -> int {
            a
        }
        captures()
    ";
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap();
    assert_eq!(val, Val::Int(3));
}

#[test]
fn can_write_to_captured_global_var() {
    let input = "
        var a = 3
        fun captures() {
            a *= 2
        }
        captures()
        a
    ";
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap();
    assert_eq!(val, Val::Int(6));
}

#[test]
fn cannot_capture_var_in_dyn_scope() {
    let input = "
        fun outer() {
            val a = 3
            fun inner() -> int {
                a
            }
        }
    ";
    let mut ctx = Context::default();
    let err = ctx.parse_and_eval(input).unwrap_err();
    assert_eq!(
        err,
        crate::Error::NotImplemented(
            "Capturing variables from a dynamic scope is not yet implemented",
            Span::pos(39)
        )
    );
}

#[test]
fn cannot_capture_var_to_write_in_dyn_scope() {
    let input = "
        fun outer() {
            var a = 3
            fun inner() {
                a *= 3
            }
            a
        }
    ";
    let mut ctx = Context::default();
    let err = ctx.parse_and_eval(input).unwrap_err();
    assert_eq!(
        err,
        crate::Error::NotImplemented(
            "Capturing variables from a dynamic scope is not yet implemented",
            Span::pos(39)
        )
    );
}
