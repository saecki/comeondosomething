use std::f64::consts;

use crate::{BuiltinConst, Context, DataType, Pos, Span, Val};

#[test]
fn undefined_var() {
    let input = "print(x)";
    let mut ctx = Context::default();
    let error = ctx.parse_and_eval(input).unwrap_err();
    assert_eq!(
        error,
        crate::Error::UndefinedVar("x".into(), Span::pos(0, 6))
    );
}

#[test]
fn undefined_outside_scope() {
    let input = "{ val x = 7 }; println(x);";
    let mut ctx = Context::default();
    let error = ctx.parse_and_eval(input).unwrap_err();
    assert_eq!(
        error,
        crate::Error::UndefinedVar("x".into(), Span::pos(0, 23))
    );
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
        crate::Error::ImmutableAssign("x".into(), Span::pos(0, 11), Span::pos(0, 15)),
    );
}

#[test]
fn cannot_assign_twice_to_builtin_const() {
    let input = "TAU += 4";
    let mut ctx = Context::default();
    let error = ctx.parse_and_eval(input).unwrap_err();
    assert_eq!(
        error,
        crate::Error::ConstAssign(
            (BuiltinConst::Tau, Span::cols(0, 0, 3)),
            Span::cols(0, 4, 6)
        ),
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
fn cannot_redefine_builtin_const() {
    let input = "val PI = 3.14";
    let mut ctx = Context::default();
    let error = ctx.parse_and_eval(input).unwrap_err();
    assert_eq!(
        error,
        crate::Error::RedefinedBuiltinConst("PI".into(), Span::cols(0, 4, 6)),
    );
}

#[test]
fn cannot_redefine_builtin_function() {
    let input = "fun sqrt() { }";
    let mut ctx = Context::default();
    let error = ctx.parse_and_eval(input).unwrap_err();
    assert_eq!(
        error,
        crate::Error::RedefinedBuiltinFun("sqrt".into(), Span::cols(0, 4, 8)),
    );
}

#[test]
fn cannot_redefine_function() {
    let input = "fun a(i: int) { }; fun a() { }";
    let mut ctx = Context::default();
    let error = ctx.parse_and_eval(input).unwrap_err();
    assert_eq!(
        error,
        crate::Error::RedefinedFun("a".into(), Span::pos(0, 4), Span::pos(0, 23)),
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
        crate::Error::CastFailed((DataType::Int, Span::pos(0, 17)), DataType::Str)
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
            (DataType::Int, Span::pos(0, 17)),
            (DataType::Str, Span::cols(0, 22, 25))
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
        crate::Error::CastFailed((DataType::Int, Span::pos(0, 17)), DataType::Str)
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
            (DataType::Int, Span::pos(3, 12)),
            (DataType::Float, Span::cols(5, 12, 16))
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
    assert_eq!(error, crate::Error::ExpectedExpr(Span::cols(0, 9, 19)));
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
            vec![Span::pos(2, 16), Span::pos(4, 16)],
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
            vec![Span::pos(2, 16), Span::pos(4, 16)],
        )
    );
}

#[test]
fn early_return() {
    let input = "
        fun test() -> int {
            if true {
                return 12
            }

            0
        }
        test()
    ";
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap();
    assert_eq!(val, Val::Int(12));
}

#[test]
fn simple_return() {
    let input = "
        fun test() -> int {
            return 32
        }
        test()
    ";
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap();
    assert_eq!(val, Val::Int(32));
}

#[test]
fn cannot_return_from_global_context() {
    let input = "return 3";
    let mut ctx = Context::default();
    let err = ctx.parse_and_eval(input).unwrap_err();
    assert_eq!(err, crate::Error::GlobalContextReturn(Span::cols(0, 0, 6)),);
}

#[test]
fn early_return_data_type_doesnt_match() {
    let input = "
        fun test() -> bool {
            if true {
                return 12
            }

            false
        }
        test()
    ";
    let mut ctx = Context::default();
    let err = ctx.parse_and_eval(input).unwrap_err();
    assert_eq!(
        err,
        crate::Error::MismatchedType {
            expected: DataType::Bool,
            found: DataType::Int,
            spans: vec![Span::cols(3, 23, 25), Span::cols(1, 22, 26)]
        }
    );
}

#[test]
fn if_expr_early_return_data_type_1() {
    let input = "
        fun test() -> int {
            val a = if false {
                return 12
            } else {
                1
            }

            a
        }
        test()
    ";
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap();
    assert_eq!(val, Val::Int(1));
}

#[test]
fn if_expr_early_return_data_type_2() {
    let input = "
        fun test() -> int {
            val a = if true {
                1
            } else {
                return 12
            }

            a
        }
        test()
    ";
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap();
    assert_eq!(val, Val::Int(1));
}

#[test]
fn unused_var() {
    let input = "val a = 0";
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap();
    assert_eq!(val, Val::Unit);
    assert_eq!(
        ctx.warnings,
        vec![crate::Warning::UnusedVar(
            "a".into(),
            Span::cols(0, 4, 5),
        )],
    );
}

#[test]
fn unused_fun() {
    let input = "fun a() {}";
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap();
    assert_eq!(val, Val::Unit);
    assert_eq!(
        ctx.warnings,
        vec![crate::Warning::UnusedFun(
            "a".into(),
            Span::cols(0, 4, 5),
        )],
    );
}

#[test]
fn code_after_return_is_unreachable() {
    let input = r#"
        fun test() {
            return

            println("unreachable")
        }
        test()
    "#;
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap();
    assert_eq!(val, Val::Unit);
    assert_eq!(
        ctx.warnings,
        vec![crate::Warning::Unreachable(Span::cols(4, 12, 34))],
    );
}

#[test]
fn code_after_if_expr_is_unreachable() {
    let input = "
        fun test(a: bool, b: bool) {
            if a {
                return
            } else if b {
                return
            } else {
                return
            }

            val u = 3
        }
        test(false, true)
    ";
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap();
    assert_eq!(val, Val::Unit);
    assert_eq!(
        ctx.warnings,
        vec![crate::Warning::Unreachable(Span::cols(10, 12, 21))],
    );
}

#[test]
fn if_cond_returns_block_unreachable() {
    let input = "
        fun test() -> int {
            if return 42 {
                32
            } else if false {
                4
            } else {
                9
            }
        }
        test()
    ";
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap();
    assert_eq!(val, Val::Int(42));
    assert_eq!(
        ctx.warnings,
        vec![crate::Warning::Unreachable(Span::new(
            Pos::new(2, 25),
            Pos::new(8, 13)
        ))],
    );
}

#[test]
fn unnecessary_cast() {
    let input = "3 as int";
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap();
    assert_eq!(val, Val::Int(3));
    assert_eq!(
        ctx.warnings,
        vec![crate::Warning::UnnecesaryCast(
            DataType::Int,
            Span::cols(0, 2, 8),
        )],
    );
}

#[test]
fn type_check_is_always_true() {
    let input = "'u' is char";
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap();
    assert_eq!(val, Val::Bool(true));
    assert_eq!(
        ctx.warnings,
        vec![crate::Warning::TypeCheckIsAlwaysTrue(
            DataType::Char,
            Span::cols(0, 4, 11),
        )],
    );
}
