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
