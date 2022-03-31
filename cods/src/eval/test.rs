use crate::{CRange, Context, Val};

#[test]
fn undefined_var() {
    let input = "print(x)";
    let mut ctx = Context::default();
    let error = ctx.parse_and_eval(input).unwrap_err();
    assert_eq!(
        error,
        crate::Error::UndefinedVar("x".into(), CRange::pos(6))
    );
}

#[test]
fn undefined_outside_scope() {
    let input = "{ val x = 7 }; println(x);";
    let mut ctx = Context::default();
    let error = ctx.parse_and_eval(input).unwrap_err();
    assert_eq!(
        error,
        crate::Error::UndefinedVar("x".into(), CRange::pos(23))
    );
}

#[test]
fn defined_inside_scope() {
    let input = "{ val x = 7; println(x); x }";
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap().unwrap();
    assert_eq!(val, Val::Int(7));
}

#[test]
fn can_assign_to_var_in_outer_scope() {
    let input = "var x = 2; { x = 7 }; x";
    let mut ctx = Context::default();
    let val = ctx.parse_and_eval(input).unwrap().unwrap();
    assert_eq!(val, Val::Int(7));
}

#[test]
fn cannot_assign_twice_to_immutable_var() {
    let input = "val x = 2; x = 4";
    let mut ctx = Context::default();
    let error = ctx.parse_and_eval(input).unwrap_err();
    assert_eq!(
        error,
        crate::Error::ImmutableAssign("x".into(), CRange::pos(11), CRange::pos(15)),
    );
}

#[test]
fn cannot_redefine_function() {
    let input = "fun a(i) { }; fun a() { }";
    let mut ctx = Context::default();
    let error = ctx.parse_and_eval(input).unwrap_err();
    assert_eq!(
        error,
        crate::Error::RedefinedFun("a".into(), CRange::pos(4), CRange::pos(18)),
    );
}
