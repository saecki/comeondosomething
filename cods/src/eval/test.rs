use std::collections::HashMap;

use crate::{
    Ast, CRange, Context, Expr, ExprT, Ident, IdentRange, Idents, Scope, Scopes, Val, Var,
};

#[test]
fn resolve_var() {
    let mut ctx = Context {
        idents: Idents(vec!["x".into()]),
        scopes: Scopes(vec![Scope {
            vars: HashMap::from_iter([(
                Ident(0),
                Var::new(
                    IdentRange::new(Ident(0), CRange::pos(0)),
                    Some(Val::Int(4)),
                    false,
                ),
            )]),
            ..Default::default()
        }]),
        ..Default::default()
    };
    let expr = Ast::expr(Expr::new(ExprT::Ident(Ident(0)), CRange::pos(0)));

    let val = ctx.eval(&expr).unwrap().unwrap();
    assert_eq!(Val::Int(4), val);
}

#[test]
fn undefined_var() {
    let mut ctx = Context {
        idents: Idents(vec!["x".into()]),
        scopes: Scopes(vec![Scope::default()]),
        ..Default::default()
    };
    let expr = Ast::expr(Expr::new(ExprT::Ident(Ident(0)), CRange::pos(0)));

    let val = ctx.eval(&expr).unwrap_err();
    assert_eq!(crate::Error::UndefinedVar("x".into(), CRange::pos(0)), val);
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
