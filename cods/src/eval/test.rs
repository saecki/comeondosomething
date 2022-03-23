use std::collections::HashMap;

use crate::{Ast, Context, Expr, ExprT, Ident, Range, Scope, Val, Var};

#[test]
fn resolve_var() {
    let mut ctx = Context {
        idents: vec!["x".into()],
        scope: Scope {
            vars: HashMap::from_iter([(Ident(0), Var::new(Some(Val::Int(4))))]),
        },
        ..Default::default()
    };
    let expr = Ast::expr(Expr::new(ExprT::Var(Ident(0)), Range::pos(0)));

    let val = ctx.eval(&expr).unwrap().unwrap();
    assert_eq!(Val::Int(4), val);
}

#[test]
fn undefined_var() {
    let mut ctx = Context {
        idents: vec!["x".into()],
        scope: Scope {
            vars: HashMap::from_iter([(Ident(0), Var::new(None))]),
        },
        ..Default::default()
    };
    let expr = Ast::expr(Expr::new(ExprT::Var(Ident(0)), Range::pos(0)));

    let val = ctx.eval(&expr).unwrap_err();
    assert_eq!(crate::Error::UndefinedVar("x".into(), Range::pos(0)), val);
}
