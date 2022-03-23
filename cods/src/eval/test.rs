use crate::{Ast, Context, Expr, ExprT, Range, Scope, Val, Var, VarId};

#[test]
fn resolve_var() {
    let mut ctx = Context {
        scope: Scope {
            vars: vec![Var {
                name: "x".into(),
                value: Some(Val::Int(4)),
            }],
        },
        ..Default::default()
    };
    let expr = Ast::expr(Expr::new(ExprT::Var(VarId(0)), Range::pos(0)));

    let val = ctx.eval(&expr).unwrap().unwrap();
    assert_eq!(Val::Int(4), val);
}

#[test]
fn undefined_var() {
    let mut ctx = Context {
        scope: Scope {
            vars: vec![Var {
                name: "x".into(),
                value: None,
            }],
        },
        ..Default::default()
    };
    let expr = Ast::expr(Expr::new(ExprT::Var(VarId(0)), Range::pos(0)));

    let val = ctx.eval(&expr).unwrap_err();
    assert_eq!(crate::Error::UndefinedVar("x".into(), Range::pos(0)), val);
}
