use crate::{Expr, Context, Val, Data, Range, ValT, Var, VarId};

#[test]
fn resolve_var() {
    let mut ctx = Context {
        vars: vec![Var {
            name: "x".into(),
            value: Some(ValT::int(4)),
        }],
        ..Default::default()
    };
    let expr = Expr::val(Val::new(ValT::Var(VarId(0)), Range::pos(0)));

    let val = ctx.eval(&expr).unwrap().unwrap();
    assert_eq!(Data::Int(4), val);
}

#[test]
fn undefined_var() {
    let mut ctx = Context {
        vars: vec![Var {
            name: "x".into(),
            value: None,
        }],
        ..Default::default()
    };
    let expr = Expr::val(Val::new(ValT::Var(VarId(0)), Range::pos(0)));

    let val = ctx.eval(&expr).unwrap_err();
    assert_eq!(crate::Error::UndefinedVar("x".into(), Range::pos(0)), val);
}

#[test]
fn circular_ref() {
    let mut ctx = Context {
        vars: vec![
            Var {
                name: "x".into(),
                value: Some(ValT::Var(VarId(1))),
            },
            Var {
                name: "y".into(),
                value: Some(ValT::Var(VarId(0))),
            },
        ],
        ..Default::default()
    };
    let expr = Expr::val(Val::new(ValT::Var(VarId(0)), Range::pos(0)));

    let val = ctx.eval(&expr).unwrap_err();
    assert_eq!(
        crate::Error::CircularRef(vec!["x".into(), "y".into(), "x".into()], Range::pos(0)),
        val
    );
}

#[test]
fn self_ref() {
    let mut ctx = Context {
        vars: vec![Var {
            name: "x".into(),
            value: Some(ValT::Var(VarId(0))),
        }],
        ..Default::default()
    };
    let expr = Expr::val(Val::new(ValT::Var(VarId(0)), Range::pos(0)));

    let val = ctx.eval(&expr).unwrap_err();
    assert_eq!(
        crate::Error::CircularRef(vec!["x".into(), "x".into()], Range::pos(0)),
        val
    );
}
