use crate::{Context, Data, Expr, Range, Val, ValT, Var, VarId};

#[test]
fn resolve_var() {
    let mut ctx = Context {
        vars: vec![Var {
            name: "x".into(),
            value: Some(Data::Int(4)),
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
