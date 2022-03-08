use crate::{Calc, Context, Num, PlainVal, Range, Val, Var, VarId};

#[test]
fn resolve_var() {
    let mut ctx = Context {
        vars: vec![Var {
            name: "x".into(),
            value: Some(Val::int(4)),
        }],
        ..Default::default()
    };
    let calc = Calc::num(Num::new(Val::Var(VarId(0)), Range::pos(0)));

    let val = ctx.eval(&calc).unwrap().unwrap();
    assert_eq!(PlainVal::Int(4), val);
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
    let calc = Calc::num(Num::new(Val::Var(VarId(0)), Range::pos(0)));

    let val = ctx.eval(&calc).unwrap_err();
    assert_eq!(crate::Error::UndefinedVar("x".into(), Range::pos(0)), val);
}

#[test]
fn circular_ref() {
    let mut ctx = Context {
        vars: vec![
            Var {
                name: "x".into(),
                value: Some(Val::Var(VarId(1))),
            },
            Var {
                name: "y".into(),
                value: Some(Val::Var(VarId(0))),
            },
        ],
        ..Default::default()
    };
    let calc = Calc::num(Num::new(Val::Var(VarId(0)), Range::pos(0)));

    let val = ctx.eval(&calc).unwrap_err();
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
            value: Some(Val::Var(VarId(0))),
        }],
        ..Default::default()
    };
    let calc = Calc::num(Num::new(Val::Var(VarId(0)), Range::pos(0)));

    let val = ctx.eval(&calc).unwrap_err();
    assert_eq!(
        crate::Error::CircularRef(vec!["x".into(), "x".into()], Range::pos(0)),
        val
    );
}
