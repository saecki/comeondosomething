use crate::{Ast, AstT, CRange, Context, Expr, ExprT, Ident, IdentRange, Val};

#[test]
fn semicolon() {
    let mut ctx = Context::default();
    let asts = ctx.parse_str("y = 34; y").unwrap();
    let expected = vec![
        Ast::new(
            AstT::Assign(
                IdentRange::new(Ident(0), CRange::pos(0)),
                Box::new(Ast::new(
                    AstT::Expr(Expr::new(ExprT::Val(Val::Int(34)), CRange::of(4, 6))),
                    CRange::of(4, 6),
                )),
            ),
            CRange::of(0, 6),
        ),
        Ast::new(
            AstT::Expr(Expr::new(ExprT::Ident(Ident(0)), CRange::of(8, 9))),
            CRange::of(8, 9),
        ),
    ];
    assert_eq!(expected, asts);
}

#[test]
fn postfix_op_on_newline() {
    let mut ctx = Context::default();
    let error = ctx.parse_str("y = 3\n! true").unwrap_err();

    assert_eq!(error, crate::Error::MissingOperator(CRange::pos(7)));
}
