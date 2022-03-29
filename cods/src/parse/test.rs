use crate::{Ast, AstT, Context, Expr, ExprT, Ident, Range, Val};

#[test]
fn semi() {
    let mut ctx = Context::default();
    let asts = ctx.parse_str("y = 34; y").unwrap();
    let expected = vec![
        Ast::new(
            AstT::Assign(
                Ident(0),
                Box::new(Ast::new(
                    AstT::Expr(Expr::new(ExprT::Val(Val::Int(34)), Range::of(4, 6))),
                    Range::of(4, 6),
                )),
            ),
            Range::of(0, 6),
        ),
        Ast::new(
            AstT::Expr(Expr::new(ExprT::Ident(Ident(0)), Range::of(8, 9))),
            Range::of(8, 9),
        ),
    ];
    assert_eq!(expected, asts);
}

#[test]
fn newline_suffix_op() {
    let mut ctx = Context::default();
    let error = ctx.parse_str("y = 3\n! true").unwrap_err();

    assert_eq!(error, crate::Error::MissingOperator(Range::pos(7)));
}
