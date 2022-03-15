use crate::{Ast, AstT, Context, Expr, ExprT, Range, Val, VarId};

#[test]
fn semi() {
    let mut ctx = Context::default();
    let asts = ctx.parse_str("y = 34; y").unwrap();
    let expected = vec![
        Ast::new(
            AstT::Assignment(
                VarId(0),
                Box::new(Ast::new(
                    AstT::Expr(Expr::new(ExprT::Val(Val::Int(34)), Range::of(4, 6))),
                    Range::of(4, 6),
                )),
            ),
            Range::of(0, 6),
        ),
        Ast::new(
            AstT::Expr(Expr::new(ExprT::Var(VarId(0)), Range::of(8, 9))),
            Range::of(8, 9),
        ),
    ];
    assert_eq!(expected, asts);
}
