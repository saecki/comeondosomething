use crate::{Ast, AstT, Context, Ident, IdentSpan, Span, Val, ValSpan};

#[test]
fn semicolon() {
    let mut ctx = Context::default();
    let asts = ctx.parse_str("y = 34; y").unwrap();
    let expected = vec![
        Ast::new(
            AstT::Assign(
                IdentSpan::new(Ident(0), Span::pos(0)),
                Box::new(Ast::new(
                    AstT::Val(ValSpan::new(Val::Int(34), Span::of(4, 6))),
                    Span::of(4, 6),
                )),
            ),
            Span::of(0, 6),
        ),
        Ast::new(
            AstT::Ident(IdentSpan::new(Ident(0), Span::of(8, 9))),
            Span::of(8, 9),
        ),
    ];
    assert_eq!(expected, asts);
}

#[test]
fn postfix_op_on_newline() {
    let mut ctx = Context::default();
    let error = ctx.parse_str("y = 3\n! true").unwrap_err();

    assert_eq!(error, crate::Error::MissingOperator(Span::pos(7)));
}

#[test]
fn cannot_redefine_builtin_const() {
    let input = "val PI = 3.1415";
    let mut ctx = Context::default();
    let error = ctx.parse_str(input).unwrap_err();
    assert_eq!(
        error,
        crate::Error::RedefinedBuiltinConst("PI".into(), Span::of(4, 6)),
    );
}

#[test]
fn cannot_redefine_builtin_function() {
    let input = "fun pow(i) { }";
    let mut ctx = Context::default();
    let error = ctx.parse_str(input).unwrap_err();
    assert_eq!(
        error,
        crate::Error::RedefinedBuiltinFun("pow".into(), Span::of(4, 7)),
    );
}
