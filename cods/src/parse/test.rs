use crate::{Context, Cst, Ident, IdentSpan, Infix, InfixT, Span, Val, ValSpan};

#[test]
fn semicolon() {
    let mut ctx = Context::default();
    let tokens = ctx.lex("y = 34; y").unwrap();
    let items = ctx.group(tokens).unwrap();
    let csts = ctx.parse(items).unwrap();
    let expected = vec![
        Cst::Infix(
            Box::new(Cst::Ident(IdentSpan::new(Ident(0), Span::pos(0, 0)))),
            Infix::new(InfixT::Assign, Span::pos(0, 2)),
            Box::new(Cst::Val(ValSpan::new(Val::Int(34), Span::cols(0, 4, 6)))),
        ),
        Cst::Ident(IdentSpan::new(Ident(0), Span::cols(0, 8, 9))),
    ];
    assert_eq!(expected, csts);
}

#[test]
fn postfix_op_on_newline() {
    let mut ctx = Context::default();
    let error = ctx.parse_and_check("y = 3\n! true").unwrap_err();

    assert_eq!(error, crate::Error::MissingOperator(Span::pos(1, 1)));
}
