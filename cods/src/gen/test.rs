use super::*;

use crate::eval2::{REGISTERS, STACK_SIZE};
use crate::{ast, eval2, Ast, AstT, DataType, Span, Val};

use super::State;

const SPAN: Span = Span::pos(0, 0);

#[test]
fn add_constants() {
    let op = ast::Op::AddInt;
    let args = [
        Ast::expr(AstT::Val(Val::Int(238)), DataType::Int, false, SPAN),
        Ast::expr(AstT::Val(Val::Int(-324)), DataType::Int, false, SPAN),
    ];

    let mut state = State {
        used_registers: 2,
        ..Default::default()
    };

    let mut registers = [0; REGISTERS];
    let mut stack = [0; STACK_SIZE];

    gen_op(&mut state, op, &args, 1);
    eval2::eval(&state.consts, &state.code, &mut registers, &mut stack).unwrap();

    let val: i64 = unsafe { std::mem::transmute(registers[1]) };
    assert_eq!(val, -86);
}
