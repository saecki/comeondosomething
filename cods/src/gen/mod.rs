use crate::ast::Op;
use crate::eval2::{OpCode, REGISTERS};
use crate::util::get_mut;
use crate::{Ast, AstT, Val};

#[cfg(test)]
mod test;

#[derive(Debug, Default)]
pub struct State {
    consts: Vec<u8>,
    code: Vec<u8>,
    used_registers: u8,
}

fn gen_instructions(state: &mut State, asts: &[Ast]) {
    for a in asts {
        match &a.typ {
            AstT::Error => todo!(),
            AstT::Var(_) => todo!(),
            AstT::Val(_) => todo!(),
            AstT::Op(op, args) => {
                let reg_dest = use_register(state);
                gen_op(state, *op, args, reg_dest)
            }
            AstT::Is(_, _) => todo!(),
            AstT::Cast(_, _) => todo!(),
            AstT::Unit => todo!(),
            AstT::Block(_) => todo!(),
            AstT::IfExpr(_) => todo!(),
            AstT::MatchExpr(_) => todo!(),
            AstT::WhileLoop(_) => todo!(),
            AstT::ForLoop(_) => todo!(),
            AstT::VarAssign(_, _) => todo!(),
            AstT::FunCall(_, _) => todo!(),
            AstT::Return(_) => todo!(),
            AstT::BuiltinFunCall(_, _) => todo!(),
            AstT::Spill(_) => todo!(),
        }
    }
}

fn gen_op(state: &mut State, op: Op, args: &[Ast], reg_dest: u8) {
    let mut instruction = [0; 8];
    match op {
        Op::Not => todo!(),
        Op::NegInt => todo!(),
        Op::NegFloat => todo!(),
        Op::RangeIn => todo!(),
        Op::RangeEx => todo!(),
        Op::AddInt => {
            let reg_a = use_register(state);
            let reg_b = use_register(state);
            gen_to_reg(state, &args[0], reg_a);
            gen_to_reg(state, &args[1], reg_b);
            instruction[0] = OpCode::Add as u8;
            instruction[1] = reg_dest;
            instruction[2] = reg_a;
            instruction[3] = reg_b;
        }
        Op::AddFloat => todo!(),
        Op::SubInt => {
            let reg_a = use_register(state);
            let reg_b = use_register(state);
            gen_to_reg(state, &args[0], reg_a);
            gen_to_reg(state, &args[1], reg_b);
            instruction[0] = OpCode::Sub as u8;
            instruction[1] = reg_dest;
            instruction[2] = reg_a;
            instruction[3] = reg_b;
        }
        Op::SubFloat => todo!(),
        Op::MulInt => {
            let reg_a = use_register(state);
            let reg_b = use_register(state);
            gen_to_reg(state, &args[0], reg_a);
            gen_to_reg(state, &args[1], reg_b);
            instruction[0] = OpCode::Mul as u8;
            instruction[1] = reg_dest;
            instruction[2] = reg_a;
            instruction[3] = reg_b;
        }
        Op::MulFloat => todo!(),
        Op::DivInt => {
            let reg_a = use_register(state);
            let reg_b = use_register(state);
            gen_to_reg(state, &args[0], reg_a);
            gen_to_reg(state, &args[1], reg_b);
            instruction[0] = OpCode::Div as u8;
            instruction[1] = reg_dest;
            instruction[2] = reg_a;
            instruction[3] = reg_b;
        }
        Op::DivFloat => todo!(),
        Op::PowInt => todo!(),
        Op::PowFloat => todo!(),
        Op::PowFloatInt => todo!(),
        Op::RemInt => {
            let reg_a = use_register(state);
            let reg_b = use_register(state);
            gen_to_reg(state, &args[0], reg_a);
            gen_to_reg(state, &args[1], reg_b);
            instruction[0] = OpCode::Rem as u8;
            instruction[1] = reg_dest;
            instruction[2] = reg_a;
            instruction[3] = reg_b;
        }
        Op::RemFloat => todo!(),
        Op::RemEuclidInt => todo!(),
        Op::FactorialInt => todo!(),
        Op::Eq => todo!(),
        Op::Ne => todo!(),
        Op::LtInt => todo!(),
        Op::LtFloat => todo!(),
        Op::LeInt => todo!(),
        Op::LeFloat => todo!(),
        Op::GtInt => todo!(),
        Op::GtFloat => todo!(),
        Op::GeInt => todo!(),
        Op::GeFloat => todo!(),
        Op::Or => todo!(),
        Op::And => todo!(),
        Op::BwOrInt => todo!(),
        Op::BwOrBool => todo!(),
        Op::XorInt => todo!(),
        Op::XorBool => todo!(),
        Op::BwAndInt => todo!(),
        Op::BwAndBool => todo!(),
        Op::ShlInt => todo!(),
        Op::ShrInt => todo!(),
    }
    state.code.extend_from_slice(&instruction);
}

fn gen_to_reg(state: &mut State, arg: &Ast, reg: u8) {
    let mut instruction = [0; 8];

    match &arg.typ {
        AstT::Val(v) => {
            let addr = push_const(state, v);
            instruction[0] = OpCode::LoadConst as u8;
            instruction[1] = reg;
            *get_mut(&mut instruction, 4) = addr;
        }
        _ => todo!(),
    }
    state.code.extend_from_slice(&instruction);
}

fn push_const(state: &mut State, val: &Val) -> u32 {
    match val {
        Val::Int(i) => {
            let addr = state.consts.len();
            state.consts.resize(state.consts.len() + 8, 0);
            *get_mut(&mut state.consts, addr) = *i as i64;
            addr as u32
        }
        _ => todo!(),
    }
}

fn use_register(state: &mut State) -> u8 {
    if state.used_registers >= (REGISTERS - 2) as u8 {
        todo!("all registers used");
    }

    state.used_registers += 1;
    state.used_registers
}
