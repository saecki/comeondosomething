use crate::ast::Op;
use crate::eval2::{OpCode, NUM_REGISTERS};
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
    match op {
        Op::Not => {
            let mut instruction = [0; 8];
            instruction[0] = OpCode::LoadInline as u8;
            instruction[1] = reg_dest;
            instruction[4] = 1;
            state.code.extend(&instruction);

            let mut instruction = [0; 8];
            let reg_a = use_register(state);
            gen_to_reg(state, &args[0], reg_a);
            instruction[0] = OpCode::Xor as u8;
            instruction[1] = reg_dest;
            instruction[2] = reg_dest;
            instruction[3] = reg_a;
            state.code.extend(&instruction);
        }
        Op::NegInt => {
            let mut instruction = [0; 8];
            let reg_a = use_register(state);
            gen_to_reg(state, &args[0], reg_a);
            instruction[0] = OpCode::Neg as u8;
            instruction[1] = reg_dest;
            instruction[2] = reg_a;
            state.code.extend(&instruction);
        }
        Op::NegFloat => {
            let mut instruction = [0; 8];
            let reg_a = use_register(state);
            gen_to_reg(state, &args[0], reg_a);
            instruction[0] = OpCode::FNeg as u8;
            instruction[1] = reg_dest;
            instruction[2] = reg_a;
            state.code.extend(&instruction);
        }
        Op::RangeIn => todo!(),
        Op::RangeEx => todo!(),
        Op::AddInt => gen_two_op(state, OpCode::Add, args, reg_dest),
        Op::AddFloat => gen_two_op(state, OpCode::FAdd, args, reg_dest),
        Op::SubInt => gen_two_op(state, OpCode::Sub, args, reg_dest),
        Op::SubFloat => gen_two_op(state, OpCode::FSub, args, reg_dest),
        Op::MulInt => gen_two_op(state, OpCode::IMul, args, reg_dest),
        Op::MulFloat => gen_two_op(state, OpCode::FMul, args, reg_dest),
        Op::DivInt => gen_two_op(state, OpCode::IDiv, args, reg_dest),
        Op::DivFloat => gen_two_op(state, OpCode::FDiv, args, reg_dest),
        Op::PowInt => todo!(),
        Op::PowFloat => todo!(),
        Op::PowFloatInt => todo!(),
        Op::RemInt => gen_two_op(state, OpCode::IRem, args, reg_dest),
        Op::RemFloat => gen_two_op(state, OpCode::FRem, args, reg_dest),
        Op::RemEuclidInt => todo!(),
        Op::FactorialInt => todo!(),
        Op::Eq => gen_two_op(state, OpCode::Eq, args, reg_dest),
        Op::Ne => gen_two_op(state, OpCode::Ne, args, reg_dest),
        Op::LtInt => gen_two_op(state, OpCode::ILt, args, reg_dest),
        Op::LtFloat => gen_two_op(state, OpCode::FLt, args, reg_dest),
        Op::LeInt => gen_two_op(state, OpCode::ILe, args, reg_dest),
        Op::LeFloat => gen_two_op(state, OpCode::FLe, args, reg_dest),
        Op::GtInt => gen_two_op(state, OpCode::IGt, args, reg_dest),
        Op::GtFloat => gen_two_op(state, OpCode::FGt, args, reg_dest),
        Op::GeInt => gen_two_op(state, OpCode::IGe, args, reg_dest),
        Op::GeFloat => gen_two_op(state, OpCode::FGe, args, reg_dest),
        Op::Or => gen_two_op(state, OpCode::Or, args, reg_dest),
        Op::And => gen_two_op(state, OpCode::And, args, reg_dest),
        Op::BwOrInt => gen_two_op(state, OpCode::Or, args, reg_dest),
        Op::BwOrBool => gen_two_op(state, OpCode::Or, args, reg_dest),
        Op::XorInt => gen_two_op(state, OpCode::Xor, args, reg_dest),
        Op::XorBool => gen_two_op(state, OpCode::Xor, args, reg_dest),
        Op::BwAndInt => gen_two_op(state, OpCode::And, args, reg_dest),
        Op::BwAndBool => gen_two_op(state, OpCode::And, args, reg_dest),
        Op::ShlInt => gen_two_op(state, OpCode::Shl, args, reg_dest),
        Op::ShrInt => gen_two_op(state, OpCode::Shr, args, reg_dest),
    }
}

fn gen_two_op(state: &mut State, op_code: OpCode, args: &[Ast], reg_dest: u8) {
    let mut instruction = [0; 8];
    let reg_a = use_register(state);
    let reg_b = use_register(state);
    gen_to_reg(state, &args[0], reg_a);
    gen_to_reg(state, &args[1], reg_b);
    instruction[0] = op_code as u8;
    instruction[1] = reg_dest;
    instruction[2] = reg_a;
    instruction[3] = reg_b;
    state.code.extend(&instruction);
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
        Val::Bool(b) => {
            let addr = state.consts.len();
            state.consts.resize(state.consts.len() + 1, 0);
            *get_mut(&mut state.consts, addr) = *b as u8;
            addr as u32
        }
        _ => todo!(),
    }
}

fn use_register(state: &mut State) -> u8 {
    if state.used_registers >= (NUM_REGISTERS - 2) as u8 {
        todo!("all registers used");
    }

    state.used_registers += 1;
    state.used_registers
}
