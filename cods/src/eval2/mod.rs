use crate::util::{get, get_mut};

#[cfg(test)]
mod test;

pub const REGISTERS: usize = 256;
pub const STACK_PTR: usize = 255;
pub const STACK_SIZE: usize = 1024 * 1024;

/// The first byte of every instruction is the op code.
///
/// NOTE: this has to be kept in sync with the `TryFrom` implementation.
#[repr(u8)]
#[rustfmt::skip]
pub enum OpCode {
    Push      = 0x00,
    Pop       = 0x01,
    JmpAbs    = 0x02,
    JmpRel    = 0x03,
    Cbzr      = 0x04,
    Cbnz      = 0x05,
    Cbeq      = 0x06,
    Cbne      = 0x07,
    Cblt      = 0x08,
    Cble      = 0x09,
    Cbgt      = 0x0A,
    Cbge      = 0x0B,
    LoadConst = 0x0C,
    Load      = 0x0D,
    Store     = 0x0E,
    Move      = 0x0F,
    Eq        = 0x10,
    Ne        = 0x11,
    Lt        = 0x12,
    Le        = 0x13,
    Gt        = 0x14,
    Ge        = 0x15,
    Or        = 0x16,
    Xor       = 0x17,
    And       = 0x18,
    Shl       = 0x19,
    Shr       = 0x1A,
    Add    = 0x1B,
    Sub    = 0x1C,
    Mul    = 0x1D,
    IMul   = 0x1E,
    Div    = 0x1F,
    IDiv   = 0x20,
    Rem    = 0x21,
    IRem   = 0x22,
    FAdd  = 0x23,
    FSub  = 0x24,
    FMul  = 0x25,
    FDiv  = 0x26,
    FRem  = 0x27,
}

impl TryFrom<u8> for OpCode {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value > 0x27 {
            Err(())
        } else {
            unsafe { Ok(std::mem::transmute(value)) }
        }
    }
}

/// Register 0 is the stack pointer
pub fn eval(
    consts: &[u8],
    code: &[u8],
    registers: &mut [u64; REGISTERS],
    stack: &mut [u8; STACK_SIZE],
) -> crate::Result<()> {
    let mut instruction_ptr: i32 = 0;

    registers[STACK_PTR] = STACK_SIZE as u64;

    while (instruction_ptr as usize) < code.len() {
        dbg!(registers[STACK_PTR]);
        dbg!(registers[1]);
        dbg!(&stack[STACK_SIZE - 16..]);
        let [op, args @ ..] = *get::<[u8; 8]>(code, instruction_ptr as usize);
        let op: OpCode = OpCode::try_from(op).expect("Unknown op code");
        instruction_ptr += 8;
        match op {
            OpCode::Push => {
                let size = *get::<u32>(&args, 3) as u64;
                if registers[STACK_PTR] < size {
                    return Err(crate::Error::StackOverflow);
                }
                registers[STACK_PTR] -= size;
            }
            OpCode::Pop => {
                let size = *get::<u32>(&args, 3) as u64;
                if registers[STACK_PTR] + size > STACK_SIZE as u64 {
                    return Err(crate::Error::StackUnderflow);
                }
                registers[STACK_PTR] += size;
            }
            OpCode::JmpAbs => {
                instruction_ptr = *get::<i32>(&args, 3);
            }
            OpCode::JmpRel => {
                instruction_ptr += *get::<i32>(&args, 3);
            }
            OpCode::Cbzr => {
                let reg_cmp = args[0] as usize;
                if registers[reg_cmp] == 0 {
                    instruction_ptr += *get::<i32>(&args, 3);
                }
            }
            OpCode::Cbnz => {
                let reg_cmp = args[0] as usize;
                if registers[reg_cmp as usize] != 0 {
                    instruction_ptr += *get::<i32>(&args, 3);
                }
            }
            OpCode::Cbeq => {
                let reg_a = args[0] as usize;
                let reg_b = args[1] as usize;
                if registers[reg_a] == registers[reg_b] {
                    instruction_ptr += *get::<i32>(&args, 3);
                }
            }
            OpCode::Cbne => {
                let reg_a = args[0] as usize;
                let reg_b = args[1] as usize;
                if registers[reg_a] != registers[reg_b] {
                    instruction_ptr += *get::<i32>(&args, 3);
                }
            }
            OpCode::Cblt => {
                let reg_a = args[0] as usize;
                let reg_b = args[1] as usize;
                if registers[reg_a] < registers[reg_b] {
                    instruction_ptr += *get::<i32>(&args, 3);
                }
            }
            OpCode::Cble => {
                let reg_a = args[0] as usize;
                let reg_b = args[1] as usize;
                if registers[reg_a] <= registers[reg_b] {
                    instruction_ptr += *get::<i32>(&args, 3);
                }
            }
            OpCode::Cbgt => {
                let reg_a = args[0] as usize;
                let reg_b = args[1] as usize;
                if registers[reg_a] > registers[reg_b] {
                    instruction_ptr += *get::<i32>(&args, 3);
                }
            }
            OpCode::Cbge => {
                let reg_a = args[0] as usize;
                let reg_b = args[1] as usize;
                if registers[reg_a] >= registers[reg_b] {
                    instruction_ptr += *get::<i32>(&args, 3);
                }
            }
            OpCode::LoadConst => {
                let reg_dest = args[0] as usize;
                let addr = *get::<u32>(&args, 3);
                registers[reg_dest] = *get::<u64>(consts, addr as usize);
            }
            OpCode::Load => {
                let reg_dest = args[0] as usize;
                let reg_addr = args[1] as usize;
                let offset = *get::<i32>(&args, 3) as i64;
                let addr = registers[reg_addr] as i64 + offset;
                registers[reg_dest] = *get::<u64>(stack, addr as usize);
            }
            OpCode::Store => {
                let reg_src = args[0] as usize;
                let reg_addr = args[1] as usize;
                let offset = *get::<i32>(&args, 3) as i64;
                dbg!(offset);
                let addr = registers[reg_addr] as i64 + offset;
                dbg!(addr);
                *get_mut::<u64>(stack, addr as usize) = registers[reg_src];
            }
            OpCode::Move => {
                let reg_dest = args[0] as usize;
                let reg_src = args[1] as usize;
                registers[reg_dest] = registers[reg_src];
            }
            OpCode::Eq => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = (registers[reg_a] == registers[reg_b]) as u64;
            }
            OpCode::Ne => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = (registers[reg_a] != registers[reg_b]) as u64;
            }
            OpCode::Lt => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = (registers[reg_a] < registers[reg_b]) as u64;
            }
            OpCode::Le => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = (registers[reg_a] <= registers[reg_b]) as u64;
            }
            OpCode::Gt => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = (registers[reg_a] > registers[reg_b]) as u64;
            }
            OpCode::Ge => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = (registers[reg_a] >= registers[reg_b]) as u64;
            }
            OpCode::Or => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = registers[reg_a] | registers[reg_b];
            }
            OpCode::Xor => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = registers[reg_a] ^ registers[reg_b];
            }
            OpCode::And => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = registers[reg_a] & registers[reg_b];
            }
            OpCode::Shl => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = registers[reg_a] << registers[reg_b];
            }
            OpCode::Shr => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = registers[reg_a] >> registers[reg_b];
            }
            OpCode::Add => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = registers[reg_a].wrapping_add(registers[reg_b]);
            }
            OpCode::FAdd => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = int(float(registers[reg_a]) + float(registers[reg_b]));
            }
            OpCode::Sub => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = registers[reg_a].wrapping_sub(registers[reg_b]);
            }
            OpCode::FSub => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = int(float(registers[reg_a]) - float(registers[reg_b]));
            }
            OpCode::Mul => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = registers[reg_a] * registers[reg_b];
            }
            OpCode::IMul => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = unsigned(signed(registers[reg_a]) * signed(registers[reg_b]));
            }
            OpCode::FMul => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = int(float(registers[reg_a]) * float(registers[reg_b]));
            }
            OpCode::Div => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = registers[reg_a] / registers[reg_b];
            }
            OpCode::IDiv => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = unsigned(signed(registers[reg_a]) / signed(registers[reg_b]));
            }
            OpCode::FDiv => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = int(float(registers[reg_a]) / float(registers[reg_b]));
            }
            OpCode::Rem => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = registers[reg_a] % registers[reg_b];
            }
            OpCode::IRem => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = unsigned(signed(registers[reg_a]) % signed(registers[reg_b]));
            }
            OpCode::FRem => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = int(float(registers[reg_a]) % float(registers[reg_b]));
            }
        }
    }

    dbg!(registers[STACK_PTR]);
    dbg!(registers[1]);
    dbg!(&stack[STACK_SIZE - 16..]);

    Ok(())
}

fn signed(val: u64) -> i64 {
    unsafe { std::mem::transmute(val) }
}

fn unsigned(val: i64) -> u64 {
    unsafe { std::mem::transmute(val) }
}

fn float(val: u64) -> f64 {
    unsafe { std::mem::transmute(val) }
}

fn int(val: f64) -> u64 {
    unsafe { std::mem::transmute(val) }
}
