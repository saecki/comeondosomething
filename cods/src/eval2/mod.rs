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
    Push     = 0x00,
    Pop      = 0x01,
    JmpAbs   = 0x02,
    JmpRel   = 0x03,
    Cbzr     = 0x04,
    Cbnz     = 0x05,
    Cbeq     = 0x06,
    Cbne     = 0x07,
    Cblt     = 0x08,
    Cble     = 0x09,
    Cbgt     = 0x0A,
    Cbge     = 0x0B,
    Ldc      = 0x0C,
    Lds      = 0x0D,
    Str      = 0x0E,
    Mov      = 0x0F,
    Eq       = 0x10,
    Ne       = 0x11,
    Lt       = 0x12,
    Le       = 0x13,
    Gt       = 0x14,
    Ge       = 0x15,
    Or       = 0x16,
    Xor      = 0x17,
    And      = 0x18,
    Shl      = 0x19,
    Shr      = 0x1A,
    AddInt   = 0x1B,
    SubInt   = 0x1C,
    MulInt   = 0x1D,
    DivInt   = 0x1E,
    RemInt   = 0x1F,
    AddFloat = 0x20,
    SubFloat = 0x21,
    MulFloat = 0x22,
    DivFloat = 0x23,
    RemFloat = 0x24,
}

impl TryFrom<u8> for OpCode {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value > 0x24 {
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
    registers: &mut [i64; REGISTERS],
    stack: &mut [u8; STACK_SIZE],
) -> crate::Result<()> {
    let mut instruction_ptr: i32 = 0;

    registers[STACK_PTR] = STACK_SIZE as i64;

    while (instruction_ptr as usize) < code.len() {
        dbg!(registers[STACK_PTR]);
        dbg!(registers[1]);
        dbg!(&stack[STACK_SIZE - 16..]);
        let [op, args @ ..] = *get::<[u8; 8]>(code, instruction_ptr as usize);
        let op: OpCode = OpCode::try_from(op).expect("Unknown op code");
        instruction_ptr += 8;
        match op {
            OpCode::Push => {
                let size = *get::<i32>(&args, 3) as i64;
                if registers[STACK_PTR] - size < 0 {
                    return Err(crate::Error::StackOverflow);
                }
                registers[STACK_PTR] -= size as i64;
            }
            OpCode::Pop => {
                let size = *get::<i32>(&args, 3) as i64;
                if registers[STACK_PTR] + size > STACK_SIZE as i64 {
                    return Err(crate::Error::StackUnderflow);
                }
                registers[STACK_PTR] += size as i64;
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
            OpCode::Ldc => {
                let reg_dest = args[0] as usize;
                let addr = *get::<u32>(&args, 3);
                registers[reg_dest] = *get::<i64>(consts, addr as usize);
            }
            OpCode::Lds => {
                let reg_dest = args[0] as usize;
                let reg_addr = args[1] as usize;
                let offset = *get::<i32>(&args, 3) as i64;
                let addr = registers[reg_addr] + offset;
                registers[reg_dest] = *get::<i64>(stack, addr as usize);
            }
            OpCode::Str => {
                let reg_src = args[0] as usize;
                let reg_addr = args[1] as usize;
                let offset = *get::<i32>(&args, 3) as i64;
                dbg!(offset);
                let addr = registers[reg_addr] + offset;
                dbg!(addr);
                *get_mut::<i64>(stack, addr as usize) = registers[reg_src];
            }
            OpCode::Mov => {
                let reg_dest = args[0] as usize;
                let reg_src = args[1] as usize;
                registers[reg_dest] = registers[reg_src];
            }
            OpCode::Eq => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = (registers[reg_a] == registers[reg_b]) as i64;
            }
            OpCode::Ne => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = (registers[reg_a] != registers[reg_b]) as i64;
            }
            OpCode::Lt => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = (registers[reg_a] < registers[reg_b]) as i64;
            }
            OpCode::Le => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = (registers[reg_a] <= registers[reg_b]) as i64;
            }
            OpCode::Gt => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = (registers[reg_a] > registers[reg_b]) as i64;
            }
            OpCode::Ge => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = (registers[reg_a] >= registers[reg_b]) as i64;
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
            OpCode::AddInt => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = registers[reg_a] + registers[reg_b];
            }
            OpCode::AddFloat => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = int(float(registers[reg_a]) + float(registers[reg_b]));
            }
            OpCode::SubInt => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = registers[reg_a] - registers[reg_b];
            }
            OpCode::SubFloat => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = int(float(registers[reg_a]) - float(registers[reg_b]));
            }
            OpCode::MulInt => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = registers[reg_a] * registers[reg_b];
            }
            OpCode::MulFloat => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = int(float(registers[reg_a]) * float(registers[reg_b]));
            }
            OpCode::DivInt => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = registers[reg_a] / registers[reg_b];
            }
            OpCode::DivFloat => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = int(float(registers[reg_a]) / float(registers[reg_b]));
            }
            OpCode::RemInt => {
                let reg_dest = args[0] as usize;
                let reg_a = args[1] as usize;
                let reg_b = args[2] as usize;
                registers[reg_dest] = registers[reg_a] % registers[reg_b];
            }
            OpCode::RemFloat => {
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

fn float(val: i64) -> f64 {
    unsafe { std::mem::transmute(val) }
}

fn int(val: f64) -> i64 {
    unsafe { std::mem::transmute(val) }
}
