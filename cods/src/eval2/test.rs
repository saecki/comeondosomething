use super::*;

#[test]
fn add_int() {
    let code = vec![OpCode::Add as u8, 3, 1, 2, 0, 0, 0, 0];
    let mut registers = [0; REGISTERS];
    registers[1] = 4;
    registers[2] = 9;

    let mut stack = [0; STACK_SIZE];
    eval(&[], &code, &mut registers, &mut stack).unwrap();

    let val = registers[3];
    assert_eq!(val, 13);
}

#[test]
fn sub_signed_int() {
    let code = vec![OpCode::Sub as u8, 3, 1, 2, 0, 0, 0, 0];
    let mut registers = [0; REGISTERS];
    registers[1] = 4;
    registers[2] = 9;

    let mut stack = [0; STACK_SIZE];
    eval(&[], &code, &mut registers, &mut stack).unwrap();

    let val = signed(registers[3]);
    assert_eq!(val, -5);
}

#[test]
fn mul_int() {
    let code = vec![OpCode::Mul as u8, 3, 1, 2, 0, 0, 0, 0];
    let mut registers = [0; REGISTERS];
    registers[1] = 4;
    registers[2] = 9;

    let mut stack = [0; STACK_SIZE];
    eval(&[], &code, &mut registers, &mut stack).unwrap();

    let val = registers[3];
    assert_eq!(val, 36);
}

#[test]
fn div_int() {
    let code = vec![OpCode::Div as u8, 3, 1, 2, 0, 0, 0, 0];
    let mut registers = [0; REGISTERS];
    registers[1] = 4;
    registers[2] = 9;

    let mut stack = [0; STACK_SIZE];
    eval(&[], &code, &mut registers, &mut stack).unwrap();

    let val = registers[3];
    assert_eq!(val, 0);
}

#[test]
fn rem_int() {
    let code = vec![OpCode::Rem as u8, 3, 1, 2, 0, 0, 0, 0];
    let mut registers = [0; REGISTERS];
    registers[1] = 37;
    registers[2] = 6;

    let mut stack = [0; STACK_SIZE];
    eval(&[], &code, &mut registers, &mut stack).unwrap();

    let val = registers[3];
    assert_eq!(val, 1);
}

#[test]
fn add_float() {
    let code = vec![OpCode::FAdd as u8, 3, 1, 2, 0, 0, 0, 0];
    let mut registers = [0; REGISTERS];
    registers[1] = int(4.0);
    registers[2] = int(9.0);

    let mut stack = [0; STACK_SIZE];
    eval(&[], &code, &mut registers, &mut stack).unwrap();

    let val = float(registers[3]);
    assert_eq!(val, 13.0);
}

#[test]
fn sub_float() {
    let code = vec![OpCode::FSub as u8, 3, 1, 2, 0, 0, 0, 0];
    let mut registers = [0; REGISTERS];
    registers[1] = int(4.0);
    registers[2] = int(9.0);

    let mut stack = [0; STACK_SIZE];
    eval(&[], &code, &mut registers, &mut stack).unwrap();

    let val = float(registers[3]);
    assert_eq!(val, -5.0);
}

#[test]
fn mul_float() {
    let code = vec![OpCode::FMul as u8, 3, 1, 2, 0, 0, 0, 0];
    let mut registers = [0; REGISTERS];
    registers[1] = int(4.0);
    registers[2] = int(9.0);

    let mut stack = [0; STACK_SIZE];
    eval(&[], &code, &mut registers, &mut stack).unwrap();

    let val = float(registers[3]);
    assert_eq!(val, 36.0);
}

#[test]
fn div_float() {
    let code = vec![OpCode::FDiv as u8, 3, 1, 2, 0, 0, 0, 0];
    let mut registers = [0; REGISTERS];
    registers[1] = int(4.0);
    registers[2] = int(9.0);

    let mut stack = [0; STACK_SIZE];
    eval(&[], &code, &mut registers, &mut stack).unwrap();

    let val = float(registers[3]);
    assert_eq!(val, 0.4444444444444444);
}

#[test]
fn rem_float() {
    let code = vec![OpCode::FRem as u8, 3, 1, 2, 0, 0, 0, 0];
    let mut registers = [0; REGISTERS];
    registers[1] = int(37.0);
    registers[2] = int(6.0);

    let mut stack = [0; STACK_SIZE];
    eval(&[], &code, &mut registers, &mut stack).unwrap();

    let val = float(registers[3]);
    assert_eq!(val, 1.0);
}

#[test]
fn mov() {
    #[rustfmt::skip]
    let code = vec![
        OpCode::Move as u8, 1, 2, 0, 0, 0, 0, 0,
    ];
    let mut registers = [0; REGISTERS];
    registers[2] = 42;

    let mut stack = [0; STACK_SIZE];
    eval(&[], &code, &mut registers, &mut stack).unwrap();

    let val = registers[1];
    assert_eq!(val, 42);
}

#[test]
fn store() {
    #[rustfmt::skip]
    let code = vec![
        OpCode::Push as u8, 0, 0, 0,  8, 0, 0, 0,
        OpCode::Store as u8,  1, STACK_PTR as u8, 0,  0, 0, 0, 0,
    ];
    let mut registers = [0; REGISTERS];
    registers[1] = 42;

    let mut stack = [0; STACK_SIZE];
    eval(&[], &code, &mut registers, &mut stack).unwrap();

    let val: i64 = *get(&stack, STACK_SIZE - 8);
    assert_eq!(val, 42);
    assert_eq!(registers[STACK_PTR] as usize, STACK_SIZE - 8);
}

#[test]
fn store_load() {
    #[rustfmt::skip]
    let code = vec![
        OpCode::Push as u8, 0, 0, 0,  8, 0, 0, 0,
        OpCode::Store as u8,  1, STACK_PTR as u8, 0,  0, 0, 0, 0,
        OpCode::Load as u8,  2, STACK_PTR as u8, 0,  0, 0, 0, 0,
    ];
    let mut registers = [0; REGISTERS];
    registers[1] = 42;

    let mut stack = [0; STACK_SIZE];
    eval(&[], &code, &mut registers, &mut stack).unwrap();

    assert_eq!(registers[2], 42);
}
