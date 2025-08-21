use std::fmt::{self, Display};

pub const ANSI_ESC: &str = "\x1B[0m";

#[macro_export]
macro_rules! cprintln {
    ($col:ty, $pat:expr $(,$args:expr),*) => {{
        print!("{}", <$col>::NORMAL);
        print!($pat, $($args,)*);
        println!("{ANSI_ESC}");
    }}
}

#[macro_export]
macro_rules! cprint {
    ($col:ty, $pat:expr $(,$args:expr),*) => {{
        print!("{}", <$col>::NORMAL);
        print!($pat, $($args,)*);
        print!("{ANSI_ESC}");
    }}
}

#[macro_export]
macro_rules! bprintln {
    ($col:ty, $pat:expr $(,$args:expr),*) => {{
        print!("{}", <$col>::BOLD);
        print!($pat, $($args,)*);
        println!("{ANSI_ESC}");
    }}
}

#[macro_export]
macro_rules! bprint {
    ($col:ty, $pat:expr $(,$args:expr),*) => {{
        print!("{}", <$col>::BOLD);
        print!($pat, $($args,)*);
        print!("{ANSI_ESC}");
    }}
}

pub trait Color: Sized {
    const COLOR_CODE: u8;

    const NORMAL: WriteAnsi = WriteAnsi {
        bold: false,
        color: Self::COLOR_CODE,
    };

    const BOLD: WriteAnsi = WriteAnsi {
        bold: true,
        color: Self::COLOR_CODE,
    };
}

pub struct WriteAnsi {
    bold: bool,
    color: u8,
}

impl Display for WriteAnsi {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("\x1B[")?;
        if self.bold {
            f.write_str("1;")?;
        }
        write!(f, "{}", self.color)?;
        f.write_str("m")?;
        Ok(())
    }
}

pub struct DRed;
impl Color for DRed {
    const COLOR_CODE: u8 = 31;
}

pub struct DGreen;
impl Color for DGreen {
    const COLOR_CODE: u8 = 32;
}

pub struct DYellow;
impl Color for DYellow {
    const COLOR_CODE: u8 = 33;
}

pub struct DBlue;
impl Color for DBlue {
    const COLOR_CODE: u8 = 34;
}

pub struct LRed;
impl Color for LRed {
    const COLOR_CODE: u8 = 91;
}

pub struct LGreen;
impl Color for LGreen {
    const COLOR_CODE: u8 = 92;
}

pub struct LYellow;
impl Color for LYellow {
    const COLOR_CODE: u8 = 93;
}

pub struct LBlue;
impl Color for LBlue {
    const COLOR_CODE: u8 = 94;
}
