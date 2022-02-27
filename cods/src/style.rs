use std::fmt;

pub const ANSI_ESC: &str = "\x1B[0m";

#[macro_export]
macro_rules! cprintln {
    ($col:ty, $pat:expr $(,$args:expr),*) => {{
        print!("{}", <$col>::normal());
        print!($pat, $($args,)*);
        println!("{ANSI_ESC}");
    }}
}

#[macro_export]
macro_rules! cprint {
    ($col:ty, $pat:expr $(,$args:expr),*) => {{
        print!("{}", <$col>::normal());
        print!($pat, $($args,)*);
        print!("{ANSI_ESC}");
    }}
}

#[macro_export]
macro_rules! bprintln {
    ($col:ty, $pat:expr $(,$args:expr),*) => {{
        print!("{}", <$col>::bold());
        print!($pat, $($args,)*);
        println!("{ANSI_ESC}");
    }}
}

#[macro_export]
macro_rules! bprint {
    ($col:ty, $pat:expr $(,$args:expr),*) => {{
        print!("{}", <$col>::bold());
        print!($pat, $($args,)*);
        print!("{ANSI_ESC}");
    }}
}

pub trait Color: Sized {
    fn color_code() -> u8;

    fn normal() -> WriteAnsi {
        WriteAnsi {
            bold: false,
            color: Self::color_code(),
        }
    }

    fn bold() -> WriteAnsi {
        WriteAnsi {
            bold: true,
            color: Self::color_code(),
        }
    }
}

pub struct WriteAnsi {
    bold: bool,
    color: u8,
}

impl fmt::Display for WriteAnsi {
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
    fn color_code() -> u8 {
        31
    }
}

pub struct DGreen;
impl Color for DGreen {
    fn color_code() -> u8 {
        32
    }
}

pub struct DYellow;
impl Color for DYellow {
    fn color_code() -> u8 {
        33
    }
}

pub struct DBlue;
impl Color for DBlue {
    fn color_code() -> u8 {
        34
    }
}

pub struct LRed;
impl Color for LRed {
    fn color_code() -> u8 {
        91
    }
}

pub struct LGreen;
impl Color for LGreen {
    fn color_code() -> u8 {
        92
    }
}

pub struct LYellow;
impl Color for LYellow {
    fn color_code() -> u8 {
        93
    }
}

pub struct LBlue;
impl Color for LBlue {
    fn color_code() -> u8 {
        94
    }
}
