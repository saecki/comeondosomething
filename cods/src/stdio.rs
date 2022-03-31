use core::fmt;
use std::io::{self, Stdin, Stdout, Write};

#[derive(Debug)]
pub struct Stdio {
    pub stdout: Stdout,
    pub stdin: Stdin,
}

impl Default for Stdio {
    fn default() -> Self {
        Self {
            stdout: io::stdout(),
            stdin: io::stdin(),
        }
    }
}

impl Stdio {
    pub fn print(&mut self, args: fmt::Arguments) {
        let _ = self.stdout.write_fmt(args);
    }
}
