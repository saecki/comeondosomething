use std::cmp::min;
use std::fmt;
use std::fmt::Write;
use std::marker::PhantomData;

use unicode_width::UnicodeWidthChar;

use crate::{range, Cmd, Group, Mod, Num, Op, Par, Range};

pub type Result<T> = std::result::Result<T, Error>;

pub trait UserFacing<C: Color>: Sized + fmt::Debug {
    fn description(&self) -> String;
    fn ranges(&self) -> Vec<Range>;

    fn display<'a>(&'a self, input: &'a str) -> DisplayUserFacing<'a, Self, C> {
        DisplayUserFacing {
            input,
            error: self,
            c: PhantomData::<C>,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    Parsing(Range),
    MissingOperand(Range),
    MissingOperator(Range),
    MissingCommandParenthesis(Range),
    MissingClosingParenthesis(Par),
    UnexpectedGroup(Group),
    UnexpectedNumber(Num),
    UnexpectedOperator(Op),
    UnexpectedCommand(Cmd),
    UnexpectedModifier(Mod),
    UnexpectedParenthesis(Par),
    InvalidNumberFormat(Range),
    DivideByZero(Num, Num),
    NegativeFactorial(Range),
}

impl UserFacing<Red> for Error {
    fn description(&self) -> String {
        match self {
            Self::Parsing(_) => format!("A parsing error occured"),
            Self::MissingOperand(_) => format!("Missing an operand"),
            Self::MissingOperator(_) => format!("Missing an operator"),
            Self::MissingCommandParenthesis(_) => format!("Missing a command parenthesis"),
            Self::MissingClosingParenthesis(_) => format!("Missing a matching closing parenthesis"),
            Self::UnexpectedGroup(_) => format!("Found an unexpected group"),
            Self::UnexpectedNumber(_) => format!("Found an unexpected number"),
            Self::UnexpectedOperator(_) => format!("Found an unexpected operator"),
            Self::UnexpectedCommand(_) => format!("Found an unexpected command"),
            Self::UnexpectedModifier(_) => format!("Found an unexpected modifier"),
            Self::UnexpectedParenthesis(_) => format!("Found an unexpected parenthesis"),
            Self::InvalidNumberFormat(_) => format!("Invalid number format"),
            Self::DivideByZero(_, _) => format!("Attempted to divide by 0"),
            Self::NegativeFactorial(_) => {
                format!("Attempted to calculate the factorial of a negative number")
            }
        }
    }

    fn ranges(&self) -> Vec<Range> {
        match self {
            Self::Parsing(r) => vec![*r],
            Self::MissingOperand(r) => vec![*r],
            Self::MissingOperator(r) => vec![*r],
            Self::MissingCommandParenthesis(r) => vec![*r],
            Self::MissingClosingParenthesis(p) => vec![p.range()],
            Self::UnexpectedGroup(g) => vec![g.range],
            Self::UnexpectedNumber(n) => vec![n.range],
            Self::UnexpectedOperator(o) => vec![o.range()],
            Self::UnexpectedCommand(c) => vec![c.range()],
            Self::UnexpectedModifier(m) => vec![m.range()],
            Self::UnexpectedParenthesis(p) => vec![p.range()],
            Self::InvalidNumberFormat(r) => vec![*r],
            Self::DivideByZero(a, b) => vec![a.range, b.range],
            Self::NegativeFactorial(r) => vec![*r],
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Warning {
    ConfusingCase(Range, &'static str),
    MismatchedParenthesis(Par, Par),
}

impl UserFacing<Yellow> for Warning {
    fn description(&self) -> String {
        match self {
            Self::ConfusingCase(_, lit) => format!("Confusing casing, consider writing '{}'", lit),
            Self::MismatchedParenthesis(_, _) => format!("Parenthesis do not match"),
        }
    }

    fn ranges(&self) -> Vec<Range> {
        match self {
            Self::ConfusingCase(r, _) => vec![*r],
            Self::MismatchedParenthesis(a, b) => vec![a.range(), b.range()],
        }
    }
}

pub struct DisplayUserFacing<'a, U: UserFacing<C>, C: Color> {
    input: &'a str,
    error: &'a U,
    c: PhantomData<C>,
}

impl<U: UserFacing<C>, C: Color> fmt::Display for DisplayUserFacing<'_, U, C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ranges = self.error.ranges();
        let lines = range_lines(self.input);

        for (nr, (lr, l)) in lines.iter().enumerate() {
            let intersecting: Vec<_> = ranges
                .iter()
                .filter(|r| r.intersects(lr))
                .map(|r| {
                    let ms = r.start.saturating_sub(lr.start);
                    let me = min(r.end.saturating_sub(lr.start), lr.len());
                    range(ms, me)
                })
                .collect();

            if !intersecting.is_empty() {
                mark_ranges::<C>(f, nr + 1, l, &intersecting)?;
            }
        }

        write!(
            f,
            "     {}{}{}",
            C::ansi_start(),
            self.error.description(),
            C::ansi_end()
        )?;
        Ok(())
    }
}

fn mark_ranges<C: Color>(
    f: &mut fmt::Formatter<'_>,
    line_nr: usize,
    line: &str,
    ranges: &[Range],
) -> fmt::Result {
    write!(f, "{:02} │ {}\n   │ ", line_nr, line)?;

    let mut chars = line.chars();
    let mut pos = 0;
    let mut peeked = 0;

    for r in ranges {
        let mut offset = 0;
        for _ in pos..(r.start) {
            if let Some(c) = chars.next() {
                pos += 1;
                offset += c.width().unwrap_or(0);
            }
        }
        for _ in 0..peeked {
            if offset > 0 {
                offset -= 1;
                peeked -= 1;
            }
        }

        let mut width = 0;
        for _ in (pos)..(r.end) {
            if let Some(c) = chars.next() {
                pos += 1;
                width += c.width().unwrap_or(0);
            }
        }
        if width == 0 && peeked == 0 {
            width = 1;
        };
        for _ in 0..peeked {
            if width > 1 {
                offset -= 1;
                peeked -= 1;
            }
        }

        for _ in 0..offset {
            f.write_char(' ')?;
        }
        f.write_str(C::ansi_start())?;
        for _ in 0..width {
            f.write_char('^')?;
        }
        f.write_str(C::ansi_end())?;
    }

    f.write_char('\n')?;

    Ok(())
}

fn range_lines(string: &str) -> Vec<(Range, &str)> {
    let mut lines = Vec::new();
    let mut line_start = (0, 0);
    let mut pos = (0, 0);
    let mut pushed_line = false;

    let mut chars = string.chars();
    while let Some(c) = chars.next() {
        match c {
            '\r' => {
                if !pushed_line {
                    let range = range(line_start.0, pos.0);
                    let line = &string[line_start.1..pos.1];
                    lines.push((range, line));
                }
                pushed_line = true;
            }
            '\n' => {
                if !pushed_line {
                    let range = range(line_start.0, pos.0);
                    let line = &string[line_start.1..pos.1];
                    lines.push((range, line));
                }

                // We know these chars are 1 byte wide
                line_start = (pos.0 + 1, pos.1 + 1);
                pushed_line = false;
            }
            _ => pushed_line = false,
        }

        pos.0 += 1;
        pos.1 = string.len() - chars.as_str().len();
    }

    if !pushed_line {
        let range = range(line_start.0, pos.0);
        let line = &string[line_start.1..pos.1];
        lines.push((range, line));
    }

    lines
}

pub trait Color: Sized {
    fn ansi_start() -> &'static str;
    fn ansi_end() -> &'static str {
        "\x1B[0m"
    }
}

pub struct Red;
impl Color for Red {
    fn ansi_start() -> &'static str {
        "\x1B[;31m"
    }
}

pub struct Yellow;
impl Color for Yellow {
    fn ansi_start() -> &'static str {
        "\x1B[;33m"
    }
}
