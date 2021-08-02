use std::cmp::min;
use std::fmt;
use std::fmt::Write;
use std::marker::PhantomData;

use unicode_width::UnicodeWidthChar;

use crate::{range, Color, LBlue, Range, ANSI_ESC};

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
            "   {blue}│{esc} {col}{desc}{esc}",
            desc = self.error.description(),
            col = C::bold(),
            blue = LBlue::bold(),
            esc = ANSI_ESC,
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
    write!(
        f,
        "{blue}{nr:02} │{esc} {ln}\n   {blue}│{esc} ",
        nr = line_nr,
        ln = line,
        blue = LBlue::bold(),
        esc = ANSI_ESC,
    )?;

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
        for _ in pos..(r.end) {
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
        write!(f, "{}", C::bold())?;
        for _ in 0..width {
            f.write_char('^')?;
        }
        f.write_str(ANSI_ESC)?;
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
                    let range = range(line_start.0, pos.0 + 1);
                    let line = &string[line_start.1..pos.1];
                    lines.push((range, line));
                }
                pushed_line = true;
            }
            '\n' => {
                if !pushed_line {
                    let range = range(line_start.0, pos.0 + 1);
                    let line = &string[line_start.1..pos.1];
                    lines.push((range, line));
                }

                // We know this char is 1 byte wide
                line_start = (pos.0 + 1, pos.1 + 1);
                pushed_line = false;
            }
            _ => pushed_line = false,
        }

        pos.0 += 1;
        pos.1 = string.len() - chars.as_str().len();
    }

    if !pushed_line {
        let range = range(line_start.0, pos.0 + 1);
        let line = &string[line_start.1..pos.1];
        lines.push((range, line));
    }

    lines
}
