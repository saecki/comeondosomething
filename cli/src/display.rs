use std::cmp::min;
use std::fmt::Write;
use std::fmt::{self, Display};
use std::marker::PhantomData;

use cods::{Range, UserFacing};
use unicode_width::UnicodeWidthChar;

use crate::style::{LRed, LYellow};
use crate::{Color, LBlue, ANSI_ESC};

impl DisplayUserFacing<LRed> for cods::Error {}
impl DisplayUserFacing<LYellow> for cods::Warning {}
pub trait DisplayUserFacing<C: Color>: UserFacing {
    fn display<'a>(&'a self, input: &'a str) -> FmtUserFacing<'a, Self, C> {
        FmtUserFacing {
            input,
            error: self,
            c: PhantomData::<C>,
        }
    }
}

pub struct FmtUserFacing<'a, U: DisplayUserFacing<C>, C: Color> {
    input: &'a str,
    error: &'a U,
    c: PhantomData<C>,
}

impl<U: DisplayUserFacing<C>, C: Color> Display for FmtUserFacing<'_, U, C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ranges = self.error.ranges();
        let lines = range_lines(self.input);

        let mut hl_lines = Vec::new();
        for (nr, (lr, l)) in lines.iter().enumerate() {
            let intersecting: Vec<_> = ranges
                .iter()
                .filter(|r| r.intersects(lr))
                .map(|r| {
                    let ms = r.start.saturating_sub(lr.start);
                    let me = min(r.end.saturating_sub(lr.start), lr.len());
                    Range::of(ms, me)
                })
                .collect();

            if !intersecting.is_empty() {
                hl_lines.push((nr + 1, l, intersecting));
            }
        }

        let nr_width = hl_lines.last().map_or(2, |(nr, _, _)| {
            (*nr as f32).log10() as usize + 1
        });

        for (nr, l, ranges) in hl_lines {
            mark_ranges::<C>(f, nr, nr_width, l, &ranges)?;
        }

        write!(
            f,
            "{spc:nr_width$} {blue}│{esc} {col}{desc}{esc}",
            spc = ' ',
            desc = self.error,
            col = C::bold(),
            blue = LBlue::bold(),
            esc = ANSI_ESC,
        )?;
        Ok(())
    }
}

#[allow(clippy::mut_range_bound)]
fn mark_ranges<C: Color>(
    f: &mut fmt::Formatter<'_>,
    line_nr: usize,
    nr_width: usize,
    line: &str,
    ranges: &[Range],
) -> fmt::Result {
    write!(
        f,
        "{blue}{nr:nr_w$} │{esc} {ln}\n{spc:nr_w$} {blue}│{esc} ",
        nr = line_nr,
        nr_w = nr_width,
        ln = line,
        spc = ' ',
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
                let range = Range::of(line_start.0, pos.0 + 1);
                let line = &string[line_start.1..pos.1];
                lines.push((range, line));
                pushed_line = true;
            }
            '\n' => {
                if !pushed_line {
                    let range = Range::of(line_start.0, pos.0 + 1);
                    let line = &string[line_start.1..pos.1];
                    lines.push((range, line));
                }

                // We know this char is 1 byte wide
                line_start = (pos.0 + 1, pos.1 + 1);
                pushed_line = true;
            }
            _ => pushed_line = false,
        }

        pos.0 += 1;
        pos.1 = string.len() - chars.as_str().len();
    }

    if !pushed_line {
        let range = Range::of(line_start.0, pos.0 + 1);
        let line = &string[line_start.1..pos.1];
        lines.push((range, line));
    }

    lines
}
