use std::fmt;

use cods::{Error, Pos, Span, UserFacing, Warning};

pub fn write_diagnostics(
    f: &mut impl fmt::Write,
    errors: &[Error],
    warnigns: &[Warning],
) -> fmt::Result {
    write!(f, "{{\"errors\":[")?;
    if let Some((first, others)) = errors.split_first() {
        write_userfacing(f, first)?;
        for s in others {
            f.write_char(',')?;
            write_userfacing(f, s)?;
        }
    }
    write!(f, "],\"warnings\":[")?;
    if let Some((first, others)) = warnigns.split_first() {
        write_userfacing(f, first)?;
        for s in others {
            f.write_char(',')?;
            write_userfacing(f, s)?;
        }
    }
    write!(f, "]}}")
}

fn write_userfacing(f: &mut impl fmt::Write, w: &impl UserFacing) -> fmt::Result {
    write!(f, "{{\"spans\":[")?;
    let spans = w.spans();
    if let Some((first, others)) = spans.split_first() {
        write_span(f, first)?;
        for s in others {
            f.write_char(',')?;
            write_span(f, s)?;
        }
    }
    write!(f, "],\"desc\":\"")?;
    w.description(f, "", "")?;
    write!(f, "\"}}")
}

fn write_span(f: &mut impl fmt::Write, s: &Span) -> fmt::Result {
    write!(f, "{{\"start\":")?;
    write_pos(f, s.start)?;
    write!(f, ",\"end\":")?;
    write_pos(f, s.end)?;
    write!(f, "}}")
}

fn write_pos(f: &mut impl fmt::Write, p: Pos) -> fmt::Result {
    write!(f, "{{\"col\":{},\"line\":{}}}", p.col, p.line)
}
