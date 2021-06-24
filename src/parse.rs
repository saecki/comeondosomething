use std::cmp::Ordering;

use crate::{items_range, pos, range, Item, Op, Range};

pub fn parse(items: &[Item]) -> crate::Result<Calc> {
    let r = items_range(items).unwrap_or_else(|| pos(0));
    _parse(r, items)
}

fn _parse(r: Range, items: &[Item]) -> crate::Result<Calc> {
    if items.is_empty() {
        return Err(crate::Error::MissingOperand(r));
    } else if items.len() == 1 {
        match &items[0] {
            Item::Group { items, range } => return _parse(*range, items),
            &Item::Num(n) => return Ok(Calc::Num(n.val)),
            &Item::Op(o) => return Err(crate::Error::UnexpectedOperator(o)),
        }
    }

    let mut ops: Vec<_> = items
        .iter()
        .enumerate()
        .filter_map(|(i, t)| t.op().map(|o| (i, o)))
        .collect();

    ops.sort_by(|(i1, t1), (i2, t2)| {
        if t1.priority() < t2.priority() {
            Ordering::Less
        } else if t1.priority() > t2.priority() {
            Ordering::Greater
        } else if i1 < i2 {
            Ordering::Less
        } else {
            Ordering::Greater
        }
    });

    if let Some(&(i, o)) = ops.last() {
        let a = &items[0..i];
        let ra = items_range(a).unwrap_or_else(|| range(r.start, o.range().start));
        let ca = Box::new(_parse(ra, a)?);

        let b = &items[(i + 1)..];
        let rb = items_range(b).unwrap_or_else(|| range(o.range().end, r.end));
        let cb = Box::new(_parse(rb, b)?);

        return match o {
            Op::Add(_) => Ok(Calc::Add(ca, cb)),
            Op::Sub(_) => Ok(Calc::Sub(ca, cb)),
            Op::Mul(_) => Ok(Calc::Mul(ca, cb)),
            Op::Div(_) => Ok(Calc::Div(ca, cb)),
        };
    }

    Err(crate::Error::MissingOperator(range(
        items[0].range().end,
        items[1].range().start,
    )))
}

#[derive(Clone, Debug, PartialEq)]
pub enum Calc {
    Num(f64),
    Add(Box<Calc>, Box<Calc>),
    Sub(Box<Calc>, Box<Calc>),
    Mul(Box<Calc>, Box<Calc>),
    Div(Box<Calc>, Box<Calc>),
}

impl Calc {
    pub fn calc(&self) -> f64 {
        match self {
            Calc::Num(n) => *n,
            Calc::Add(a, b) => a.calc() + b.calc(),
            Calc::Sub(a, b) => a.calc() - b.calc(),
            Calc::Mul(a, b) => a.calc() * b.calc(),
            Calc::Div(a, b) => a.calc() / b.calc(),
        }
    }
}
