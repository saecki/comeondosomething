use std::cmp::Ordering;

use crate::{items_range, pos, range, Calc, Cmd, Item, Mod, Op, Range};

pub fn parse(items: &[Item]) -> crate::Result<Calc> {
    let r = items_range(items).unwrap_or_else(|| pos(0));
    _parse(r, items)
}

fn _parse(r: Range, items: &[Item]) -> crate::Result<Calc> {
    if items.is_empty() {
        return Err(crate::Error::MissingOperand(r));
    } else if items.len() == 1 {
        return match &items[0] {
            Item::Group(g) => _parse(g.range, &g.items),
            Item::Num(n) => Ok(Calc::Num(*n)),
            Item::Op(o) => Err(crate::Error::UnexpectedOperator(*o)),
            Item::Cmd(c) => Err(crate::Error::MissingOperand(pos(c.range().end))),
            Item::Mod(m) => Err(crate::Error::MissingOperand(pos(m.range().start))),
        };
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
            Op::Pow(_) => Ok(Calc::Pow(ca, cb)),
        };
    }

    let modifier = items
        .iter()
        .enumerate()
        .filter_map(|(i, t)| t.modifier().map(|o| (i, o)))
        .next();

    if let Some((i, m)) = modifier {
        let a = &items[0..i];
        let ar = items_range(a).unwrap_or_else(|| range(r.start, m.range().start));
        let ac = Box::new(_parse(ar, a)?);

        if let Some(i) = items.get(i + 1) {
            let range = range(m.range().end, i.range().start);
            return Err(crate::Error::MissingOperator(range));
        }

        return Ok(match m {
            Mod::Degree(r) => Calc::Degree(ac, r),
            Mod::Factorial(r) => Calc::Factorial(ac, r),
        });
    }

    if items.len() == 2 {
        if let Item::Cmd(cmd) = items[0] {
            return match &items[1] {
                Item::Group(g) => {
                    let inner = Box::new(_parse(g.range, &g.items)?);
                    Ok(match cmd {
                        Cmd::Sqrt(r) => Calc::Sqrt(inner, r),
                        Cmd::Sin(r) => Calc::Sin(inner, r),
                        Cmd::Cos(r) => Calc::Cos(inner, r),
                        Cmd::Tan(r) => Calc::Tan(inner, r),
                    })
                }
                i => {
                    let range = range(cmd.range().end, i.range().start);
                    Err(crate::Error::MissingCommandParenthesis(range))
                }
            };
        }
    }

    Err(crate::Error::MissingOperator(range(
        items[0].range().end,
        items[1].range().start,
    )))
}
