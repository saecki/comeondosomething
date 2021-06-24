use std::cmp::Ordering;

use crate::{Item, Op};

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

pub fn parse(items: &[Item]) -> crate::Result<Calc> {
    parse_int(0, items)
}

fn parse_int(pos: usize, items: &[Item]) -> crate::Result<Calc> {
    if items.is_empty() {
        return Err(crate::Error::MissingOperand(pos));
    } else if items.len() == 1 {
        match &items[0] {
            Item::Group(g) => {
                return parse_int(pos + 1, g); // TODO proper position
            }
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

    if let Some(&(i, t)) = ops.last() {
        let a = Box::new(parse_int(pos, &items[0..i])?);
        let b = Box::new(parse_int(t.pos() + 1, &items[(i + 1)..(items.len())])?);

        return match t {
            Op::Add(_) => Ok(Calc::Add(a, b)),
            Op::Sub(_) => Ok(Calc::Sub(a, b)),
            Op::Mul(_) => Ok(Calc::Mul(a, b)),
            Op::Div(_) => Ok(Calc::Div(a, b)),
        };
    }

    Err(crate::Error::MissingOperator(pos)) // TODO proper position
}
