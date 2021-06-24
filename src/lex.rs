use crate::{range, Num, Op, Par, Range, Token};

#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    Group(Vec<Item>),
    Op(Op),
    Num(Num),
}

impl Item {
    pub const fn try_from(token: &Token) -> Option<Self> {
        match *token {
            Token::Op(o) => Some(Self::Op(o)),
            Token::Num(n) => Some(Self::Num(n)),
            Token::Par(_) => None,
        }
    }

    pub const fn op(&self) -> Option<Op> {
        match self {
            Self::Op(o) => Some(*o),
            _ => None,
        }
    }

    pub fn range(&self) -> Option<Range> {
        match self {
            Self::Group(g) => items_range(g),
            Self::Num(n) => Some(n.range),
            Self::Op(o) => Some(o.range()),
        }
    }
}

pub fn items_range(items: &[Item]) -> Option<Range> {
    let first = items.first().and_then(|i| i.range());
    let last = items.last().and_then(|i| i.range());

    match (first, last) {
        (Some(f), Some(l)) => Some(range(f.start, l.end)),
        _ => None,
    }
}

pub fn lex(tokens: &[Token]) -> crate::Result<Vec<Item>> {
    let mut items = Vec::new();
    let mut pos = 0;

    let mut par_stack = Vec::new();
    let pars = tokens
        .iter()
        .enumerate()
        .filter_map(|(i, t)| t.par().map(|o| (i, o)));

    for (i, p) in pars {
        if p.is_opening() {
            par_stack.push((i, p));
        } else if let Some((start, end)) = matching_parenthesis(i, p, &mut par_stack)? {
            let prev_tokens = tokens[pos..start].iter().filter_map(Item::try_from);
            items.extend(prev_tokens);
            items.push(Item::Group(lex(&tokens[(start + 1)..end])?));
            pos = end + 1;
        }
    }

    if let Some(&(_, p)) = par_stack.last() {
        return Err(crate::Error::MissingClosingParenthesis(p));
    }
    let remaining_tokens = tokens[pos..].iter().filter_map(Item::try_from);
    items.extend(remaining_tokens);

    Ok(items)
}

/// Returns a range enclosed by parenthesis
fn matching_parenthesis(
    close_pos: usize,
    close_par: Par,
    par_stack: &mut Vec<(usize, Par)>,
) -> crate::Result<Option<(usize, usize)>> {
    match par_stack.pop() {
        Some((i1, p1)) if p1.matches(close_par) => {
            if par_stack.is_empty() {
                Ok(Some((i1, close_pos)))
            } else {
                Ok(None)
            }
        }
        Some((_, open_par)) => Err(crate::Error::MismatchedParenthesis {
            opening: open_par,
            found: close_par,
        }),
        _ => Err(crate::Error::UnexpectedParenthesis(close_par)),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{num, pos, tokenize, Op};

    #[test]
    fn no_parenthesis() {
        let tokens = tokenize("423.42 * 64.52").unwrap();
        let items = lex(&tokens).unwrap();

        assert_eq!(
            items,
            vec![
                Item::Num(num(423.42, 0, 6)),
                Item::Op(Op::Mul(pos(7))),
                Item::Num(num(64.52, 9, 14)),
            ]
        );
    }

    #[test]
    fn add_parenthesis() {
        let tokens = tokenize("(23.13 + 543.23) * 34.2").unwrap();
        let items = lex(&tokens).unwrap();

        assert_eq!(
            items,
            vec![
                Item::Group(vec![
                    Item::Num(num(23.13, 1, 6)),
                    Item::Op(Op::Add(pos(7))),
                    Item::Num(num(543.23, 9, 15))
                ]),
                Item::Op(Op::Mul(pos(17))),
                Item::Num(num(34.2, 19, 23)),
            ]
        );
    }
}
