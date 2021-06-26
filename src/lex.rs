use crate::{range, Cmd, Mod, Num, Op, Par, Range, Token};

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
            items.push(Item::Group(group(
                lex(&tokens[(start + 1)..end])?,
                tokens[start].range().end,
                tokens[end].range().start,
            )));
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

/// Returns the indexes of parenthesis enclosing the outermost group or tokens
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
        Some((_, open_par)) => Err(crate::Error::MismatchedParenthesis(open_par, close_par)),
        _ => Err(crate::Error::UnexpectedParenthesis(close_par)),
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    Group(Group),
    Num(Num),
    Op(Op),
    Cmd(Cmd),
    Mod(Mod),
}

impl Item {
    pub const fn try_from(token: &Token) -> Option<Self> {
        match *token {
            Token::Num(n) => Some(Self::Num(n)),
            Token::Op(o) => Some(Self::Op(o)),
            Token::Cmd(c) => Some(Self::Cmd(c)),
            Token::Mod(m) => Some(Self::Mod(m)),
            Token::Par(_) => None,
        }
    }

    pub const fn op(&self) -> Option<Op> {
        match self {
            Self::Op(o) => Some(*o),
            _ => None,
        }
    }

    pub const fn modifier(&self) -> Option<Mod> {
        match self {
            Self::Mod(m) => Some(*m),
            _ => None,
        }
    }

    pub const fn cmd(&self) -> Option<Cmd> {
        match self {
            Self::Cmd(c) => Some(*c),
            _ => None,
        }
    }

    pub fn range(&self) -> Range {
        match self {
            Self::Group(g) => g.range,
            Self::Num(n) => n.range,
            Self::Op(o) => o.range(),
            Self::Cmd(c) => c.range(),
            Self::Mod(m) => m.range(),
        }
    }
}

pub fn items_range(items: &[Item]) -> Option<Range> {
    let first = items.first().map(|i| i.range());
    let last = items.last().map(|i| i.range());

    match (first, last) {
        (Some(f), Some(l)) => Some(range(f.start, l.end)),
        _ => None,
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Group {
    pub items: Vec<Item>,
    pub range: Range,
}

pub const fn group(items: Vec<Item>, start: usize, end: usize) -> Group {
    Group {
        items,
        range: range(start, end),
    }
}

#[cfg(test)]
mod test {
    use crate::{num, pos, tokenize, Op, Val};

    use super::*;

    #[test]
    fn no_parenthesis() {
        let tokens = tokenize("423.42 * 64.52").unwrap();
        let items = lex(&tokens).unwrap();

        assert_eq!(
            items,
            vec![
                Item::Num(num(Val::Float(423.42), 0, 6)),
                Item::Op(Op::Mul(pos(7))),
                Item::Num(num(Val::Float(64.52), 9, 14)),
            ]
        );
    }

    #[test]
    fn add_parenthesis() {
        let tokens = tokenize("(23.13 + 543.23) * 34").unwrap();
        let items = lex(&tokens).unwrap();

        assert_eq!(
            items,
            vec![
                Item::Group(group(
                    vec![
                        Item::Num(num(Val::Float(23.13), 1, 6)),
                        Item::Op(Op::Add(pos(7))),
                        Item::Num(num(Val::Float(543.23), 9, 15))
                    ],
                    1,
                    15,
                )),
                Item::Op(Op::Mul(pos(17))),
                Item::Num(num(Val::Int(34), 19, 21)),
            ]
        );
    }
}
