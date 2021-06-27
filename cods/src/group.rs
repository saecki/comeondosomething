use std::ops;

use crate::{range, Cmd, Context, Mod, Num, Op, Par, Range, Sep, Token};

impl Context {
    pub fn group(&mut self, tokens: &[Token]) -> crate::Result<Vec<Item>> {
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
            } else if let Some(group_range) = self.matching_parenthesis(i, p, &mut par_stack)? {
                let prev_range = group_range.tokens_before(pos);
                let prev_tokens = tokens[prev_range].iter().filter_map(Item::try_from);
                items.extend(prev_tokens);

                items.push(Item::Group(Group {
                    items: self.group(&tokens[group_range.tokens()])?,
                    range: group_range.chars(tokens),
                }));
                pos = group_range.tokens_after(0).start;
            }
        }

        if let Some(&(i, p)) = par_stack.first() {
            self.errors.push(crate::Error::MissingClosingParenthesis(p));

            let prev_tokens = tokens[pos..i].iter().filter_map(Item::try_from);
            items.extend(prev_tokens);

            let range = range(p.range().end, tokens.last().unwrap().range().end);
            items.push(Item::Group(Group {
                items: self.group(&tokens[(i + 1)..])?,
                range,
            }));
        } else {
            let remaining_tokens = tokens[pos..].iter().filter_map(Item::try_from);
            items.extend(remaining_tokens);
        }

        Ok(items)
    }

    /// Returns the indexes of parenthesis enclosing the outermost group or tokens
    fn matching_parenthesis(
        &mut self,
        close_pos: usize,
        close_par: Par,
        par_stack: &mut Vec<(usize, Par)>,
    ) -> crate::Result<Option<GroupRange>> {
        match par_stack.pop() {
            Some((open_pos, open_par)) if open_par.matches(close_par) => {
                if par_stack.is_empty() {
                    Ok(Some(GroupRange {
                        start: open_pos + 1,
                        end: close_pos,
                        ..Default::default()
                    }))
                } else {
                    Ok(None)
                }
            }
            Some((open_pos, open_par)) => {
                self.warnings
                    .push(crate::Warning::MismatchedParenthesis(open_par, close_par));
                if par_stack.is_empty() {
                    Ok(Some(GroupRange {
                        start: open_pos + 1,
                        end: close_pos,
                        ..Default::default()
                    }))
                } else {
                    Ok(None)
                }
            }
            None => {
                self.errors
                    .push(crate::Error::UnexpectedParenthesis(close_par));
                Ok(Some(GroupRange {
                    missing_start_par: true,
                    start: 0,
                    missing_end_par: false,
                    end: close_pos,
                }))
            }
        }
    }
}

#[derive(Default)]
/// A range of token indices inside a group
struct GroupRange {
    missing_start_par: bool,
    /// including
    start: usize,
    missing_end_par: bool,
    /// excluding
    end: usize,
}

impl GroupRange {
    fn tokens_before(&self, start: usize) -> ops::Range<usize> {
        if self.missing_start_par {
            start..self.start.saturating_sub(2)
        } else {
            start..self.start.saturating_sub(1)
        }
    }

    fn tokens(&self) -> ops::Range<usize> {
        (self.start)..(self.end)
    }

    fn tokens_after(&self, end: usize) -> ops::Range<usize> {
        if self.missing_end_par {
            (self.end)..end
        } else {
            (self.end + 1)..end
        }
    }

    fn chars(&self, tokens: &[Token]) -> Range {
        let start = if self.missing_start_par {
            tokens[self.start].range().start
        } else {
            tokens[self.start - 1].range().end
        };

        let end = if self.missing_end_par {
            tokens[self.end - 1].range().end
        } else {
            tokens[self.end].range().start
        };

        range(start, end)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    Group(Group),
    Num(Num),
    Op(Op),
    Cmd(Cmd),
    Mod(Mod),
    Sep(Sep),
}

impl Item {
    pub const fn try_from(token: &Token) -> Option<Self> {
        match *token {
            Token::Num(n) => Some(Self::Num(n)),
            Token::Op(o) => Some(Self::Op(o)),
            Token::Cmd(c) => Some(Self::Cmd(c)),
            Token::Mod(m) => Some(Self::Mod(m)),
            Token::Par(_) => None,
            Token::Sep(s) => Some(Self::Sep(s)),
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

    pub const fn sep(&self) -> Option<Sep> {
        match self {
            Self::Sep(c) => Some(*c),
            _ => None,
        }
    }

    pub const fn is_sep(&self) -> bool {
        matches!(self, Self::Sep(_))
    }

    pub fn range(&self) -> Range {
        match self {
            Self::Group(g) => g.range,
            Self::Num(n) => n.range,
            Self::Op(o) => o.range(),
            Self::Cmd(c) => c.range(),
            Self::Mod(m) => m.range(),
            Self::Sep(s) => s.range(),
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
    use crate::{num, pos, Op, Val};

    use super::*;

    #[test]
    fn no_parenthesis() {
        let mut ctx = Context::default();
        let tokens = ctx.tokenize("423.42 * 64.52").unwrap();
        let items = ctx.group(&tokens).unwrap();

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
        let mut ctx = Context::default();
        let tokens = ctx.tokenize("(23.13 + 543.23) * 34").unwrap();
        let items = ctx.group(&tokens).unwrap();

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
