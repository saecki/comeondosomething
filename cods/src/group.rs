use std::ops;

use crate::{Cmd, Context, Mod, Num, Op, Par, ParType, Range, Sep, Token, Var};

impl<T: Var> Context<T> {
    pub fn group(&mut self, tokens: &[Token<T>]) -> crate::Result<Vec<Item<T>>, T> {
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
            } else if let Some(group_range) = self.matching_parentheses(i, p, &mut par_stack) {
                let prev_range = group_range.tokens_before(pos);
                let prev_tokens = tokens[prev_range].iter().filter_map(Item::try_from);
                items.extend(prev_tokens);

                items.push(Item::Group(Group {
                    items: self.group(&tokens[group_range.tokens()])?,
                    range: group_range.chars(tokens),
                    par_type: group_range.par_type,
                }));
                pos = group_range.tokens_after(0).start;
            }
        }

        if let Some(&(i, p)) = par_stack.first() {
            self.errors.push(crate::Error::MissingClosingParenthesis(p));

            let prev_tokens = tokens[pos..i].iter().filter_map(Item::try_from);
            items.extend(prev_tokens);

            let range = Range::of(p.range().end, tokens.last().unwrap().range().end);
            items.push(Item::Group(Group {
                items: self.group(&tokens[(i + 1)..])?,
                range,
                par_type: p.par_type(),
            }));
        } else {
            let remaining_tokens = tokens[pos..].iter().filter_map(Item::try_from);
            items.extend(remaining_tokens);
        }

        Ok(items)
    }

    /// Returns the indices of parentheses enclosing the outermost group or tokens
    fn matching_parentheses(
        &mut self,
        close_pos: usize,
        close_par: Par,
        par_stack: &mut Vec<(usize, Par)>,
    ) -> Option<GroupRange> {
        match par_stack.pop() {
            Some((open_pos, open_par)) if open_par.matches(close_par) => {
                if par_stack.is_empty() {
                    Some(GroupRange {
                        start: open_pos + 1,
                        missing_start_par: false,
                        end: close_pos,
                        missing_end_par: false,
                        par_type: open_par.par_type(),
                    })
                } else {
                    None
                }
            }
            Some((open_pos, open_par)) => {
                self.warnings
                    .push(crate::Warning::MismatchedParentheses(open_par, close_par));
                if par_stack.is_empty() {
                    Some(GroupRange {
                        start: open_pos + 1,
                        missing_start_par: false,
                        end: close_pos,
                        missing_end_par: false,
                        par_type: ParType::Mixed,
                    })
                } else {
                    None
                }
            }
            None => {
                self.errors
                    .push(crate::Error::UnexpectedParenthesis(close_par));
                None
            }
        }
    }
}

/// A range of token indices inside a group
struct GroupRange {
    missing_start_par: bool,
    /// including
    start: usize,
    missing_end_par: bool,
    /// excluding
    end: usize,
    par_type: ParType,
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

    fn chars(&self, tokens: &[Token<impl Var>]) -> Range {
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

        Range::of(start, end)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Item<T: Var> {
    Group(Group<T>),
    Num(Num<T>),
    Op(Op),
    Cmd(Cmd),
    Mod(Mod),
    Sep(Sep),
}

impl<T: Var> Item<T> {
    pub fn try_from(token: &Token<T>) -> Option<Self> {
        match *token {
            Token::Num(n) => Some(Self::Num(n)),
            Token::Op(o) => Some(Self::Op(o)),
            Token::Cmd(c) => Some(Self::Cmd(c)),
            Token::Mod(m) => Some(Self::Mod(m)),
            Token::Par(_) => None,
            Token::Sep(s) => Some(Self::Sep(s)),
        }
    }

    pub fn op(&self) -> Option<Op> {
        match self {
            Self::Op(o) => Some(*o),
            _ => None,
        }
    }

    pub fn modifier(&self) -> Option<Mod> {
        match self {
            Self::Mod(m) => Some(*m),
            _ => None,
        }
    }

    pub fn cmd(&self) -> Option<Cmd> {
        match self {
            Self::Cmd(c) => Some(*c),
            _ => None,
        }
    }

    pub fn sep(&self) -> Option<Sep> {
        match self {
            Self::Sep(c) => Some(*c),
            _ => None,
        }
    }

    pub fn is_op(&self) -> bool {
        matches!(self, Self::Op(_))
    }

    pub fn is_sep(&self) -> bool {
        matches!(self, Self::Sep(_))
    }

    pub fn range(&self) -> Range {
        match self {
            Self::Group(g) => g.range,
            Self::Num(n) => n.range,
            Self::Op(o) => o.range,
            Self::Cmd(c) => c.range(),
            Self::Mod(m) => m.range(),
            Self::Sep(s) => s.range(),
        }
    }
}

pub fn items_range(items: &[Item<impl Var>]) -> Option<Range> {
    let first = items.first().map(|i| i.range());
    let last = items.last().map(|i| i.range());

    match (first, last) {
        (Some(f), Some(l)) => Some(Range::of(f.start, l.end)),
        _ => None,
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Group<T: Var> {
    pub items: Vec<Item<T>>,
    pub range: Range,
    pub par_type: ParType,
}

impl<T: Var> Group<T> {
    pub fn new(items: Vec<Item<T>>, range: Range, par: ParType) -> Self {
        Self {
            items,
            range,
            par_type: par,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{DummyVar, Op, OpType, Range, Val};

    use super::*;

    #[test]
    fn no_parenthesis() {
        let mut ctx = Context::<DummyVar>::default();
        let tokens = ctx.tokenize("423.42 * 64.52").unwrap();
        let items = ctx.group(&tokens).unwrap();

        assert_eq!(
            items,
            vec![
                Item::Num(Num::new(Val::Float(423.42), Range::of(0, 6))),
                Item::Op(Op::new(OpType::Mul, Range::pos(7))),
                Item::Num(Num::new(Val::Float(64.52), Range::of(9, 14))),
            ]
        );
    }

    #[test]
    fn add_parenthesis() {
        let mut ctx = Context::<DummyVar>::default();
        let tokens = ctx.tokenize("(23.13 + 543.23) * 34").unwrap();
        let items = ctx.group(&tokens).unwrap();

        assert_eq!(
            items,
            vec![
                Item::Group(Group::new(
                    vec![
                        Item::Num(Num::new(Val::Float(23.13), Range::of(1, 6))),
                        Item::Op(Op::new(OpType::Add, Range::pos(7))),
                        Item::Num(Num::new(Val::Float(543.23), Range::of(9, 15)))
                    ],
                    Range::of(1, 15),
                    ParType::Round,
                )),
                Item::Op(Op::new(OpType::Mul, Range::pos(17))),
                Item::Num(Num::new(Val::Int(34), Range::of(19, 21))),
            ]
        );
    }
}
