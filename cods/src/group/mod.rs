use std::ops;

use crate::{Context, Expr, Fun, Op, Par, ParKind, Range, Sep, Token};

#[cfg(test)]
mod test;

impl Context {
    pub fn group(&mut self, tokens: &[Token]) -> crate::Result<Vec<Item>> {
        let mut items = Vec::new();
        let mut pos = 0;

        let mut par_stack = Vec::new();
        let pars = tokens
            .iter()
            .enumerate()
            .filter_map(|(i, t)| t.as_par().map(|o| (i, o)));

        for (i, p) in pars {
            if p.is_opening() {
                par_stack.push((i, p));
            } else if let Some(group_range) = self.matching_parentheses(i, p, &mut par_stack) {
                let prev_range = group_range.tokens_before(pos);
                let prev_tokens = tokens[prev_range].iter().filter_map(Item::try_from);
                items.extend(prev_tokens);

                items.push(Item::Group(Group {
                    items: self.group(&tokens[group_range.tokens()])?,
                    range: group_range.char_range(tokens),
                    par_kind: group_range.par_type,
                }));
                pos = group_range.tokens_after(0).start;
            }
        }

        if let Some(&(i, p)) = par_stack.first() {
            self.errors.push(crate::Error::MissingClosingParenthesis(p));

            let prev_tokens = tokens[pos..i].iter().filter_map(Item::try_from);
            items.extend(prev_tokens);

            let range = Range::of(p.range.end, tokens.last().unwrap().range().end);
            items.push(Item::Group(Group {
                items: self.group(&tokens[(i + 1)..])?,
                range,
                par_kind: p.kind(),
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
            Some((open_pos, open_par)) if open_par.matches(close_par.typ) => {
                if par_stack.is_empty() {
                    Some(GroupRange {
                        start: open_pos + 1,
                        end: close_pos,
                        missing_end_par: false,
                        par_type: open_par.kind(),
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
                        end: close_pos,
                        missing_end_par: false,
                        par_type: ParKind::Mixed,
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
    /// including
    start: usize,
    missing_end_par: bool,
    /// excluding
    end: usize,
    par_type: ParKind,
}

impl GroupRange {
    fn tokens_before(&self, start: usize) -> ops::Range<usize> {
        start..self.start.saturating_sub(1)
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

    fn char_range(&self, tokens: &[Token]) -> Range {
        let start = tokens[self.start - 1].range().start;

        let end = if self.missing_end_par {
            tokens[self.end - 1].range().end
        } else {
            tokens[self.end].range().end
        };

        Range::of(start, end)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    Group(Group),
    Expr(Expr),
    Fun(Fun),

    Op(Op),
    Sep(Sep),
}

impl Item {
    pub fn try_from(token: &Token) -> Option<Self> {
        match *token {
            Token::Expr(n) => Some(Self::Expr(n)),
            Token::Op(o) => Some(Self::Op(o)),
            Token::Fun(c) => Some(Self::Fun(c)),
            Token::Par(_) => None,
            Token::Sep(s) => Some(Self::Sep(s)),
        }
    }

    pub fn as_op(&self) -> Option<Op> {
        match self {
            Self::Op(o) => Some(*o),
            _ => None,
        }
    }

    pub fn as_fun(&self) -> Option<Fun> {
        match self {
            Self::Fun(c) => Some(*c),
            _ => None,
        }
    }

    pub fn as_sep(&self) -> Option<Sep> {
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

    pub fn is_semi(&self) -> bool {
        match self {
            Self::Sep(s) => s.is_semi(),
            _ => false,
        }
    }

    pub fn is_newln(&self) -> bool {
        match self {
            Self::Sep(s) => s.is_newln(),
            _ => false,
        }
    }

    pub fn range(&self) -> Range {
        match self {
            Self::Group(g) => g.range,
            Self::Expr(n) => n.range,
            Self::Op(o) => o.range,
            Self::Fun(c) => c.range,
            Self::Sep(s) => s.range,
        }
    }
}

pub fn items_range(items: &[Item]) -> Option<Range> {
    let first = items.first().map(|i| i.range());
    let last = items.last().map(|i| i.range());

    match (first, last) {
        (Some(f), Some(l)) => Some(Range::of(f.start, l.end)),
        _ => None,
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Group {
    pub items: Vec<Item>,
    pub range: Range,
    pub par_kind: ParKind,
}

impl Group {
    pub fn new(items: Vec<Item>, range: Range, par: ParKind) -> Self {
        Self {
            items,
            range,
            par_kind: par,
        }
    }
}
