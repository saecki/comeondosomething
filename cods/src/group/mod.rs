use std::iter::Peekable;
use std::vec::IntoIter;

use crate::{CRange, Context, Par, Token};

pub use item::*;

mod item;
#[cfg(test)]
mod test;

struct State {
    tokens: Peekable<IntoIter<Token>>,
    pos: usize,
}

impl State {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
            pos: 0,
        }
    }

    fn next(&mut self) -> Option<Token> {
        self.pos += 1;
        self.tokens.next()
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }
}

impl Context {
    pub fn group(&mut self, tokens: Vec<Token>) -> crate::Result<Vec<Item>> {
        let mut state = State::new(tokens);
        let mut stack = Vec::new();
        self.group_tokens(&mut state, &mut stack)
    }

    fn group_tokens(
        &mut self,
        state: &mut State,
        stack: &mut Vec<Par>,
    ) -> crate::Result<Vec<Item>> {
        let mut items = Vec::new();

        while state.peek().is_some() {
            if let Some(Token::Par(r_par)) = state.peek() {
                if r_par.is_closing() {
                    // Only check for a match in the first 3 parenthesis on the stack
                    let matching_par = stack.iter().rev().take(3).find(|p| r_par.matches(p.typ));

                    match matching_par {
                        Some(_) => break,
                        None => {
                            self.errors.push(crate::Error::UnexpectedPar(*r_par));
                            state.next();
                            continue;
                        }
                    }
                }
            }

            let i = match state.next() {
                Some(Token::Par(l_par)) => {
                    stack.push(l_par);
                    let inner = self.group_tokens(state, stack)?;

                    match state.peek() {
                        Some(Token::Par(r_par)) if l_par.matches(r_par.typ) => {
                            let kind = l_par.kind();
                            let r = CRange::span(l_par.range, r_par.range);
                            let g = Group::new(inner, r, kind);
                            state.next();
                            Item::Group(g)
                        }
                        _ => {
                            self.errors.push(crate::Error::MissingClosingPar(l_par));
                            items.extend(inner);
                            break;
                        }
                    }
                }
                Some(Token::Expr(e)) => Item::Expr(e),
                Some(Token::Op(o)) => Item::Op(o),
                Some(Token::Pct(s)) => Item::Pct(s),
                Some(Token::Kw(k)) => Item::Kw(k),
                None => break,
            };

            items.push(i);
        }

        Ok(items)
    }
}
