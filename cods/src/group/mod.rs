use std::iter::Peekable;
use std::vec::IntoIter;

use crate::{Context, Par, ParKind, Range, Token};

pub use item::*;

mod item;
#[cfg(test)]
mod test;

struct Lexer {
    tokens: Peekable<IntoIter<Token>>,
    pos: usize,
}

impl Lexer {
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
        let mut lexer = Lexer::new(tokens);
        self.group_tokens(&mut lexer, None)
    }

    fn group_tokens(
        &mut self,
        lexer: &mut Lexer,
        current_par: Option<Par>,
    ) -> crate::Result<Vec<Item>> {
        let mut items = Vec::new();

        while lexer.peek().is_some() {
            if let Some(Token::Par(r_par)) = lexer.peek() {
                if r_par.is_closing() {
                    match current_par {
                        Some(l_par) => {
                            if !l_par.matches(r_par.typ) {
                                self.warnings
                                    .push(crate::Warning::MismatchedParentheses(l_par, *r_par));
                            }
                            break;
                        }
                        None => {
                            self.errors.push(crate::Error::UnexpectedPar(*r_par));
                            lexer.next();
                            continue;
                        }
                    }
                }
            }

            let i = match lexer.next() {
                Some(Token::Par(l_par)) => {
                    let inner = self.group_tokens(lexer, Some(l_par))?;
                    match lexer.peek() {
                        Some(Token::Par(r_par)) => {
                            let kind = ParKind::of(l_par.typ, r_par.typ);
                            let r = Range::span(l_par.range, r_par.range);
                            let g = Group::new(inner, r, kind);

                            lexer.next();

                            Item::Group(g)
                        }
                        _ => {
                            self.errors.push(crate::Error::MissingClosingPar(l_par));

                            if let Some(i) = inner.last() {
                                let kind = ParKind::Mixed;
                                let r = Range::span(l_par.range, i.range());
                                let g = Group::new(inner, r, kind);
                                Item::Group(g)
                            } else {
                                continue;
                            }
                        }
                    }
                }
                Some(Token::Expr(e)) => Item::Expr(e),
                Some(Token::Fun(f)) => Item::Fun(f),
                Some(Token::Op(o)) => Item::Op(o),
                Some(Token::Sep(s)) => Item::Sep(s),
                None => break,
            };

            items.push(i);
        }

        Ok(items)
    }
}
