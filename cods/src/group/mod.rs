use crate::{Context, Par, Token, TokenStream};

pub use item::*;

mod item;
#[cfg(test)]
mod test;

impl Context {
    pub fn group(&mut self, mut tokens: TokenStream<'_>) -> crate::Result<Vec<Item>> {
        let mut stack = Vec::new();
        self.group_tokens(&mut tokens, &mut stack)
    }

    fn group_tokens(
        &mut self,
        tokens: &mut TokenStream,
        stack: &mut Vec<Par>,
    ) -> crate::Result<Vec<Item>> {
        let mut items = Vec::new();

        while let Some(t) = self.peek_token(tokens)? {
            if let Token::Par(right_par) = t {
                if right_par.is_closing() {
                    // Only check for a match in the first 3 parenthesis on the stack
                    let matching_par = stack
                        .iter()
                        .rev()
                        .take(3)
                        .enumerate()
                        .find(|(_, p)| right_par.matches(p.typ));

                    match matching_par {
                        Some((i, _)) => {
                            for _ in 0..(i + 1) {
                                stack.pop();
                            }
                            break;
                        }
                        None => {
                            self.errors.push(crate::Error::UnexpectedPar(*right_par));
                            self.next_token(tokens)?;
                            continue;
                        }
                    }
                }
            }

            let i = match self.next_token(tokens)? {
                Some(Token::Par(left_par)) => {
                    stack.push(left_par);
                    let inner = self.group_tokens(tokens, stack)?;

                    match self.peek_token(tokens)? {
                        Some(&Token::Par(right_par)) if left_par.matches(right_par.typ) => {
                            self.next_token(tokens)?;
                            Item::Group(Group::new(left_par, right_par, inner))
                        }
                        _ => {
                            self.errors.push(crate::Error::MissingClosingPar(left_par));
                            items.extend(inner);
                            break;
                        }
                    }
                }
                Some(Token::Val(v)) => Item::Val(v),
                Some(Token::Ident(i)) => Item::Ident(i),
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
