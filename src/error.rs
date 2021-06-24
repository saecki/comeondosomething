use crate::{Op, Par};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    MissingOperand(usize),
    MissingOperator(usize),
    MissingClosingParenthesis(Par),
    UnexpectedOperator(Op),
    UnexpectedParenthesis(Par),
    MismatchedParenthesis { opening: Par, found: Par },
    InvalidCharacter { char: char, pos: usize },
    NumberFormatException { start: usize, end: usize },
}

impl Error {
    pub fn show(&self, _string: &str) -> String {
        let mut out = String::new();
        match self {
            &Self::MissingOperand(p) => mark_pos(&mut out, p),
            &Self::MissingOperator(p) => mark_pos(&mut out, p),
            &Self::MissingClosingParenthesis(p) => mark_pos(&mut out, p.pos()),
            &Self::UnexpectedOperator(o) => mark_pos(&mut out, o.pos()),
            &Self::UnexpectedParenthesis(p) => mark_pos(&mut out, p.pos()),
            &Self::MismatchedParenthesis { opening, found } => {
                mark_pos(&mut out, opening.pos());
                mark_pos(&mut out, found.pos() - opening.pos() - 1);
            }
            &Self::InvalidCharacter { char: _, pos } => mark_pos(&mut out, pos),
            &Self::NumberFormatException { start, end } => mark_range(&mut out, start, end),
        }
        out.push('\n');
        out.push_str(self.description());

        out
    }

    fn description(&self) -> &'static str {
        match self {
            &Self::MissingOperand(_) => "Missing an operand",
            &Self::MissingOperator(_) => "Missing an operator",
            &Self::MissingClosingParenthesis(_) => "Missing a closing parenthesis",
            &Self::UnexpectedOperator(_) => "Found an unexpected operator",
            &Self::UnexpectedParenthesis(_) => "Found an unexpected parenthesis",
            &Self::MismatchedParenthesis { .. } => "Parenthesis do not match",
            &Self::InvalidCharacter { .. } => "Found an invalid character",
            &Self::NumberFormatException { .. } => "Found an invalid number literal",
        }
    }
}

fn mark_pos(out: &mut String, pos: usize) {
    mark_range(out, pos, pos + 1);
}

fn mark_range(out: &mut String, start: usize, end: usize) {
    out.extend((0..start).map(|_| ' '));
    out.extend((start..end).map(|_| '^'));
}
