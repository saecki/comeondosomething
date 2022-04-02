use std::collections::VecDeque;

use crate::{Group, IdentSpan, Item, KwT, OpT, PctT, Span};

pub struct Parser {
    items: VecDeque<Item>,
    pub current_newln: bool,
    pub last_newln: bool,
}

impl Parser {
    pub fn new(items: Vec<Item>) -> Self {
        Self {
            items: VecDeque::from(items),
            current_newln: false,
            last_newln: false,
        }
    }

    fn next_item(&mut self, item: &Item) -> bool {
        self.last_newln = self.current_newln;
        self.current_newln = item.is_newln();
        !self.current_newln
    }

    pub fn next(&mut self) -> Option<Item> {
        while let Some(i) = self.items.pop_front() {
            if self.next_item(&i) {
                return Some(i);
            }
        }
        None
    }

    fn skip_one(&mut self) {
        if let Some(i) = self.items.pop_front() {
            self.next_item(&i);
        }
    }

    pub fn eat_newlns(&mut self) {
        while let Some(i) = self.items.get(0) {
            if i.is_newln() {
                self.skip_one();
            } else {
                break;
            }
        }
    }

    pub fn peek(&mut self) -> Option<&Item> {
        self.eat_newlns();
        self.items.get(0)
    }

    pub fn eat_semi(&mut self) -> bool {
        if let Some(i) = self.peek() {
            if i.is_semi() {
                self.skip_one();
                return true;
            }
        }
        false
    }

    pub fn expect_block(&mut self, pos: usize) -> crate::Result<Group> {
        match self.next() {
            Some(Item::Group(g)) if g.par_kind.is_curly() => Ok(g),
            Some(i) => Err(crate::Error::ExpectedBlock(i.span())),
            None => Err(crate::Error::ExpectedBlock(Span::pos(pos))),
        }
    }

    pub fn expect_fun_pars(&mut self, pos: usize) -> crate::Result<Group> {
        match self.next() {
            Some(Item::Group(g)) if g.par_kind.is_round() => Ok(g),
            Some(i) => Err(crate::Error::ExpectedFunPars(i.span())),
            None => Err(crate::Error::ExpectedFunPars(Span::pos(pos))),
        }
    }

    pub fn expect_ident(&mut self, pos: usize) -> crate::Result<IdentSpan> {
        match self.next() {
            Some(Item::Ident(id)) => Ok(id),
            Some(i) => Err(crate::Error::ExpectedIdent(i.span())),
            None => Err(crate::Error::ExpectedIdent(Span::pos(pos))),
        }
    }

    pub fn expect_op(&mut self, op: OpT, pos: usize) -> crate::Result<Span> {
        match self.next() {
            Some(Item::Op(o)) if o.typ == op => Ok(o.span),
            Some(i) => Err(crate::Error::ExpectedOp(op, i.span())),
            None => Err(crate::Error::ExpectedOp(op, Span::pos(pos))),
        }
    }

    pub fn expect_kw(&mut self, kw: KwT, pos: usize) -> crate::Result<Span> {
        match self.next() {
            Some(Item::Kw(k)) if k.typ == kw => Ok(k.span),
            Some(i) => Err(crate::Error::ExpectedKw(kw, i.span())),
            None => Err(crate::Error::ExpectedKw(kw, Span::pos(pos))),
        }
    }

    pub fn expect_pct(&mut self, pct: PctT, pos: usize) -> crate::Result<Span> {
        match self.next() {
            Some(Item::Pct(p)) if p.typ == pct => Ok(p.span),
            Some(i) => Err(crate::Error::ExpectedPct(pct, i.span())),
            None => Err(crate::Error::ExpectedPct(pct, Span::pos(pos))),
        }
    }
}
