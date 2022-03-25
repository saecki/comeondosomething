use std::collections::VecDeque;

use crate::Item;

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
}
