use crate::Val;

pub struct Stack {
    values: Vec<Option<Val>>,
    frames: Vec<usize>,
}

impl Default for Stack {
    fn default() -> Self {
        Self {
            values: Vec::new(),
            frames: Vec::new(),
        }
    }
}

impl Stack {
    fn frame_start(&self) -> usize {
        *self.frames.last().expect("Expected frames to be non empty")
    }

    pub fn push(&mut self, size: usize) {
        self.frames.push(self.values.len());
        self.values.reserve(size);
        for _ in 0..size {
            self.values.push(None);
        }
    }

    pub fn pop(&mut self) {
        let len = self.frame_start();
        self.frames.pop();
        self.values.truncate(len);
    }

    pub fn set_local(&mut self, var: &VarRef, val: Val) {
        let start = self.frame_start();
        self.values[start + var.idx] = Some(val);
    }

    pub fn get_local(&mut self, var: &VarRef) -> Val {
        let start = self.frame_start();
        self.values
            .get(start + var.idx)
            .unwrap()
            .as_ref()
            .expect("Expected value to be initialized")
            .clone()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct VarRef {
    // XXX: only local values
    // TODO: capture values from outer scope
    idx: usize,
}

impl VarRef {
    pub fn new(idx: usize) -> Self {
        Self { idx }
    }
}
