use crate::Val;

#[derive(Clone, Debug)]
pub struct Stack {
    /// The frame data.
    values: Vec<Val>,
    /// The starting indices of the frames.
    frames: Vec<usize>,
}

impl Default for Stack {
    fn default() -> Self {
        Self {
            values: Vec::new(),
            frames: vec![0],
        }
    }
}

impl Stack {
    fn frame_start(&self) -> usize {
        *self.frames.last().expect("Expected frames to be non empty")
    }

    pub fn resize(&mut self, size: usize) {
        self.values.resize(size, Val::Unit);
    }

    pub fn push(&mut self, size: usize) {
        self.frames.push(self.values.len());
        self.values.reserve(size);
        for _ in 0..size {
            self.values.push(Val::Unit);
        }
    }

    pub fn pop(&mut self) {
        let len = self.frame_start();
        self.frames.pop();
        self.values.truncate(len);
    }

    pub fn set(&mut self, var: &VarRef, val: Val) {
        let idx = match var {
            VarRef::Local(i) => self.frame_start() + i,
            VarRef::Global(i) => *i,
        };
        self.values[idx] = val;
    }

    pub fn get(&mut self, var: &VarRef) -> Val {
        let idx = match var {
            VarRef::Local(i) => self.frame_start() + i,
            VarRef::Global(i) => *i,
        };
        self.values
            .get(idx)
            .expect("Expected value to be initialized")
            .clone()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum VarRef {
    /// The position relative to the frame start.
    Local(usize),
    /// The position relative to the start of the stack.
    Global(usize),
}
