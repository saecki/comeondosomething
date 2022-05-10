use crate::Val;

#[derive(Debug)]
pub struct Stack {
    /// The frame data.
    values: Vec<Option<Val>>,
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
    
    fn frame_size(&self) -> usize {
        self.values.len() - self.frame_start()
    }

    /// Extends the current frame to `size`.
    /// 
    /// Panics if `size` is less than the current frame size.
    pub fn extend_to(&mut self, size: usize) {
        let additional = size - self.frame_size();
        self.values.reserve(additional);
        for _ in 0..additional {
            self.values.push(None);
        }
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
        let idx = match var {
            VarRef::Local(i) => self.frame_start() + i,
            VarRef::Global(i) => *i,
        };
        self.values[idx] = Some(val);
    }

    pub fn get_local(&mut self, var: &VarRef) -> Val {
        let idx = match var {
            VarRef::Local(i) => self.frame_start() + i,
            VarRef::Global(i) => *i,
        };
        self.values
            .get(idx)
            .unwrap()
            .as_ref()
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
