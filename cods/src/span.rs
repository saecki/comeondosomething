/// Span of character indices
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub const fn of(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub const fn span(a: Self, b: Self) -> Self {
        Self::of(a.start, b.end)
    }

    pub const fn between(a: Self, b: Self) -> Self {
        Self::of(a.end, b.start)
    }

    pub const fn pos(pos: usize) -> Self {
        Self::of(pos, pos + 1)
    }

    pub const fn after(&self) -> Self {
        Self::pos(self.end)
    }

    pub const fn len(&self) -> usize {
        self.end - self.start
    }

    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub const fn intersects(&self, other: &Self) -> bool {
        self.contains(other.start) || other.contains(self.start)
    }

    pub const fn contains(&self, pos: usize) -> bool {
        self.start <= pos && self.end > pos
    }
}
