/// Span of character indices
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub const fn of(start: u32, end: u32) -> Self {
        Self { start, end }
    }

    pub const fn pos(pos: u32) -> Self {
        Self::of(pos, pos + 1)
    }

    pub const fn across(a: Self, b: Self) -> Self {
        Self::of(a.start, b.end)
    }

    pub const fn between(a: Self, b: Self) -> Self {
        Self::of(a.end, b.start)
    }

    pub const fn before(&self) -> Self {
        Self::pos(self.start.saturating_sub(1))
    }

    pub const fn after(&self) -> Self {
        Self::pos(self.end)
    }

    pub const fn len(&self) -> u32 {
        self.end - self.start
    }

    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub const fn intersects(&self, other: &Self) -> bool {
        self.contains(other.start) || other.contains(self.start)
    }

    pub const fn contains(&self, pos: u32) -> bool {
        self.start <= pos && self.end > pos
    }
}
