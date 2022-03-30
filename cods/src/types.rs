/// Range of character indices
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CRange {
    pub start: usize,
    pub end: usize,
}

impl CRange {
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

    pub const fn offset(&self, offset: isize) -> Self {
        Self::of(
            (self.start as isize + offset) as usize,
            (self.end as isize + offset) as usize,
        )
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct IdentRange {
    pub ident: Ident,
    pub range: CRange,
}

impl IdentRange {
    pub const fn new(ident: Ident, range: CRange) -> Self {
        Self { ident, range }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Ident(pub usize);
