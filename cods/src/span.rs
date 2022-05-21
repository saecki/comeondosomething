#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    /// inclusive
    pub start: Pos,
    /// exclusive
    pub end: Pos,
}

impl From<Pos> for Span {
    fn from(pos: Pos) -> Self {
        Self::new(pos, Pos::new(pos.line, pos.col + 1))
    }
}

impl Span {
    pub const fn new(start: Pos, end: Pos) -> Self {
        Self { start, end }
    }

    pub const fn pos(line: u32, col: u32) -> Self {
        Self::new(Pos::new(line, col), Pos::new(line, col + 1))
    }

    pub const fn cols(line: u32, start_col: u32, end_col: u32) -> Self {
        Self::new(Pos::new(line, start_col), Pos::new(line, end_col))
    }

    pub const fn across(a: Self, b: Self) -> Self {
        Self::new(a.start, b.end)
    }

    pub fn between(a: Self, b: Self) -> Self {
        let end_line = u32::max(a.start.line, b.start.line);
        let end_col = if end_line == a.end.line {
            u32::max(a.end.col + 1, b.start.col)
        } else {
            b.start.col
        };
        Self::new(a.end, Pos::new(end_line, end_col))
    }

    pub const fn before(&self) -> Self {
        Self::pos(self.start.line, self.start.col.saturating_sub(1))
    }

    pub const fn after(&self) -> Self {
        Self::pos(self.end.line, self.end.col)
    }

    pub const fn intersects(&self, other: &Self) -> bool {
        self.contains(&other.start) || other.contains(&self.start)
    }

    pub const fn contains(&self, pos: &Pos) -> bool {
        (self.start.line < pos.line || self.start.line == pos.line && self.start.col <= pos.col)
            && (self.end.line > pos.line || self.end.line == pos.line && self.end.col > pos.col)
    }
}

/// Character position
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Pos {
    /// 0-indexed
    pub line: u32,
    /// 0-indexed
    pub col: u32,
}

impl Pos {
    pub const fn new(line: u32, col: u32) -> Self {
        Self { line, col }
    }
}
