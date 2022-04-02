use crate::Span;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Idents(Vec<String>);

impl Idents {
    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn push(&mut self, name: &str) -> Ident {
        for (id, n) in self.0.iter().enumerate() {
            if n == name {
                return Ident(id);
            }
        }

        let id = self.0.len();
        self.0.push(name.to_owned());
        Ident(id)
    }

    pub fn name(&self, id: Ident) -> &str {
        &self.0[id.0]
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct IdentSpan {
    pub ident: Ident,
    pub span: Span,
}

impl IdentSpan {
    pub const fn new(ident: Ident, span: Span) -> Self {
        Self { ident, span }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Ident(pub usize);
