use crate::Span;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Files {
    inputs: Vec<String>,
    idents: Vec<&'static str>,
}

impl Files {
    pub fn clear(&mut self) {
        self.inputs.clear();
        self.idents.clear();
    }

    pub(crate) fn push_file(&mut self, input: String) -> &'static str {
        let text = unsafe { std::mem::transmute(input.as_str()) };
        self.inputs.push(input);
        text
    }

    pub(crate) fn push_ident(&mut self, name: &'static str) -> Ident {
        for (id, n) in self.idents.iter().enumerate() {
            if *n == name {
                return Ident(id);
            }
        }

        let id = self.idents.len();
        self.idents.push(name);
        Ident(id)
    }

    pub fn ident_name<'a>(&'a self, id: Ident) -> &'a str {
        &self.idents[id.0]
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
