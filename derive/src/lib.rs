use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use std::borrow::Cow;
use std::fmt::Write as _;

struct Enum {
    vis: Option<Ident>,
    name: Ident,
    rename_all: Option<Case>,
    members: Vec<Member>,
}

#[derive(Clone, Copy)]
enum Case {
    /// caseExample
    Camel,
    /// CaseExample
    Pascal,
    /// case_example
    Snake,
    /// CASE_EXAMPLE
    ScreamingSnake,
    /// case-example
    Kebab,
    /// CASE-EXAMPLE
    ScreamingKebab,
}

struct Member {
    ident: Ident,
    rename: Option<Literal>,
}

fn error(msg: &str, span: Span) -> TokenStream {
    TokenStream::from_iter([
        TokenTree::Ident(Ident::new("compile_error", span)),
        TokenTree::Punct({
            let mut pct = Punct::new('!', Spacing::Alone);
            pct.set_span(span);
            pct
        }),
        TokenTree::Group({
            let mut group = Group::new(
                Delimiter::Parenthesis,
                TokenStream::from_iter([TokenTree::Literal({
                    let msg = format!("[cods_derive] {msg}");
                    let mut lit = Literal::string(&msg);
                    lit.set_span(span);
                    lit
                })]),
            );
            group.set_span(span);
            group
        }),
        TokenTree::Punct({
            let mut pct = Punct::new(';', Spacing::Alone);
            pct.set_span(span);
            pct
        }),
    ])
}

macro_rules! expect_token {
    ($token:expr, $tok:ident, $msg:literal, $span:expr) => {
        match $token {
            Some(TokenTree::$tok(t)) => Ok(t),
            Some(t) => {
                let msg = format!("expected {}", $msg);
                Err(error(&msg, t.span()))
            }
            None => {
                let msg = format!("missing {}", $msg);
                Err(error(&msg, $span))
            }
        }
    };
    ($token:expr, $tok:ident if $guard:literal, $msg:literal, $span:expr) => {
        match $token {
            Some(TokenTree::$tok(t)) if t.to_string() == $guard => Ok(t),
            Some(t) => {
                let msg = format!("expected {}", $msg);
                Err(error(&msg, t.span()))
            }
            None => {
                let msg = format!("missing {}", $msg);
                Err(error(&msg, $span))
            }
        }
    };
}

fn parse_enum(input: TokenStream) -> Result<Enum, TokenStream> {
    let mut tokens = input.into_iter().peekable();

    let first_span = tokens.peek().unwrap().span();

    let mut rename_all = None;
    loop {
        match tokens.peek() {
            Some(TokenTree::Punct(p)) if p.to_string() == "#" => {
                let p_span = p.span();
                tokens.next();

                let attributes = expect_token!(tokens.next(), Group, "attribute list", p_span)?;
                let attr_open_span = attributes.span_open();
                let mut attributes = attributes.stream().into_iter();

                let ident = expect_token!(
                    attributes.next(),
                    Ident,
                    "attribute identifier",
                    attr_open_span
                )?;

                if ident.to_string() != "cods" {
                    continue;
                }

                let attribute_args =
                    expect_token!(attributes.next(), Group, "attributes args", ident.span())?;
                let mut attribute_args = attribute_args.stream().into_iter();

                match attribute_args.next() {
                    Some(TokenTree::Ident(i)) if i.to_string() == "rename_all" => {
                        let eq =
                            expect_token!(attribute_args.next(), Punct if "=", "`=`", i.span())?;

                        let case = expect_token!(
                            attribute_args.next(),
                            Literal,
                            "case string literal",
                            eq.span()
                        )?;
                        rename_all = match case.to_string().trim_matches('"') {
                            "camelCase" => Some(Case::Camel),
                            "PascalCase" => Some(Case::Pascal),
                            "snake_case" => Some(Case::Snake),
                            "SCREAMING_SNAKE_CASE" => Some(Case::ScreamingSnake),
                            "kebab-case" => Some(Case::Kebab),
                            "SCREAMING-KEBAB-CASE" => Some(Case::ScreamingKebab),
                            _ => {
                                return Err(error("unknown case", case.span()));
                            }
                        };
                    }
                    Some(t) => {
                        let msg = format!("unexpected token: {t}");
                        return Err(error(&msg, t.span()));
                    }
                    None => (),
                }
            }
            _ => break,
        }
    }

    let mut vis = None;
    if let Some(TokenTree::Ident(i)) = tokens.peek() {
        if i.to_string() == "pub" {
            vis = Some(i.clone());
            tokens.next();
        }
    }

    let enum_kw = expect_token!(
        tokens.next(),
        Ident if "enum",
        "enum keyword",
        vis.as_ref().map_or(first_span, |v| v.span())
    )?;

    let name = expect_token!(tokens.next(), Ident, "enum name", enum_kw.span())?;

    let body = expect_token!(tokens.next(), Group, "enum body", name.span())?;
    let mut body = body.stream().into_iter().peekable();

    let mut members = Vec::new();
    while let Some(t) = body.peek() {
        let rename = match t {
            TokenTree::Punct(p) if p.to_string() == "#" => {
                let p_span = p.span();
                body.next();

                let attributes = expect_token!(body.next(), Group, "attribute list", p_span)?;
                let attr_open_span = attributes.span_open();
                let mut attributes = attributes.stream().into_iter();

                let ident = expect_token!(
                    attributes.next(),
                    Ident,
                    "attribute identifier",
                    attr_open_span
                )?;

                if ident.to_string() != "cods" {
                    continue;
                }

                let attribute_args =
                    expect_token!(attributes.next(), Group, "attributes args", ident.span())?;
                let mut attribute_args = attribute_args.stream().into_iter();

                match attribute_args.next() {
                    Some(TokenTree::Ident(i)) if i.to_string() == "rename" => {
                        let eq =
                            expect_token!(attribute_args.next(), Punct if "=", "`=`", i.span())?;

                        let r = expect_token!(
                            attribute_args.next(),
                            Literal,
                            "rename literal",
                            eq.span()
                        )?;
                        Some(r)
                    }
                    Some(t) => {
                        let msg = format!("unexpected token: {t}");
                        return Err(error(&msg, t.span()));
                    }
                    None => None,
                }
            }
            _ => None,
        };

        #[allow(unreachable_code)]
        let ident = expect_token!(
            body.next(),
            Ident,
            "enum variant name or attributes",
            unreachable!()
        )?;
        let ident_span = ident.span();
        members.push(Member { ident, rename });

        expect_token!(body.next(), Punct if ",", ",", ident_span)?;
    }

    Ok(Enum {
        vis,
        name,
        rename_all,
        members,
    })
}

fn transform_case(input: &str, case: Case) -> Cow<str> {
    match case {
        Case::Camel => {
            let mut output = input.to_string();
            output[0..1].make_ascii_lowercase();
            Cow::Owned(output)
        }
        Case::Pascal => Cow::Borrowed(input),
        Case::Snake => {
            let mut output = String::with_capacity(input.len() + 3);
            let mut iter = input.chars();
            output.push(iter.next().unwrap().to_ascii_lowercase());
            for c in iter {
                if c.is_ascii_uppercase() {
                    output.push('_');
                }
                output.push(c.to_ascii_lowercase());
            }
            Cow::Owned(output)
        }
        Case::ScreamingSnake => {
            let mut output = String::with_capacity(input.len() + 3);
            let mut iter = input.chars();
            output.push(iter.next().unwrap().to_ascii_uppercase());
            for c in iter {
                if c.is_ascii_uppercase() {
                    output.push('_');
                }
                output.push(c.to_ascii_uppercase());
            }
            Cow::Owned(output)
        }
        Case::Kebab => {
            let mut output = String::with_capacity(input.len() + 3);
            let mut iter = input.chars();
            output.push(iter.next().unwrap().to_ascii_lowercase());
            for c in iter {
                if c.is_ascii_uppercase() {
                    output.push('-');
                }
                output.push(c.to_ascii_lowercase());
            }
            Cow::Owned(output)
        }
        Case::ScreamingKebab => {
            let mut output = String::with_capacity(input.len() + 3);
            let mut iter = input.chars();
            output.push(iter.next().unwrap().to_ascii_uppercase());
            for c in iter {
                if c.is_ascii_uppercase() {
                    output.push('-');
                }
                output.push(c.to_ascii_uppercase());
            }
            Cow::Owned(output)
        }
    }
}

#[proc_macro_derive(EnumDisplay, attributes(cods))]
pub fn derive_display(input: TokenStream) -> TokenStream {
    let Enum {
        name,
        rename_all,
        members,
        ..
    } = match parse_enum(input) {
        Ok(e) => e,
        Err(t) => return t,
    };

    let mut output = format!(
        "impl std::fmt::Display for {name} {{
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {{
                match self {{"
    );

    for m in members {
        let m_ident = m.ident.to_string();
        let _ = match m.rename {
            Some(r) => write!(output, "Self::{m_ident} => write!(f, {r}),"),
            None => match rename_all {
                Some(case) => {
                    let new_ident = transform_case(&m_ident, case);
                    write!(output, "Self::{m_ident} => write!(f, \"{new_ident}\"),")
                }
                None => write!(output, "Self::{m_ident} => write!(f, \"{m_ident}\"),"),
            },
        };
    }

    output.push_str("}}}");
    output.parse().unwrap()
}

#[proc_macro_derive(EnumFromStr, attributes(cods))]
pub fn derive_from_str(input: TokenStream) -> TokenStream {
    let Enum {
        name,
        rename_all,
        members,
        ..
    } = match parse_enum(input) {
        Ok(e) => e,
        Err(t) => return t,
    };

    let mut output = format!(
        "impl std::str::FromStr for {name} {{
            type Err = ();

            fn from_str(input: &str) -> Result<Self, Self::Err> {{
                match input {{"
    );

    for m in members {
        let m_ident = m.ident.to_string();
        let _ = match m.rename {
            Some(r) => write!(output, "{r} => Ok(Self::{m_ident}),"),
            None => match rename_all {
                Some(case) => {
                    let new_ident = transform_case(&m_ident, case);
                    write!(output, "\"{new_ident}\" => Ok(Self::{m_ident}),")
                }
                None => write!(output, "\"{m_ident}\" => Ok(Self::{m_ident}),"),
            },
        };
    }

    let _ = write!(output, "_ => Err(()),");

    output.push_str("}}}");
    output.parse().unwrap()
}

#[proc_macro_derive(EnumMembersArray, attributes(cods))]
pub fn derive_members_array(input: TokenStream) -> TokenStream {
    let Enum {
        vis, name, members, ..
    } = match parse_enum(input) {
        Ok(e) => e,
        Err(t) => return t,
    };

    let count = members.len();
    let vis = vis.map_or("".to_string(), |v| v.to_string());
    let mut output = format!(
        "impl {name} {{
            {vis} fn members() -> &'static [{name}; {count}] {{
                &[\n"
    );

    for m in members {
        let m_ident = m.ident.to_string();
        let _ = writeln!(output, "{name}::{m_ident},");
    }

    let _ = write!(
        output,
        "       ]
            }}
        }}"
    );

    output.parse().unwrap()
}
