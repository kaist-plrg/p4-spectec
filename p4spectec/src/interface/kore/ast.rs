//! KORE abstract syntax, as kompile writes it (K 7.1.337)
//!
//! Only what `definition.kore` and krun's initial terms use is represented.
//! For example, `\dv{SortInt{}}("1")` is an application of `\dv`
//! to the string `1`, with the sort `SortInt{}`.

use std::fmt::{self, Display, Write};

// == Sorts and patterns

/// A sort: `SortInt{}`, `SortMap{}`, or a sort variable `S`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Sort {
    Sort(String, Vec<Sort>),
    Var(String),
}

/// A pattern.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Pattern {
    /// An element variable: `VarX:SortInt{}`.
    Var(String, Sort),
    /// A set variable: `@X:SortInt{}`.
    SetVar(String, Sort),
    /// A symbol, alias, or `\connective` applied to sorts and arguments.
    App(String, Vec<Sort>, Vec<Pattern>),
    /// A string literal, unescaped.
    Str(String),
}

/// An attribute is a pattern, e.g. `priority{}("50")`.
pub type Attr = Pattern;

// == Sentences

/// A sentence of a module.
#[derive(Clone, Debug)]
pub enum Sentence {
    Import(String),
    SortDecl {
        name: String,
        params: Vec<String>,
        attrs: Vec<Attr>,
    },
    SymbolDecl {
        name: String,
        params: Vec<String>,
        args: Vec<Sort>,
        result: Sort,
        attrs: Vec<Attr>,
    },
    AliasDecl {
        name: String,
        /// The left-hand side, `name{}(X1, ..., Xn)`.
        lhs: Pattern,
        body: Pattern,
    },
    Axiom {
        pattern: Pattern,
        attrs: Vec<Attr>,
    },
    Claim {
        pattern: Pattern,
        attrs: Vec<Attr>,
    },
}

/// A module.
#[derive(Clone, Debug)]
pub struct Module {
    pub name: String,
    pub sentences: Vec<Sentence>,
}

/// A definition: modules in file order.
#[derive(Clone, Debug)]
pub struct Definition {
    pub modules: Vec<Module>,
}

impl Definition {
    /// Every sentence, in file order.
    pub fn sentences(&self) -> impl Iterator<Item = &Sentence> {
        self.modules
            .iter()
            .flat_map(|module| module.sentences.iter())
    }
}

// == Printing
//
// KORE text, as the LLVM backend parses it. A string has one character
// per byte; bytes outside printable ASCII are written as `\xHH`.

impl Display for Sort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Sort::Sort(name, sorts) => {
                write!(f, "{name}{{")?;
                write_list(f, sorts)?;
                f.write_char('}')
            }
            Sort::Var(name) => f.write_str(name),
        }
    }
}

fn write_list<T: Display>(f: &mut fmt::Formatter<'_>, items: &[T]) -> fmt::Result {
    for (idx, item) in items.iter().enumerate() {
        if idx > 0 {
            f.write_str(", ")?;
        }
        item.fmt(f)?;
    }
    Ok(())
}

impl Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Terms nest deeply, e.g. long K sequences and maps
        stacker::maybe_grow(64 * 1024, 16 * 1024 * 1024, || match self {
            Pattern::Var(name, sort) | Pattern::SetVar(name, sort) => write!(f, "{name}:{sort}"),
            Pattern::App(name, sorts, args) => {
                write!(f, "{name}{{")?;
                write_list(f, sorts)?;
                f.write_str("}(")?;
                write_list(f, args)?;
                f.write_char(')')
            }
            Pattern::Str(text) => {
                f.write_char('"')?;
                for c in text.chars() {
                    match c {
                        '"' => f.write_str("\\\"")?,
                        '\\' => f.write_str("\\\\")?,
                        ' '..='~' => f.write_char(c)?,
                        c if u32::from(c) <= 0xff => write!(f, "\\x{:02x}", u32::from(c))?,
                        c => write!(f, "\\U{:08x}", u32::from(c))?,
                    }
                }
                f.write_char('"')
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nested_sort() {
        let sort = Sort::Sort("SortMInt".to_owned(), vec![Sort::Sort("Sort8".to_owned(), vec![])]);
        assert_eq!(sort.to_string(), "SortMInt{Sort8{}}");
        let dv = Pattern::App("\\dv".to_owned(), vec![sort], vec![Pattern::Str("1p8".to_owned())]);
        assert_eq!(dv.to_string(), "\\dv{SortMInt{Sort8{}}}(\"1p8\")");
    }
}
