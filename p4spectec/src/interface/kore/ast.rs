//! KORE abstract syntax, as kompile writes it (K 7.1.337)
//!
//! Only what `definition.kore` and krun's initial terms use is represented.
//! For example, `\dv{SortInt{}}("1")` is an application of `\dv`
//! to the string `1`, with the sort `SortInt{}`.

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
