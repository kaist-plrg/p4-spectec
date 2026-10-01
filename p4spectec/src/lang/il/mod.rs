//! Internal language
//!
//! IL is EL after elaboration:
//! every expression carries its type, notation is resolved to mixfix forms,
//! declarations and bodies are merged into definitions,
//! and iterations name the variables they bind.
//! `ast` is the model, `eq` compares it ignoring spans,
//! `free` collects identifiers,
//! `fresh` mints variables, `var` turns variables into expressions,
//! `print` renders it.

pub mod ast;
pub mod eq;
pub mod free;
pub mod fresh;
pub mod has_call;
pub mod print;
pub mod var;
