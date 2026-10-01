//! Elaboration language
//!
//! EL is the parsed specification as written:
//! types may be plain or notation, expressions are untyped,
//! and definitions are separate declarations and bodies.
//! `ast` is the model, `eq` compares it ignoring source spans,
//! `free` collects identifiers, `print` renders it back to text.

pub mod ast;
pub mod eq;
pub mod free;
pub mod print;
