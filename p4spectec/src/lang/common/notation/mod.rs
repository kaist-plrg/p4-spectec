//! Mixfix notation shared by the language representations
//!
//! A notation form such as `C |- e : t` is a `Mixfix`:
//! atoms (`|-`, `:`) interleaved with argument holes.
//! `Mixop` is the shape alone; `Atom` is one literal piece.

pub mod atom;
pub mod mixfix;
pub mod mixop;
