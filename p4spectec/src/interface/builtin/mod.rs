//! Runtime implementations of the standard SpecTec builtins
//!
//! The dispatcher validates arity,
//! then each family decodes its arguments and computes one result.
//! For example, `$sum_int([2, 5])` returns the value `7`.
//! Every builtin has the signature `fn(&mut Arena, &[Typ], &[Value])`.

pub mod call;
pub mod error;
pub mod extract;
pub mod fresh;
pub mod ints;
pub mod lists;
pub mod maps;
pub mod nats;
pub mod numerics;
pub mod sets;
pub mod texts;

pub use error::BuiltinError;
