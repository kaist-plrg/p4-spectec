//! Runtime operations over internal-language types
//!
//! `expand` unfolds aliases, `equiv` and `sub` compare expanded types,
//! `subst` applies substitutions, with `fresh` binders for function types,
//! `error` names the failures.

mod equiv;
mod error;
mod expand;
mod fresh;
mod sub;
mod subst;

pub use equiv::*;
pub use error::*;
pub use expand::*;
pub use sub::*;
pub use subst::*;

pub(crate) use fresh::Fresh;
