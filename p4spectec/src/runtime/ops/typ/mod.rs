//! Runtime operations over internal-language types
//!
//! `expand` unfolds aliases, `equiv` and `sub` compare expanded types,
//! `constructor` counts constructors, `cardinality` classifies inhabitants,
//! `subst` applies substitutions, with `fresh` binders for function types,
//! `error` names the failures.

mod cardinality;
mod constructor;
mod equiv;
mod error;
mod expand;
mod fresh;
mod sub;
mod subst;

pub use cardinality::*;
pub use constructor::*;
pub use equiv::*;
pub use error::*;
pub use expand::*;
pub use sub::*;
pub use subst::*;

pub(crate) use fresh::Fresh;
