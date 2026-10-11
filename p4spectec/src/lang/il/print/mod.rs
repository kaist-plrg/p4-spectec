//! Text rendering for IL syntax
//!
//! `source` implements source rendering.
//! `prepared` renders arena-backed syntax through borrowed views.

mod prepared;
mod source;

pub use prepared::{ArgRef, ExpRef, PatternRef};
