//! Meta-language representations of specifications
//!
//! `shared` encodes and decodes the syntax IL and AL have in common,
//! together with the conversion cache and errors;
//! `il` adds the elaborated IL given to the IL-to-AL specification,
//! and `al` adds AL, runtime values, and the AL runner interface.

pub mod al;
pub mod il;
pub mod shared;
