//! Language models and codecs for P4 `SpecTec`
//!
//! `parse`, `elab`, `algo`, `structure`, and `prosify` transform source paths
//! into EL, IL, AL, SL, and PL through `frontend` and `pass`;
//! `backend_doc` renders EL as canonical LaTeX and EL and PL as AsciiDoc;
//! `interp` and `runner` execute AL, SL, or PL against a P4 program,
//! with `interface` builtins and `sim_plugin` architectures;
//! `lang`, `runtime`, `stf`, and `util` are the shared data and codecs.

#[path = "backend-doc/mod.rs"]
pub mod backend_doc;
pub mod diagnostic;
pub mod frontend;
pub mod interface;
pub mod interp;
pub mod lang;
pub mod pass;
mod pipeline;
pub mod runner;
pub mod runtime;
#[path = "sim-plugin/mod.rs"]
pub mod sim_plugin;
pub mod stf;
pub mod util;

pub use pipeline::{
    Error, algo, algo_with_warnings, elab, elab_with_warnings, parse, prosify,
    prosify_with_warnings, structure, structure_with_warnings,
};
