//! Compiler snapshots for the complete meta specification
//!
//! Elaboration, AL lowering, and structure lowering have separate expectations.
//! Comparisons ignore renderer decoration at line ends and never rewrite files.

use std::path::Path;

use p4spectec::lang::traits::print::Print;

use super::{Result, source};

/// Checks the three compiler stages against the preserved meta snapshots.
pub fn run() -> Result<()> {
    let paths = [source("spec-meta")];

    // Check each stage independently so a mismatch identifies its boundary
    check("elab", &Print::to_string(&p4spectec::elab(&paths)?))?;
    check("algo", &Print::to_string(&p4spectec::algo(&paths)?))?;
    check("structure", &Print::to_string(&p4spectec::structure(&paths, true)?))?;
    eprintln!("metacirc snapshots: elab, algo, structure");
    Ok(())
}

/// Compares a compiler stage without rewriting its expectation.
fn check(name: &str, text_actual: &str) -> Result<()> {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("expected/metacirc")
        .join(format!("{name}.expected"));
    crate::snapshot::check_readonly(&path, &(text_actual.to_owned() + "\n"))?;
    Ok(())
}
