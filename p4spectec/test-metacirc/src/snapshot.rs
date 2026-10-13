//! Compiler snapshots for the complete meta specification
//!
//! Elaboration, AL lowering, and structure lowering have separate expectations.
//! Comparisons ignore renderer decoration at line ends and never rewrite files.

use std::{fs, path::Path};

use p4spectec::lang::traits::print::Print;

use crate::{Result, source};

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

/// Reports the first differing line without dumping a whole specification.
fn check(name: &str, text_actual: &str) -> Result<()> {
    // Remove only decorative spaces emitted after declarations and headers
    let text_actual = (text_actual.to_owned() + "\n")
        .split('\n')
        .map(|text| text.trim_end_matches([' ', '\t']))
        .collect::<Vec<_>>()
        .join("\n");
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("expected")
        .join(format!("{name}.expected"));
    let text_expect = fs::read_to_string(&path)?;

    // Equal prefixes with different line counts still fail the snapshot
    if text_actual != text_expect {
        let num_line = text_actual
            .lines()
            .zip(text_expect.lines())
            .position(|(text_a, text_b)| text_a != text_b)
            .unwrap_or_else(|| text_actual.lines().count().min(text_expect.lines().count()))
            + 1;
        return Err(format!("{}: snapshot differs at line {num_line}", path.display()).into());
    }
    Ok(())
}
