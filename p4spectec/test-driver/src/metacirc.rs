//! Compiler snapshots for the AL meta specification

use std::{error::Error, path::Path};

use expect_test::expect_file;
use p4spectec::lang::traits::print::Print;

/// The result of a metacircular snapshot check.
pub type Result<T> = std::result::Result<T, Box<dyn Error>>;

/// Checks elaboration, AL lowering, and structure lowering snapshots.
pub fn run() -> Result<()> {
    let paths = [source("spec-meta")];

    check("elab", Print::to_string(&p4spectec::elab(&paths)?));
    check("algo", Print::to_string(&p4spectec::algo(&paths)?));
    check("structure", Print::to_string(&p4spectec::structure(&paths, true)?));

    eprintln!("metacirc snapshots: elab, algo, structure");
    Ok(())
}

fn check(name: &str, text_actual: String) {
    let path_relative = Path::new("expected/metacirc").join(format!("{name}.expected"));
    let path_expected = crate::suite::expected_path(&path_relative);
    crate::snapshot::check(expect_file![path_expected], &(text_actual + "\n"));
}

fn source(name: &str) -> std::path::PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join(name)
}
