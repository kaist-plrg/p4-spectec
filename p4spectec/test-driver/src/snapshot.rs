use std::path::PathBuf;

use expect_test::ExpectFile;
use serde::Deserialize;

/// Registers a transformation snapshot owned by its module.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Snapshot {
    pub name: String,
    pub inputs: Vec<PathBuf>,
    pub expected: PathBuf,
}

// Renderers leave decorative spaces after declarations and rule-group headers
fn normalize_rendered(text: &str) -> String {
    text.split('\n')
        .map(|line| line.trim_end_matches([' ', '\t']))
        .collect::<Vec<_>>()
        .join("\n")
}

pub fn check(expected: ExpectFile, actual: &str) {
    expected.assert_eq(&normalize_rendered(actual));
}
