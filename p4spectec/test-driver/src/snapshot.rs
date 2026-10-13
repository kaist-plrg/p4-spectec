//! Snapshot comparison for acceptance drivers
//!
//! Both checks normalize decorative trailing spaces emitted by renderers
//! `check` supports expectation promotion through expect-test
//! `check_readonly` reports mismatches without rewriting files

use crate::{Error, Result};
use std::{fs, path::Path};

use expect_test::ExpectFile;

// Renderers leave decorative spaces after declarations and rule-group headers
fn normalize_rendered(text: &str) -> String {
    text.split('\n')
        .map(|line| line.trim_end_matches([' ', '\t']))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Compares a snapshot using the configured expectation promotion policy.
pub fn check(expected: ExpectFile, actual: &str) {
    expected.assert_eq(&normalize_rendered(actual));
}

/// Compares a snapshot without permitting expectation promotion.
pub fn check_readonly(path: &Path, actual: &str) -> Result<()> {
    // Read the preserved expectation and normalize only renderer decoration
    let expected = fs::read_to_string(path)?;
    let actual = normalize_rendered(actual);
    // Compare complete outputs, including their final newline
    if actual != expected {
        // Report the first mismatch even when one output is a strict prefix
        let num_line = actual
            .lines()
            .zip(expected.lines())
            .position(|(text_a, text_b)| text_a != text_b)
            .unwrap_or_else(|| actual.lines().count().min(expected.lines().count()))
            + 1;
        return Err(Error::Invalid(format!(
            "{}: snapshot differs at line {num_line}",
            path.display()
        )));
    }
    Ok(())
}
