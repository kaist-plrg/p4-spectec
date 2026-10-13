//! Snapshot comparison for acceptance drivers
//!
//! Checks normalize decorative trailing spaces emitted by renderers and
//! support expectation promotion through expect-test.

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
