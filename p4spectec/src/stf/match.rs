//! Expected-packet matching
//!
//! Length is checked first, then corresponding nibbles are compared with `*`
//! as the only wildcard; for example, `a0` matches `a*` but not `*`.

/// Tests an actual packet against an STF expectation
///
/// `*` matches exactly one nibble.
pub fn matches(actual: &str, expected: &str) -> bool {
    actual.chars().count() == expected.chars().count()
        && actual
            .chars()
            .zip(expected.chars())
            .all(|(actual, expected)| expected == '*' || actual == expected)
}
