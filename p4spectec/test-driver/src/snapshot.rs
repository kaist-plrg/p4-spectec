use expect_test::ExpectFile;

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
