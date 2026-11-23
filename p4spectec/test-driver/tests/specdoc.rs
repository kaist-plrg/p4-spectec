//! AsciiDoc rendering warnings of specification sources
//!
//! Each fixture under `tests/p4spectec/specdoc/neg` must convert to PL without warnings
//! and warn when rendered as AsciiDoc;
//! its warnings are compared with the snapshot `<stem>.snap` beside it.

use p4spectec::{diagnostic::RenderConfig, specdoc::adoc};

use p4spectec_test::harness::fixture::{check, fixtures, must_pass};

// = Suites

/// One directory under `tests/p4spectec/`.
struct Fixtures {
    /// Holds the fixtures, relative to `tests/p4spectec/`.
    dir: &'static str,
}

impl Fixtures {
    /// Renders each fixture as AsciiDoc, which must warn, and compares the warnings with its snapshot.
    fn check(&self) {
        for path in fixtures(self.dir, &[".watsup"]) {
            // Conversion must pass without warnings before rendering warns
            let (result, warnings) = p4spectec::prosify_with_warnings(std::slice::from_ref(&path));
            let spec_pl = must_pass(&path, "PL conversion", result);
            assert!(warnings.is_empty(), "{}: conversion warned", path.display());
            let mut warnings = Vec::new();
            adoc::pl::render_spec(&mut warnings, &spec_pl);
            assert!(!warnings.is_empty(), "{}: AsciiDoc rendered without warnings", path.display());
            check(&path.with_extension("snap"), &warnings, &RenderConfig::default());
        }
    }
}

// = Tests

const NEG: Fixtures = Fixtures { dir: "specdoc/neg" };

#[test]
fn neg() {
    NEG.check();
}
