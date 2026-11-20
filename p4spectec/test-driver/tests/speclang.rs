//! The P4 specification at every conversion stage
//!
//! Each test converts the sources under `spec/` from the repository root
//! and compares the printed or rendered text with `expected/speclang/<name>.expected`;
//! `UPDATE_EXPECT=1` rewrites the expected file instead.

use std::path::{Path, PathBuf};

use expect_test::expect_file;

use p4spectec::{
    diagnostic::{RenderConfig, Renderer},
    lang::traits::print::Print,
    specdoc::{adoc, latex},
};

use p4spectec_test::harness::corpus;

/// Returns the specification sources, entering the repository root
/// so that source paths in the output stay stable.
fn spec() -> Vec<PathBuf> {
    let root = corpus::root().canonicalize().expect("repository root");
    std::env::set_current_dir(&root).expect("enter repository root");
    vec![PathBuf::from("spec")]
}

/// Compares `text` with snapshot `name`.
///
/// Renderers leave decorative spaces after declarations and rule-group headers,
/// so trailing spaces and tabs are stripped from every line first.
fn check(name: &str, text: &str) {
    let text = text
        .split('\n')
        .map(|line| line.trim_end_matches([' ', '\t']))
        .collect::<Vec<_>>()
        .join("\n");
    let path_expected = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("expected/speclang")
        .join(format!("{name}.expected"));
    expect_file![path_expected].assert_eq(&text);
}

#[test]
fn elab() {
    let spec_il = p4spectec::elab(&spec()).unwrap_or_else(|report| panic!("{report}"));
    check("elab", &(Print::to_string(&spec_il) + "\n"));
}

#[test]
fn algo() {
    let spec_al = p4spectec::algo(&spec()).unwrap_or_else(|report| panic!("{report}"));
    check("algo", &(Print::to_string(&spec_al) + "\n"));
}

#[test]
fn structure_without_rule_groups() {
    let spec_sl = p4spectec::structure(&spec(), true).unwrap_or_else(|report| panic!("{report}"));
    check("structure-without-rule-groups", &(Print::to_string(&spec_sl) + "\n"));
}

#[test]
fn structure_with_rule_groups() {
    let spec_sl = p4spectec::structure(&spec(), false).unwrap_or_else(|report| panic!("{report}"));
    check("structure-with-rule-groups", &(Print::to_string(&spec_sl) + "\n"));
}

#[test]
fn prose() {
    let spec_pl = p4spectec::prosify(&spec()).unwrap_or_else(|report| panic!("{report}"));
    check("prose", &(Print::to_string(&spec_pl) + "\n"));
}

#[test]
fn adoc_el() {
    // Render each EL definition as a separate AsciiDoc block
    let spec_el = p4spectec::parse(spec()).unwrap_or_else(|report| panic!("{report}"));
    let texts = spec_el.iter().map(adoc::el::render_def).collect::<Vec<_>>();
    check("adoc-el", &texts.join("\n\n"));
}

#[test]
fn latex_el() {
    // Render each EL definition as a separate LaTeX block, without link destinations
    let spec_el = p4spectec::parse(spec()).unwrap_or_else(|report| panic!("{report}"));
    let anchor_ctx = latex::AnchorContext::default();
    let texts = spec_el
        .iter()
        .map(|def| latex::render_def(&anchor_ctx, def).unwrap_or_else(|report| panic!("{report}")))
        .collect::<Vec<_>>();
    check("latex-el", &texts.join("\n\n"));
}

#[test]
fn adoc_pl() {
    let spec_pl = p4spectec::prosify(&spec()).unwrap_or_else(|report| panic!("{report}"));
    // Print the rendering warnings, which the snapshot does not hold
    let mut warnings = Vec::new();
    let text = adoc::pl::render_spec(&mut warnings, &spec_pl);
    for report in warnings {
        eprint!(
            "{}",
            Renderer::new(RenderConfig::default())
                .render_to_string(&report)
                .unwrap()
        );
    }
    // Require the same anchors on a second rendering
    assert!(
        text == adoc::pl::render_spec(&mut Vec::new(), &spec_pl),
        "AsciiDoc arm anchors changed on repeated rendering"
    );
    check("adoc-pl", &text);
}
