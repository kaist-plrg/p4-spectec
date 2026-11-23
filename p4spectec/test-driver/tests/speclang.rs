//! The P4 specification at every conversion stage
//!
//! A `Suite` names a snapshot and how to print or render the sources under `spec/`;
//! its test converts them from the repository root
//! and compares the text with `tests/p4spectec/speclang/<name>.snap`.
//! `INSTA_UPDATE=always` rewrites the snapshot instead.

use std::path::PathBuf;

use p4spectec::{
    diagnostic::{RenderConfig, Renderer},
    lang::traits::print::Print,
    specdoc::adoc,
};

use p4spectec_test::harness;

// = Helpers

/// Returns the specification sources, entering the repository root
/// so that source paths in the output stay stable.
fn spec() -> Vec<PathBuf> {
    let root = harness::root().canonicalize().expect("repository root");
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
    insta::with_settings!({
        snapshot_path => harness::root().join("tests/p4spectec/speclang"),
        prepend_module_to_snapshot => false,
        omit_expression => true,
    }, {
        insta::assert_snapshot!(name, text);
    });
}

// = Suites

/// Whether structuring keeps rule groups.
#[derive(Clone, Copy)]
enum RuleGroups {
    With,
    Without,
}

/// Ways a suite prints or renders the specification.
#[derive(Clone, Copy)]
enum Output {
    /// Prints the elaborated IL.
    Elab,
    /// Prints the AL.
    Algo,
    /// Prints the SL.
    Structure(RuleGroups),
    /// Prints the PL.
    Prose,
    /// Renders each EL definition as a separate AsciiDoc block.
    AdocEl,
    /// Renders the PL as AsciiDoc, requiring the same anchors on a second rendering.
    AdocPl,
}

impl Output {
    /// Converts the specification and answers its text.
    fn text(self) -> String {
        match self {
            Self::Elab => {
                let spec_il = p4spectec::elab(&spec()).unwrap_or_else(|report| panic!("{report}"));
                Print::to_string(&spec_il) + "\n"
            }
            Self::Algo => {
                let spec_al = p4spectec::algo(&spec()).unwrap_or_else(|report| panic!("{report}"));
                Print::to_string(&spec_al) + "\n"
            }
            Self::Structure(rule_groups) => {
                let without_rule_groups = matches!(rule_groups, RuleGroups::Without);
                let spec_sl = p4spectec::structure(&spec(), without_rule_groups)
                    .unwrap_or_else(|report| panic!("{report}"));
                Print::to_string(&spec_sl) + "\n"
            }
            Self::Prose => {
                let spec_pl =
                    p4spectec::prosify(&spec()).unwrap_or_else(|report| panic!("{report}"));
                Print::to_string(&spec_pl) + "\n"
            }
            Self::AdocEl => {
                let spec_el = p4spectec::parse(spec()).unwrap_or_else(|report| panic!("{report}"));
                let texts = spec_el.iter().map(adoc::el::render_def).collect::<Vec<_>>();
                texts.join("\n\n")
            }
            Self::AdocPl => {
                let spec_pl =
                    p4spectec::prosify(&spec()).unwrap_or_else(|report| panic!("{report}"));
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
                text
            }
        }
    }
}

/// One printing or rendering of the specification.
struct Suite {
    /// Names the snapshot `tests/p4spectec/speclang/<name>.snap`.
    name: &'static str,
    /// Names how the specification is printed or rendered.
    output: Output,
}

impl Suite {
    /// Prints or renders the specification as the suite names
    /// and compares the text with its snapshot.
    fn check(&self) {
        check(self.name, &self.output.text());
    }
}

// = Tests

const ELAB: Suite = Suite { name: "elab", output: Output::Elab };

#[test]
fn elab() {
    ELAB.check();
}

const ALGO: Suite = Suite { name: "algo", output: Output::Algo };

#[test]
fn algo() {
    ALGO.check();
}

const STRUCTURE_WITH_RULE_GROUPS: Suite =
    Suite { name: "structure-with-rule-groups", output: Output::Structure(RuleGroups::With) };

#[test]
fn structure_with_rule_groups() {
    STRUCTURE_WITH_RULE_GROUPS.check();
}

const STRUCTURE_WITHOUT_RULE_GROUPS: Suite =
    Suite { name: "structure-without-rule-groups", output: Output::Structure(RuleGroups::Without) };

#[test]
fn structure_without_rule_groups() {
    STRUCTURE_WITHOUT_RULE_GROUPS.check();
}

const PROSE: Suite = Suite { name: "prose", output: Output::Prose };

#[test]
fn prose() {
    PROSE.check();
}

const ADOC_EL: Suite = Suite { name: "adoc-el", output: Output::AdocEl };

#[test]
fn adoc_el() {
    ADOC_EL.check();
}

const ADOC_PL: Suite = Suite { name: "adoc-pl", output: Output::AdocPl };

#[test]
fn adoc_pl() {
    ADOC_PL.check();
}
