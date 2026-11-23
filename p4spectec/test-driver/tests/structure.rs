//! SL conversion errors of specification sources
//!
//! Each fixture under `tests/p4spectec/structure/neg` must convert to AL and fail to structure,
//! with rule groups and without them alike;
//! its error is compared with the snapshot `<stem>.snap` beside it.

use p4spectec::{diagnostic::RenderConfig, pass};

use p4spectec_test::harness::fixture::{check, fixtures, must_pass};

// = Suites

/// Whether structuring keeps rule groups.
#[derive(Clone, Copy)]
enum RuleGroups {
    With,
    Without,
}

/// One directory under `tests/p4spectec/`.
struct Fixtures {
    /// Holds the fixtures, relative to `tests/p4spectec/`.
    dir: &'static str,
    /// Names whether structuring keeps rule groups.
    rule_groups: RuleGroups,
}

impl Fixtures {
    /// Structures each fixture, which must fail, and compares the error with its snapshot.
    fn check(&self) {
        let without_rule_groups = matches!(self.rule_groups, RuleGroups::Without);
        for path in fixtures(self.dir, &[".watsup"]) {
            let spec_al =
                must_pass(&path, "AL conversion", p4spectec::algo(std::slice::from_ref(&path)));
            match pass::structure::convert(spec_al, without_rule_groups) {
                Ok(_) => panic!("{}: structuring unexpectedly passed", path.display()),
                Err(report) => {
                    check(&path.with_extension("snap"), &[*report], &RenderConfig::default())
                }
            }
        }
    }
}

// = Tests

const NEG_WITH_RULE_GROUPS: Fixtures =
    Fixtures { dir: "structure/neg", rule_groups: RuleGroups::With };

#[test]
fn neg_with_rule_groups() {
    NEG_WITH_RULE_GROUPS.check();
}

const NEG_WITHOUT_RULE_GROUPS: Fixtures =
    Fixtures { dir: "structure/neg", rule_groups: RuleGroups::Without };

#[test]
fn neg_without_rule_groups() {
    NEG_WITHOUT_RULE_GROUPS.check();
}
