//! Public specification transformation entry points
//!
//! Exercises ordered file input, stage errors, and rule-group preservation
//! through the library API used by host applications.

use std::path::{Path, PathBuf};

use p4spec_rust::{
    Error,
    diagnostic::{Report, ReportKind},
    lang::traits::print::Print,
};

fn fixture(path: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures")
        .join(path)
}

fn print_spec<Spec: Print>(
    (result, warnings): (Result<Spec, Error>, Vec<Report>),
) -> (Result<String, Error>, Vec<Report>) {
    (result.map(|spec| Print::to_string(&spec)), warnings)
}

#[test]
fn transformations_preserve_input_order() {
    let paths = [fixture("cli/second.watsup"), fixture("cli/simple.watsup")];
    let outputs = [
        Print::to_string(&p4spec_rust::parse(&paths).unwrap()),
        Print::to_string(&p4spec_rust::elab(&paths).unwrap()),
        Print::to_string(&p4spec_rust::algo(&paths).unwrap()),
        Print::to_string(&p4spec_rust::structure(&paths, true).unwrap()),
        Print::to_string(&p4spec_rust::prosify(&paths).unwrap()),
    ];
    for output in outputs {
        let lines: Vec<_> = output.lines().filter(|line| !line.is_empty()).collect();
        assert_eq!(lines, ["var y : nat", "var x : nat"]);
    }
}

#[test]
fn pipeline_errors_preserve_the_failing_stage_and_location() {
    let path = fixture("frontend/negative/malformed-token.watsup");
    let report = p4spec_rust::prosify(std::slice::from_ref(&path)).unwrap_err();
    let ReportKind::Cause(diagnostic) = &report.kind else { panic!("expected diagnostic cause") };
    assert_eq!(diagnostic.code.as_deref(), Some("parse/character-invalid"));
    assert_eq!(diagnostic.labels[0].span.left.file.as_ref(), path.to_str().unwrap());
    assert_eq!(diagnostic.labels[0].span.left.line, 1);
    assert_eq!(diagnostic.labels[0].span.left.column, 0);
    assert_eq!(diagnostic.labels[0].span.right.column, 1);

    let path = fixture("elaboration/operator_not_defined.watsup");
    let report = p4spec_rust::prosify(std::slice::from_ref(&path)).unwrap_err();
    let mut reports = vec![report.as_ref()];
    let mut located = false;
    while let Some(report) = reports.pop() {
        if let ReportKind::Cause(diagnostic) = &report.kind {
            located |= diagnostic.source == "elab"
                && diagnostic.code.as_deref() == Some("elab/operator-binop-type-mismatch")
                && diagnostic
                    .labels
                    .iter()
                    .any(|label| label.span.left.file.as_ref() == path.to_str().unwrap());
        }
        reports.extend(&report.children);
    }
    assert!(located, "elaboration must retain original spans");

    let path = fixture("algorithmic/impure_else_premises.watsup");
    let report = p4spec_rust::prosify(std::slice::from_ref(&path)).unwrap_err();
    let ReportKind::Cause(diagnostic) = &report.kind else { panic!("expected diagnostic cause") };
    assert_eq!(diagnostic.code.as_deref(), Some("algo/otherwise-condition-invalid"));
    assert_eq!(diagnostic.labels[0].span.left.file.as_ref(), path.to_str().unwrap());
    assert_eq!(diagnostic.labels[0].span.left.line, 8);
}

#[test]
fn structure_exposes_rule_group_preservation() {
    let paths = [fixture("structure/definitions.watsup")];
    for without_rule_groups in [false, true] {
        let spec_sl = p4spec_rust::structure(&paths, without_rule_groups).unwrap();
        assert_eq!(Print::to_string(&spec_sl).contains("Group "), !without_rule_groups);
    }
    // Prose conversion requires the relation's rule groups
    let spec_pl = p4spec_rust::prosify(&paths).unwrap();
    let output = Print::to_string(&spec_pl);
    assert!(output.contains("Group ret:"), "{output}");
    assert!(output.contains("Group else:"), "{output}");
}

#[test]
fn transformations_return_ordered_warnings_on_success() {
    let paths = [fixture("structure/definitions.watsup")];
    let outputs = [
        print_spec(p4spec_rust::elab_with_warnings(&paths)),
        print_spec(p4spec_rust::algo_with_warnings(&paths)),
        print_spec(p4spec_rust::structure_with_warnings(&paths, false)),
        print_spec(p4spec_rust::structure_with_warnings(&paths, true)),
        print_spec(p4spec_rust::prosify_with_warnings(&paths)),
    ];
    let texts_expect = [
        Print::to_string(&p4spec_rust::elab(&paths).unwrap()),
        Print::to_string(&p4spec_rust::algo(&paths).unwrap()),
        Print::to_string(&p4spec_rust::structure(&paths, false).unwrap()),
        Print::to_string(&p4spec_rust::structure(&paths, true).unwrap()),
        Print::to_string(&p4spec_rust::prosify(&paths).unwrap()),
    ];
    for ((result, warnings), text_expect) in outputs.into_iter().zip(texts_expect) {
        assert_eq!(result.unwrap(), text_expect);
        let codes: Vec<_> = warnings
            .iter()
            .map(|report| {
                let ReportKind::Cause(diagnostic) = &report.kind else {
                    panic!("expected warning cause")
                };
                diagnostic.code.as_deref().unwrap()
            })
            .collect();
        assert_eq!(codes, ["elab/relation-rule-missing", "elab/function-clause-missing"]);
    }
}

#[test]
fn transformations_keep_committed_warnings_on_elaboration_failure() {
    let path = std::env::temp_dir()
        .join(format!("p4spec-pipeline-warning-elab-error-{}.watsup", std::process::id()));
    std::fs::write(&path, "relation R: nat |- nat\ndef $missing = 0\n").unwrap();
    let outputs = [
        print_spec(p4spec_rust::elab_with_warnings(std::slice::from_ref(&path))),
        print_spec(p4spec_rust::algo_with_warnings(std::slice::from_ref(&path))),
        print_spec(p4spec_rust::structure_with_warnings(std::slice::from_ref(&path), true)),
        print_spec(p4spec_rust::prosify_with_warnings(std::slice::from_ref(&path))),
    ];
    std::fs::remove_file(path).unwrap();
    for (result, warnings) in outputs {
        let report = result.unwrap_err();
        let ReportKind::Cause(diagnostic) = &report.kind else { panic!("diagnostic cause") };
        assert_eq!(diagnostic.code.as_deref(), Some("elab/function-declaration-required"));
        assert_eq!(diagnostic.source, "elab");
        assert_eq!(warnings.len(), 1);
        let ReportKind::Cause(diagnostic) = &warnings[0].kind else {
            panic!("expected committed warning cause")
        };
        assert_eq!(diagnostic.code.as_deref(), Some("elab/relation-input-hint-missing"));
    }
}

#[test]
fn transformations_keep_warnings_on_algorithmic_failure() {
    let path = std::env::temp_dir()
        .join(format!("p4spec-pipeline-warning-algo-error-{}.watsup", std::process::id()));
    std::fs::write(&path, "dec $missing : nat\n").unwrap();
    let paths = [path.clone(), fixture("algorithmic/impure_else_premises.watsup")];
    let outputs = [
        print_spec(p4spec_rust::algo_with_warnings(&paths)),
        print_spec(p4spec_rust::structure_with_warnings(&paths, true)),
        print_spec(p4spec_rust::prosify_with_warnings(&paths)),
    ];
    std::fs::remove_file(path).unwrap();
    for (result, warnings) in outputs {
        let report = result.unwrap_err();
        let ReportKind::Cause(diagnostic) = &report.kind else { panic!("diagnostic cause") };
        assert_eq!(diagnostic.code.as_deref(), Some("algo/otherwise-condition-invalid"));
        assert_eq!(diagnostic.source, "algo");
        assert_eq!(warnings.len(), 1);
        let ReportKind::Cause(diagnostic) = &warnings[0].kind else {
            panic!("expected elaboration warning cause")
        };
        assert_eq!(diagnostic.code.as_deref(), Some("elab/function-clause-missing"));
    }
}

#[test]
fn transformations_return_no_warnings_on_frontend_failure() {
    let paths = [fixture("frontend/negative/malformed-token.watsup")];
    let outputs = [
        print_spec(p4spec_rust::elab_with_warnings(&paths)),
        print_spec(p4spec_rust::algo_with_warnings(&paths)),
        print_spec(p4spec_rust::structure_with_warnings(&paths, true)),
        print_spec(p4spec_rust::prosify_with_warnings(&paths)),
    ];
    for (result, warnings) in outputs {
        let report = result.unwrap_err();
        let ReportKind::Cause(diagnostic) = &report.kind else { panic!("diagnostic cause") };
        assert_eq!(diagnostic.code.as_deref(), Some("parse/character-invalid"));
        assert_eq!(diagnostic.source, "parse");
        assert!(warnings.is_empty());
    }
}

#[test]
fn transformations_preserve_structuring_reports_and_prior_warnings() {
    let paths =
        [fixture("structure/definitions.watsup"), fixture("structure/generic-subtype.watsup")];
    for (result, warnings) in [
        print_spec(p4spec_rust::structure_with_warnings(&paths, true)),
        print_spec(p4spec_rust::structure_with_warnings(&paths, false)),
        print_spec(p4spec_rust::prosify_with_warnings(&paths)),
    ] {
        assert_eq!(warnings.len(), 2);
        let report = result.unwrap_err();
        let ReportKind::Cause(diagnostic) = &report.kind else { panic!("diagnostic cause") };
        assert_eq!(diagnostic.code.as_deref(), Some("structure/type-operation-invalid"));
        assert_eq!(diagnostic.source, "structure");
        assert_eq!(diagnostic.labels[0].span.left.file.as_ref(), paths[1].to_str().unwrap());
        assert_eq!(diagnostic.labels[0].span.left.line, 4);
    }
}
