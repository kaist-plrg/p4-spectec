mod render;
mod report;

use p4spec_rust::{
    diagnostic::{Diagnostic, Label, LabelStyle, Report, ReportKind, Severity},
    lang::common::source::{Position, Span},
};

fn span(file: &str, line_l: usize, col_l: usize, line_r: usize, col_r: usize) -> Span {
    Span::new(Position::new(file, line_l, col_l), Position::new(file, line_r, col_r))
}

fn report(span: Span) -> Report {
    Diagnostic {
        severity: Severity::Error,
        code: Some("parse/text-escape-invalid".to_owned()),
        message: "invalid escape in text literal".to_owned(),
        labels: vec![Label {
            style: LabelStyle::Primary,
            span,
            message: "invalid escape".to_owned(),
        }],
        notes: vec!["use a supported escape".to_owned()],
        source: "parse",
    }
    .into()
}

fn cause(report: &Report) -> &Diagnostic {
    let ReportKind::Cause(diagnostic) = &report.kind else { panic!("expected a cause") };
    diagnostic
}

fn cause_mut(report: &mut Report) -> &mut Diagnostic {
    let ReportKind::Cause(diagnostic) = &mut report.kind else { panic!("expected a cause") };
    diagnostic
}

#[test]
fn parser_failures_reach_cli_with_their_codes_and_spans() {
    let fixtures = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("test-driver/expected/diagnostic/parse");
    let cases = [
        ("parse-illegal-escape.watsup", "parse/text-escape-invalid", "3:5", "^^"),
        (
            "parse-relation-body-must-be-notation.watsup",
            "parse/relation-signature-invalid",
            "3:13",
            "^^^",
        ),
    ];
    let commands: &[&[&str]] = &[
        &["elab"],
        &["algo"],
        &["struct"],
        &["prose"],
        &["run", "--al", "--rel", "Pass", "-p", "unused.p4"],
        &["run", "--sl", "--rel", "Pass", "-p", "unused.p4"],
        &["run", "--pl", "--rel", "Pass", "-p", "unused.p4"],
        &["sim", "--al", "--arch", "ebpf", "-p", "unused.p4", "--stf", "unused.stf"],
        &["sim", "--sl", "--arch", "ebpf", "-p", "unused.p4", "--stf", "unused.stf"],
        &["sim", "--pl", "--arch", "ebpf", "-p", "unused.p4", "--stf", "unused.stf"],
    ];
    for (file, code, loc, underline) in cases {
        for args in commands {
            let output = std::process::Command::new(env!("CARGO_BIN_EXE_p4spec-rust"))
                .args(*args)
                .arg(fixtures.join(file))
                .output()
                .unwrap();
            assert_eq!(output.status.code(), Some(1));
            assert!(output.stdout.is_empty());
            let text = String::from_utf8(output.stderr).unwrap();
            assert!(text.contains(&format!("error[{code}]")), "{text}");
            assert!(text.contains(&format!("{file}:{loc}")), "{text}");
            assert!(text.contains(underline), "{text}");
            assert!(!text.contains('\u{1b}'), "{text}");
        }
    }
}
