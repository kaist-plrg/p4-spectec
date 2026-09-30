use p4spec_rust::diagnostic::ReportKind;
use p4spec_rust::lang::traits::print::Print;
use p4spec_rust::stf::{
    ast::{
        Action, Argument, Condition, CounterCheck, CounterKind, CounterTarget, MatchKind, Name,
        Statement, TableMatch,
    },
    r#match, parse,
};
use std::path::{Path, PathBuf};

#[test]
fn test_parses_commands_in_source_order() {
    let source = r#"
        packet 1 001122aB
        expect 2 0011**ab$
        add ingress.ipv4_lpm 10 hdr.ipv4.dstAddr:0x0a000001/24 set_nhop(port:3) = entry0
        check_counter c_packets(4) packets >= 0x10
        wait
    "#;

    let statements = parse::parse_str("commands.stf", source).expect("valid STF");
    assert_eq!(statements.len(), 5);
    assert_eq!(
        statements[0].node,
        Statement::Packet { port: "1".into(), packet: "001122aB".into() }
    );
    assert_eq!(
        statements[1].node,
        Statement::Expect {
            port: "2".into(),
            packet_expected: Some("0011**ab".into()),
            exact: true,
        }
    );
    assert_eq!(
        statements[2].node,
        Statement::Add {
            table: "ingress.ipv4_lpm".into(),
            priority: Some(10),
            matches: vec![TableMatch {
                name: "hdr.ipv4.dstAddr".into(),
                kind: MatchKind::Slash("0x0a000001".into(), "24".into()),
            }],
            action: Action {
                name: "set_nhop".into(),
                args: vec![Argument { id: "port".into(), num: "3".into() }],
            },
            id: Some("entry0".into()),
        }
    );
    assert_eq!(
        statements[3].node,
        Statement::CheckCounter {
            counter: "c_packets".into(),
            target: CounterTarget::Index("4".into()),
            check: CounterCheck {
                kind: Some(CounterKind::Packets),
                condition: Condition::Ge,
                num: "0x10".into(),
            },
        }
    );
    assert!(
        statements
            .windows(2)
            .all(|pair| pair[0].span.left <= pair[1].span.left)
    );
    assert_eq!(statements[0].span.left.line, 2);
}

#[test]
fn test_parses_packet_wildcards_and_comments() {
    let source = "# generated\nexpect 0 0a**??ff\nno_packet\n";
    let statements = parse::parse_str("wildcards.stf", source).expect("valid STF");
    assert_eq!(
        statements[0].node,
        Statement::Expect {
            port: "0".into(),
            packet_expected: Some("0a**ff".into()),
            exact: false,
        }
    );
    assert_eq!(statements[1].node, Statement::NoPacket);
}

#[test]
fn test_reports_filename_line_and_column() {
    let error = parse::parse_str("bad.stf", "packet port nope\n").unwrap_err();
    let ReportKind::Cause(diagnostic) = &error.kind else { panic!("expected cause") };
    assert_eq!(diagnostic.labels[0].span.left.file.as_ref(), "bad.stf");
    assert_eq!(diagnostic.labels[0].span.left.line, 1);
}

#[test]
fn test_rejects_priorities_outside_the_ocaml_integer_range() {
    let source = "add table 4611686018427387904 field:1 action()\n";

    let error = parse::parse_str("priority.stf", source).expect_err("priority overflow");

    let ReportKind::Cause(diagnostic) = &error.kind else { panic!("expected cause") };
    assert_eq!(diagnostic.code.as_deref(), Some("stf/priority-out-of-bounds"));
}

#[test]
fn test_rejects_digits_outside_the_selected_radix() {
    for num in ["0b102", "12b", "0x0g"] {
        let source = format!("register_read r {num}\n");
        let error = parse::parse_str("number.stf", &source).expect_err(num);
        let ReportKind::Cause(diagnostic) = &error.kind else { panic!("expected cause") };
        assert_eq!(diagnostic.code.as_deref(), Some("stf/number-invalid"));
        assert!(diagnostic.message.ends_with(num));
        assert_eq!(diagnostic.labels[0].span.left.file.as_ref(), "number.stf");
        assert_eq!(diagnostic.labels[0].span.left.line, 1);
    }
}

#[test]
fn test_transforms_names_matches_and_actions() {
    let name = Name::from("pipe0.tbl").rewrite_substring(&["pipe"], "ingress");
    assert_eq!(name.as_str(), "ingress.tbl");

    let name = Name::from("foo.MyIngress.bar").replace_substring(&["myingress"], "ingress");
    assert_eq!(name.as_str(), "foo.ingress.bar");

    let table_match =
        TableMatch { name: "hdr.$valid$".into(), kind: MatchKind::Number("1".into()) }
            .rewrite_valid();
    assert_eq!(table_match.name.as_str(), "hdr.isValid()");

    let action = Action { name: "MyIngress.ipv4.set_port".into(), args: vec![] }
        .replace_substring(&["myingress"], "ingress")
        .into_unqualified();
    assert_eq!(action.name.as_str(), "set_port");
}

#[test]
fn test_compares_wildcard_packets_and_prints_statements() {
    assert!(r#match::matches("a01f", "a**f"));
    assert!(!r#match::matches("a01f", "a*f"));

    let action = Action { name: "drop".into(), args: vec![] };
    let statement = Statement::SetDefault { table: "ingress.tbl".into(), action: action.clone() };
    assert_eq!(Print::to_string(&statement), "setdefault \"ingress.tbl\" \"drop\"()");
    assert_eq!(Print::to_string(&action), "\"drop\"()");
    let program = parse::parse_str("print.stf", "wait\nno_packet\n").unwrap();
    assert_eq!(Print::to_string(&program), "wait\nno_packet");
}

#[test]
fn test_prints_empty_node_port_list_with_trailing_separator() {
    let statement = Statement::McNodeCreate { replication_id: "1".into(), ports: vec![] };

    assert_eq!(Print::to_string(&statement), "mc_node_create 1 ");
}

#[test]
fn test_parses_repository_stf_corpus() {
    let repository = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let mut files = Vec::new();
    collect_stf(&repository.join("p4spec/test"), &mut files);
    collect_stf(&repository.join("testdata"), &mut files);
    files.sort();
    assert!(!files.is_empty());

    let mut failures = Vec::new();
    for file in &files {
        if let Err(error) = parse::parse_file(file) {
            failures.push(format!("{}: {error}", file.display()));
        }
    }
    assert!(
        failures.is_empty(),
        "failed to parse {} of {} STF files:\n{}",
        failures.len(),
        files.len(),
        failures.join("\n")
    );
}

fn collect_stf(directory: &Path, files: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(directory) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_stf(&path, files);
        } else if path.extension().is_some_and(|extension| extension == "stf") {
            files.push(path);
        }
    }
}

#[test]
fn test_numeric_summary_retains_diagnostic_code() {
    let error = parse::parse_str("number.stf", "register_read r 0b102\n").unwrap_err();
    assert!(error.to_string().starts_with("error[stf/number-invalid]:"));
}

#[test]
fn test_missing_file_is_input_failure_without_source_occurrence() {
    let error = parse::parse_file("/definitely/missing/p4spec-input.stf").unwrap_err();
    let ReportKind::Cause(diagnostic) = &error.kind else { panic!("expected cause") };
    assert_eq!(diagnostic.code.as_deref(), Some("stf/input-unreadable"));
    assert_eq!(
        diagnostic.labels[0].span.left.file.as_ref(),
        "/definitely/missing/p4spec-input.stf"
    );
    assert_eq!(diagnostic.labels[0].span.left.line, 0);
    assert_eq!(diagnostic.labels[0].span.left, diagnostic.labels[0].span.right);
}

#[test]
fn test_lexical_and_grammar_failures_preserve_diagnostic_metadata() {
    use p4spec_rust::diagnostic::{LabelStyle, Severity};

    for (source, code, message, col_l, col_r) in [
        ("@", "stf/character-invalid", "invalid character '@'", 0, 1),
        (
            "\"unterminated",
            "stf/quoted-identifier-incomplete",
            "unterminated quoted identifier",
            0,
            13,
        ),
        ("setdefault tab", "stf/token-unexpected", "unexpected token", 14, 14),
    ] {
        let report = parse::parse_str("invalid.stf", source).unwrap_err();
        let ReportKind::Cause(diagnostic) = &report.kind else { panic!("expected cause") };
        assert_eq!(diagnostic.source, "stf");
        assert_eq!(diagnostic.severity, Severity::Error);
        assert_eq!(diagnostic.code.as_deref(), Some(code));
        assert_eq!(diagnostic.message, message);
        assert!(diagnostic.notes.is_empty());
        assert!(report.children.is_empty());
        assert_eq!(diagnostic.labels.len(), 1);
        let label = &diagnostic.labels[0];
        assert_eq!(label.style, LabelStyle::Primary);
        assert!(!label.line_only);
        assert_eq!(label.span.left.file.as_ref(), "invalid.stf");
        assert_eq!(label.span.left.line, 1);
        assert_eq!(label.span.right.line, 1);
        assert_eq!(label.span.left.column, col_l);
        assert_eq!(label.span.right.column, col_r);
    }
}
