use p4spec_rust::diagnostic::ReportKind;
use p4spec_rust::lang::data::value::ValueArena;
use p4spec_rust::{
    interface::p4::{
        error::P4Error,
        parse::{parse_file, parse_string},
        unparse::P4Unparser,
    },
    lang::data::{
        typ::TypKind,
        value::{Value, ValueKind},
    },
};

fn first_binary<'a>(arena: &'a ValueArena, value: &'a Value) -> Option<&'a Value> {
    if let TypKind::Var(id, _) = arena.typ(value).as_ref()
        && id.node == "binaryExpression"
    {
        return Some(value);
    }
    match arena.kind(value) {
        ValueKind::Case(case) => case
            .args()
            .into_iter()
            .find_map(|value| first_binary(arena, value)),
        _ => None,
    }
}

fn binary_part<'a>(arena: &'a ValueArena, value: &'a Value, index: usize) -> &'a Value {
    match arena.kind(value) {
        ValueKind::Case(case) => case.args().into_iter().nth(index).unwrap(),
        _ => panic!("binary expression must be a case value"),
    }
}

fn operator<'a>(arena: &'a ValueArena, value: &'a Value) -> String {
    P4Unparser::default()
        .render(arena, binary_part(arena, value, 1))
        .unwrap()
}

#[test]
fn test_right_shift_preserves_source_parser_asymmetric_bitwise_binding() {
    let mut arena = ValueArena::new();
    let program_before = parse_string(
        &mut arena,
        "shift.p4",
        "control C() { apply { bit<4> x; x = 4w1 | 4w2 ^ 4w3 & 4w4 >> 4w5; } }",
    )
    .unwrap();
    let shift = first_binary(&arena, &program_before).unwrap();
    assert_eq!(operator(&arena, shift), ">>");
    let bit_or = binary_part(&arena, shift, 0);
    assert_eq!(operator(&arena, bit_or), "|");
    let bit_xor = binary_part(&arena, bit_or, 2);
    assert_eq!(operator(&arena, bit_xor), "^");
    assert_eq!(operator(&arena, binary_part(&arena, bit_xor, 2)), "&");

    let program_after = parse_string(
        &mut arena,
        "shift.p4",
        "control C() { apply { bit<4> x; x = 4w1 >> 4w2 & 4w3 ^ 4w4 | 4w5; } }",
    )
    .unwrap();
    let bit_or = first_binary(&arena, &program_after).unwrap();
    assert_eq!(operator(&arena, bit_or), "|");
    let bit_xor = binary_part(&arena, bit_or, 0);
    assert_eq!(operator(&arena, bit_xor), "^");
    let bit_and = binary_part(&arena, bit_xor, 0);
    assert_eq!(operator(&arena, bit_and), "&");
    assert_eq!(operator(&arena, binary_part(&arena, bit_and, 0)), ">>");
}

#[test]
fn test_right_and_left_shift_share_left_associative_source_precedence() {
    let mut arena = ValueArena::new();
    let program_right_then_left = parse_string(
        &mut arena,
        "shift.p4",
        "control C() { apply { bit<4> x; x = 4w1 >> 4w2 << 4w3; } }",
    )
    .unwrap();
    let left_shift = first_binary(&arena, &program_right_then_left).unwrap();
    assert_eq!(operator(&arena, left_shift), "<<");
    assert_eq!(operator(&arena, binary_part(&arena, left_shift, 0)), ">>");

    let program_left_then_right = parse_string(
        &mut arena,
        "shift.p4",
        "control C() { apply { bit<4> x; x = 4w1 << 4w2 >> 4w3; } }",
    )
    .unwrap();
    let right_shift = first_binary(&arena, &program_left_then_right).unwrap();
    assert_eq!(operator(&arena, right_shift), ">>");
    assert_eq!(operator(&arena, binary_part(&arena, right_shift, 0)), "<<");
}

#[test]
fn test_binary_expression_span_preserves_mapped_token_order() {
    let mut arena = ValueArena::new();
    let program = parse_string(
        &mut arena,
        "preprocessed.p4",
        r#"control C() { apply { bit<4> x; x =
# 200 "later.p4"
4w1
# 10 "earlier.p4"
& 4w2; } }"#,
    )
    .unwrap();
    let binary = first_binary(&arena, &program).unwrap();

    assert_eq!(arena.span(binary).left.file.as_ref(), "later.p4");
    assert_eq!(arena.span(binary).left.line, 200);
    assert_eq!(arena.span(binary).right.file.as_ref(), "earlier.p4");
    assert_eq!(arena.span(binary).right.line, 10);
}

use std::{
    fs,
    path::{Path, PathBuf},
};

/// Draw a single-line progress bar to stderr. libtest only captures the
/// `print!`/`eprint!` macros, so a direct `io::stderr()` write stays visible
/// while the oracle grinds through the p4c corpus (run with `--nocapture`).
#[test]
fn test_parses_empty_and_declaration_programs() {
    let mut arena = ValueArena::new();
    for source in [
        "",
        "const bit<8> width = 8w3;",
        "type bit<8> PortId;",
        "header H { bit<8> field; }",
        "control C() { apply { } }",
        "parser P() { state start { transition accept; } }",
    ] {
        let program = parse_string(&mut arena, "fixture.p4", source)
            .unwrap_or_else(|error| panic!("failed to parse {source:?}: {error}"));
        assert!(matches!(arena.kind(&program), ValueKind::Case(_)));
        assert_eq!(arena.span(&program).left.file.as_ref(), "fixture.p4");
    }
}

#[test]
fn test_syntax_errors_retain_the_source_location() {
    let mut arena = ValueArena::new();
    let error = parse_string(&mut arena, "broken.p4", "const bit<8> x = ;")
        .expect_err("reject a missing initializer");
    let ReportKind::Cause(diagnostic) = &error.report().kind else { panic!("expected cause") };
    assert!(matches!(error, P4Error::Syntax(_)));
    assert_eq!(diagnostic.labels[0].span.left.file.as_ref(), "broken.p4");
    assert_eq!(diagnostic.labels[0].span.left.line, 1);
}

#[test]
fn test_parses_nested_conditionals_and_switch_fallthrough() {
    let mut arena = ValueArena::new();
    let source = r#"
control C() {
    apply {
        bool comparison = a < b > (c);
        if (true) if (false) exit; else exit;
        switch (1) {
            1:
            2: { exit; }
        }
    }
}
"#;
    parse_string(&mut arena, "control.p4", source).expect("parse nested control flow");
}

#[test]
fn test_type_argument_calls_preserve_bodies_across_whitespace_and_comments() {
    for (text_a, text_b) in [
        ("obj.f<bit<8>>(0);", "obj.f /* call */ < /* type */ bit<8> > /* args */ (0);"),
        ("f<E>();", "f < E > ();"),
        ("f<int, bool>();", "f < int, bool > ();"),
        ("bool x = a < b > (c);", "bool x = a /* lhs */ < b > (c);"),
        ("bool x = a < E.A > (c);", "bool x = a /* lhs */ < E.A > (c);"),
    ] {
        let mut arena = ValueArena::new();
        let text_a = format!("enum E {{ A }} control C() {{ apply {{ {text_a} }} }}");
        let text_b = format!("enum E {{ A }} control C() {{ apply {{ {text_b} }} }}");
        let value_a = parse_string(&mut arena, "calls.p4", &text_a).unwrap();
        let value_b = parse_string(&mut arena, "calls.p4", &text_b).unwrap();
        assert_eq!(arena.canon_id(&value_a), arena.canon_id(&value_b));
    }
}

#[test]
fn test_classifies_names_after_preceding_declarations_reduce() {
    let mut arena = ValueArena::new();
    let source = r#"
control Inner() { apply {} }
package Outer(Inner inner);
Outer(Inner()) main;
"#;

    parse_string(&mut arena, "lookahead.p4", source)
        .expect("parse newly declared constructor names");
}

#[test]
fn test_parses_the_positive_p4_corpus() {
    let mut arena = ValueArena::new();
    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let corpus = manifest.join("../p4spec/test/micro");
    let includes = [manifest.join("../p4c/p4include")];
    let mut files = Vec::new();
    for directory in
        ["programs", "programs-boot", "programs-neg", "sim-ebpf", "sim-psa", "sim-v1model"]
    {
        collect_p4_files(&corpus.join(directory), &mut files);
    }
    files.sort();
    assert!(!files.is_empty(), "the P4 corpus must be present");

    let failures: Vec<_> = files
        .iter()
        .filter_map(|file| {
            parse_file(&mut arena, &includes, file)
                .err()
                .map(|error| format!("{}: {error}", file.display()))
        })
        .collect();
    assert!(failures.is_empty(), "P4 parse failures:\n{}", failures.join("\n"));
}

#[test]
fn test_rejects_the_negative_p4_parse_corpus() {
    let mut arena = ValueArena::new();
    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let corpus = manifest.join("../p4spec/test/micro/programs-parse-neg");
    let includes = [manifest.join("../p4c/p4include")];
    let mut files = Vec::new();
    collect_p4_files(&corpus, &mut files);
    files.sort();
    assert!(!files.is_empty(), "the negative P4 corpus must be present");

    let accepted: Vec<_> = files
        .iter()
        .filter_map(|file| match parse_file(&mut arena, &includes, file) {
            Ok(_) => Some(file.display().to_string()),
            Err(P4Error::Syntax(_)) => None,
            Err(error) => panic!("input setup failed for {}: {error}", file.display()),
        })
        .collect();
    assert!(accepted.is_empty(), "invalid P4 programs were accepted:\n{}", accepted.join("\n"));
}

fn collect_p4_files(directory: &Path, files: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(directory).expect("read P4 corpus directory") {
        let path = entry.expect("read P4 corpus entry").path();
        if path.extension().is_some_and(|extension| extension == "p4") {
            files.push(path);
        }
    }
}

#[test]
fn test_empty_productions_use_previous_token_end_across_whitespace() {
    let mut arena = ValueArena::new();
    use p4spec_rust::lang::{
        common::source::Span,
        data::{typ::TypKind, value::Value},
    };
    fn spans<'a>(arena: &'a ValueArena, value: &'a Value, name: &str, output: &mut Vec<Span>) {
        if let TypKind::Var(id, _) = arena.typ(value).as_ref()
            && id.node == name
        {
            output.push(arena.span(value).clone());
        }
        if let ValueKind::Case(case) = arena.kind(value) {
            for value in case.args() {
                spans(arena, value, name, output);
            }
        }
    }
    let source = "control C() {\n  action a( \n    bit<8> x) { }\n  apply { }\n}";
    let value = parse_string(&mut arena, "empty.p4", source).unwrap();
    let mut directions = Vec::new();
    spans(&arena, &value, "direction", &mut directions);
    assert_eq!(directions.len(), 1);
    assert_eq!((directions[0].left.line, directions[0].left.column), (2, 11));
    assert_eq!(directions[0].left, directions[0].right);
    let mut annotations = Vec::new();
    spans(&arena, &value, "annotationList", &mut annotations);
    assert!(
        annotations
            .iter()
            .any(|span| span.left.line == 1 && span.left.column == 13)
    );
    assert!(annotations.iter().all(|span| span.left == span.right));
    let mut names = Vec::new();
    spans(&arena, &value, "identifier", &mut names);
    assert!(
        names
            .iter()
            .any(|span| span.left.line == 3 && span.left.column == 11 && span.right.column == 12)
    );
}

#[test]
fn test_initial_empty_production_precedes_whitespace_and_line_directives() {
    let mut arena = ValueArena::new();
    use p4spec_rust::lang::{
        common::source::Position,
        data::{typ::TypKind, value::Value},
    };
    fn initial_annotation<'a>(arena: &'a ValueArena, value: &'a Value) -> Option<&'a Value> {
        if let TypKind::Var(id, _) = arena.typ(value).as_ref()
            && id.node == "annotationList"
        {
            return Some(value);
        }
        match arena.kind(value) {
            ValueKind::Case(case) => case
                .args()
                .into_iter()
                .find_map(|value| initial_annotation(arena, value)),
            _ => None,
        }
    }
    for prefix in ["\n  ", "# 20 \"included.p4\"\n  "] {
        let source = format!("{prefix}control C() {{ apply {{ }} }}");
        let value = parse_string(&mut arena, "initial.p4", &source).unwrap();
        let annotation = initial_annotation(&arena, &value).unwrap();
        assert_eq!(arena.span(annotation).left, Position::new("initial.p4", 1, 0));
        assert_eq!(arena.span(annotation).right, arena.span(annotation).left);
    }
}

#[test]
fn test_syntax_error_after_whitespace_uses_offending_token_span() {
    let mut arena = ValueArena::new();
    let source = "\n const bit<8> x =   ;";
    let error = parse_string(&mut arena, "syntax.p4", source).unwrap_err();
    let ReportKind::Cause(diagnostic) = &error.report().kind else { panic!("expected cause") };
    let column = source.lines().nth(1).unwrap().find(';').unwrap();
    assert_eq!(
        (diagnostic.labels[0].span.left.line, diagnostic.labels[0].span.left.column),
        (2, column)
    );
    assert_eq!(
        (diagnostic.labels[0].span.right.line, diagnostic.labels[0].span.right.column),
        (2, column + 1)
    );
}

#[test]
fn test_syntax_summary_retains_diagnostic_code() {
    let mut arena = ValueArena::new();
    let error = parse_string(&mut arena, "broken.p4", "const bit<8> x = ;").unwrap_err();
    assert!(error.to_string().starts_with("error[p4/syntax-invalid]:"));
}

#[test]
fn test_line_markers_preserve_logical_span_without_original_source_columns() {
    let mut arena = ValueArena::new();
    let source = "# 42 \"original.p4\"\nconst bit<8> x = ;";
    let error = parse_string(&mut arena, "expanded.p4", source).unwrap_err();
    let ReportKind::Cause(diagnostic) = &error.report().kind else { panic!("expected cause") };
    let label = &diagnostic.labels[0];
    assert!(label.line_only);
    assert_eq!(label.span.left.file.as_ref(), "original.p4");
    assert_eq!((label.span.left.line, label.span.left.column), (42, 17));
    assert_eq!((label.span.right.line, label.span.right.column), (42, 18));

    let mut renderer = p4spec_rust::diagnostic::Renderer::new(Default::default());
    renderer.insert_source("original.p4", "unrelated original text");
    let text = renderer.render_to_string(error.report()).unwrap();
    assert!(text.contains("original.p4:42"), "{text}");
    assert!(!text.contains("unrelated original text"), "{text}");
}

#[test]
fn test_hash_in_comments_does_not_disable_direct_source_locations() {
    let mut arena = ValueArena::new();
    let source = "/* # 42 \"original.p4\" */\nconst bit<8> x = ;";
    let error = parse_string(&mut arena, "direct.p4", source).unwrap_err();
    let ReportKind::Cause(diagnostic) = &error.report().kind else { panic!("expected cause") };
    assert!(!diagnostic.labels[0].line_only);
    assert_eq!(diagnostic.labels[0].span.left.file.as_ref(), "direct.p4");
    assert_eq!(diagnostic.labels[0].span.left.line, 2);
}
