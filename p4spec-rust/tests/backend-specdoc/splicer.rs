use p4spec_rust::backend_specdoc::splicer::{parser, source::Source};

#[test]
fn marker_parser_preserves_ocaml_identifier_and_group_grammar() {
    let mut source = Source::new("fixture.adoc", "${syntax: A_b x.y f' g` h-*}");
    assert!(!parser::parse_splice_start(&mut source, "func-source"));
    assert!(parser::parse_splice_start(&mut source, "syntax"));
    assert_eq!(parser::parse_ids(&mut source).unwrap(), ["A_b", "x.y", "f'", "g`", "h-*"]);
    assert!(source.eos());
    let mut source = Source::new("fixture.adoc", " Rel/group trailing");
    assert_eq!(parser::parse_id_with_sub(&mut source).unwrap(), ("Rel".into(), "group".into()));
    assert_eq!(source.remaining(), "trailing");
}

#[test]
fn identifier_error_retains_byte_position_after_unicode() {
    let mut source = Source::new("fixture.adoc", "한글\n  !");
    source.advn("한글\n  ".len());
    let error = parser::parse_ids(&mut source).unwrap_err();
    assert_eq!(error.span().left.line, 2);
    assert_eq!(error.span().left.column, 2);
    assert_eq!(error.to_string(), "cannot parse identifier");
}

use p4spec_rust::backend_specdoc::splicer::splice_strings;

#[test]
fn source_splice_matches_ocaml_fixture() {
    let spec_el = crate::spec_fixture::parse("syntax Flag = nat\nrelation Check : CHECK\nrulegroup Check/value {\nrule Check/ok: x\n}\ndec $identity(nat) : nat\ndef $identity(x) = x\ntbl def $truth = | 0 => false\n").unwrap();
    let skeleton = "SYNTAX\n${syntax: Flag}\nRELATION\n${relation-title-source: Check}\nRULE\n${rulegroup-source: Check/value}\nFUNCTION TITLE\n${func-title-source: identity}\nFUNCTION\n${func-source: identity}\nTABLE\n${table-source: truth}";
    let (result, _) = splice_strings(&spec_el, &Vec::new(), &[("fixture.adoc", skeleton)]);
    let texts = result.unwrap();
    assert_eq!(
        format!("{}\n", texts[0]),
        include_str!("../../../p4spec/test/backend-splice/source.expected")
    );
}

#[test]
fn plain_text_unknown_markers_and_unicode_remain_unchanged() {
    let text = "한글\r\n${unknown: anything}\n끝";
    let (result, _) = splice_strings(&Vec::new(), &Vec::new(), &[("fixture.adoc", text)]);
    assert_eq!(result.unwrap(), [text]);
}

fn specs() -> (p4spec_rust::lang::el::ast::Spec, p4spec_rust::lang::pl::ast::Spec) {
    use p4spec_rust::pass::{algo, elaborate, prosify, structure};
    let spec_el =
        crate::spec_fixture::parse(include_str!("../fixtures/splicer/spec.watsup")).unwrap();
    let spec_il = elaborate::convert(spec_el.clone()).unwrap();
    let spec_al = algo::convert(spec_il).unwrap();
    let spec_sl = structure::convert(spec_al, false).unwrap();
    let spec_pl = prosify::convert(spec_sl).unwrap();
    (spec_el, spec_pl)
}

#[test]
fn all_eighteen_markers_match_ocaml_output() {
    let (spec_el, spec_pl) = specs();
    let skeleton = include_str!("../fixtures/splicer/skeleton.adoc");
    let (result, _) = splice_strings(&spec_el, &spec_pl, &[("fixture.adoc", skeleton)]);
    let texts = result.unwrap();
    assert_eq!(texts[0], include_str!("../fixtures/splicer/expected.adoc"));
    let (result, _) = splice_strings(&spec_el, &spec_pl, &[("fixture.adoc", skeleton)]);
    assert_eq!(texts, result.unwrap());
    let (result, _) = splice_strings(&spec_el, &spec_pl, &[("fixture.adoc", &texts[0])]);
    assert_eq!(texts, result.unwrap());
}

fn warning_messages(warnings: &[p4spec_rust::diagnostic::Report]) -> Vec<&str> {
    warnings
        .iter()
        .map(|report| match &report.kind {
            p4spec_rust::diagnostic::ReportKind::Cause(diagnostic) => diagnostic.message.as_str(),
            _ => panic!("expected splice warning"),
        })
        .collect()
}

#[test]
fn anchors_resolve_across_files_and_are_emitted_only_once() {
    let (spec_el, spec_pl) = specs();
    let sources = [
        ("body.adoc", "${func-prose: identity}\n${func-latex: identity}"),
        ("titles.adoc", "${func-title-prose: identity identity}\n${func-title-latex: identity}"),
        ("duplicate.adoc", "${func-title-prose: identity}"),
    ];
    let (result, warnings) = splice_strings(&spec_el, &spec_pl, &sources);
    let texts = result.unwrap();
    assert!(texts[0].contains("xref:function_prose_identity["));
    assert_eq!(
        texts
            .concat()
            .matches("id=\"function_prose_identity\"")
            .count(),
        1
    );
    assert_eq!(
        texts
            .concat()
            .matches("id=\"function_latex_identity\"")
            .count(),
        1
    );
    assert!(warning_messages(&warnings).contains(&"duplicate func-title-prose target: identity"));
}

#[test]
fn missing_keys_warn_and_usage_stays_separate_by_marker() {
    let (spec_el, spec_pl) = specs();
    let (result, warnings) = splice_strings(
        &spec_el,
        &spec_pl,
        &[("fixture.adoc", "${func-source: missing identity missing}")],
    );
    assert!(result.unwrap()[0].contains("def $identity"));
    let messages = warning_messages(&warnings);
    assert_eq!(
        messages
            .iter()
            .filter(|message| **message == "func-source splice key not found: missing")
            .count(),
        2
    );
    assert!(messages.contains(&"unused 0 func-source splices out of 1 (0.00%)"));
    assert!(messages.contains(&"unused 1 func-latex splices out of 1 (100.00%)"));
    assert!(messages.contains(&"unused 1 func-prose splices out of 1 (100.00%)"));
}

struct Directory(std::path::PathBuf);
impl Directory {
    fn new() -> Self {
        static NEXT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        let num = NEXT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let path = std::env::temp_dir().join(format!("p4spec-splice-{}-{num}", std::process::id()));
        std::fs::create_dir(&path).unwrap();
        Self(path)
    }
    fn path(&self, name: &str) -> std::path::PathBuf {
        self.0.join(name)
    }
}
impl Drop for Directory {
    fn drop(&mut self) {
        std::fs::remove_dir_all(&self.0).unwrap();
    }
}

#[test]
fn file_batch_preserves_all_outputs_after_a_late_parse_error() {
    use p4spec_rust::backend_specdoc::splicer::splice_files;
    let dir = Directory::new();
    let path_a = dir.path("a.adoc");
    let path_b = dir.path("b.adoc");
    let text_a = "untouched ${syntax: absent}";
    let text_b = "bad ${syntax: !}";
    std::fs::write(&path_a, text_a).unwrap();
    std::fs::write(&path_b, text_b).unwrap();
    let (result, warnings) = splice_files(
        &Vec::new(),
        &Vec::new(),
        &[(path_a.clone(), path_a.clone()), (path_b.clone(), path_b.clone())],
    );
    assert!(result.is_err());
    assert!(warning_messages(&warnings).contains(&"syntax splice key not found: absent"));
    assert_eq!(std::fs::read_to_string(&path_a).unwrap(), text_a);
    assert_eq!(std::fs::read_to_string(&path_b).unwrap(), text_b);
    assert_eq!(std::fs::read_dir(&dir.0).unwrap().count(), 2);
}

#[test]
fn file_batch_reads_all_inputs_before_replacing_any_output() {
    use p4spec_rust::backend_specdoc::splicer::splice_files;
    let dir = Directory::new();
    let path_a = dir.path("a.adoc");
    let path_b = dir.path("b.adoc");
    let path_c = dir.path("nested/c.adoc");
    std::fs::write(&path_a, "A").unwrap();
    std::fs::write(&path_b, "B").unwrap();
    let (result, _) = splice_files(
        &Vec::new(),
        &Vec::new(),
        &[(path_a, path_b.clone()), (path_b.clone(), path_c.clone())],
    );
    result.unwrap();
    assert_eq!(std::fs::read_to_string(path_b).unwrap(), "A");
    assert_eq!(std::fs::read_to_string(path_c).unwrap(), "B");
}

#[test]
fn unused_keys_are_sorted_and_grouped_by_five() {
    let spec_el = crate::spec_fixture::parse("extern syntax z\nextern syntax c\nextern syntax b\nextern syntax g\nextern syntax a\nextern syntax f\nextern syntax e\n").unwrap();
    let (result, warnings) = splice_strings(&spec_el, &Vec::new(), &[]);
    assert!(result.unwrap().is_empty());
    let messages = warning_messages(&warnings);
    assert_eq!(messages[0], "unused 7 syntax splices out of 7 (100.00%)");
    assert_eq!(messages[1], "\ta, b, c, e, f");
    assert_eq!(messages[2], "\tg, z");
}

#[test]
fn malformed_markers_fail_but_rulegroup_omits_its_closing_brace() {
    let (spec_el, spec_pl) = specs();
    for text in ["${syntax: flag", "${syntax: ${syntax: flag}}", "${rulegroup-source: Check/}"] {
        let (result, _) = splice_strings(&spec_el, &spec_pl, &[("fixture.adoc", text)]);
        assert!(result.is_err(), "{text}");
    }
    let (result, _) =
        splice_strings(&spec_el, &spec_pl, &[("fixture.adoc", "${rulegroup-source: Check/value")]);
    assert!(result.unwrap()[0].contains("rule Check/on:"));
}

#[test]
fn last_definition_wins_without_changing_marker_order() {
    let spec_el =
        crate::spec_fixture::parse("syntax A = nat\nsyntax A = bool\nsyntax B = text\n").unwrap();
    let (result, _) =
        splice_strings(&spec_el, &Vec::new(), &[("fixture.adoc", "${syntax: B A B}")]);
    let text = &result.unwrap()[0];
    assert!(text.contains("B = text\n\nA = bool\n\nB = text"));
    assert_eq!(text.matches("id=\"B\"").count(), 1);
}

#[test]
fn latex_failure_preserves_files_and_el_source_span() {
    use p4spec_rust::{backend_specdoc::splicer::splice_files, lang::el::ast as el};
    let (mut spec_el, spec_pl) = specs();
    let mut span = None;
    for def_el in &mut spec_el {
        if let el::DefKind::FuncDef(def) = &mut def_el.node {
            span = Some(def.exp.span.clone());
            def.exp.node = el::ExpKind::Latex("unchecked".into());
            break;
        }
    }
    let dir = Directory::new();
    let path_a = dir.path("a.adoc");
    let path_b = dir.path("b.adoc");
    std::fs::write(&path_a, "${syntax: flag}").unwrap();
    std::fs::write(&path_b, "${func-latex: identity}").unwrap();
    let (result, _) = splice_files(
        &spec_el,
        &spec_pl,
        &[(path_a.clone(), path_a.clone()), (path_b.clone(), path_b.clone())],
    );
    assert_eq!(result.unwrap_err().span(), span.unwrap());
    assert_eq!(std::fs::read_to_string(path_a).unwrap(), "${syntax: flag}");
    assert_eq!(std::fs::read_to_string(path_b).unwrap(), "${func-latex: identity}");
}

#[test]
fn staging_failure_leaves_existing_outputs_and_removes_temporary_files() {
    use p4spec_rust::backend_specdoc::splicer::splice_files;
    let dir = Directory::new();
    let path_input = dir.path("input.adoc");
    let path_output = dir.path("output.adoc");
    let path_blocked = dir.path("blocked");
    std::fs::write(&path_input, "new").unwrap();
    std::fs::write(&path_output, "old").unwrap();
    std::fs::write(&path_blocked, "file").unwrap();
    let (result, _) = splice_files(
        &Vec::new(),
        &Vec::new(),
        &[(path_input.clone(), path_output.clone()), (path_input, path_blocked.join("out.adoc"))],
    );
    assert!(result.is_err());
    assert_eq!(std::fs::read_to_string(path_output).unwrap(), "old");
    assert_eq!(std::fs::read_dir(&dir.0).unwrap().count(), 3);
}

#[test]
fn prose_counters_continue_across_markers_and_reset_between_runs() {
    use p4spec_rust::lang::pl::ast as pl;
    let (spec_el, mut spec_pl) = specs();
    for def_pl in &mut spec_pl {
        if let pl::DefKind::MetaFunc(pl::MetaFuncDef::Defined(func)) = &mut def_pl.node.node {
            let blocks = vec![func.block.clone(), func.block.clone()];
            func.block = vec![p4spec_rust::annotated_note_phrase! {
                node: pl::InstrKind::Tier(pl::TierInstr {
                    tier: pl::GroupInstr::Backtrack(pl::BacktrackInstr { blocks }),
                }),
                note: None,
                span: Default::default(),
            }];
        }
    }
    let sources = [("a.adoc", "${func-prose: identity}"), ("b.adoc", "${func-prose: identity}")];
    let (result, _) = splice_strings(&spec_el, &spec_pl, &sources);
    let texts = result.unwrap();
    assert!(texts[0].contains("bk-identity-1-arm-1"));
    assert!(texts[1].contains("bk-identity-2-arm-1"));
    let (result, _) = splice_strings(&spec_el, &spec_pl, &sources);
    assert_eq!(texts, result.unwrap());
}

#[cfg(unix)]
#[test]
fn inplace_splice_preserves_symlink_and_target_permissions() {
    use p4spec_rust::backend_specdoc::splicer::splice_files;
    use std::os::unix::fs::{PermissionsExt, symlink};
    let dir = Directory::new();
    let path_target = dir.path("target.adoc");
    let path_link = dir.path("link.adoc");
    std::fs::write(&path_target, "${syntax: absent}").unwrap();
    std::fs::set_permissions(&path_target, std::fs::Permissions::from_mode(0o640)).unwrap();
    symlink("target.adoc", &path_link).unwrap();
    let (result, _) =
        splice_files(&Vec::new(), &Vec::new(), &[(path_link.clone(), path_link.clone())]);
    result.unwrap();
    assert!(
        std::fs::symlink_metadata(&path_link)
            .unwrap()
            .file_type()
            .is_symlink()
    );
    assert_eq!(std::fs::read_to_string(&path_target).unwrap(), "[source,bison]\n----\n\n----");
    assert_eq!(std::fs::metadata(path_target).unwrap().permissions().mode() & 0o777, 0o640);
}

#[cfg(unix)]
#[test]
fn output_symlink_can_create_its_missing_target() {
    use p4spec_rust::backend_specdoc::splicer::splice_files;
    use std::os::unix::fs::symlink;
    let dir = Directory::new();
    let path_input = dir.path("input.adoc");
    let path_target = dir.path("generated.adoc");
    let path_link = dir.path("out.adoc");
    std::fs::write(&path_input, "new output").unwrap();
    symlink("generated.adoc", &path_link).unwrap();
    let (result, _) = splice_files(&Vec::new(), &Vec::new(), &[(path_input, path_link.clone())]);
    result.unwrap();
    assert!(
        std::fs::symlink_metadata(path_link)
            .unwrap()
            .file_type()
            .is_symlink()
    );
    assert_eq!(std::fs::read_to_string(path_target).unwrap(), "new output");
}

#[cfg(unix)]
#[test]
fn output_symlink_cycle_fails_without_creating_temporary_files() {
    use p4spec_rust::backend_specdoc::splicer::splice_files;
    use std::os::unix::fs::symlink;
    let dir = Directory::new();
    let path_input = dir.path("input.adoc");
    let path_link = dir.path("out.adoc");
    std::fs::write(&path_input, "new output").unwrap();
    symlink("./out.adoc", &path_link).unwrap();
    let (result, _) = splice_files(&Vec::new(), &Vec::new(), &[(path_input, path_link.clone())]);
    assert!(result.is_err());
    assert_eq!(std::fs::read_link(path_link).unwrap(), std::path::Path::new("./out.adoc"));
    assert_eq!(std::fs::read_dir(&dir.0).unwrap().count(), 2);
}
