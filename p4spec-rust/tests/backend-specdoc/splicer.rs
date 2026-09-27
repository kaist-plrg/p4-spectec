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
