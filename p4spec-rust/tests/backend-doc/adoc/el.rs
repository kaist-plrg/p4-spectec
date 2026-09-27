use p4spec_rust::{
    backend_doc::adoc::el::render_def,
    lang::{
        common::source::Span,
        el::ast::{self, DefKind, ExpKind},
    },
};

fn render(source: &str) -> String {
    let spec = crate::spec_fixture::parse(source).expect("parse EL rendering fixture");
    assert_eq!(spec.len(), 1, "fixture should contain one definition");
    render_def(&spec[0])
}

#[test]
fn test_type_definitions_use_display_atoms() {
    assert_eq!(render("syntax wrapped<T> = `{ T `}"), "wrapped<T>\n    : { T }\n    ;",);
    assert_eq!(
        render("syntax value = _EMPTY | '+' nat"),
        "value\n    : /* empty */ | + nat\n    ;",
    );
}

#[test]
fn test_relations_and_rules_use_source_atoms() {
    assert_eq!(render("relation Eval: nat '++' nat"), "relation Eval:\n  nat '++' nat",);
    assert_eq!(
        render("rule Eval/step: xs '++' ys\n  -- if true"),
        "rule Eval/step:\n  xs '++' ys\n  -- if true",
    );
}

#[test]
fn test_long_function_definition_breaks_at_eighty_columns() {
    let source = concat!(
        "def $combine(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa, ",
        "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb, ",
        "cccccccccccccccccccccccccccccc) = result",
    );

    assert_eq!(
        render(source),
        concat!(
            "def $combine(\n",
            "    aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,\n",
            "    bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,\n",
            "    cccccccccccccccccccccccccccccc)\n",
            "  = result",
        ),
    );
}

#[test]
fn test_definitions_render_their_adoc_forms() {
    let source = concat!("tbl def $is_zero =\n", "  | 0 => true\n", "  | n => false",);

    assert_eq!(render(source), "tbl def $is_zero =\n  | 0 => true\n  | n => false",);
    assert_eq!(render("extern dec $load<T>(T) : bool"), "extern dec $load<T>(T) : bool");
    assert_eq!(render("var value : (nat, text)"), "var value : (nat, text)");
}

#[test]
fn test_arithmetic_escapes_keep_their_delimiters() {
    assert_eq!(render("def $succ(n) = $(n + 1)"), "def $succ(n) = $(n + 1)");
    assert_eq!(
        render("def $f(n) = n\n  -- if $(n < 1) /\\ $(n > 0)"),
        "def $f(n) = n\n  -- if $(n < 1) /\\ $(n > 0)",
    );
}

#[test]
fn test_text_literals_use_ocaml_compatible_escaping() {
    let def = p4spec_rust::phrase! {
        node: DefKind::FuncDef(ast::FuncDef {
            id: p4spec_rust::phrase! {
                node: "literal".to_owned(),
                span: Span::default(),
            },
            tparams: vec![],
            args: vec![],
            exp: p4spec_rust::phrase! {
                node: ExpKind::Text("quote \" slash \\ tab \t newline \n snowman ☃".to_owned()),
                span: Span::default(),
            },
            prems: vec![],
        }),
        span: Span::default(),
    };

    assert_eq!(
        render_def(&def),
        "def $literal() = \"quote \\\" slash \\\\ tab \\t newline \\n snowman \\226\\152\\131\"",
    );
}

#[test]
fn test_wide_list_renders_and_drops_on_a_small_stack() {
    std::thread::Builder::new()
        .stack_size(2 * 1024 * 1024)
        .spawn(|| {
            let exp = p4spec_rust::phrase! {
                node: ExpKind::List((0..10_000).map(|_| p4spec_rust::phrase! {
                    node: ExpKind::Bool(true),
                    span: Span::default(),
                }).collect()),
                span: Span::default(),
            };
            let def = p4spec_rust::phrase! {
                node: DefKind::FuncDef(ast::FuncDef {
                    id: p4spec_rust::phrase! {
                        node: "wide".to_owned(),
                        span: Span::default(),
                    },
                    tparams: vec![],
                    args: vec![],
                    exp,
                    prems: vec![],
                }),
                span: Span::default(),
            };
            let text = render_def(&def);
            assert!(text.starts_with("def $wide()"));
            assert_eq!(text.matches("true").count(), 10_000);
            assert!(text.ends_with(']'));
        })
        .unwrap()
        .join()
        .unwrap();
}
