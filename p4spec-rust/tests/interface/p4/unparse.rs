use p4spec_rust::lang::data::value::ValueArena;

use p4spec_rust::{
    interface::p4::{error::P4UnparseError, unparse::P4Unparser},
    lang::{
        al,
        common::prim::num::Natural,
        common::{
            notation::{atom::Atom, mixfix::Mixfix},
            source::Span,
        },
        data::{
            typ,
            value::{Value, make},
        },
        el, il,
    },
};

fn id(name: &str) -> il::ast::Id {
    p4spec_rust::phrase! { node: name.to_owned(), span: Span::default() }
}

fn atom(name: &str) -> il::ast::Atom {
    p4spec_rust::phrase! {
        node: Atom::Keyword(name.to_owned()),
        span: Span::default(),
    }
}

fn hinted_def_type() -> il::ast::DefTyp {
    let notation = Mixfix::Seq(vec![Mixfix::Atom(atom("WRAP")), Mixfix::Arg(typ::make::text())]);
    let hint = p4spec_rust::phrase! {
        node: el::ast::ExpKind::Seq(vec![
            p4spec_rust::phrase! {
                node: el::ast::ExpKind::Text("show".to_owned()),
                span: Span::default(),
            },
            p4spec_rust::phrase! {
                node: el::ast::ExpKind::Hole(el::ast::Hole::Next),
                span: Span::default(),
            },
        ]),
        span: Span::default(),
    };
    p4spec_rust::phrase! {
        node: il::ast::DefTypKind::Variant(vec![il::ast::TypCase { not_typ: p4spec_rust::phrase! { node: notation, span: Span::default() }, typ_origin: p4spec_rust::phrase! {
                node: il::ast::TypOriginKind { id: id("Origin"), targs: Vec::new() },
                span: Span::default(),
            }, hints: vec![el::ast::Hint { id: id("print"), exp: hint }] }]),
        span: Span::default(),
    }
}

fn wrapped_text(arena: &mut ValueArena) -> Value {
    let wrapper_type = typ::make::var(id("Wrapper"), Vec::new());
    let value_case = Mixfix::Seq(vec![
        Mixfix::Atom(atom("WRAP")),
        Mixfix::Arg(make::text(arena, "payload".to_owned(), Span::default()).unwrap()),
    ]);
    make::case(arena, (wrapper_type).node.clone().into(), value_case, Span::default()).unwrap()
}

#[test]
fn test_unparses_scalar_and_container_values() {
    let mut arena = ValueArena::new();
    let unparser = P4Unparser::default();
    let span = Span::default();
    assert_eq!(
        {
            let value = &make::bool(&mut arena, true, span.clone()).unwrap();
            unparser.render(&arena, value)
        }
        .unwrap(),
        "true"
    );
    assert_eq!(
        {
            let value = &make::nat(&mut arena, Natural::from(42_u64), span.clone()).unwrap();
            unparser.render(&arena, value)
        }
        .unwrap(),
        "42"
    );
    assert_eq!(
        {
            let value = &make::text(&mut arena, "a\n\"b".into(), span.clone()).unwrap();
            unparser.render(&arena, value)
        }
        .unwrap(),
        "a\\n\\\"b"
    );

    let tuple_type = typ::make::tuple(vec![typ::make::bool(), typ::make::nat()]);
    let tuple = {
        let values = vec![
            make::bool(&mut arena, false, span.clone()).unwrap(),
            make::nat(&mut arena, 7_u64.into(), span.clone()).unwrap(),
        ];
        make::tuple(&mut arena, (tuple_type).node.clone().into(), values, span).unwrap()
    };
    assert_eq!(unparser.render(&arena, &tuple).unwrap(), "(false, 7)");
}

#[test]
fn test_unparses_non_ascii_text_as_decimal_bytes() {
    let mut arena = ValueArena::new();
    let value =
        make::text(&mut arena, "prefix◕‿◕😀ツsimple_table_1".to_owned(), Span::default()).unwrap();
    assert_eq!(
        P4Unparser::default().render(&arena, &value).unwrap(),
        "prefix\\226\\151\\149\\226\\128\\191\\226\\151\\149\\240\\159\\152\\128\\227\\131\\132simple_table_1"
    );
}

#[test]
fn test_unsupported_values_return_typed_errors() {
    let mut arena = ValueArena::new();
    let structure = make::structure(
        &mut arena,
        (typ::make::bool()).node.clone().into(),
        Vec::new(),
        Span::default(),
    )
    .unwrap();
    assert_eq!(
        P4Unparser::default().render(&arena, &structure),
        Err(P4UnparseError::ValueUnsupported("Struct"))
    );
}

#[test]
fn test_print_hints_are_loaded_from_al() {
    let mut arena = ValueArena::new();
    let al_spec = vec![p4spec_rust::phrase! {
        node: al::ast::DefKind::Typ(al::ast::TypDef::Defined(Box::new(al::ast::DefinedTyp {
            id: id("Wrapper"),
            tparams: Vec::new(),
            def_typ: hinted_def_type(),
            hints: Vec::new(),
        }))),
        span: Span::default(),
    }];
    let value = wrapped_text(&mut arena);

    assert_eq!(
        P4Unparser::from_al_spec(&al_spec).render(&arena, &value),
        Ok("show payload".to_owned())
    );
}
