use p4spec_rust::lang::{
    common::{
        notation::{atom::Atom, mixop::Mixop},
        source::{Position, Span},
    },
    pl::annot::{Hints, HintsKind},
    sl::ast::Id,
};
use p4spec_rust::runtime::envs::prosify::HEnv;

fn span(file: &str) -> Span {
    Span::new(Position::new(file, 1, 0), Position::new(file, 1, 4))
}

fn id(file: &str) -> Id {
    p4spec_rust::phrase! { node: "same".to_owned(), span: span(file) }
}

fn mixop(name: &str, file: &str) -> Mixop {
    Mixop::Atom(p4spec_rust::phrase! { node: Atom::Keyword(name.to_owned()), span: span(file) })
}

fn hints(file: &str) -> Hints {
    p4spec_rust::phrase! { node: HintsKind::default(), span: span(file) }
}

#[test]
fn hint_lookup_returns_declaration_locations_across_kinds_and_case_shapes() {
    let mut henv = HEnv::default();
    henv.insert_func(&id("function"), hints("function"));
    henv.insert_rel(&id("relation"), hints("relation"));
    henv.insert_case(&id("type"), &mixop("A", "case-a"), hints("case-a"));
    henv.insert_case(&id("type"), &mixop("B", "case-b"), hints("case-b"));

    assert_eq!(henv.get_func(&id("use")).unwrap().span, span("function"));
    assert_eq!(henv.get_rel(&id("use")).unwrap().span, span("relation"));
    assert_eq!(henv.get_case(&id("use"), &mixop("A", "use")).unwrap().span, span("case-a"));
    assert_eq!(henv.get_case(&id("use"), &mixop("B", "use")).unwrap().span, span("case-b"));
    assert!(henv.get_case(&id("use"), &mixop("C", "use")).is_none());
}

#[test]
fn replacing_hints_also_replaces_the_stored_declaration_location() {
    let mut henv = HEnv::default();
    henv.insert_func(&id("earlier"), hints("earlier"));
    henv.insert_func(&id("later"), hints("later"));
    assert_eq!(henv.get_func(&id("use")).unwrap().span, span("later"));
}
