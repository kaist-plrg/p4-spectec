use p4spec_rust::lang::{
    common::{
        notation::{atom::Atom, mixop::Mixop},
        source::{Position, Span},
    },
    pl::annot::Hints,
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

#[test]
fn hint_lookup_returns_declaration_locations_across_kinds_and_case_shapes() {
    let mut henv = HEnv::default();
    henv.insert_func(&id("function"), Hints::default());
    henv.insert_rel(&id("relation"), Hints::default());
    henv.insert_case(&span("case-a"), &id("type"), &mixop("A", "case-a"), Hints::default());
    henv.insert_case(&span("case-b"), &id("type"), &mixop("B", "case-b"), Hints::default());

    assert_eq!(henv.get_func(&id("use")).unwrap().0, &span("function"));
    assert_eq!(henv.get_rel(&id("use")).unwrap().0, &span("relation"));
    assert_eq!(henv.get_case(&id("use"), &mixop("A", "use")).unwrap().0, &span("case-a"));
    assert_eq!(henv.get_case(&id("use"), &mixop("B", "use")).unwrap().0, &span("case-b"));
    assert!(henv.get_case(&id("use"), &mixop("C", "use")).is_none());
}

#[test]
fn replacing_hints_also_replaces_the_stored_declaration_location() {
    let mut henv = HEnv::default();
    henv.insert_func(&id("earlier"), Hints::default());
    henv.insert_func(&id("later"), Hints::default());
    assert_eq!(henv.get_func(&id("use")).unwrap().0, &span("later"));
}
