use std::{
    cmp::Ordering,
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
};

use p4spec_rust::{
    lang::common::{
        notation::{atom::Atom, mixfix::Mixfix, mixop::Mixop},
        source::{NotePhrase, Phrase, Position, Span},
    },
    note_phrase, phrase,
};

fn atom(node: Atom) -> p4spec_rust::lang::common::notation::mixfix::AtomPhrase {
    phrase!(node: node, span: Span::default())
}

fn span(line: usize) -> Span {
    Span::new(Position::new("notation.spec", line, 2), Position::new("notation.spec", line, 5))
}

fn hash(value: &impl Hash) -> u64 {
    let mut hasher = DefaultHasher::new();
    value.hash(&mut hasher);
    hasher.finish()
}

#[test]
fn test_map_preserves_nested_labels_arguments_and_locations() {
    let arg_l: NotePhrase<u32, u8, u8> = note_phrase!(node: 17, note: 3_u8, span: 7_u8);
    let arg_r: NotePhrase<u32, u8, u8> = note_phrase!(node: 29, note: 5_u8, span: 9_u8);
    let mixfix = Mixfix::Brack(
        phrase!(node: Atom::LParen, span: span(11)),
        Box::new(Mixfix::Infix(
            Box::new(Mixfix::Arg(arg_l)),
            phrase!(node: Atom::Arrow, span: span(13)),
            Box::new(Mixfix::Seq(vec![
                Mixfix::Atom(phrase!(node: Atom::Keyword("tail".to_owned()), span: span(17))),
                Mixfix::Arg(arg_r),
            ])),
        )),
        phrase!(node: Atom::RParen, span: span(19)),
    );
    let mixfix_restored = mixfix.map(|arg| *arg);
    assert!(matches!(&mixfix_restored, Mixfix::Brack(_, mixfix, _)
        if matches!(mixfix.as_ref(), Mixfix::Infix(_, _, mixfix)
            if matches!(mixfix.as_ref(), Mixfix::Seq(_)))));

    assert_eq!(mixfix_restored.into_args(), vec![arg_l, arg_r]);
}

#[test]
fn test_syntax_comparisons_and_hashing_ignore_atom_spans() {
    let mixfix = Mixfix::Infix(
        Box::new(Mixfix::Arg(17)),
        phrase!(node: Atom::Arrow, span: span(11)),
        Box::new(Mixfix::Arg(29)),
    );
    let mixfix_relocated = Mixfix::Infix(
        Box::new(Mixfix::Arg(17)),
        phrase!(node: Atom::Arrow, span: span(37)),
        Box::new(Mixfix::Arg(29)),
    );

    assert_eq!(mixfix, mixfix_relocated);
    assert_eq!(mixfix.cmp(&mixfix_relocated), Ordering::Equal);
    assert_eq!(hash(&mixfix), hash(&mixfix_relocated));
    assert!(mixfix.eq_shape(&mixfix_relocated));
    assert!(mixfix.eq_by(&mixfix_relocated, PartialEq::eq));
    assert_eq!(mixfix.cmp_by(&mixfix_relocated, Ord::cmp), Ordering::Equal);

    let (mixop, args) = mixfix_relocated.split();
    let mixfix_filled = Mixop::fill(&mixop, args.into_iter().copied()).unwrap();
    assert_eq!(mixfix_filled, mixfix_relocated);

    let mixfix_changed = mixfix_relocated.map(|arg| arg + 1);
    assert!(!mixfix.eq_by(&mixfix_changed, PartialEq::eq));
    assert_eq!(mixfix.cmp_by(&mixfix_changed, Ord::cmp), Ordering::Less);
}

#[test]
fn test_into_args_preserves_nested_argument_order() {
    let mixfix = Mixfix::Seq(vec![
        Mixfix::Brack(atom(Atom::LParen), Box::new(Mixfix::Arg(1)), atom(Atom::RParen)),
        Mixfix::Infix(
            Box::new(Mixfix::Arg(2)),
            atom(Atom::Arrow),
            Box::new(Mixfix::Seq(vec![Mixfix::Arg(3), Mixfix::Arg(4)])),
        ),
    ]);

    let args = mixfix.into_args();

    assert_eq!(args, vec![1, 2, 3, 4]);
}

#[test]
fn test_eq_shape_borrows_nested_mixfixes_and_ignores_arguments() {
    let left = Mixfix::Infix(
        Box::new(Mixfix::Arg(1)),
        atom(Atom::Arrow),
        Box::new(Mixfix::Brack(atom(Atom::LParen), Box::new(Mixfix::Arg(2)), atom(Atom::RParen))),
    );
    let right = Mixfix::Infix(
        Box::new(Mixfix::Arg("left")),
        atom(Atom::Arrow),
        Box::new(Mixfix::Brack(
            atom(Atom::LParen),
            Box::new(Mixfix::Arg("right")),
            atom(Atom::RParen),
        )),
    );

    assert!(left.eq_shape(&right));
    assert!(!left.eq_shape(&Mixfix::<&str>::Seq(vec![Mixfix::Arg("left")])));
}

#[test]
fn test_at_distinguishes_outer_span_and_empty_notation_components() {
    use p4spec_rust::lang::traits::at::At;
    let mixfix: Mixfix<Phrase<usize>> = Mixfix::Brack(
        phrase!(node: Atom::LParen, span: span(11)),
        Box::new(Mixfix::Seq(vec![
            Mixfix::Seq(vec![]),
            Mixfix::Arg(phrase!(node: 0, span: span(13))),
        ])),
        phrase!(node: Atom::RParen, span: span(19)),
    );
    let not_typ = phrase!(node: mixfix, span: span(13));
    assert_eq!(not_typ.at(), span(13));
    assert_eq!(not_typ.node.at(), Span::new(span(11).left, span(19).right));
    assert_eq!(Mixfix::<Phrase<usize>>::Seq(vec![]).at(), Span::default());
}

#[test]
fn test_at_covers_notation_atoms_and_preserves_default_argument_spans() {
    use p4spec_rust::lang::traits::at::At;
    let mixfix: Mixfix<Phrase<usize>> = Mixfix::Infix(
        Box::new(Mixfix::Seq(vec![
            Mixfix::Atom(phrase!(node: Atom::Keyword("start".to_owned()), span: span(7))),
            Mixfix::Arg(phrase!(node: 0, span: span(13))),
        ])),
        phrase!(node: Atom::Arrow, span: span(23)),
        Box::new(Mixfix::Seq(vec![])),
    );
    assert_eq!(mixfix.at(), Span::new(span(7).left, span(23).right));
    let mixfix = Mixfix::Seq(vec![mixfix, Mixfix::Arg(phrase!(node: 1, span: Span::default()))]);
    assert_eq!(mixfix.at(), Span::new(Position::default(), span(23).right));
}

#[test]
fn test_try_map_preserves_order_locations_and_stops_on_error() {
    let mixfix = Mixfix::Brack(
        phrase!(node: Atom::LParen, span: span(3)),
        Box::new(Mixfix::Infix(
            Box::new(Mixfix::Arg(1)),
            atom(Atom::Arrow),
            Box::new(Mixfix::Seq(vec![Mixfix::Arg(2), Mixfix::Arg(3), Mixfix::Arg(4)])),
        )),
        phrase!(node: Atom::RParen, span: span(9)),
    );
    let mut args = Vec::new();
    let result = mixfix.try_map(|arg| {
        args.push(*arg);
        if *arg == 3 { Err("stop") } else { Ok(arg + 10) }
    });
    assert_eq!(result, Err("stop"));
    assert_eq!(args, vec![1, 2, 3]);
    let mixfix_result = mixfix.try_map(|arg| Ok::<_, ()>(arg + 10)).unwrap();
    assert_eq!(mixfix_result, mixfix.map(|arg| arg + 10));
    let Mixfix::Brack(atom_l, _, atom_r) = mixfix_result else { panic!("brackets") };
    assert_eq!(atom_l.span, span(3));
    assert_eq!(atom_r.span, span(9));
    let mixfix_atom: Mixfix<u32> = Mixfix::Atom(atom(Atom::Arrow));
    assert_eq!(mixfix_atom.try_map(|_| Err::<u32, _>("unreachable")), Ok(mixfix_atom));
}
