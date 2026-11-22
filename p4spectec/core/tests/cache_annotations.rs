//! Demonstrate annotation-dependent pure host calls through a cached definition.
use p4spectec::{
    SpecLang,
    lang::{
        common::source::Span,
        data::{
            arena::Arena,
            typ,
            value::flat::{self as value, Value},
        },
        il::ast::Typ,
    },
    runner::{
        Config, Eval, Extern, ExternError, Interface, Interpreter, NullInterface, Runner,
        RunnerContext, SideEffect,
    },
};
use std::path::PathBuf;

struct NoteHost;
impl Extern for NoteHost {
    fn eval_rel<I, F>(
        _: &mut RunnerContext<'_, I, F, Self>,
        _: &str,
        _: &[Value],
    ) -> Eval<Vec<Value>, ExternError>
    where
        F: Interface,
        I: Interpreter<F, Self>,
    {
        Eval::new(
            Err(ExternError::diagnostic_message("unexpected relation")),
            SideEffect::Impure,
        )
    }
    fn eval_func<I, F>(
        ctx: &mut RunnerContext<'_, I, F, Self>,
        name: &str,
        _: &[Typ],
        values: &[Value],
    ) -> Eval<Value, ExternError>
    where
        F: Interface,
        I: Interpreter<F, Self>,
    {
        let [arg] = values else {
            panic!("expected one argument")
        };
        let inspected = match name {
            "inspect_note" => *arg,
            "inspect_nested" => {
                let [inner] = value::get::list(ctx.arena(), arg).unwrap() else {
                    panic!("expected one nested list")
                };
                *inner
            }
            _ => panic!("unexpected host function"),
        };
        let is_bool_list = ctx.arena().value.typ(inspected.note).as_ref()
            == &typ::make::list(typ::make::bool()).node;
        let result = value::make::bool(ctx.arena_mut(), is_bool_list, Span::default())
            .map_err(ExternError::from);
        Eval::new(result, SideEffect::Pure)
    }
    fn clear(&mut self) {}
}

#[test]
fn nested_annotations_matter_even_with_identical_outer_annotations() {
    for (stage, cache) in [SpecLang::Al, SpecLang::Sl, SpecLang::Pl]
        .into_iter()
        .flat_map(|stage| [false, true].map(move |cache| (stage, cache)))
    {
        let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("tests/fixtures/cache-annotations.watsup");
        let (spec, warnings) = p4spectec::runner_spec_with_warnings(stage, &[path]);
        assert!(warnings.is_empty(), "{warnings:?}");
        let mut arena = Arena::new();
        let mut runner = Runner::new(
            &mut arena,
            spec.unwrap(),
            Config::new(cache, false, true, false),
            NullInterface,
            NoteHost,
        )
        .unwrap();
        let nat_type = typ::make::list(typ::make::nat());
        let bool_type = typ::make::list(typ::make::bool());
        let outer_type = typ::make::list(typ::make::list(typ::make::int()));
        let nat_inner =
            value::make::list(&mut arena, nat_type.node.into(), vec![], Span::default()).unwrap();
        let bool_inner =
            value::make::list(&mut arena, bool_type.node.into(), vec![], Span::default()).unwrap();
        let nat_outer = value::make::list(
            &mut arena,
            outer_type.node.clone().into(),
            vec![nat_inner],
            Span::default(),
        )
        .unwrap();
        let bool_outer = value::make::list(
            &mut arena,
            outer_type.node.into(),
            vec![bool_inner],
            Span::default(),
        )
        .unwrap();
        assert_eq!(
            arena.value.typ(nat_outer.note),
            arena.value.typ(bool_outer.note)
        );
        assert_eq!(
            arena.value.canon_id(nat_outer.node),
            arena.value.canon_id(bool_outer.node)
        );
        let first = runner
            .eval_func(&mut arena, "forward_nested", &[], &[nat_outer])
            .result
            .unwrap();
        let second = runner
            .eval_func(&mut arena, "forward_nested", &[], &[bool_outer])
            .result
            .unwrap();
        assert!(!value::get::bool(&arena, &first).unwrap());
        assert!(value::get::bool(&arena, &second).unwrap());
    }
}

#[test]
fn pure_external_host_can_observe_input_annotation() {
    for (stage, cache) in [SpecLang::Al, SpecLang::Sl, SpecLang::Pl]
        .into_iter()
        .flat_map(|stage| [false, true].map(move |cache| (stage, cache)))
    {
        let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("tests/fixtures/cache-annotations.watsup");
        let (spec, warnings) = p4spectec::runner_spec_with_warnings(stage, &[path]);
        assert!(warnings.is_empty(), "{warnings:?}");
        let mut arena = Arena::new();
        let mut runner = Runner::new(
            &mut arena,
            spec.unwrap(),
            Config::new(cache, false, true, false),
            NullInterface,
            NoteHost,
        )
        .unwrap();
        let nat_type = typ::make::list(typ::make::nat());
        let bool_type = typ::make::list(typ::make::bool());
        let nat_input =
            value::make::list(&mut arena, nat_type.node.into(), vec![], Span::default()).unwrap();
        let bool_input =
            value::make::list(&mut arena, bool_type.node.into(), vec![], Span::default()).unwrap();
        let first = runner
            .eval_func(&mut arena, "forward_note", &[], &[nat_input])
            .result
            .unwrap();
        let second = runner
            .eval_func(&mut arena, "forward_note", &[], &[bool_input])
            .result
            .unwrap();
        assert!(!value::get::bool(&arena, &first).unwrap());
        assert!(value::get::bool(&arena, &second).unwrap());
    }
}

#[test]
fn relation_cache_respects_input_annotations() {
    for (stage, cache) in [SpecLang::Al, SpecLang::Sl, SpecLang::Pl]
        .into_iter()
        .flat_map(|stage| [false, true].map(move |cache| (stage, cache)))
    {
        let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("tests/fixtures/cache-annotations.watsup");
        let (spec, warnings) = p4spectec::runner_spec_with_warnings(stage, &[path]);
        assert!(warnings.is_empty(), "{warnings:?}");
        let mut arena = Arena::new();
        let mut runner = Runner::new(
            &mut arena,
            spec.unwrap(),
            Config::new(cache, false, true, false),
            NullInterface,
            NoteHost,
        )
        .unwrap();
        let nat_type = typ::make::list(typ::make::nat());
        let bool_type = typ::make::list(typ::make::bool());
        let nat_input = value::make::list(
            &mut arena,
            nat_type.node.clone().into(),
            vec![],
            Span::default(),
        )
        .unwrap();
        let bool_input = value::make::list(
            &mut arena,
            bool_type.node.clone().into(),
            vec![],
            Span::default(),
        )
        .unwrap();
        let first = runner
            .eval_rel(&mut arena, "Echo", &[nat_input])
            .result
            .unwrap();
        let second = runner
            .eval_rel(&mut arena, "Echo", &[bool_input])
            .result
            .unwrap();
        assert_eq!(arena.value.typ(first[0].note).as_ref(), &nat_type.node);
        assert_eq!(arena.value.typ(second[0].note).as_ref(), &bool_type.node);
    }
}
