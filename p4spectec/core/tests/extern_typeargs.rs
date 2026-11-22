//! Type arguments passed to external functions at each executable stage.
use p4spectec::{
    SpecLang,
    lang::{
        common::source::Span,
        data::{
            arena::Arena,
            typ::{self, TypKind},
            value::flat::{self as value, Value},
        },
        il::ast::Typ,
    },
    runner::{
        Config, Eval, Extern, ExternError, Interface, Interpreter, NullInterface, Runner,
        RunnerContext, SideEffect,
    },
};
use std::{cell::RefCell, path::PathBuf, rc::Rc};

struct RecordingHost(Rc<RefCell<Vec<(String, Vec<TypKind>)>>>);

impl Extern for RecordingHost {
    fn eval_rel<I, F>(
        _: &mut RunnerContext<'_, I, F, Self>,
        _: &str,
        _: &[Value],
    ) -> Eval<Vec<Value>, ExternError>
    where
        F: Interface,
        I: Interpreter<F, Self>,
    {
        Eval::new(Err(ExternError::diagnostic_message("unexpected relation")), SideEffect::Pure)
    }

    fn eval_func<I, F>(
        ctx: &mut RunnerContext<'_, I, F, Self>,
        name: &str,
        targs: &[Typ],
        _: &[Value],
    ) -> Eval<Value, ExternError>
    where
        F: Interface,
        I: Interpreter<F, Self>,
    {
        ctx.external()
            .0
            .borrow_mut()
            .push((name.to_owned(), targs.iter().map(|typ| typ.node.clone()).collect()));
        Eval::new(
            value::make::bool(ctx.arena_mut(), true, Span::default()).map_err(ExternError::from),
            SideEffect::Pure,
        )
    }

    fn clear(&mut self) {}
}

fn check_stage(stage: SpecLang, tco: bool) {
    let path =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/extern-typeargs.watsup");
    let (spec, warnings) = p4spectec::runner_spec_with_warnings(stage, &[path]);
    assert!(warnings.is_empty(), "{warnings:?}");
    let mut arena = Arena::new();
    let calls = Rc::new(RefCell::new(Vec::new()));
    let mut runner = Runner::new(
        &mut arena,
        spec.unwrap(),
        Config::new(false, false, true, tco),
        NullInterface,
        RecordingHost(calls.clone()),
    )
    .unwrap();
    let nested_empty = value::make::list(
        &mut arena,
        typ::make::list(typ::make::list(typ::make::bool()))
            .node
            .into(),
        vec![],
        Span::default(),
    )
    .unwrap();
    let empty = value::make::list(
        &mut arena,
        typ::make::list(typ::make::bool()).node.into(),
        vec![],
        Span::default(),
    )
    .unwrap();
    for (name, type_args, values) in [
        ("inspect", vec![typ::make::nat()], vec![empty]),
        ("through", vec![typ::make::bool()], vec![empty]),
        ("through_compound", vec![typ::make::bool()], vec![nested_empty]),
        ("pair", vec![typ::make::bool(), typ::make::nat()], vec![]),
        ("without_types", vec![], vec![]),
    ] {
        let result = runner.eval_func(&mut arena, name, &type_args, &values);
        assert!(value::get::bool(&arena, &result.result.unwrap()).unwrap());
    }
    assert_eq!(
        *calls.borrow(),
        vec![
            ("inspect".to_owned(), vec![typ::make::nat().node]),
            ("inspect".to_owned(), vec![typ::make::bool().node]),
            ("inspect".to_owned(), vec![typ::make::list(typ::make::bool()).node]),
            ("pair".to_owned(), vec![typ::make::bool().node, typ::make::nat().node]),
            ("without_types".to_owned(), vec![]),
        ]
    );
}

#[test]
fn al_forwards_external_type_arguments() {
    for tco in [false, true] {
        check_stage(SpecLang::Al, tco);
    }
}
#[test]
fn sl_forwards_external_type_arguments() {
    for tco in [false, true] {
        check_stage(SpecLang::Sl, tco);
    }
}
#[test]
fn pl_forwards_external_type_arguments() {
    for tco in [false, true] {
        check_stage(SpecLang::Pl, tco);
    }
}
