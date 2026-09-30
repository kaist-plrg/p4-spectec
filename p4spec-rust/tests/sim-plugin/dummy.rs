use p4spec_rust::lang::data::value::ValueArena;
use p4spec_rust::{
    lang::common::source::Span,
    lang::data::{
        typ::TypKind,
        value::{Value, ValueKind, get, make},
    },
    sim_plugin::dummy::Dummy,
    util::json::json,
};

use super::{has_extern_failure, parse_program, repo, runner, runner_from_spec};

fn contains_null_object_state(arena: &ValueArena, value: &Value) -> bool {
    let is_null_object_state = matches!(
        (arena.typ(value).as_ref(), arena.kind(value)),
        (TypKind::Var(id, targs), ValueKind::Extern(payload))
            if id.node == "objectState" && targs.is_empty() && payload.as_ref().is_null()
    );
    is_null_object_state
        || match arena.kind(value) {
            ValueKind::Struct(fields) => fields
                .iter()
                .any(|(_, value)| contains_null_object_state(arena, value)),
            ValueKind::Case(value_case) => value_case
                .args()
                .into_iter()
                .any(|value| contains_null_object_state(arena, value)),
            ValueKind::Tuple(values) | ValueKind::List(values) => values
                .iter()
                .any(|value| contains_null_object_state(arena, value)),
            ValueKind::Opt(Some(value)) => contains_null_object_state(arena, value),
            _ => false,
        }
}

#[test]
fn test_program_inst_initializes_dummy_object() {
    let mut runner = runner(Dummy);
    let program = parse_program(
        runner.arena_mut(),
        &repo()
            .join("p4c/testdata/p4_16_samples")
            .join("action_profile-bmv2.p4"),
    );

    let values = runner.eval_program("Program_inst", program).unwrap();

    assert!(
        values
            .iter()
            .any(|value| contains_null_object_state(runner.arena(), value))
    );
}

#[test]
fn test_unsupported_extern_fails() {
    let spec = repo().join("p4spec-rust/tests/fixtures/sim-plugin/unsupported-extern.watsup");
    let mut runner = runner_from_spec(&spec, Dummy);

    let error = {
        let (name, values) =
            ("Unsupported", &[make::bool(runner.arena_mut(), true, Span::default()).unwrap()]);
        runner.context().call_rel(name, values)
    }
    .unwrap_err();

    assert!(has_extern_failure(
        &error,
        "sim/extern-relation-unsupported",
        "unimplemented extern relation: Unsupported"
    ));

    for name in ["ExternFunctionCall_eval", "ExternMethodCall_eval"] {
        let error = runner.context().call_extern_rel(name, &[]).unwrap_err();
        assert!(has_extern_failure(
            &error,
            "sim/extern-relation-unsupported",
            &format!("unimplemented extern relation: {name}")
        ));
    }

    let value_ctx = make::bool(runner.arena_mut(), true, Span::default()).unwrap();
    let value_name =
        make::text(runner.arena_mut(), "static_assert".to_owned(), Span::default()).unwrap();
    let typ_name = p4spec_rust::lang::data::typ::make::var(
        p4spec_rust::phrase!(node: "nameIR".to_owned(), span: Span::default()),
        Vec::new(),
    );
    let typ_names = p4spec_rust::lang::data::typ::make::list(typ_name);
    let value_names = {
        let values = ["message", "check"]
            .into_iter()
            .map(|name| make::text(runner.arena_mut(), name.to_owned(), Span::default()).unwrap())
            .collect();
        make::list(runner.arena_mut(), typ_names.node.clone().into(), values, Span::default())
    }
    .unwrap();
    let error = runner
        .context()
        .call_rel("ExternFunctionCall_eval_lctk", &[value_ctx, value_name, value_names])
        .unwrap_err();

    assert!(has_extern_failure(
        &error,
        "sim/extern-function-unsupported",
        "unsupported local compile-time known extern function call: static_assert(message, check)"
    ));
}

#[test]
fn test_dummy_initializes_null_architecture_state_without_effects() {
    let mut runner = runner(Dummy);
    let (value, effected) = runner
        .context()
        .call_extern_func("init_archState", &[], &[])
        .unwrap();
    assert_eq!(get::external(runner.arena(), &value).unwrap().as_ref(), &json::Null);
    assert!(
        matches!(runner.arena().typ(&value).as_ref(), TypKind::Var(id, _) if id.node == "archState")
    );
    assert!(!effected);
}
