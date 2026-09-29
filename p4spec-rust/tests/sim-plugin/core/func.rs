use p4spec_rust::lang::data::value::ValueArena;

use p4spec_rust::{
    frontend::parse::parse_mixop,
    lang::{
        common::source::Span,
        data::{
            typ::make as make_typ,
            value::{Value, get, make},
        },
    },
    sim_plugin::dummy::Dummy,
};

use super::super::{has_extern_failure, parse_program, repo, runner};

fn case(arena: &mut ValueArena, shape: &str, args: Vec<Value>, typ: &str) -> Value {
    let mixop = parse_mixop(shape).unwrap();
    let value_case = p4spec_rust::lang::common::notation::mixop::Mixop::fill(&mixop, args).unwrap();
    let typ = make_typ::var(
        p4spec_rust::phrase!(node: typ.to_owned(), span: Span::default()),
        Vec::new(),
    );
    make::case(arena, typ.node.clone().into(), value_case, Span::default()).unwrap()
}

fn run_static_assert(names_param: &[&str]) -> (Value, Value) {
    let spec = repo().join("p4spec-rust/tests/fixtures/sim-plugin/unsupported-extern.watsup");
    let mut runner = super::super::runner_from_spec(&spec, Dummy);
    let value_check = {
        let value_2 = vec![make::bool(runner.arena_mut(), true, Span::default()).unwrap()];
        case(runner.arena_mut(), "_B bool", value_2, "value")
    };
    let value_message = {
        let value_2 =
            vec![make::text(runner.arena_mut(), "unused".to_owned(), Span::default()).unwrap()];
        case(runner.arena_mut(), "'\"' text '\"'", value_2, "value")
    };
    let value_ctx = case(
        runner.arena_mut(),
        "CTX value value",
        vec![value_check, value_message],
        "typingContext",
    );
    let typ_name = make_typ::var(
        p4spec_rust::phrase!(node: "nameIR".to_owned(), span: Span::default()),
        Vec::new(),
    );
    let typ_names = make_typ::list(typ_name);
    let values_name = names_param
        .iter()
        .map(|name| make::text(runner.arena_mut(), (*name).to_owned(), Span::default()).unwrap())
        .collect();
    let value_names =
        make::list(runner.arena_mut(), typ_names.node.clone().into(), values_name, Span::default())
            .unwrap();
    let value_name =
        make::text(runner.arena_mut(), "static_assert".to_owned(), Span::default()).unwrap();

    let values = runner
        .context()
        .call_rel("ExternFunctionCall_eval_lctk", &[value_ctx, value_name, value_names])
        .unwrap();

    (value_check, values[0])
}

#[test]
fn test_static_assert_returns_true() {
    for names_param in [&["check"][..], &["check", "message"][..]] {
        let (value_check, value_result) = run_static_assert(names_param);
        assert!((value_check == value_result));
    }

    let mut runner = runner(Dummy);
    let program = parse_program(
        runner.arena_mut(),
        &repo().join("p4spec-rust/tests/fixtures/sim-plugin/static-assert-true.p4"),
    );

    runner.eval_program("Program_ok", program).unwrap();
}

#[test]
fn test_static_assert_false_default_message() {
    let mut runner = runner(Dummy);
    let program = parse_program(
        runner.arena_mut(),
        &repo().join("p4spec-rust/tests/fixtures/sim-plugin/static-assert-false-default.p4"),
    );

    let error = runner.eval_program("Program_ok", program).unwrap_err();

    assert!(has_extern_failure(&error, "sim/assertion-unmet", "static_assert failed"));
}

#[test]
fn test_static_assert_false_custom_message() {
    let mut runner = runner(Dummy);
    let program = parse_program(
        runner.arena_mut(),
        &repo().join("p4spec-rust/tests/fixtures/sim-plugin/static-assert-false-custom.p4"),
    );

    let error = runner.eval_program("Program_ok", program).unwrap_err();

    assert!(has_extern_failure(&error, "sim/assertion-unmet", "custom assertion failure"));
}

#[test]
fn test_verify_reads_both_arguments_even_when_true() {
    let (mut runner, value_ctx, value_arch) = super::packet_runner(0, 0);
    let value_signal =
        make::text(runner.arena_mut(), "signal".to_owned(), Span::default()).unwrap();
    runner
        .context()
        .interp_mut()
        .values_var
        .insert("toSignal".to_owned(), value_signal);
    for check in [true, false] {
        let value_bool = make::bool(runner.arena_mut(), check, Span::default()).unwrap();
        let mixop = p4spec_rust::frontend::parse::parse_mixop("_B bool").unwrap();
        let value_case =
            p4spec_rust::lang::common::notation::mixop::Mixop::fill(&mixop, vec![value_bool])
                .unwrap();
        let value_check = make::case(
            runner.arena_mut(),
            make_typ::bool().node.into(),
            value_case,
            Span::default(),
        )
        .unwrap();
        runner
            .context()
            .interp_mut()
            .values_var
            .insert("check".to_owned(), value_check);
        runner.context().interp_mut().calls.clear();
        let output = p4spec_rust::sim_plugin::core::func::verify(
            &mut runner.context(),
            value_ctx,
            value_arch,
        )
        .unwrap();
        assert_eq!(output.0, value_ctx);
        assert_eq!(output.1, value_arch);
        let ctx = runner.context();
        let names: Vec<_> = ctx
            .interp()
            .calls
            .iter()
            .map(|(_, values)| {
                get::text(ctx.arena(), get::case(ctx.arena(), &values[0]).unwrap().args()[0])
                    .unwrap()
            })
            .collect();
        assert_eq!(names, ["check", "toSignal"]);
        if !check {
            assert_eq!(*get::case(ctx.arena(), &output.2).unwrap().args()[0], value_signal);
        }
    }
}
