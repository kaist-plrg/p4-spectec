use p4spec_rust::{
    lang::{
        common::source::Span,
        data::{
            typ,
            value::{Value, ValueError, get, make},
        },
        il::ast::Typ,
    },
    runner::{
        Extern, ExternError, Interface, InterfaceError, Interpreter, NullInterface, Runner,
        RunnerContext,
    },
    sim_plugin::{
        ebpf::{self, Ebpf},
        io::{Rx, Tx},
        state::SimState,
    },
};

#[derive(Debug, thiserror::Error)]
enum TestError {
    #[error(transparent)]
    Extern(#[from] ExternError),
    #[error(transparent)]
    Interface(#[from] InterfaceError),
}

struct PhaseInterp {
    name_bad: &'static str,
    calls: Vec<String>,
    values: Vec<Value>,
}

impl<Iface: Interface, Ext: Extern> Interpreter<Iface, Ext> for PhaseInterp {
    type Spec = ();
    type Error = TestError;

    fn clear(&mut self) {}

    fn reset(&mut self) {}

    fn eval_program(
        ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        _: Value,
    ) -> Result<Vec<Value>, TestError> {
        assert_eq!(name, "EBPF_init");
        Ok(vec![ctx.interp().values[0]])
    }

    fn eval_func(
        _: &mut RunnerContext<'_, Self, Iface, Ext>,
        _: &str,
        _: &[Typ],
        _: &[Value],
    ) -> Result<Value, TestError> {
        unreachable!()
    }

    fn eval_rel(
        ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        values: &[Value],
    ) -> Result<Vec<Value>, TestError> {
        ctx.interp_mut().calls.push(name.to_owned());
        let values_state = ctx.interp().values.clone();
        let (arity, mut values_result) = match name {
            "EBPF_init_packet_in" => {
                assert_eq!(values.len(), 3);
                (2, vec![values_state[1], values_state[1]])
            }
            "EBPF_init_globals" => {
                assert_eq!(values, [values_state[1], values_state[1]]);
                (1, vec![values_state[2]])
            }
            "EBPF_parse" => {
                assert_eq!(values, [values_state[2], values_state[1]]);
                (3, vec![values_state[3], values_state[3], values_state[0]])
            }
            "EBPF_filter" => {
                assert_eq!(values, [values_state[3], values_state[3]]);
                (3, vec![values_state[4], values_state[4], values_state[0]])
            }
            "Lvalue_read" => {
                assert_eq!(values[1..3], [values_state[4], values_state[4]]);
                (1, vec![values_state[5]])
            }
            _ => panic!("unexpected relation: {name}"),
        };
        if ctx.interp().name_bad == name {
            values_result.truncate(arity - 1);
        }
        Ok(values_result)
    }
}

fn runner(name_bad: &'static str) -> Runner<PhaseInterp, NullInterface, Ebpf> {
    let mut runner_phase = Runner::new(
        (),
        PhaseInterp { name_bad, calls: vec![], values: vec![] },
        NullInterface,
        Ebpf::default(),
    );
    let mut values = Vec::new();
    for name in ["original", "packet", "globals", "parsed", "filtered"] {
        values
            .push(make::text(runner_phase.arena_mut(), name.to_owned(), Span::default()).unwrap());
    }
    let value_true = make::bool(runner_phase.arena_mut(), true, Span::default()).unwrap();
    let mixop = p4spec_rust::frontend::parse::parse_mixop("_B bool").unwrap();
    let mixfix =
        p4spec_rust::lang::common::notation::mixop::Mixop::fill(&mixop, vec![value_true]).unwrap();
    let typ = typ::make::var(
        p4spec_rust::phrase!(node: "value".to_owned(), span: Span::default()),
        vec![],
    );
    values.push(
        make::case(runner_phase.arena_mut(), typ.node.into(), mixfix, Span::default()).unwrap(),
    );
    runner_phase.context().interp_mut().values = values;
    runner_phase
}

#[test]
fn test_phase_arity_failure_retains_only_completed_phase_state() {
    for (name, expected, actual, name_ctx, name_arch) in [
        ("EBPF_init_packet_in", 2, 1, "original", "original"),
        ("EBPF_init_globals", 1, 0, "packet", "packet"),
        ("EBPF_parse", 3, 2, "globals", "packet"),
        ("EBPF_filter", 3, 2, "parsed", "parsed"),
        ("Lvalue_read", 1, 0, "filtered", "filtered"),
    ] {
        let mut runner_phase = runner(name);
        let value = runner_phase.context().interp().values[0];
        let mut state = SimState {
            value_ctx: value,
            value_arch: value,
            txs: vec![Tx { port: 0, packet: "prior packet".to_owned() }],
        };
        let error = ebpf::drive_pipe(
            &mut runner_phase.context(),
            &mut state,
            &Rx { port: 1, packet: "aB".to_owned() },
        )
        .unwrap_err();
        assert!(
            matches!(error, TestError::Extern(ExternError::Value(ValueError::CountMismatch { expected: count_expected, actual: count_actual })) if count_expected == expected && count_actual == actual)
        );
        assert_eq!(get::text(runner_phase.arena(), &state.value_ctx).unwrap(), name_ctx);
        assert_eq!(get::text(runner_phase.arena(), &state.value_arch).unwrap(), name_arch);
        assert!(state.txs.is_empty());
        assert_eq!(runner_phase.context().interp().calls.last().unwrap(), name);
    }
}

#[test]
fn test_initialization_requires_two_outputs_and_filter_result_is_ignored() {
    let mut runner_phase = runner("");
    let value = runner_phase.context().interp().values[0];
    assert!(matches!(
        ebpf::init_pipe(&mut runner_phase.context(), value),
        Err(TestError::Extern(ExternError::Value(ValueError::CountMismatch {
            expected: 2,
            actual: 1
        })))
    ));
    let mut state = SimState { value_ctx: value, value_arch: value, txs: vec![] };
    ebpf::drive_pipe(
        &mut runner_phase.context(),
        &mut state,
        &Rx { port: 3, packet: "aB".to_owned() },
    )
    .unwrap();
    assert_eq!(state.txs.len(), 1);
    assert_eq!(state.txs[0].port, 3);
    assert_eq!(state.txs[0].packet, "aB");
}

#[test]
fn test_extern_init_and_function_report_argument_counts_before_dispatch() {
    let mut runner_phase = runner("");
    let value = runner_phase.context().interp().values[0];
    for actual in [0, 3, 5] {
        let values = vec![value; actual];
        let error_init = runner_phase
            .context()
            .call_extern_func("init_objectState", &[], &values)
            .unwrap_err();
        let error_func = runner_phase
            .context()
            .call_extern_rel("ExternFunctionCall_eval", &values)
            .unwrap_err();
        for error in [error_init, error_func] {
            assert!(matches!(
                error,
                TestError::Extern(ExternError::Value(ValueError::CountMismatch {
                    expected: 4,
                    actual: count,
                })) if count == actual
            ));
        }
    }
    assert!(runner_phase.context().interp().calls.is_empty());
}

impl From<TestError> for ExternError {
    fn from(error: TestError) -> Self {
        match error {
            TestError::Extern(error) => error,
            TestError::Interface(error) => ExternError::Report(error.into_report()),
        }
    }
}
