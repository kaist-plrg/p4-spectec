use crate::interp::report::ReportExt;
use p4spec_rust::runner::InterpreterError;

use p4spec_rust::{
    lang::{
        common::source::Span,
        data::value::{ValueArena, make},
    },
    sim_plugin::{
        io::{Expectation, Tx},
        state::SimState,
        stf_runner::Run,
    },
};

fn tx(port: usize, packet: &str) -> Tx {
    Tx { port, packet: packet.to_owned() }
}

fn run() -> Run {
    let mut arena = ValueArena::new();
    let value = make::bool(&mut arena, false, Span::default()).unwrap();
    Run::new(SimState { value_ctx: value, value_arch: value, txs: vec![] })
}

#[test]
fn test_output_matches_only_first_transmission() {
    let mut run_case = run();
    run_case
        .on_tx_expect(Expectation { tx: tx(1, "AA"), exact: false })
        .unwrap();
    run_case
        .on_tx_expect(Expectation { tx: tx(2, "BB"), exact: true })
        .unwrap();
    run_case.state.txs = vec![tx(1, "AACC"), tx(2, "BB")];
    assert_eq!(run_case.on_tx_output().unwrap(), Some(tx(1, "AA")));
    assert_eq!(run_case.tx_output_queue, vec![tx(2, "BB")]);
    assert_eq!(run_case.expect_queue.len(), 1);
    assert_eq!(run_case.finish().unwrap_err().code(), Some("sim/packet-expectation-incomplete"));
}

#[test]
fn test_first_same_port_mismatch_preserves_queues() {
    let mut run_case = run();
    run_case
        .on_tx_expect(Expectation { tx: tx(1, "BB"), exact: true })
        .unwrap();
    run_case
        .on_tx_expect(Expectation { tx: tx(1, "AA"), exact: true })
        .unwrap();
    run_case.state.txs = vec![tx(1, "AA")];
    assert_eq!(run_case.on_tx_output().unwrap_err().code(), Some("sim/packet-mismatch"));
    assert_eq!(run_case.expect_queue.len(), 2);
    let mut run_case = run();
    run_case.state.txs = vec![tx(1, "BB"), tx(1, "AA")];
    run_case.on_tx_output().unwrap();
    assert_eq!(
        run_case
            .on_tx_expect(Expectation { tx: tx(1, "AA"), exact: true })
            .unwrap_err()
            .code(),
        Some("sim/packet-mismatch")
    );
    assert_eq!(run_case.tx_output_queue.len(), 2);
}

#[test]
fn test_output_before_expect_logs_actual_and_preserves_other_ports() {
    let mut run_case = run();
    run_case.state.txs = vec![tx(2, "BB"), tx(1, "AACC")];
    assert_eq!(run_case.on_tx_output().unwrap(), None);
    assert_eq!(
        run_case
            .on_tx_expect(Expectation { tx: tx(1, "A*"), exact: false })
            .unwrap(),
        Some(tx(1, "AACC"))
    );
    assert_eq!(run_case.tx_output_queue, vec![tx(2, "BB")]);
    assert_eq!(
        run_case
            .on_tx_expect(Expectation { tx: tx(2, ""), exact: false })
            .unwrap(),
        Some(tx(2, "BB"))
    );
    run_case.finish().unwrap();
}

#[test]
fn test_dropped_packet_retains_expectation() {
    let mut run_case = run();
    run_case
        .on_tx_expect(Expectation { tx: tx(1, ""), exact: false })
        .unwrap();
    assert_eq!(run_case.on_tx_output().unwrap(), None);
    assert_eq!(run_case.finish().unwrap_err().code(), Some("sim/packet-expectation-incomplete"));
}

use p4spec_rust::{
    lang::{
        common::prim::num,
        data::{
            typ,
            value::{
                Value,
                external::{self, Encoding},
                get,
            },
        },
        il::ast::Typ,
    },
    runner::{Extern, Interface, Interpreter, NullInterface, Runner, RunnerContext},
    sim_plugin::{ebpf::Ebpf, stf_runner},
    stf::{
        self,
        ast::{Action, Argument, MatchKind, Statement, TableMatch},
    },
};

#[derive(Default)]
struct StfInterp {
    calls: Vec<(String, Vec<Value>)>,
    initialized: bool,
    failure: Option<InterpreterError>,
}

impl<Iface: Interface, Ext: Extern> Interpreter<Iface, Ext> for StfInterp {
    type Spec = ();

    fn clear(&mut self) {
        self.calls.clear();
    }

    fn reset(&mut self) {
        self.calls.clear();
        self.initialized = false;
    }

    fn eval_program(
        ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        _program: Value,
    ) -> Result<Vec<Value>, InterpreterError> {
        assert_eq!(name, "EBPF_init");
        assert!(!ctx.interp().initialized, "previous run's interpreter state was not reset");
        ctx.interp_mut().initialized = true;
        let value = make::bool(ctx.arena_mut(), false, Span::default())?;
        Ok(vec![value, value])
    }

    fn eval_rel(
        _ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        _name: &str,
        _values: &[Value],
    ) -> Result<Vec<Value>, InterpreterError> {
        unreachable!()
    }

    fn eval_func(
        ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        _targs: &[Typ],
        values: &[Value],
    ) -> Result<Value, InterpreterError> {
        ctx.interp_mut()
            .calls
            .push((name.to_owned(), values.to_vec()));
        if let Some(failure) = ctx.interp_mut().failure.take() {
            return Err(failure);
        }
        match name {
            "find_object_unqualified_e" => Ok(make::opt(
                ctx.arena_mut(),
                typ::make::opt(typ::make::bool()).node.into(),
                Some(values[0]),
                Span::default(),
            )?),
            "tableObject_add_entry" => {
                let value = make::tuple(
                    ctx.arena_mut(),
                    typ::make::bool().node.into(),
                    values[1..].to_vec(),
                    Span::default(),
                )?;
                Ok(make::opt(
                    ctx.arena_mut(),
                    typ::make::opt(typ::make::bool()).node.into(),
                    Some(value),
                    Span::default(),
                )?)
            }
            "tableObject_add_default_action" => Ok(make::tuple(
                ctx.arena_mut(),
                typ::make::bool().node.into(),
                values[1..].to_vec(),
                Span::default(),
            )?),
            "update_object_unqualified_e" => Ok(values[2]),
            _ => panic!("unexpected specification call {name}"),
        }
    }
}

fn stf_runner(external: Ebpf) -> (Runner<StfInterp, NullInterface, Ebpf>, Run) {
    let mut runner = Runner::new((), StfInterp::default(), NullInterface, external);
    let value = make::bool(runner.arena_mut(), false, Span::default()).unwrap();
    let run_case = Run::new(SimState { value_ctx: value, value_arch: value, txs: vec![] });
    (runner, run_case)
}

fn statement(stmt: Statement) -> p4spec_rust::lang::common::source::Phrase<Statement> {
    p4spec_rust::phrase!(node: stmt, span: Span::default())
}

#[test]
fn test_add_escapes_table_names_but_set_default_preserves_them() {
    let text_name = "prefix◕‿◕😀ツ\"\\\n\tsimple_table_1";
    let text_escaped = "prefix\\226\\151\\149\\226\\128\\191\\226\\151\\149\\240\\159\\152\\128\\227\\131\\132\\\"\\\\\\n\\tsimple_table_1";
    let action = Action { name: "NoAction".into(), args: vec![] };
    for (stmt, text_expect) in [
        (
            Statement::Add {
                table: text_name.into(),
                priority: None,
                matches: vec![],
                action: action.clone(),
                id: None,
            },
            text_escaped,
        ),
        (Statement::SetDefault { table: text_name.into(), action }, text_name),
    ] {
        let (mut runner, mut run_case) = stf_runner(Ebpf::default());
        stf_runner::run_stf_stmt(&mut runner, &mut run_case, &statement(stmt)).unwrap();
        let calls = runner.context().interp().calls.clone();
        assert_eq!(calls[0].0, "find_object_unqualified_e");
        assert_eq!(get::text(runner.arena(), &calls[0].1[1]).unwrap(), text_expect);
        let call = calls.last().unwrap();
        assert_eq!(call.0, "update_object_unqualified_e");
        assert_eq!(get::text(runner.arena(), &call.1[1]).unwrap(), text_expect);
    }
}

#[test]
fn test_ordered_table_encoding_and_register_failure() {
    let (mut runner, mut run_case) = stf_runner(Ebpf::default());
    let action = Action {
        name: "pipe_act".into(),
        args: vec![
            Argument { id: "second".into(), num: "0x7fffffffffffffff".into() },
            Argument { id: "first".into(), num: "0b10".into() },
        ],
    };
    let stmt = statement(Statement::Add {
        table: "tab\"".into(),
        priority: Some(7),
        matches: vec![
            TableMatch { name: "hdr$12.field$0".into(), kind: MatchKind::Number("0xF*".into()) },
            TableMatch { name: "bin".into(), kind: MatchKind::Number("0b10".into()) },
            TableMatch { name: "upper".into(), kind: MatchKind::Number("0XFF".into()) },
            TableMatch {
                name: "prefix".into(),
                kind: MatchKind::Slash("0xAB".into(), "0b1000".into()),
            },
        ],
        action: action.clone(),
        id: Some("ignored".into()),
    });
    stf_runner::run_stf_stmt(&mut runner, &mut run_case, &stmt).unwrap();
    let value_added = run_case.state.value_arch;
    let values = get::tuple(runner.arena(), &value_added).unwrap();
    let value_priority = get::opt(runner.arena(), &values[1]).unwrap().unwrap();
    assert_eq!(num::to_int(get::num(runner.arena(), &value_priority).unwrap()), &7.into());
    let values_key = get::list(runner.arena(), &values[2]).unwrap();
    let values_key = values_key
        .iter()
        .map(|value| get::tuple(runner.arena(), value).unwrap())
        .collect::<Vec<_>>();
    assert_eq!(get::text(runner.arena(), &values_key[0][0]).unwrap(), "hdr[12].field[0]");
    for (values, shape, text) in values_key[..3]
        .iter()
        .zip(["_HEX text", "_BIN text", "_DEC text"])
        .zip(["F*", "10", "0XFF"])
        .map(|((values, shape), text)| (values, shape, text))
    {
        let case = get::case(runner.arena(), &values[1]).unwrap();
        let shape = p4spec_rust::frontend::parse::parse_mixop(shape).unwrap();
        assert!(case.eq_shape(&shape));
        assert_eq!(get::text(runner.arena(), case.args()[0]).unwrap(), text);
    }
    let case = get::case(runner.arena(), &values_key[3][1]).unwrap();
    let shape = p4spec_rust::frontend::parse::parse_mixop("text _SLASH nat").unwrap();
    assert!(case.eq_shape(&shape));
    let values = case.args();
    assert_eq!(get::text(runner.arena(), values[0]).unwrap(), "0xAB");
    assert_eq!(num::to_int(get::num(runner.arena(), values[1]).unwrap()), &8.into());
    let values = get::tuple(runner.arena(), &value_added).unwrap();
    let values_action = get::tuple(runner.arena(), &values[3]).unwrap();
    assert_eq!(get::text(runner.arena(), &values_action[0]).unwrap(), "act");
    let values_arg = get::list(runner.arena(), &values_action[1]).unwrap();
    let values_arg = values_arg
        .iter()
        .map(|value| get::tuple(runner.arena(), value).unwrap())
        .collect::<Vec<_>>();
    assert_eq!(get::text(runner.arena(), &values_arg[0][0]).unwrap(), "second");
    assert_eq!(num::to_int(get::num(runner.arena(), &values_arg[0][1]).unwrap()), &i64::MAX.into());
    assert_eq!(get::text(runner.arena(), &values_arg[1][0]).unwrap(), "first");
    let calls = runner.context().interp().calls.clone();
    assert_eq!(get::text(runner.arena(), &calls[0].1[1]).unwrap(), "tab\\\"");
    stf_runner::run_stf_stmt(
        &mut runner,
        &mut run_case,
        &statement(Statement::SetDefault { table: "tab\"".into(), action }),
    )
    .unwrap();
    let values = get::tuple(runner.arena(), &run_case.state.value_arch).unwrap();
    assert_eq!(values[0], value_added);
    let calls = runner.context().interp().calls.clone();
    assert_eq!(get::text(runner.arena(), &calls[3].1[1]).unwrap(), "tab\"");
    let value_default = run_case.state.value_arch;
    let error = stf_runner::run_stf_stmt(
        &mut runner,
        &mut run_case,
        &statement(Statement::RegisterWrite {
            name: "r".into(),
            index: "0".into(),
            value: "1".into(),
        }),
    )
    .unwrap_err();
    assert!(
        error
            .to_string()
            .contains("register_write is not implemented for the ebpf simulator")
    );
    assert_eq!(run_case.state.value_arch, value_default);
}

#[test]
fn test_native_steps_clear_raw_outputs_without_flushing_pending_queues() {
    let (mut runner, mut run_case) = stf_runner(Ebpf::default());
    run_case.state.txs = vec![tx(1, "AAFF")];
    run_case.on_tx_output().unwrap();
    let stmts = stf::parse::parse_str(
        "commands.stf",
        "wait\nmirroring_get 4611686018427387904\nexpect 1 aa*\nno_packet\n",
    )
    .unwrap();
    stf_runner::run_stf_stmt(&mut runner, &mut run_case, &stmts[0]).unwrap();
    assert!(run_case.state.txs.is_empty());
    assert_eq!(run_case.tx_output_queue, vec![tx(1, "AAFF")]);
    stf_runner::run_stf_stmt(&mut runner, &mut run_case, &stmts[1]).unwrap();
    assert_eq!(
        stf_runner::run_stf_stmt(&mut runner, &mut run_case, &stmts[2]).unwrap(),
        Some(tx(1, "AAFF"))
    );
    assert_eq!(run_case.matches, vec![tx(1, "AAFF")]);
    let report = stf_runner::run_stf_stmt(&mut runner, &mut run_case, &stmts[3]).unwrap_err();
    assert_eq!(report.code(), Some("sim/statement-unsupported"));
    assert_eq!(report.span(), stmts[3].span);
    run_case.finish().unwrap();
}

#[test]
fn test_unlocated_statement_failures_do_not_invent_source_labels() {
    use p4spec_rust::diagnostic::ReportKind;

    for (source, code) in [
        ("no_packet\n", "sim/statement-unsupported"),
        ("expect 1 bb\n", "sim/packet-mismatch"),
        ("packet 18446744073709551616 aa\n", "sim/integer-invalid"),
    ] {
        let (mut runner, mut run_case) = stf_runner(Ebpf::default());
        run_case.tx_output_queue.push(tx(1, "AA"));
        let mut stmts = stf::parse::parse_str("generated.stf", source).unwrap();
        stmts[0].span = Span::default();
        let report = stf_runner::run_stf_stmt(&mut runner, &mut run_case, &stmts[0]).unwrap_err();
        assert_eq!(report.code(), Some(code));
        let ReportKind::Cause(diagnostic) = &report.kind else { panic!("expected cause") };
        assert!(diagnostic.labels.is_empty(), "{code}: {:?}", diagnostic.labels);
    }
}

#[test]
fn test_integer_parsing_preserves_word_range_and_radix_prefixes() {
    for (port, port_expect) in [
        ("18446744073709551615", usize::MAX),
        ("0XFFFFFFFFFFFFFFFF", usize::MAX),
        ("4611686018427387904", 1_usize << 62),
        ("0B10", 2),
        ("0o17", 15),
        ("1_000", 1000),
        ("00010", 10),
    ] {
        let (mut runner, mut run_case) = stf_runner(Ebpf::default());
        let stmt = statement(Statement::Expect {
            port: port.into(),
            packet_expected: Some("AA".into()),
            exact: true,
        });
        assert!(
            stf_runner::run_stf_stmt(&mut runner, &mut run_case, &stmt)
                .unwrap()
                .is_none()
        );
        assert_eq!(run_case.expect_queue[0].tx.port, port_expect);
    }
}

#[test]
fn test_integer_failure_is_located_and_precedes_pipeline_dispatch() {
    let (mut runner, mut run_case) = stf_runner(Ebpf::default());
    for source in [
        "packet 18446744073709551616 AA",
        "expect 18446744073709551616 AA",
        "register_write r 0 0x****************",
    ] {
        let stmts = stf::parse::parse_str("overflow.stf", source).unwrap();
        let report = stf_runner::run_stf_stmt(&mut runner, &mut run_case, &stmts[0]).unwrap_err();
        assert_eq!(report.code(), Some("sim/integer-invalid"));
        assert_eq!(report.span(), stmts[0].span);
    }
    assert!(runner.context().interp().calls.is_empty());
}

#[test]
fn test_fresh_run_resets_interpreter_state_and_queues() {
    let (mut runner, _) = stf_runner(Ebpf::default());
    let path = std::env::temp_dir().join(format!("p4spec-stf-reset-{}.p4", std::process::id()));
    std::fs::write(&path, "").unwrap();
    let mut run_case = stf_runner::init_pipe(&mut runner, &[], &path).unwrap();
    run_case
        .on_tx_expect(Expectation { tx: tx(1, "AA"), exact: true })
        .unwrap();
    let run_case = stf_runner::init_pipe(&mut runner, &[], &path).unwrap();
    std::fs::remove_file(path).unwrap();
    assert!(run_case.state.txs.is_empty());
    assert!(run_case.matches.is_empty());
    run_case.finish().unwrap();
}

#[test]
fn test_native_codec_configuration_survives_context_and_reset() {
    use p4spec_rust::sim_plugin::{core::object::PacketIn, ebpf::pipe::ExternObject};

    for encoding in [Encoding::ArenaRelative, Encoding::ArenaIndependent] {
        let (mut runner, _) = stf_runner(Ebpf::new(encoding));
        for _ in 0..2 {
            let object = ExternObject::PacketIn(PacketIn::init("AB").unwrap());
            let value_object = object.to_value(runner.arena_mut(), encoding).unwrap();
            assert_eq!(
                {
                    let mut ctx = runner.context();
                    ExternObject::from_value(ctx.arena_mut(), encoding, &value_object).unwrap()
                },
                object,
            );

            let (value_arch, _) = runner
                .context()
                .call_extern_func("init_archState", &[], &[])
                .unwrap();
            let json_arch = get::external(runner.arena(), &value_arch).unwrap().clone();
            external::decode_with::<()>(runner.arena_mut(), encoding, &json_arch).unwrap();
            let json = get::external(runner.arena(), &value_object)
                .unwrap()
                .clone();
            runner.reset();
            let object_after = ExternObject::PacketIn(PacketIn::init("CD").unwrap());
            let value_object = object_after.to_value(runner.arena_mut(), encoding).unwrap();
            assert_eq!(
                ExternObject::from_value(runner.arena_mut(), encoding, &value_object).unwrap(),
                object_after,
            );

            if encoding == Encoding::ArenaIndependent {
                assert_eq!(
                    external::decode_with::<ExternObject>(runner.arena_mut(), encoding, &json)
                        .unwrap(),
                    object
                );
            }
        }
    }
}

#[test]
fn test_default_encoding_uses_relative_native_payloads() {
    use p4spec_rust::sim_plugin::{core::object::PacketIn, ebpf::pipe::ExternObject};

    let mut runner = Runner::new((), StfInterp::default(), NullInterface, Ebpf::default());
    let encoding = Encoding::default();
    assert_eq!(encoding, Encoding::ArenaRelative);
    let object = ExternObject::PacketIn(PacketIn::init("AB").unwrap());
    let value_object = object.to_value(runner.arena_mut(), encoding).unwrap();
    assert_eq!(
        get::external(runner.arena(), &value_object)
            .unwrap()
            .as_ref(),
        &external::encode_with(runner.arena(), Encoding::ArenaRelative, &object).unwrap(),
    );
}

#[test]
fn test_runner_codec_preserves_immutable_nested_register_snapshots_in_each_mode() {
    use p4spec_rust::{
        lang::data::value::external::encode,
        sim_plugin::{
            core::object::PacketIn,
            psa::{
                arch::Arch,
                object::Register,
                packet::{Entrypoint, Packet},
                pipe::ObjectState,
            },
        },
    };

    for encoding in [Encoding::ArenaRelative, Encoding::ArenaIndependent] {
        let (mut runner, _) = stf_runner(Ebpf::new(encoding));
        let value_typ =
            make::text(runner.arena_mut(), "register type".into(), Span::default()).unwrap();
        let value = make::int(runner.arena_mut(), 0xcafe.into(), Span::default()).unwrap();
        let object = ObjectState::Register(Register { value_typ, values: vec![value; 6] });
        let value_object = object.to_value(runner.arena_mut(), encoding).unwrap();
        let arch = Arch {
            queue: [Packet {
                value_ctx: value_object,
                packet_in: PacketIn::init("AB").unwrap(),
                entrypoint: Entrypoint::Ingress,
            }]
            .into(),
            ..Arch::default()
        };
        let value_arch = {
            let mut ctx = runner.context();
            arch.to_value(ctx.arena_mut(), encoding).unwrap()
        };
        let json = get::external(runner.arena(), &value_arch).unwrap().clone();
        let mut ctx = runner.context();
        let ObjectState::Register(mut object_changed) =
            ObjectState::from_value(ctx.arena_mut(), encoding, &value_object).unwrap()
        else {
            panic!("expected register");
        };
        object_changed.values[5] =
            make::int(runner.arena_mut(), 0xcaff.into(), Span::default()).unwrap();
        let object_changed = ObjectState::Register(object_changed);
        let value_changed = object_changed
            .to_value(runner.arena_mut(), encoding)
            .unwrap();
        let mut arch_changed = arch.clone();
        arch_changed.queue[0].value_ctx = value_changed;
        let value_arch_changed = arch_changed.to_value(runner.arena_mut(), encoding).unwrap();

        // Read the old nested snapshot after publishing its replacement
        let object_decoded =
            ObjectState::from_value(runner.arena_mut(), encoding, &value_object).unwrap();
        if encoding == Encoding::ArenaRelative {
            assert_eq!(object_decoded, object);
        }
        assert_eq!(
            encode(runner.arena(), &object_decoded).unwrap(),
            encode(runner.arena(), &object).unwrap(),
        );
        let arch_decoded = Arch::from_value(runner.arena_mut(), encoding, &value_arch).unwrap();
        if encoding == Encoding::ArenaRelative {
            assert_eq!(arch_decoded, arch);
        }
        assert_eq!(
            &external::encode_with(runner.arena(), encoding, &arch_decoded).unwrap(),
            json.as_ref(),
        );
        let object_nested =
            ObjectState::from_value(runner.arena_mut(), encoding, &arch_decoded.queue[0].value_ctx)
                .unwrap();
        assert_eq!(
            encode(runner.arena(), &object_nested).unwrap(),
            encode(runner.arena(), &object).unwrap(),
        );
        let object_decoded =
            ObjectState::from_value(runner.arena_mut(), encoding, &value_changed).unwrap();
        if encoding == Encoding::ArenaRelative {
            assert_eq!(object_decoded, object_changed);
        }
        assert_eq!(
            encode(runner.arena(), &object_decoded).unwrap(),
            encode(runner.arena(), &object_changed).unwrap(),
        );
        let arch_decoded =
            Arch::from_value(runner.arena_mut(), encoding, &value_arch_changed).unwrap();
        let object_nested =
            ObjectState::from_value(runner.arena_mut(), encoding, &arch_decoded.queue[0].value_ctx)
                .unwrap();
        assert_eq!(
            encode(runner.arena(), &object_nested).unwrap(),
            encode(runner.arena(), &object_changed).unwrap(),
        );
        let json_changed = get::external(runner.arena(), &value_arch_changed).unwrap();
        assert_ne!(&json, json_changed);
        assert_eq!(&json, get::external(runner.arena(), &value_arch).unwrap());
    }
}

#[test]
fn test_runner_codec_imports_independent_nested_native_state() {
    use p4spec_rust::{
        lang::{
            common::source::Position,
            data::value::external::{decode_with, encode},
        },
        sim_plugin::{
            core::object::PacketIn,
            psa::{
                arch::Arch,
                object::Register,
                packet::{Entrypoint, Packet},
                pipe::ObjectState,
            },
        },
    };

    fn span_at(line: usize) -> Span {
        Span::new(
            Position::new("nested-import.p4", line, 2),
            Position::new("nested-import.p4", line, 8),
        )
    }

    let (json_arch, json_outer, json_inner) = {
        let (mut runner, _) = stf_runner(Ebpf::new(Encoding::ArenaIndependent));
        let encoding = Encoding::ArenaIndependent;
        let value_typ = make::text(runner.arena_mut(), "T".into(), span_at(1)).unwrap();
        let value = make::int(runner.arena_mut(), 0xcafe.into(), span_at(2)).unwrap();
        let object_inner = ObjectState::Register(Register { value_typ, values: vec![value; 2] });
        let value_inner = object_inner.to_value(runner.arena_mut(), encoding).unwrap();
        let value_inner = Value {
            span: make::bool(runner.arena_mut(), false, span_at(3))
                .unwrap()
                .span,
            ..value_inner
        };
        let object_outer =
            ObjectState::Register(Register { value_typ, values: vec![value_inner; 2] });
        let value_outer = object_outer.to_value(runner.arena_mut(), encoding).unwrap();
        let value_outer = Value {
            span: make::bool(runner.arena_mut(), false, span_at(4))
                .unwrap()
                .span,
            ..value_outer
        };
        let arch = Arch {
            queue: [Packet {
                value_ctx: value_outer,
                packet_in: PacketIn::init("AB").unwrap(),
                entrypoint: Entrypoint::Ingress,
            }]
            .into(),
            ..Arch::default()
        };
        let value_arch = arch.to_value(runner.arena_mut(), encoding).unwrap();
        let value_arch = Value {
            span: make::bool(runner.arena_mut(), false, span_at(5))
                .unwrap()
                .span,
            ..value_arch
        };
        (
            encode(runner.arena(), &value_arch).unwrap(),
            encode(runner.arena(), &value_outer).unwrap(),
            encode(runner.arena(), &value_inner).unwrap(),
        )
    };

    // The source arena is gone; each native layer retains independent encoding
    let (mut runner, _) = stf_runner(Ebpf::new(Encoding::ArenaIndependent));
    let encoding = Encoding::ArenaIndependent;
    let value_arch: Value =
        decode_with(runner.arena_mut(), Encoding::ArenaIndependent, &json_arch).unwrap();
    let arch = Arch::from_value(runner.arena_mut(), encoding, &value_arch).unwrap();
    let value_outer = arch.queue[0].value_ctx;
    let ObjectState::Register(object_outer) =
        ObjectState::from_value(runner.arena_mut(), encoding, &value_outer).unwrap()
    else {
        panic!("expected outer register");
    };
    let value_inner = object_outer.values[0];
    assert_eq!(
        encode(runner.arena(), &object_outer.values[1]).unwrap(),
        encode(runner.arena(), &value_inner).unwrap(),
    );
    let object_inner = ObjectState::from_value(runner.arena_mut(), encoding, &value_inner).unwrap();
    for value in object_outer.values {
        let object_decoded = ObjectState::from_value(runner.arena_mut(), encoding, &value).unwrap();
        assert_eq!(
            encode(runner.arena(), &object_decoded).unwrap(),
            encode(runner.arena(), &object_inner).unwrap(),
        );
    }
    for (value, json, line) in
        [(value_arch, &json_arch, 5), (value_outer, &json_outer, 4), (value_inner, &json_inner, 3)]
    {
        assert_eq!(runner.arena().span(&value), &span_at(line));
        assert_eq!(&encode(runner.arena(), &value).unwrap(), json);
    }
    assert_eq!(encode(runner.arena(), &value_arch).unwrap(), json_arch);
}

#[test]
#[ignore = "original 62-command PSA case; independent arena needs several GB and snapshots use disk"]
fn test_native_stf_encoding_modes_preserve_outputs_and_state() {
    use p4spec_rust::{
        frontend::parse::parse_files,
        lang::data::value::external::encode,
        pass::{algo, elaborate},
        runner::{Config, build_al},
        sim_plugin::{
            psa::{Psa, pipe},
            stf_runner as sim_runner,
        },
    };
    use std::{
        fs,
        path::{Path, PathBuf},
        time::{SystemTime, UNIX_EPOCH},
    };

    struct Snapshots(PathBuf);

    impl Drop for Snapshots {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }

    fn snapshot(
        path: &Path,
        encoding: Encoding,
        command: usize,
        name: &str,
        mut json: serde_json::Value,
    ) {
        json.sort_all_objects();
        let bytes = serde_json::to_vec(&json).unwrap();
        drop(json);
        let path = path.join(format!("{command:02}-{name}.json"));
        match encoding {
            Encoding::ArenaRelative => fs::write(path, bytes).unwrap(),
            Encoding::ArenaIndependent => {
                let bytes_expect = fs::read(&path).unwrap();
                assert!(
                    bytes == bytes_expect,
                    "{name} differs after command {command}: actual {} bytes, expected {} bytes",
                    bytes.len(),
                    bytes_expect.len(),
                );
                fs::remove_file(path).unwrap();
            }
        }
    }

    stacker::grow(32 * 1024 * 1024, || {
        let path =
            super::repo().join("p4c/testdata/p4_16_samples/psa-register-read-write-2-bmv2.p4");
        assert!(path.is_file(), "missing original P4 fixture: {}", path.display());
        assert!(path.with_extension("stf").is_file(), "missing original STF fixture");
        let includes = [super::repo().join("p4c/p4include")];
        let stmts = stf::parse::parse_file(path.with_extension("stf")).unwrap();
        assert_eq!(stmts.len(), 62, "original fixture command count");
        let path_snapshots = std::env::temp_dir().join(format!(
            "p4spec-stf-encoding-{}-{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos(),
        ));
        fs::create_dir(&path_snapshots).unwrap();
        let snapshots = Snapshots(path_snapshots);
        let spec_el = parse_files([super::repo().join("spec")]).unwrap();
        let spec_il = elaborate::convert(spec_el).unwrap();
        let spec_al = algo::convert(spec_il).unwrap();

        // Drop each runner before building the next; retain no state history
        for encoding in [Encoding::ArenaRelative, Encoding::ArenaIndependent] {
            let mut runner =
                build_al(spec_al.clone(), Config::new(true, false, false), Psa::new(encoding))
                    .unwrap();
            let mut run_case = sim_runner::init_pipe(&mut runner, &includes, &path).unwrap();
            let values = ["ip", "ig", "reg"]
                .into_iter()
                .map(|name| {
                    make::text(runner.arena_mut(), name.to_owned(), Span::default()).unwrap()
                })
                .collect();
            let typ = typ::make::list(typ::make::var(
                p4spec_rust::phrase!(node: "id".to_owned(), span: Span::default()),
                vec![],
            ));
            let value_id =
                make::list(runner.arena_mut(), typ.node.into(), values, Span::default()).unwrap();
            for command in 0..=stmts.len() {
                let tx_matched = if command == 0 {
                    None
                } else {
                    sim_runner::run_stf_stmt(&mut runner, &mut run_case, &stmts[command - 1])
                        .unwrap_or_else(|error| panic!("{encoding} command {command}: {error}"))
                };
                // Compare known native state; root trees contain opaque externs
                let arch = pipe::find_arch_state(&mut runner.context(), run_case.state.value_arch)
                    .unwrap();
                assert!(arch.queue.is_empty(), "{encoding} command {command}");
                snapshot(
                    &snapshots.0,
                    encoding,
                    command,
                    "architecture",
                    encode(runner.arena(), &arch).unwrap(),
                );
                let object = pipe::find_object_state(
                    &mut runner.context(),
                    run_case.state.value_arch,
                    value_id,
                )
                .unwrap();
                let pipe::ObjectState::Register(reg) = object else {
                    panic!("expected fixture register");
                };
                assert_eq!(reg.values.len(), 6, "fixture register size");
                snapshot(
                    &snapshots.0,
                    encoding,
                    command,
                    "register",
                    encode(runner.arena(), &reg).unwrap(),
                );
                let json = serde_json::json!({
                    "matched": tx_matched.as_ref().map(|tx| (tx.port, &tx.packet)),
                    "transmissions": run_case.state.txs.iter()
                        .map(|tx| (tx.port, &tx.packet)).collect::<Vec<_>>(),
                    "outputs": run_case.tx_output_queue.iter()
                        .map(|tx| (tx.port, &tx.packet)).collect::<Vec<_>>(),
                    "expectations": run_case.expect_queue.iter()
                        .map(|expect| (expect.tx.port, &expect.tx.packet, expect.exact))
                        .collect::<Vec<_>>(),
                    "matches": run_case.matches.iter()
                        .map(|tx| (tx.port, &tx.packet)).collect::<Vec<_>>(),
                });
                snapshot(&snapshots.0, encoding, command, "outputs", json);
            }
            run_case.finish().unwrap();
            assert_eq!(run_case.matches.len(), 31, "{encoding} matched packets");
        }
        assert_eq!(fs::read_dir(&snapshots.0).unwrap().count(), 0);
    });
}

#[test]
fn test_runtime_reentry_preserves_causes_under_statement_frame() {
    use p4spec_rust::{
        diagnostic::{Diagnostic, Label, Report, ReportKind, Severity},
        lang::common::source::Position,
    };

    let (mut runner, mut run_case) = stf_runner(Ebpf::default());
    let mut stmt = statement(Statement::SetDefault {
        table: "tab".into(),
        action: Action { name: "action".into(), args: vec![] },
    });
    stmt.span =
        Span::new(Position::new("commands.stf", 2, 0), Position::new("commands.stf", 2, 20));
    let span_spec = Span::new(Position::new("spec", 3, 0), Position::new("spec", 3, 8));
    let cause = |message, labels| {
        Report::from(Diagnostic::new(
            "fixture",
            Severity::Error,
            Some("fixture/reentry".into()),
            message,
            labels,
            vec!["retained note".into()],
        ))
    };
    runner.context().interp_mut().failure = Some(InterpreterError::Mismatch(vec![
        cause("unlocated", vec![]),
        cause("located", vec![Label::secondary(&span_spec, "origin")]),
        Report::frame(span_spec.clone(), "located call", vec![cause("nested", vec![])]),
    ]));
    let report = stf_runner::run_stf_stmt(&mut runner, &mut run_case, &stmt).unwrap_err();
    assert!(matches!(
        &report.kind,
        ReportKind::Frame { message, span } if message == "while executing STF statement" && span == &stmt.span
    ));
    assert_eq!(report.children.len(), 1);
    let report = &report.children[0];
    assert!(matches!(
        &report.kind,
        ReportKind::Frame { message, span } if message == "execution failed" && span == &Span::default()
    ));
    assert_eq!(report.children.len(), 3);
    for (report, message, labels) in [
        (&report.children[0], "unlocated", vec![]),
        (&report.children[1], "located", vec![Label::secondary(&span_spec, "origin")]),
        (&report.children[2].children[0], "nested", vec![]),
    ] {
        let ReportKind::Cause(diagnostic) = &report.kind else { panic!("expected cause") };
        assert_eq!(diagnostic.labels, labels);
        assert_eq!(diagnostic.message, message);
        assert_eq!(diagnostic.source, "fixture");
        assert_eq!(diagnostic.code.as_deref(), Some("fixture/reentry"));
        assert_eq!(diagnostic.severity, Severity::Error);
        assert_eq!(diagnostic.notes, ["retained note"]);
    }
    assert!(
        matches!(&report.children[2].kind, ReportKind::Frame { span, .. } if span == &span_spec)
    );
}
