#[path = "scheduler.rs"]
mod scheduler;

#[test]
fn test_clone_info_preserves_tuple_json() {
    use p4spec_rust::{
        lang::data::value::ValueArena,
        sim_plugin::{
            spec::pack,
            v1model::packet::{CloneInfo, CloneType},
        },
    };

    let mut arena = ValueArena::new();
    let value_session = pack::p4_fixed_bit(&mut arena, 32.into(), 7.into()).unwrap();
    let value_idx = pack::p4_fixed_bit(&mut arena, 8.into(), 3.into()).unwrap();
    for (name, clone_type) in [("I2E", CloneType::I2E), ("E2E", CloneType::E2E)] {
        let value_clone_type = pack::p4_enum(&mut arena, "CloneType", name).unwrap();
        let info = CloneInfo::new(&arena, &value_clone_type, &value_session, &value_idx).unwrap();
        assert_eq!(info, CloneInfo(clone_type, 7, 3));
        let json = serde_json::json!([name, 7, 3]);
        assert_eq!(serde_json::to_value(info).unwrap(), json);
        assert_eq!(serde_json::from_value::<CloneInfo>(json).unwrap(), info);
    }
}

#[test]
fn test_native_v1model_micro_fixture() {
    use p4spec_rust::lang::data::value::external::Encoding;
    use p4spec_rust::{
        sim_plugin::{
            io::Rx,
            v1model::{self, V1Model},
        },
        stf::ast::Statement,
    };

    for encoding in [Encoding::ArenaRelative, Encoding::ArenaIndependent] {
        let mut runner = super::runner(V1Model::new(encoding));
        let path = super::repo().join("p4spec/test/micro/sim-v1model/v1model.p4");
        let program = super::parse_program(runner.arena_mut(), &path);
        let mut state = v1model::init_pipe(&mut runner.context(), program).unwrap();
        let stmts = p4spec_rust::stf::parse::parse_file(path.with_extension("stf")).unwrap();
        let mut txs = Vec::new();
        let mut txs_expect = Vec::new();
        for stmt in stmts {
            match stmt.node {
                Statement::Packet { port, packet } => {
                    v1model::drive_pipe(
                        &mut runner.context(),
                        &mut state,
                        &Rx { port: port.parse().unwrap(), packet },
                    )
                    .unwrap();
                    txs.extend(state.txs.iter().map(|tx| (tx.port, tx.packet.clone())));
                }
                Statement::Expect { port, packet_expected: Some(packet), .. } => {
                    txs_expect.push((port.parse::<usize>().unwrap(), packet.to_ascii_uppercase()))
                }
                _ => panic!("micro fixture contains only packet/expect commands"),
            }
        }
        assert_eq!(txs, txs_expect);
        assert!(
            v1model::pipe::find_arch_state(&mut runner.context(), state.value_arch)
                .unwrap()
                .queue
                .is_empty()
        );
    }
}

#[test]
fn test_hash_adjust_range_boundaries() {
    use p4spec_rust::sim_plugin::v1model::func;

    assert_eq!(func::adjust(&5.into(), &12.into(), &20.into()).unwrap(), 11.into());
    assert_eq!(func::adjust(&5.into(), &0.into(), &20.into()).unwrap(), 5.into());
    assert!(func::adjust(&5.into(), &5.into(), &20.into()).is_err());
    assert!(func::adjust(&5.into(), &3.into(), &20.into()).is_err());
    assert_eq!(func::adjust(&5.into(), &12.into(), &(-20).into()).unwrap(), 6.into());
}

#[test]
fn test_direct_meter_rejects_invalid_meter_type_with_its_own_diagnostic() {
    use p4spec_rust::{
        diagnostic::ReportKind,
        lang::{
            common::source::Span,
            data::{
                typ,
                value::{ValueArena, make},
            },
        },
        runner::ExternError,
        sim_plugin::{spec::pack, v1model::object::DirectMeter},
    };
    let mut arena = ValueArena::new();
    let value_name = make::text(&mut arena, "type".to_owned(), Span::default()).unwrap();
    let value_ids = make::list(
        &mut arena,
        typ::make::list(typ::make::text()).node.into(),
        vec![value_name],
        Span::default(),
    )
    .unwrap();
    let typ_value = typ::make::var(
        p4spec_rust::phrase!(node: "value".to_owned(), span: Span::default()),
        vec![],
    );
    for (id_enum, id_type) in [("CounterType", "packets"), ("MeterType", "invalid")] {
        let value_type = pack::p4_enum(&mut arena, id_enum, id_type).unwrap();
        let value_args = make::list(
            &mut arena,
            typ::make::list(typ_value.clone()).node.into(),
            vec![value_type],
            Span::default(),
        )
        .unwrap();
        let error = DirectMeter::init(&arena, value_ids, value_ids, value_args).unwrap_err();
        let ExternError(report) = error;
        let ReportKind::Cause(diagnostic) = &report.kind else { panic!("expected meter cause") };
        assert_eq!(diagnostic.code.as_deref(), Some("sim/meter-type-invalid"));
        assert_eq!(diagnostic.source, "sim");
        assert_eq!(
            diagnostic.message,
            format!("invalid MeterType enum value: {id_enum}.{id_type}")
        );
    }
}
