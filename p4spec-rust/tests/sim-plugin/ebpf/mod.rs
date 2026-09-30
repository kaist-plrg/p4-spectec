use p4spec_rust::{
    lang::{
        common::source::Span,
        data::{
            typ,
            value::{Value, make},
        },
    },
    sim_plugin::{
        ebpf::{self, Ebpf, pipe::ExternObject},
        io::{Rx, Tx},
        spec::func,
    },
    stf::ast::Statement,
};

fn counter_counts<Interp>(
    runner: &mut p4spec_rust::runner::Runner<Interp, p4spec_rust::runner::BuiltinInterface, Ebpf>,
    value_arch: Value,
    names: &[&str],
) -> Vec<u32>
where
    Interp: p4spec_rust::runner::Interpreter<p4spec_rust::runner::BuiltinInterface, Ebpf>,
{
    let values = names
        .iter()
        .map(|name| make::text(runner.arena_mut(), (*name).to_owned(), Span::default()).unwrap())
        .collect();
    let typ = typ::make::list(typ::make::var(
        p4spec_rust::phrase!(node: "nameIR".to_owned(), span: Span::default()),
        vec![],
    ));
    let value_id =
        make::list(runner.arena_mut(), typ.node.into(), values, Span::default()).unwrap();
    let value_state =
        func::find_object_state_e(&mut runner.context(), value_arch, value_id).unwrap();
    let encoding = p4spec_rust::lang::data::value::external::Encoding::ArenaRelative;
    let ExternObject::CounterArray(counter) =
        ExternObject::from_value(runner.arena_mut(), encoding, &value_state).unwrap()
    else {
        panic!("counter object")
    };
    counter.counts
}

fn pipeline() -> (
    p4spec_rust::runner::Runner<
        p4spec_rust::interp::al::AlInterp,
        p4spec_rust::runner::BuiltinInterface,
        Ebpf,
    >,
    p4spec_rust::sim_plugin::state::SimState,
) {
    let mut runner = super::runner(Ebpf::default());
    let program = super::parse_program(
        runner.arena_mut(),
        &super::repo().join("p4spec-rust/tests/fixtures/sim-plugin/ebpf/counter.p4"),
    );
    let state = ebpf::init_pipe(&mut runner.context(), program).unwrap();
    (runner, state)
}

#[test]
fn test_parser_reject_preserves_state() {
    let (mut runner, mut state) = pipeline();
    state.txs.push(Tx { port: 9, packet: "old".to_owned() });
    ebpf::drive_pipe(&mut runner.context(), &mut state, &Rx { port: 2, packet: "0000".to_owned() })
        .unwrap();
    assert!(state.txs.is_empty());
    assert_eq!(counter_counts(&mut runner, state.value_arch, &["main", "prs", "parsed"]), [1, 0]);
    assert_eq!(counter_counts(&mut runner, state.value_arch, &["main", "filt", "counted"]), [0, 0]);
}

#[test]
fn test_filter_accept_controls_transmission() {
    let (mut runner, mut state) = pipeline();
    for (packet, accepted) in [("0100", true), ("0200", false), ("01", false)] {
        ebpf::drive_pipe(
            &mut runner.context(),
            &mut state,
            &Rx { port: 7, packet: packet.to_owned() },
        )
        .unwrap();
        assert_eq!(state.txs.len(), usize::from(accepted));
        if accepted {
            assert_eq!(state.txs[0].port, 7);
            assert_eq!(state.txs[0].packet, packet);
        }
    }
}

#[test]
fn test_counter_state_persists_across_packets() {
    let (mut runner, mut state) = pipeline();
    for packet in ["0100", "0101", "0200"] {
        ebpf::drive_pipe(
            &mut runner.context(),
            &mut state,
            &Rx { port: 0, packet: packet.to_owned() },
        )
        .unwrap();
    }
    assert_eq!(counter_counts(&mut runner, state.value_arch, &["main", "prs", "parsed"]), [3, 0]);
    assert_eq!(counter_counts(&mut runner, state.value_arch, &["main", "filt", "counted"]), [6, 3]);
}

#[test]
fn test_sl_counter_state_persists_across_packets_in_both_determinism_modes() {
    use p4spec_rust::{
        frontend::parse::parse_files,
        pass::{algo, elaborate, structure},
        runner::{Config, build_sl},
    };

    let spec_el = parse_files([super::repo().join("spec")]).unwrap();
    let spec_il = elaborate::convert(spec_el).unwrap();
    let spec_al = algo::convert(spec_il).unwrap();
    let spec_sl = structure::convert(spec_al, true).unwrap();
    for det in [false, true] {
        let mut runner =
            build_sl(spec_sl.clone(), Config::new(true, det, false), Ebpf::new(Default::default()))
                .unwrap();
        let program = super::parse_program(
            runner.arena_mut(),
            &super::repo().join("p4spec-rust/tests/fixtures/sim-plugin/ebpf/counter.p4"),
        );
        let mut state = ebpf::init_pipe(&mut runner.context(), program).unwrap();
        for packet in ["0100", "0101", "0200"] {
            ebpf::drive_pipe(
                &mut runner.context(),
                &mut state,
                &Rx { port: 0, packet: packet.to_owned() },
            )
            .unwrap();
        }
        assert_eq!(
            counter_counts(&mut runner, state.value_arch, &["main", "prs", "parsed"]),
            [3, 0]
        );
        assert_eq!(
            counter_counts(&mut runner, state.value_arch, &["main", "filt", "counted"]),
            [6, 3]
        );
    }
}

#[test]
fn test_native_ebpf_micro_fixture_packets() {
    use p4spec_rust::lang::data::value::external::Encoding;

    for encoding in [Encoding::ArenaRelative, Encoding::ArenaIndependent] {
        let mut runner = super::runner(Ebpf::new(encoding));
        let path = super::repo().join("p4spec/test/micro/sim-ebpf/ebpf.p4");
        let program = super::parse_program(runner.arena_mut(), &path);
        let mut state = ebpf::init_pipe(&mut runner.context(), program).unwrap();
        let program_stf = p4spec_rust::stf::parse::parse_file(path.with_extension("stf")).unwrap();
        let mut txs = Vec::new();
        let mut txs_expect = Vec::new();
        for stmt in program_stf {
            match stmt.node {
                Statement::Packet { port, packet } => {
                    ebpf::drive_pipe(
                        &mut runner.context(),
                        &mut state,
                        &Rx { port: port.parse().unwrap(), packet },
                    )
                    .unwrap();
                    txs.extend(state.txs.iter().map(|tx| (tx.port, tx.packet.clone())));
                }
                Statement::Expect { port, packet_expected: Some(packet), exact: _ } => {
                    txs_expect.push((port.parse::<usize>().unwrap(), packet))
                }
                _ => panic!("micro fixture contains packet and expectation statements"),
            }
        }
        assert_eq!(txs.len(), 2);
        assert_eq!(txs, txs_expect);
    }
}

#[test]
fn test_stf_transformation_retains_payload_and_source_replacement_order() {
    let program = p4spec_rust::stf::parse::parse_str("ebpf.stf", "add PIPE_c1_table 10 key:3 PIPE_c1_action(arg:2)\nsetdefault pipe_t _NoAction()\npacket 0 ab\n").unwrap();
    let stmts: Vec<_> = program
        .into_iter()
        .map(|stmt| ebpf::transform_stf_stmt(stmt.node))
        .collect();
    let Statement::Add { table, priority, matches, action, .. } = &stmts[0] else { panic!("add") };
    assert_eq!(table.as_str(), "main.filt.c1.table");
    assert_eq!(*priority, Some(10));
    assert_eq!(matches[0].name.as_str(), "key");
    assert_eq!(action.name.as_str(), "action");
    assert_eq!(action.args[0].num, "2");
    let Statement::SetDefault { table, action } = &stmts[1] else { panic!("default") };
    assert_eq!(table.as_str(), "main.filt.t");
    assert_eq!(action.name.as_str(), "NoAction");
    assert!(matches!(&stmts[2], Statement::Packet { packet, .. } if packet == "ab"));
}

#[path = "object.rs"]
mod object;

#[path = "pipe.rs"]
mod pipe;
