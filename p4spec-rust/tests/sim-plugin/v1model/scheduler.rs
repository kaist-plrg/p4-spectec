use num_traits::ToPrimitive;

use p4spec_rust::{
    lang::{
        common::{notation::atom::Atom, source::Span},
        data::{
            typ::{self, Typ},
            value::{Value, ValueArena, get, make},
        },
    },
    runner::{
        ExternError, Interface, Interpreter, InterpreterError, NullInterface, Runner, RunnerContext,
    },
    sim_plugin::{
        core::object::{PacketIn, PacketOut},
        io::{Rx, Tx},
        spec::{pack, unpack},
        state::SimState,
        v1model::{
            arch::Arch,
            packet::{Action, CloneInfo, CloneType, Entrypoint, Packet},
            pipe::{self, ObjectState, V1Model},
        },
    },
};

#[derive(Clone, Copy)]
enum Scenario {
    Normal,
    CloneDrop,
    Recirculate,
}

struct TraceInterp {
    scenario: Scenario,
    events: Vec<String>,
    egress_count: usize,
}

type TestRunner = Runner<TraceInterp, NullInterface, V1Model>;

fn typ_named(name: &str) -> Typ {
    typ::make::var(p4spec_rust::phrase!(node: name.to_owned(), span: Span::default()), vec![])
}

fn field(arena: &ValueArena, value: Value, name: &str) -> Value {
    get::structure(arena, &value)
        .unwrap()
        .iter()
        .find(|(atom, _)| atom.node == Atom::Keyword(name.to_owned()))
        .unwrap()
        .1
}

fn record(arena: &mut ValueArena, fields: Vec<(&str, Value)>) -> Value {
    let fields = fields
        .into_iter()
        .map(|(name, value)| {
            (
                p4spec_rust::phrase!(node: Atom::Keyword(name.to_owned()), span: Span::default()),
                value,
            )
        })
        .collect();
    make::structure(arena, typ_named("test").node.into(), fields, Span::default()).unwrap()
}

fn update(arena: &mut ValueArena, value: Value, name: &str, value_field: Value) -> Value {
    let mut fields = get::structure(arena, &value).unwrap().to_vec();
    fields
        .iter_mut()
        .find(|(atom, _)| atom.node == Atom::Keyword(name.to_owned()))
        .unwrap()
        .1 = value_field;
    make::structure(arena, typ_named("test").node.into(), fields, Span::default()).unwrap()
}

fn option(arena: &mut ValueArena, value: Value) -> Value {
    make::opt(arena, typ_named("test").node.into(), Some(value), Span::default()).unwrap()
}

fn int(arena: &ValueArena, value_ctx: Value, name: &str) -> i64 {
    (unpack::p4_fixed_bit(arena, &field(arena, value_ctx, name))
        .unwrap()
        .1)
        .to_i64()
        .unwrap()
}

fn write_int(arena: &mut ValueArena, value_ctx: Value, name: &str, width: i64, int: i64) -> Value {
    let value = pack::p4_fixed_bit(arena, width.into(), int.into()).unwrap();
    update(arena, value_ctx, name, value)
}

impl<Iface: Interface> Interpreter<Iface, V1Model> for TraceInterp {
    type Spec = ();

    fn clear(&mut self) {}

    fn reset(&mut self) {}

    fn eval_program(
        _: &mut RunnerContext<'_, Self, Iface, V1Model>,
        _: &str,
        _: Value,
    ) -> Result<Vec<Value>, InterpreterError> {
        unreachable!()
    }

    fn eval_func(
        ctx: &mut RunnerContext<'_, Self, Iface, V1Model>,
        name: &str,
        _: &[Typ],
        values: &[Value],
    ) -> Result<Value, InterpreterError> {
        match name {
            "find_archState_e" => Ok(field(ctx.arena(), values[0], "STATE")),
            "update_archState_e" => Ok(update(ctx.arena_mut(), values[0], "STATE", values[1])),
            "find_objectState_e" | "update_objectState_e" => {
                let value_name = *get::one(get::list(ctx.arena(), &values[1]).unwrap()).unwrap();
                let name_field = get::text(ctx.arena(), &value_name).unwrap().to_owned();
                let value = if name == "find_objectState_e" {
                    field(ctx.arena(), values[0], &name_field)
                } else {
                    update(ctx.arena_mut(), values[0], &name_field, values[2])
                };
                Ok(option(ctx.arena_mut(), value))
            }
            _ => panic!("unexpected function {name}"),
        }
    }

    fn eval_rel(
        ctx: &mut RunnerContext<'_, Self, Iface, V1Model>,
        name: &str,
        values: &[Value],
    ) -> Result<Vec<Value>, InterpreterError> {
        match name {
            "V1Model_init_packet_in" | "V1Model_init_packet_out" => {
                let name_field = name.strip_prefix("V1Model_init_").unwrap();
                let value_arch = update(ctx.arena_mut(), values[1], name_field, values[2]);
                Ok(vec![values[0], value_arch])
            }
            "V1Model_init_globals" => Ok(vec![values[0]]),
            "Lvalue_read" | "Lvalue_write" => {
                let value_name = *get::case(ctx.arena(), &values[3]).unwrap().args()[1];
                let name_field = get::text(ctx.arena(), &value_name).unwrap().to_owned();
                Ok(vec![if name == "Lvalue_read" {
                    field(ctx.arena(), values[1], &name_field)
                } else {
                    update(ctx.arena_mut(), values[1], &name_field, values[4])
                }])
            }
            "V1Model_setup_preserved_meta_fields" => {
                ctx.interp_mut().events.push("preserve".to_owned());
                Ok(vec![values[0]])
            }
            "V1Model_parser" | "V1Model_verify" | "V1Model_ingress" | "V1Model_egress"
            | "V1Model_check" | "V1Model_deparse" => {
                let phase = name.strip_prefix("V1Model_").unwrap();
                ctx.interp_mut().events.push(phase.to_owned());
                let mut value_ctx = values[0];
                let mut value_arch = values[1];
                let value_arch_state = field(ctx.arena(), value_arch, "STATE");
                let encoding = p4spec_rust::lang::data::value::external::Encoding::ArenaRelative;
                let mut arch = Arch::from_value(ctx.arena_mut(), encoding, &value_arch_state)?;
                if matches!(phase, "ingress" | "egress") {
                    assert_eq!(arch.action, Action::default());
                }
                if phase == "egress" {
                    assert_eq!(
                        int(ctx.arena(), value_ctx, "egress_spec"),
                        int(ctx.arena(), value_ctx, "egress_port")
                    );
                    ctx.interp_mut().egress_count += 1;
                    if ctx.interp().egress_count == 1 {
                        match ctx.interp().scenario {
                            Scenario::CloneDrop => {
                                arch.action.clone_opt = Some(CloneInfo(CloneType::E2E, 1, 0));
                                value_ctx =
                                    write_int(ctx.arena_mut(), value_ctx, "egress_spec", 9, 511);
                            }
                            Scenario::Recirculate => arch.action.recirculate_opt = Some(2),
                            Scenario::Normal => {}
                        }
                    }
                }
                let value_arch_state = arch.to_value(ctx.arena_mut(), encoding)?;
                value_arch = update(ctx.arena_mut(), value_arch, "STATE", value_arch_state);
                let effects = int(ctx.arena(), value_arch, "effects") + 1;
                value_arch = write_int(ctx.arena_mut(), value_arch, "effects", 32, effects);
                let typ = typ::make::opt(typ::make::var(
                    p4spec_rust::phrase!(node: "value".to_owned(), span: Span::default()),
                    Vec::new(),
                ));
                let value_opt = make::opt(ctx.arena_mut(), typ.node.into(), None, Span::default())
                    .map_err(ExternError::from)?;
                let mixop = p4spec_rust::frontend::parse::parse_mixop("RETURN value?").unwrap();
                let value_case = p4spec_rust::lang::common::notation::mixop::Mixop::fill(
                    &mixop,
                    vec![value_opt],
                )
                .unwrap();
                let typ = typ::make::var(
                    p4spec_rust::phrase!(node: "returnResult".to_owned(), span: Span::default()),
                    Vec::new(),
                );
                let value_result =
                    make::case(ctx.arena_mut(), typ.node.into(), value_case, Span::default())
                        .map_err(ExternError::from)?;
                Ok(vec![value_ctx, value_arch, value_result])
            }
            _ => panic!("unexpected relation {name}"),
        }
    }
}

fn setup(scenario: Scenario) -> (TestRunner, SimState) {
    let mut runner = Runner::new(
        (),
        TraceInterp { scenario, events: vec![], egress_count: 0 },
        NullInterface,
        V1Model::default(),
    );
    let mut fields = Vec::new();
    for (name, width, int) in [
        ("egress_spec", 9, 3),
        ("egress_port", 9, 0),
        ("mcast_grp", 16, 0),
        ("instance_type", 32, 0),
        ("egress_rid", 16, 0),
    ] {
        fields.push((
            name,
            pack::p4_fixed_bit(runner.arena_mut(), width.into(), int.into()).unwrap(),
        ));
    }
    let value_ctx = record(runner.arena_mut(), fields);
    let mut arch = Arch::default();
    arch.mirrortable.insert(1, 7);
    let encoding = p4spec_rust::lang::data::value::external::Encoding::ArenaRelative;
    let value_arch_state = arch.to_value(runner.arena_mut(), encoding).unwrap();
    let value_in = ObjectState::PacketIn(PacketIn::init("AB").unwrap())
        .to_value(runner.arena_mut(), encoding)
        .unwrap();
    let value_out = ObjectState::PacketOut(PacketOut::default())
        .to_value(runner.arena_mut(), encoding)
        .unwrap();
    let value_effects = pack::p4_fixed_bit(runner.arena_mut(), 32.into(), 0.into()).unwrap();
    let value_arch = record(
        runner.arena_mut(),
        vec![
            ("STATE", value_arch_state),
            ("packet_in", value_in),
            ("packet_out", value_out),
            ("effects", value_effects),
        ],
    );
    (runner, SimState { value_ctx, value_arch, txs: vec![] })
}

fn arch(runner: &mut TestRunner, state: &SimState) -> Arch {
    pipe::find_arch_state(&mut runner.context(), state.value_arch).unwrap()
}

fn save_arch(runner: &mut TestRunner, state: &mut SimState, arch: &Arch) {
    state.value_arch =
        pipe::update_arch_state(&mut runner.context(), state.value_arch, arch).unwrap();
}

fn enqueue(runner: &mut TestRunner, state: &mut SimState) {
    pipe::schedule_packet(&mut runner.context(), state, Entrypoint::Egress).unwrap();
}

#[test]
fn test_egress_drop_keeps_clone_and_skips_post() {
    let (mut runner, mut state) = setup(Scenario::CloneDrop);
    enqueue(&mut runner, &mut state);
    pipe::run_scheduler(&mut runner.context(), &mut state).unwrap();
    assert_eq!(
        runner.context().interp().events,
        ["egress", "preserve", "egress", "check", "deparse"]
    );
    assert_eq!(
        state
            .txs
            .iter()
            .map(|tx| (tx.port, tx.packet.as_str()))
            .collect::<Vec<_>>(),
        [(7, "AB")]
    );
    assert_eq!(int(runner.arena(), state.value_arch, "effects"), 4);
    assert!(arch(&mut runner, &state).queue.is_empty());
}

#[test]
fn test_recirculation_skips_post_and_runs_queued_packet() {
    let (mut runner, mut state) = setup(Scenario::Recirculate);
    enqueue(&mut runner, &mut state);
    pipe::run_scheduler(&mut runner.context(), &mut state).unwrap();
    assert_eq!(
        runner.context().interp().events,
        [
            "egress", "preserve", "check", "deparse", "parser", "verify", "ingress", "egress",
            "check", "deparse"
        ]
    );
    assert_eq!(
        state
            .txs
            .iter()
            .map(|tx| (tx.port, tx.packet.as_str()))
            .collect::<Vec<_>>(),
        [(3, "AB")]
    );
    assert_eq!(int(runner.arena(), state.value_arch, "effects"), 9);
    assert!(arch(&mut runner, &state).queue.is_empty());
}

#[test]
fn test_scheduler_resets_packet_actions_and_retains_prior_transmissions() {
    let (mut runner, mut state) = setup(Scenario::Normal);
    enqueue(&mut runner, &mut state);
    let mut arch_state = arch(&mut runner, &state);
    arch_state.action.clone_opt = Some(CloneInfo(CloneType::E2E, 1, 0));
    arch_state.action.resubmit_opt = Some(1);
    arch_state.action.recirculate_opt = Some(2);
    save_arch(&mut runner, &mut state, &arch_state);
    state.txs.push(Tx { port: 99, packet: "CD".to_owned() });
    pipe::run_scheduler(&mut runner.context(), &mut state).unwrap();
    assert_eq!(runner.context().interp().events, ["egress", "check", "deparse"]);
    assert_eq!(state.txs.iter().map(|tx| tx.port).collect::<Vec<_>>(), [99, 3]);
    assert_eq!(arch(&mut runner, &state).action, Action::default());
}

#[test]
fn test_multicast_order_and_ingress_queue_priority() {
    let (mut runner, mut state) = setup(Scenario::Normal);
    enqueue(&mut runner, &mut state);
    let mut arch_state = arch(&mut runner, &state);
    arch_state.multicast.group_create(8);
    arch_state.multicast.node_create(11, &[2]);
    arch_state.multicast.node_create(22, &[9, 4]);
    arch_state.multicast.node_associate(8, 0);
    arch_state.multicast.node_associate(8, 1);
    assert_eq!(arch_state.multicast.groups[&8], [1, 0]);
    save_arch(&mut runner, &mut state, &arch_state);
    assert!(pipe::schedule_multicast(&mut runner.context(), &mut state, &arch_state, 8).unwrap());
    assert_eq!(int(runner.arena(), state.value_ctx, "egress_spec"), 2);
    let mut arch_state = arch(&mut runner, &state);
    assert_eq!(
        arch_state
            .queue
            .iter()
            .map(|pkt| int(runner.arena(), pkt.value_ctx, "egress_spec"))
            .collect::<Vec<_>>(),
        [3, 9, 4, 2]
    );
    arch_state.queue.push_front(Packet {
        value_ctx: state.value_ctx,
        packet_in: PacketIn::init("EF").unwrap(),
        entrypoint: Entrypoint::Ingress,
    });
    save_arch(&mut runner, &mut state, &arch_state);
    pipe::schedule_packet(&mut runner.context(), &mut state, Entrypoint::Ingress).unwrap();
    let mut arch_state = arch(&mut runner, &state);
    assert_eq!(arch_state.queue.pop_front().unwrap().packet_in, PacketIn::init("AB").unwrap());
    assert_eq!(arch_state.queue.pop_front().unwrap().packet_in, PacketIn::init("EF").unwrap());
    save_arch(&mut runner, &mut state, &arch_state);
    pipe::run_scheduler(&mut runner.context(), &mut state).unwrap();
    assert_eq!(state.txs.iter().map(|tx| tx.port).collect::<Vec<_>>(), [3, 9, 4, 2]);
}

#[test]
fn test_drive_pipe_clears_prior_transmissions_for_forwarded_and_dropped_inputs() {
    for drop in [false, true] {
        let (mut runner, mut state) = setup(Scenario::Normal);
        let rx = Rx { port: 1, packet: "AB".to_owned() };
        pipe::drive_pipe(&mut runner.context(), &mut state, &rx).unwrap();
        assert_eq!(state.txs, [Tx { port: 3, packet: "AB".to_owned() }]);

        if drop {
            state.value_ctx = write_int(runner.arena_mut(), state.value_ctx, "egress_spec", 9, 511);
        }
        let rx = Rx { port: 2, packet: "CD".to_owned() };
        pipe::drive_pipe(&mut runner.context(), &mut state, &rx).unwrap();
        if drop {
            assert!(state.txs.is_empty(), "dropped input retained prior transmissions");
        } else {
            assert_eq!(state.txs, [Tx { port: 3, packet: "CD".to_owned() }]);
        }
    }
}
