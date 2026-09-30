use p4spec_rust::{
    lang::{
        common::{
            notation::{atom::Atom, mixop::Mixop},
            source::Span,
        },
        data::{
            typ::{self, Typ},
            value::{Value, ValueArena, get, make},
        },
    },
    runner::{
        Extern, ExternError, Interface, InterfaceError, Interpreter, NullInterface, Runner,
        RunnerContext,
    },
    sim_plugin::{
        core::object::PacketIn,
        io::Tx,
        psa::{
            arch::Arch,
            multicast::Node,
            packet::{Entrypoint, Packet},
            pipe::{self, ObjectState, Psa},
        },
        spec::pack,
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

#[derive(Default)]
struct FailureInterp {
    updates_arch: usize,
    value_arch_completed: Option<Value>,
}

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

fn update_field(arena: &mut ValueArena, value: Value, name: &str, value_field: Value) -> Value {
    let mut fields = get::structure(arena, &value).unwrap().to_vec();
    fields
        .iter_mut()
        .find(|(atom, _)| atom.node == Atom::Keyword(name.to_owned()))
        .unwrap()
        .1 = value_field;
    make::structure(arena, typ_named("arch").node.into(), fields, Span::default()).unwrap()
}

fn option(arena: &mut ValueArena, value: Value) -> Value {
    make::opt(
        arena,
        typ::make::opt(typ_named("objectState")).node.into(),
        Some(value),
        Span::default(),
    )
    .unwrap()
}

fn name_of_id(arena: &ValueArena, value_id: &Value) -> String {
    let value_name = get::one(get::list(arena, value_id).unwrap()).unwrap();
    get::text(arena, value_name).unwrap().to_owned()
}

impl<Iface: Interface, Ext: Extern> Interpreter<Iface, Ext> for FailureInterp {
    type Spec = ();
    type Error = TestError;

    fn clear(&mut self) {}

    fn reset(&mut self) {}

    fn eval_program(
        _: &mut RunnerContext<'_, Self, Iface, Ext>,
        _: &str,
        _: Value,
    ) -> Result<Vec<Value>, TestError> {
        unreachable!()
    }

    fn eval_func(
        ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<Value, TestError> {
        assert!(targs.is_empty());
        match name {
            "find_archState_e" => Ok(field(ctx.arena(), values[0], "STATE")),
            "update_archState_e" => {
                ctx.interp_mut().updates_arch += 1;
                if ctx.interp().updates_arch == 2 {
                    return Err(ExternError::Message("restore failed".to_owned()).into());
                }
                let value_arch = update_field(ctx.arena_mut(), values[0], "STATE", values[1]);
                ctx.interp_mut().value_arch_completed = Some(value_arch);
                Ok(value_arch)
            }
            "find_objectState_e" => {
                let name = name_of_id(ctx.arena(), &values[1]);
                let value = field(ctx.arena(), values[0], &name);
                Ok(option(ctx.arena_mut(), value))
            }
            "update_objectState_e" => {
                let name = name_of_id(ctx.arena(), &values[1]);
                let value_arch = update_field(ctx.arena_mut(), values[0], &name, values[2]);
                Ok(option(ctx.arena_mut(), value_arch))
            }
            _ => panic!("unexpected function {name}"),
        }
    }

    fn eval_rel(
        ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        values: &[Value],
    ) -> Result<Vec<Value>, TestError> {
        match name {
            "Lvalue_read" => {
                let value_name = *get::case(ctx.arena(), &values[3]).unwrap().args()[1];
                let name_field = get::text(ctx.arena(), &value_name).unwrap().to_owned();
                let value = match name_field.as_str() {
                    "clone" => {
                        let value = make::bool(ctx.arena_mut(), true, Span::default()).unwrap();
                        let mixop = p4spec_rust::frontend::parse::parse_mixop("_B bool").unwrap();
                        let mixfix = Mixop::fill(&mixop, vec![value]).unwrap();
                        make::case(
                            ctx.arena_mut(),
                            typ_named("value").node.into(),
                            mixfix,
                            Span::default(),
                        )
                        .unwrap()
                    }
                    "clone_session_id" => {
                        pack::p4_fixed_bit(ctx.arena_mut(), 16.into(), 1.into()).unwrap()
                    }
                    "class_of_service" => {
                        pack::p4_fixed_bit(ctx.arena_mut(), 8.into(), 0.into()).unwrap()
                    }
                    _ => panic!("unexpected metadata field {name_field}"),
                };
                Ok(vec![value])
            }
            "PSA_egress_init_metadata" => {
                let value_ctx =
                    make::text(ctx.arena_mut(), "clone context".to_owned(), Span::default())
                        .unwrap();
                Ok(vec![value_ctx])
            }
            _ => panic!("unexpected relation {name}"),
        }
    }
}

#[test]
fn test_clone_restoration_failure_preserves_queued_clones_and_completed_state() {
    let mut runner = Runner::new((), FailureInterp::default(), NullInterface, Psa::default());
    let value_ctx =
        make::text(runner.arena_mut(), "original context".to_owned(), Span::default()).unwrap();
    let mut arch = Arch::default();
    arch.mirrortable.insert(1, 2);
    arch.multicast.groups.insert(2, vec![0]);
    arch.multicast
        .nodes
        .insert(0, vec![Node { port: 12, instance: 4 }]);
    arch.queue.push_back(Packet {
        value_ctx,
        packet_in: PacketIn::init("CD").unwrap(),
        entrypoint: Entrypoint::Egress,
    });
    let encoding = p4spec_rust::lang::data::value::external::Encoding::ArenaRelative;
    let value_state = arch.to_value(runner.arena_mut(), encoding).unwrap();
    let mut pkt_in = PacketIn::init("AB").unwrap();
    pkt_in.idx = 4;
    let value_in = ObjectState::PacketIn(pkt_in)
        .to_value(runner.arena_mut(), encoding)
        .unwrap();
    let value_egress = ObjectState::PacketIn(PacketIn::init("CD").unwrap())
        .to_value(runner.arena_mut(), encoding)
        .unwrap();
    let fields = [
        ("STATE", value_state),
        ("ingress_packet_in", value_in),
        ("egress_packet_in", value_egress),
    ]
    .into_iter()
    .map(|(name, value)| {
        (p4spec_rust::phrase!(node: Atom::Keyword(name.to_owned()), span: Span::default()), value)
    })
    .collect();
    let value_arch =
        make::structure(runner.arena_mut(), typ_named("arch").node.into(), fields, Span::default())
            .unwrap();
    let tx = Tx { port: 99, packet: "prior output".to_owned() };
    let mut state = SimState { value_ctx, value_arch, txs: vec![tx] };

    assert!(
        matches!(pipe::run_pre(&mut runner.context(), &mut state), Err(TestError::Extern(ExternError::Message(msg))) if msg == "restore failed")
    );
    assert_eq!(runner.context().interp().updates_arch, 2);
    assert_eq!(Some(state.value_arch), runner.context().interp().value_arch_completed);
    assert_eq!(state.value_ctx, value_ctx);
    assert_eq!(state.txs.len(), 1);
    assert_eq!(state.txs[0].port, 99);
    assert_eq!(state.txs[0].packet, "prior output");
    let arch = pipe::find_arch_state(&mut runner.context(), state.value_arch).unwrap();
    assert_eq!(arch.queue.len(), 2);
    assert_eq!(arch.queue[0].packet_in, PacketIn::init("CD").unwrap());
    assert_eq!(arch.queue[1].packet_in, PacketIn::init("AB").unwrap());
    assert_eq!(get::text(runner.arena(), &arch.queue[1].value_ctx), Ok("clone context"));
    let value_in = field(runner.arena(), state.value_arch, "ingress_packet_in");
    assert_eq!(
        ObjectState::from_value(runner.arena_mut(), encoding, &value_in).unwrap(),
        ObjectState::PacketIn(PacketIn::init("AB").unwrap())
    );
}
