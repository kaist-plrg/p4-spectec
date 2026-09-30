#[path = "object/counter.rs"]
mod counter;
#[path = "object/hash.rs"]
mod hash;
#[path = "object/internet_checksum.rs"]
mod internet_checksum;
#[path = "object/meter.rs"]
mod meter;
#[path = "object/register.rs"]
mod register;

use std::collections::BTreeMap;

use num_bigint::BigInt;
use p4spec_rust::{
    lang::{
        common::source::Span,
        data::{
            typ,
            value::{Value, ValueArena, get, make},
        },
    },
    runner::{
        Extern, ExternError, Interface, Interpreter, InterpreterError, NullInterface, Runner,
        RunnerContext,
    },
    sim_plugin::{
        dummy::Dummy,
        psa::object::{Color, Counter, HashExtern, InternetChecksum, Meter, Register},
        spec::{pack, unpack},
    },
};

#[derive(Default)]
struct ObjectInterp {
    values_var: BTreeMap<String, Value>,
    value_default: Option<Value>,
    calls: Vec<String>,
}

impl<Iface: Interface, Ext: Extern> Interpreter<Iface, Ext> for ObjectInterp {
    type Spec = ();

    fn clear(&mut self) {}

    fn reset(&mut self) {}

    fn eval_program(
        _: &mut RunnerContext<'_, Self, Iface, Ext>,
        _: &str,
        _: Value,
    ) -> Result<Vec<Value>, InterpreterError> {
        unreachable!()
    }

    fn eval_rel(
        _: &mut RunnerContext<'_, Self, Iface, Ext>,
        _: &str,
        _: &[Value],
    ) -> Result<Vec<Value>, InterpreterError> {
        unreachable!()
    }

    fn eval_func(
        ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        _: &[typ::Typ],
        values: &[Value],
    ) -> Result<Value, InterpreterError> {
        let name_call = if name == "find_var_e" {
            let value_name = *get::case(ctx.arena(), &values[0]).unwrap().args()[0];
            get::text(ctx.arena(), &value_name).unwrap().to_owned()
        } else {
            name.to_owned()
        };
        ctx.interp_mut().calls.push(name_call.clone());
        match name {
            "find_var_e" => ctx
                .interp()
                .values_var
                .get(&name_call)
                .copied()
                .ok_or_else(|| ExternError::message(format!("missing {name_call}")).into()),
            "default" => Ok(ctx.interp().value_default.unwrap()),
            "find_type_e" => {
                let value_typ = ctx.interp().value_default;
                Ok(make::opt(
                    ctx.arena_mut(),
                    typ::make::opt(typ::make::text()).node.into(),
                    value_typ,
                    Span::default(),
                )
                .unwrap())
            }
            "cast_op" => Ok(values[1]),
            _ => panic!("unexpected {name}"),
        }
    }
}

type ObjectRunner = Runner<ObjectInterp, NullInterface, Dummy>;

fn setup() -> (ObjectRunner, Value, Value) {
    let mut runner = Runner::new((), ObjectInterp::default(), NullInterface, Dummy);
    let value_ctx = make::text(runner.arena_mut(), "ctx".to_owned(), Span::default()).unwrap();
    let value_arch = make::text(runner.arena_mut(), "arch".to_owned(), Span::default()).unwrap();
    runner.context().interp_mut().value_default = Some(value_ctx);
    (runner, value_ctx, value_arch)
}

fn list(arena: &mut ValueArena, values: Vec<Value>) -> Value {
    make::list(arena, typ::make::list(typ::make::text()).node.into(), values, Span::default())
        .unwrap()
}

fn arguments(arena: &mut ValueArena, args: &[(&str, Value)]) -> (Value, Value) {
    let values_name = args
        .iter()
        .map(|(name, _)| make::text(arena, (*name).to_owned(), Span::default()).unwrap())
        .collect();
    let value_ids = list(arena, values_name);
    let value_args = list(arena, args.iter().map(|(_, value)| *value).collect());
    (value_ids, value_args)
}

fn local(runner: &mut ObjectRunner, name: &str, int: i64) {
    let value = pack::p4_fixed_bit(runner.arena_mut(), 32.into(), int.into()).unwrap();
    runner
        .context()
        .interp_mut()
        .values_var
        .insert(name.to_owned(), value);
}

fn returned(arena: &ValueArena, value: Value) -> Value {
    let value_opt = *get::case(arena, &value).unwrap().args()[0];
    get::opt(arena, &value_opt).unwrap().unwrap()
}

fn tuple_data(runner: &mut ObjectRunner, int: i64) {
    let value_field = pack::p4_fixed_bit(runner.arena_mut(), 16.into(), int.into()).unwrap();
    let value_fields = list(runner.arena_mut(), vec![value_field]);
    let mixop = p4spec_rust::frontend::parse::parse_mixop("TUPLE `( value* `)").unwrap();
    let mixfix =
        p4spec_rust::lang::common::notation::mixop::Mixop::fill(&mixop, vec![value_fields])
            .unwrap();
    let typ_value = typ::make::var(
        p4spec_rust::phrase!(node: "value".to_owned(), span: Span::default()),
        vec![],
    );
    let value =
        make::case(runner.arena_mut(), typ_value.node.into(), mixfix, Span::default()).unwrap();
    runner
        .context()
        .interp_mut()
        .values_var
        .insert("data".to_owned(), value);
}
