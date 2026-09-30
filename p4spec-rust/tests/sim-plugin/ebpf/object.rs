#[path = "object/counter_array.rs"]
mod counter_array;

use std::collections::BTreeMap;

use p4spec_rust::{
    lang::{
        common::source::Span,
        data::{
            typ::{self, Typ},
            value::{Value, ValueArena, get, make},
        },
    },
    runner::{
        Extern, ExternError, Interface, InterfaceError, Interpreter, NullInterface, Runner,
        RunnerContext,
    },
    sim_plugin::{dummy::Dummy, ebpf::object::CounterArray, spec::pack},
};

#[derive(Debug, thiserror::Error)]
enum TestError {
    #[error(transparent)]
    Extern(#[from] ExternError),
    #[error(transparent)]
    Interface(#[from] InterfaceError),
}

#[derive(Default)]
struct CounterInterp {
    values_var: BTreeMap<String, Value>,
    calls: Vec<String>,
}

impl<Iface: Interface, Ext: Extern> Interpreter<Iface, Ext> for CounterInterp {
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

    fn eval_rel(
        _: &mut RunnerContext<'_, Self, Iface, Ext>,
        _: &str,
        _: &[Value],
    ) -> Result<Vec<Value>, TestError> {
        unreachable!()
    }

    fn eval_func(
        ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<Value, TestError> {
        assert_eq!(name, "find_var_e");
        assert!(targs.is_empty());
        let value_name = *get::case(ctx.arena(), &values[0]).unwrap().args()[0];
        let name_var = get::text(ctx.arena(), &value_name).unwrap().to_owned();
        ctx.interp_mut().calls.push(name_var.clone());
        ctx.interp()
            .values_var
            .get(&name_var)
            .copied()
            .ok_or_else(|| ExternError::Message(format!("missing local {name_var}")).into())
    }
}

type CounterRunner = Runner<CounterInterp, NullInterface, Dummy>;

fn list(arena: &mut ValueArena, values: Vec<Value>) -> Value {
    make::list(arena, typ::make::list(typ::make::text()).node.into(), values, Span::default())
        .unwrap()
}

fn names(arena: &mut ValueArena, names_param: &[&str]) -> Value {
    let values = names_param
        .iter()
        .map(|name| make::text(arena, (*name).to_owned(), Span::default()).unwrap())
        .collect();
    list(arena, values)
}

fn boolean(arena: &mut ValueArena, check: bool) -> Value {
    let value_check = make::bool(arena, check, Span::default()).unwrap();
    let mixop = p4spec_rust::frontend::parse::parse_mixop("_B bool").unwrap();
    let value_case =
        p4spec_rust::lang::common::notation::mixop::Mixop::fill(&mixop, vec![value_check]).unwrap();
    let typ_value = typ::make::var(
        p4spec_rust::phrase!(node: "value".to_owned(), span: Span::default()),
        vec![],
    );
    make::case(arena, typ_value.node.into(), value_case, Span::default()).unwrap()
}

fn local(runner: &mut CounterRunner, name: &str, int: i64) {
    let value = pack::p4_fixed_bit(runner.arena_mut(), 32.into(), int.into()).unwrap();
    runner
        .context()
        .interp_mut()
        .values_var
        .insert(name.to_owned(), value);
}

impl From<TestError> for ExternError {
    fn from(error: TestError) -> Self {
        match error {
            TestError::Extern(error) => error,
            TestError::Interface(error) => ExternError::Report(error.into_report()),
        }
    }
}
