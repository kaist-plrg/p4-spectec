#[path = "func.rs"]
mod func;
#[path = "object.rs"]
mod object;

use std::collections::BTreeMap;

use p4spec_rust::{
    lang::{
        common::source::Span,
        data::{
            typ::{self},
            value::{Value, get, make},
        },
        il::ast::Typ,
    },
    runner::{
        Extern, ExternError, Interface, InterfaceError, Interpreter, NullInterface, Runner,
        RunnerContext,
    },
    sim_plugin::{dummy::Dummy, spec::pack},
};

#[derive(Debug, thiserror::Error)]
enum TestError {
    #[error(transparent)]
    Extern(#[from] ExternError),
    #[error(transparent)]
    Interface(#[from] InterfaceError),
}

#[derive(Default)]
struct PacketInterp {
    values_var: BTreeMap<String, Value>,
    value_typ: Option<Value>,
    calls: Vec<(String, Vec<Value>)>,
    size_min: usize,
    size_max: usize,
    alignment: usize,
    arity_rel: usize,
    fail_rel: bool,
}

impl<Iface: Interface, Ext: Extern> Interpreter<Iface, Ext> for PacketInterp {
    type Spec = ();
    type Error = TestError;

    fn clear(&mut self) {}

    fn reset(&mut self) {}

    fn eval_program(
        _ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        _name: &str,
        _program: Value,
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
        ctx.interp_mut()
            .calls
            .push((name.to_owned(), values.to_vec()));
        let value = match name {
            "find_type_e" => {
                assert_eq!(get::text(ctx.arena(), &values[2]), Ok("T"));
                let value_typ = ctx.interp().value_typ;
                make::opt(
                    ctx.arena_mut(),
                    typ::make::opt(typ::make::text()).node.into(),
                    value_typ,
                    Span::default(),
                )
                .unwrap()
            }
            "subst_type_e" | "default" => *values.last().unwrap(),
            "sizeof_minSizeInBits'" | "sizeof_maxSizeInBits'" => {
                let size = if name == "sizeof_minSizeInBits'" {
                    ctx.interp().size_min
                } else {
                    ctx.interp().size_max
                };
                make::int(ctx.arena_mut(), size.into(), Span::default()).unwrap()
            }
            "find_var_e" => {
                let value_name = *get::case(ctx.arena(), &values[0]).unwrap().args()[0];
                let name_var = get::text(ctx.arena(), &value_name).unwrap();
                ctx.interp().values_var[name_var]
            }
            "bitacc_range_op" => {
                let alignment = ctx.interp().alignment;
                pack::p4_fixed_bit(ctx.arena_mut(), 3.into(), alignment.into()).unwrap()
            }
            "write_value_from_bits" => values[2],
            "write_bits_from_value" => values[0],
            _ => panic!("unexpected function {name}"),
        };
        Ok(value)
    }

    fn eval_rel(
        ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        values: &[Value],
    ) -> Result<Vec<Value>, TestError> {
        assert_eq!(name, "Lvalue_write");
        assert_eq!(values.len(), 5);
        ctx.interp_mut()
            .calls
            .push((name.to_owned(), values.to_vec()));
        if ctx.interp().fail_rel {
            return Err(ExternError::Message("write failed".to_owned()).into());
        }
        Ok(vec![values[4]; ctx.interp().arity_rel])
    }
}

type PacketRunner = Runner<PacketInterp, NullInterface, Dummy>;

fn packet_runner(size_min: usize, size_max: usize) -> (PacketRunner, Value, Value) {
    let mut runner = Runner::new(
        (),
        PacketInterp { size_min, size_max, arity_rel: 1, ..PacketInterp::default() },
        NullInterface,
        Dummy,
    );
    let value_ctx = make::text(runner.arena_mut(), "ctx".to_owned(), Span::default()).unwrap();
    let value_arch = make::text(runner.arena_mut(), "arch".to_owned(), Span::default()).unwrap();
    let value_typ = make::text(runner.arena_mut(), "T".to_owned(), Span::default()).unwrap();
    runner.context().interp_mut().value_typ = Some(value_typ);
    for name in ["hdr", "variableSizeHeader"] {
        runner
            .context()
            .interp_mut()
            .values_var
            .insert(name.to_owned(), value_ctx);
    }
    (runner, value_ctx, value_arch)
}
