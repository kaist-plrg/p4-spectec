use crate::interp::report::ReportExt;
use std::collections::VecDeque;

use p4spec_rust::{
    lang::{
        common::{notation::atom::Atom, source::Span},
        data::{
            typ::{self, Typ, TypKind},
            value::{Value, ValueArena, ValueError, ValueTag, get, make},
        },
    },
    runner::{
        Extern, ExternError, Interface, InterfaceError, Interpreter, NullInterface, Runner,
        RunnerContext,
    },
    sim_plugin::{dummy::Dummy, table},
};

#[derive(Debug, thiserror::Error)]
enum TestError {
    #[error(transparent)]
    Extern(#[from] ExternError),
    #[error(transparent)]
    Interface(#[from] InterfaceError),
}

struct Call {
    name: &'static str,
    args: Vec<Value>,
    value: Value,
}

#[derive(Default)]
struct TableInterp {
    calls: VecDeque<Call>,
}

impl<Iface: Interface, Ext: Extern> Interpreter<Iface, Ext> for TableInterp {
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
        assert!(targs.is_empty());
        let call_expect = ctx.interp_mut().calls.pop_front().expect("unexpected call");
        assert_eq!(name, call_expect.name);
        assert_eq!(values.len(), call_expect.args.len());
        for (value_actual, value_expect) in values.iter().zip(&call_expect.args) {
            assert_eq!(ctx.arena().canon_id(value_actual), ctx.arena().canon_id(value_expect));
            assert_eq!(ctx.arena().typ(value_actual), ctx.arena().typ(value_expect));
        }
        Ok(call_expect.value)
    }
}

type TableRunner = Runner<TableInterp, NullInterface, Dummy>;

fn scripted_runner() -> TableRunner {
    Runner::new((), TableInterp::default(), NullInterface, Dummy)
}

fn typ_named(name: &str) -> Typ {
    typ::make::var(p4spec_rust::phrase!(node: name.to_owned(), span: Span::default()), vec![])
}

fn text(arena: &mut ValueArena, name: &str) -> Value {
    make::text(arena, name.to_owned(), Span::default()).unwrap()
}

fn tuple(arena: &mut ValueArena, name: &str, values: Vec<Value>) -> Value {
    make::tuple(arena, typ_named(name).node.into(), values, Span::default()).unwrap()
}

fn list(arena: &mut ValueArena, name: &str, values: Vec<Value>) -> Value {
    make::list(arena, typ::make::list(typ_named(name)).node.into(), values, Span::default())
        .unwrap()
}

fn opt(arena: &mut ValueArena, value: Option<Value>) -> Value {
    make::opt(arena, typ::make::opt(typ_named("object")).node.into(), value, Span::default())
        .unwrap()
}

fn call(runner: &mut TableRunner, name: &'static str, args: &[Value], value: Value) {
    runner
        .context()
        .interp_mut()
        .calls
        .push_back(Call { name, args: args.to_vec(), value });
}

#[test]
fn test_qualified_precedence_and_repeated_lookup_for_update() {
    for qualified in [true, false] {
        let mut runner = scripted_runner();
        let value_arch = text(runner.arena_mut(), "arch");
        let value_ctx = text(runner.arena_mut(), "ctx");
        let value_name = text(runner.arena_mut(), "pipe.table");
        let value_pipe = text(runner.arena_mut(), "pipe");
        let value_short = text(runner.arena_mut(), "table");
        let value_id = list(runner.arena_mut(), "nameIR", vec![value_pipe, value_short]);
        let value_table = text(runner.arena_mut(), "table before");
        let value_updated = text(runner.arena_mut(), "table after");
        let value_arch_updated = text(runner.arena_mut(), "arch after");
        let value_some = opt(runner.arena_mut(), Some(value_table));
        let value_none = opt(runner.arena_mut(), None);
        call(
            &mut runner,
            "find_object_qualified_e",
            &[value_arch, value_id],
            if qualified { value_some } else { value_none },
        );
        if !qualified {
            call(&mut runner, "find_object_unqualified_e", &[value_arch, value_short], value_some);
        }
        call(
            &mut runner,
            "tableObject_add_default_action",
            &[value_ctx, value_table, value_ctx],
            value_updated,
        );
        call(&mut runner, "find_object_qualified_e", &[value_arch, value_id], value_some);
        call(
            &mut runner,
            "update_object_qualified_e",
            &[value_arch, value_id, value_updated],
            value_arch_updated,
        );
        assert_eq!(
            table::add_default_action(
                &mut runner.context(),
                value_ctx,
                value_arch,
                value_name,
                value_ctx
            )
            .unwrap(),
            value_arch_updated
        );
        assert!(runner.context().interp().calls.is_empty());
    }
}

#[test]
fn test_unqualified_update_fallback_and_missing_table() {
    let mut runner = scripted_runner();
    let value_arch = text(runner.arena_mut(), "arch");
    let value_name = text(runner.arena_mut(), "pipe.table");
    let value_pipe = text(runner.arena_mut(), "pipe");
    let value_short = text(runner.arena_mut(), "table");
    let value_id = list(runner.arena_mut(), "nameIR", vec![value_pipe, value_short]);
    let value_none = opt(runner.arena_mut(), None);
    call(&mut runner, "find_object_qualified_e", &[value_arch, value_id], value_none);
    call(
        &mut runner,
        "update_object_unqualified_e",
        &[value_arch, value_short, value_name],
        value_name,
    );
    assert_eq!(
        table::update_table(&mut runner.context(), value_arch, value_name, value_name).unwrap(),
        value_name
    );
    call(&mut runner, "find_object_unqualified_e", &[value_arch, value_short], value_none);
    assert!(table::find_table(&mut runner.context(), value_arch, value_short).is_err());
    assert!(runner.context().interp().calls.is_empty());
}

#[test]
fn test_key_retry_preserves_order_omits_selector_and_accepts_extra_tuple_fields() {
    let mut runner = scripted_runner();
    let value_ctx = text(runner.arena_mut(), "ctx");
    let value_arch = text(runner.arena_mut(), "arch");
    let value_name = text(runner.arena_mut(), "table");
    let value_some = opt(runner.arena_mut(), Some(value_name));
    let value_none = opt(runner.arena_mut(), None);
    let value_a = text(runner.arena_mut(), "z");
    let value_b = text(runner.arena_mut(), "a");
    let value_exact = text(runner.arena_mut(), "exact");
    let value_selector = text(runner.arena_mut(), "selector");
    let value_key_a = tuple(runner.arena_mut(), "key", vec![value_a, value_exact, value_ctx]);
    let value_key_selector =
        tuple(runner.arena_mut(), "key", vec![value_ctx, value_selector, value_ctx]);
    let value_key_b = tuple(runner.arena_mut(), "key", vec![value_b, value_exact, value_ctx]);
    let value_interface =
        list(runner.arena_mut(), "key", vec![value_key_a, value_key_selector, value_key_b]);
    let value_key_a =
        tuple(runner.arena_mut(), "tableKeyInterface", vec![value_ctx, value_a, value_b]);
    let value_key_b = tuple(runner.arena_mut(), "tableKeyInterface", vec![value_ctx, value_b]);
    let value_keys = list(runner.arena_mut(), "tableKeyInterface", vec![value_key_a, value_key_b]);
    let value_key_a = tuple(runner.arena_mut(), "tableKeyInterface", vec![value_a, value_a]);
    let value_key_b = tuple(runner.arena_mut(), "tableKeyInterface", vec![value_b, value_b]);
    let value_retry = list(runner.arena_mut(), "tableKeyInterface", vec![value_key_a, value_key_b]);
    assert!(matches!(runner.arena().typ(&value_retry).as_ref(), TypKind::Iter(_, _)));
    call(&mut runner, "find_object_unqualified_e", &[value_arch, value_name], value_some);
    call(
        &mut runner,
        "tableObject_add_entry",
        &[value_ctx, value_name, value_ctx, value_keys, value_ctx],
        value_none,
    );
    call(&mut runner, "key_interface_of_tableObject", &[value_name], value_interface);
    call(
        &mut runner,
        "tableObject_add_entry",
        &[value_ctx, value_name, value_ctx, value_retry, value_ctx],
        value_some,
    );
    call(
        &mut runner,
        "update_object_unqualified_e",
        &[value_arch, value_name, value_name],
        value_ctx,
    );
    assert_eq!(
        table::add_entry(
            &mut runner.context(),
            value_ctx,
            value_arch,
            value_name,
            value_ctx,
            value_keys,
            value_ctx
        )
        .unwrap(),
        value_ctx
    );
    assert!(runner.context().interp().calls.is_empty());
}

#[test]
fn test_retry_shape_count_and_second_failure_never_update_architecture() {
    for invalid in
        ["arity", "match kind", "key arity", "tuple projection", "key count", "second failure"]
    {
        let mut runner = scripted_runner();
        let value_arch = text(runner.arena_mut(), "arch");
        let value_name = text(runner.arena_mut(), "table");
        let value_exact = text(runner.arena_mut(), "exact");
        let value_bad = make::bool(runner.arena_mut(), false, Span::default()).unwrap();
        let value_some = opt(runner.arena_mut(), Some(value_name));
        let value_none = opt(runner.arena_mut(), None);
        let mut values_key = vec![
            value_name,
            if invalid == "match kind" { value_bad } else { value_exact },
            value_name,
        ];
        if invalid == "arity" {
            values_key.pop();
        }
        let value_interface_key = tuple(runner.arena_mut(), "key", values_key);
        let value_interface = list(runner.arena_mut(), "key", vec![value_interface_key]);
        let value_key = tuple(
            runner.arena_mut(),
            "tableKeyInterface",
            if matches!(invalid, "key arity" | "tuple projection") {
                vec![value_name]
            } else {
                vec![value_name, value_name]
            },
        );
        let value_keys = list(
            runner.arena_mut(),
            "tableKeyInterface",
            if invalid == "key count" {
                vec![]
            } else if invalid == "tuple projection" {
                vec![value_key, value_bad]
            } else {
                vec![value_key]
            },
        );
        call(&mut runner, "find_object_unqualified_e", &[value_arch, value_name], value_some);
        call(
            &mut runner,
            "tableObject_add_entry",
            &[value_arch, value_name, value_arch, value_keys, value_arch],
            value_none,
        );
        call(&mut runner, "key_interface_of_tableObject", &[value_name], value_interface);
        if invalid == "second failure" {
            call(
                &mut runner,
                "tableObject_add_entry",
                &[value_arch, value_name, value_arch, value_keys, value_arch],
                value_none,
            );
        }
        let error = table::add_entry(
            &mut runner.context(),
            value_arch,
            value_arch,
            value_name,
            value_arch,
            value_keys,
            value_arch,
        )
        .unwrap_err();
        if invalid == "tuple projection" {
            assert!(matches!(
                error,
                TestError::Extern(ExternError::Value(ValueError::KindMismatch {
                    expected: ValueTag::Tuple,
                    actual: ValueTag::Bool
                }))
            ));
        }
        assert!(runner.context().interp().calls.is_empty(), "{invalid}");
    }
}

fn field(arena: &ValueArena, value: Value, name: &str) -> Value {
    get::structure(arena, &value)
        .unwrap()
        .iter()
        .find(|(atom, _)| atom.node == Atom::Keyword(name.to_owned()))
        .unwrap()
        .1
}

#[test]
fn test_native_table_entries_append_priorities_and_default_changes_are_isolated() {
    let mut runner = super::runner(Dummy);
    let program = p4spec_rust::interface::p4::parse::parse_string(
        runner.arena_mut(),
        "tables.p4",
        r#"
        control C() {
            action a() { }
            action b() { }
            table t { key = {} actions = { a; b; } default_action = a(); }
            table other { key = {} actions = { a; b; } const default_action = a(); }
            apply { }
        }
        control Pipe();
        package Switch(Pipe c);
        Switch(C()) main;
    "#,
    )
    .unwrap();
    let values = runner
        .eval_program("Program_init", program)
        .unwrap_or_else(|error| panic!("{error}"));
    let [value_ctx, value_store] = values.as_slice() else { panic!("initialization outputs") };
    let value_ctx = *value_ctx;
    let value_store = *value_store;
    let (value_state, _) = runner
        .context()
        .call_extern_func("init_archState", &[], &[])
        .unwrap();
    let value_arch = make::structure(
        runner.arena_mut(),
        typ_named("arch").node.into(),
        vec![
            (
                p4spec_rust::phrase!(node: Atom::Keyword("STATE".to_owned()), span: Span::default()),
                value_state,
            ),
            (
                p4spec_rust::phrase!(node: Atom::Keyword("STORE".to_owned()), span: Span::default()),
                value_store,
            ),
        ],
        Span::default(),
    )
    .unwrap();
    let value_name = text(runner.arena_mut(), "main.c.t");
    let value_other = text(runner.arena_mut(), "main.c.other");
    let value_table_original =
        table::find_table(&mut runner.context(), value_arch, value_name).unwrap();
    let value_other_original =
        table::find_table(&mut runner.context(), value_arch, value_other).unwrap();
    let value_keys = list(runner.arena_mut(), "tableKeyInterface", vec![]);
    let value_args = list(runner.arena_mut(), "tableActionArgumentInterface", vec![]);
    let value_a = text(runner.arena_mut(), "a");
    let value_b = text(runner.arena_mut(), "b");
    let value_action_a =
        tuple(runner.arena_mut(), "tableActionInterface", vec![value_a, value_args]);
    let value_action_b =
        tuple(runner.arena_mut(), "tableActionInterface", vec![value_b, value_args]);
    let mut value_arch_updated = value_arch;
    for priority in [3, 17] {
        let value_priority =
            make::int(runner.arena_mut(), priority.into(), Span::default()).unwrap();
        let value_priority = make::opt(
            runner.arena_mut(),
            typ::make::opt(typ::make::int()).node.into(),
            Some(value_priority),
            Span::default(),
        )
        .unwrap();
        value_arch_updated = table::add_entry(
            &mut runner.context(),
            value_ctx,
            value_arch_updated,
            value_name,
            value_priority,
            value_keys,
            value_action_a,
        )
        .unwrap();
    }
    let value_table_updated =
        table::find_table(&mut runner.context(), value_arch_updated, value_name).unwrap();
    let value_props = *get::case(runner.arena(), &value_table_updated)
        .unwrap()
        .args()[2];
    let value_entries = field(runner.arena(), value_props, "ENTRIES");
    let value_entries = *get::case(runner.arena(), &value_entries).unwrap().args()[2];
    let values_entry = get::list(runner.arena(), &value_entries).unwrap().to_vec();
    assert_eq!(values_entry.len(), 2);
    for (value_entry, priority) in values_entry.into_iter().zip([3, 17]) {
        let value_priority = *get::case(runner.arena(), &value_entry).unwrap().args()[1];
        let value_priority = get::opt(runner.arena(), &value_priority).unwrap().unwrap();
        let value_priority = *get::case(runner.arena(), &value_priority).unwrap().args()[0];
        let value_priority = *get::case(runner.arena(), &value_priority).unwrap().args()[0];
        assert_eq!(
            p4spec_rust::lang::common::prim::num::to_int(
                get::num(runner.arena(), &value_priority).unwrap()
            ),
            &priority.into()
        );
    }
    value_arch_updated = table::add_default_action(
        &mut runner.context(),
        value_ctx,
        value_arch_updated,
        value_name,
        value_action_b,
    )
    .unwrap();
    let value_table_default =
        table::find_table(&mut runner.context(), value_arch_updated, value_name).unwrap();
    let value_props_default = *get::case(runner.arena(), &value_table_default)
        .unwrap()
        .args()[2];
    assert_ne!(
        field(runner.arena(), value_props, "DEFAULT_ACTION"),
        field(runner.arena(), value_props_default, "DEFAULT_ACTION")
    );
    assert_eq!(
        field(runner.arena(), value_props, "ENTRIES"),
        field(runner.arena(), value_props_default, "ENTRIES")
    );
    assert_eq!(
        table::find_table(&mut runner.context(), value_arch_updated, value_other).unwrap(),
        value_other_original
    );
    assert_eq!(
        table::find_table(&mut runner.context(), value_arch, value_name).unwrap(),
        value_table_original
    );
    let error = table::add_default_action(
        &mut runner.context(),
        value_ctx,
        value_arch_updated,
        value_other,
        value_action_b,
    )
    .unwrap_err();
    let error_expected = runner
        .context()
        .call_func(
            "tableObject_add_default_action",
            &[],
            &[value_ctx, value_other_original, value_action_b],
        )
        .unwrap_err();
    assert_eq!(error.into_report().render(), error_expected.into_report().render());
    assert_eq!(
        table::find_table(&mut runner.context(), value_arch_updated, value_other).unwrap(),
        value_other_original
    );
}

impl From<TestError> for ExternError {
    fn from(error: TestError) -> Self {
        match error {
            TestError::Extern(error) => error,
            TestError::Interface(error) => ExternError::Report(error.into_report()),
        }
    }
}
