use std::{cell::Cell, sync::Mutex};

use p4spec_rust::{
    diagnostic::ReportKind,
    interface::builtin::{call::Builtins, extract},
    lang::common::source::Span,
    lang::data::{
        typ,
        value::{self, Value, ValueArena, get},
    },
    lang::il::ast::Typ,
    phrase,
    runner::{
        BuiltinInterface, Extern, ExternError, Interface, InterfaceError, Interpreter, NullExtern,
        NullInterface, Runner, RunnerContext,
    },
};
use thiserror::Error;

static FRESH_BUILTIN: Mutex<()> = Mutex::new(());

#[derive(Debug, Error)]
enum FixtureError {
    #[error(transparent)]
    Interface(#[from] InterfaceError),
    #[error(transparent)]
    Extern(#[from] ExternError),
    #[error("unknown fixture call: {0}")]
    Unknown(String),
}

#[derive(Default)]
struct FixtureConfig {
    label: String,
}

struct FixtureInterpreter {
    config: FixtureConfig,
}

impl<Iface, Ext> Interpreter<Iface, Ext> for FixtureInterpreter
where
    Iface: Interface,
    Ext: Extern,
{
    type Spec = ();
    type Error = FixtureError;

    fn clear(&mut self) {}

    fn reset(&mut self) {}

    fn eval_program(
        _ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        program: Value,
    ) -> Result<Vec<Value>, Self::Error> {
        match name {
            "identity" => Ok(vec![program]),
            _ => Err(FixtureError::Unknown(name.to_owned())),
        }
    }

    fn eval_func(
        ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<Value, Self::Error> {
        match name {
            "done" => {
                Ok(value::make::text(ctx.arena_mut(), "done".to_owned(), Span::default()).unwrap())
            }
            "outer" => {
                let (value, _) = ctx.call_extern_func("first", targs, values)?;
                Ok(value)
            }
            "inner" => {
                let (value, _) = ctx.call_extern_func("second", targs, values)?;
                Ok(value)
            }
            "pure_effect" => {
                let (_, side_effected) = ctx.call_extern_func("pure", targs, values)?;
                Ok(value::make::bool(ctx.arena_mut(), side_effected, Span::default()).unwrap())
            }
            "impure_effect" => {
                let (_, side_effected) = ctx.call_extern_func("impure", targs, values)?;
                Ok(value::make::bool(ctx.arena_mut(), side_effected, Span::default()).unwrap())
            }
            "extern" => {
                let (value, _) = ctx.call_extern_func("missing", targs, values)?;
                Ok(value)
            }
            "config" => {
                let label = ctx.interp().config.label.clone();
                Ok(value::make::text(ctx.arena_mut(), label, Span::default()).unwrap())
            }
            "next_extern" => {
                let (value, _) = ctx.call_extern_func("next", targs, values)?;
                Ok(value)
            }
            "next_builtin" => {
                let id = id("fresh_typeId");
                let (value, _) = ctx.call_builtin(&id, targs, values)?;
                Ok(value)
            }
            _ => Err(FixtureError::Unknown(name.to_owned())),
        }
    }

    fn eval_rel(
        _ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        _values: &[Value],
    ) -> Result<Vec<Value>, Self::Error> {
        Err(FixtureError::Unknown(name.to_owned()))
    }
}

#[derive(Default)]
struct FixtureExtern {
    next: Cell<u64>,
}

impl Extern for FixtureExtern {
    fn eval_func<Interp, Iface>(
        &self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<(Value, bool), p4spec_rust::runner::ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        match name {
            "first" => {
                let value = ctx.call_func("inner", targs, values).map_err(Into::into)?;
                Ok((value, false))
            }
            "second" => {
                let value = ctx.call_func("done", targs, values).map_err(Into::into)?;
                Ok((value, false))
            }
            "pure" => {
                let value = value::make::bool(ctx.arena_mut(), false, Span::default()).unwrap();
                Ok((value, false))
            }
            "impure" => {
                let value = value::make::bool(ctx.arena_mut(), true, Span::default()).unwrap();
                Ok((value, true))
            }
            "next" => {
                let next = self.next.get();
                self.next.set(next + 1);
                let value =
                    value::make::text(ctx.arena_mut(), next.to_string(), Span::default()).unwrap();
                Ok((value, true))
            }
            _ => {
                let error = ExternError::Message(name.to_owned());
                Err(error)
            }
        }
    }

    fn eval_rel<Interp, Iface>(
        &self,
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        _values: &[Value],
    ) -> Result<(Vec<Value>, bool), p4spec_rust::runner::ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        let error = ExternError::Message(name.to_owned());
        Err(error)
    }

    fn clear(&mut self) {
        self.next.set(0);
    }
}

fn id(name: &str) -> p4spec_rust::lang::il::ast::Id {
    phrase!(node: name.to_owned(), span: Span::default())
}

#[test]
fn test_null_interface_reports_configuration_failure() {
    let mut arena = ValueArena::new();
    let error = NullInterface
        .call_builtin(&mut arena, &id("sum_int"), &[], &[])
        .unwrap_err();

    assert!(matches!(error, InterfaceError::InterfaceUnconfigured));
}

#[test]
fn test_builtin_interface_registers_captured_extensions_and_overrides() {
    let prefix = String::from("extra: ");
    let suffix = String::from(" :override");
    let builtins = Builtins::with_extensions([
        (
            "extra",
            Box::new(move |arena, _targs, values| {
                let value = extract::one(values)?;
                let text = format!("{prefix}{}", get::text(arena, value)?);
                Ok(value::make::text(arena, text, Span::default())?)
            }),
        ),
        (
            "strip_all_whitespace",
            Box::new(move |arena, _targs, values| {
                let value = extract::one(values)?;
                let text = format!("{}{suffix}", get::text(arena, value)?);
                Ok(value::make::text(arena, text, Span::default())?)
            }),
        ),
    ]);
    let mut interface = BuiltinInterface::new(builtins);
    let mut arena = ValueArena::new();
    let value = value::make::text(&mut arena, "a b".to_owned(), Span::default()).unwrap();
    for (name, expected) in [("extra", "extra: a b"), ("strip_all_whitespace", "a b :override")] {
        let (value, side_effected) = interface
            .call_builtin(&mut arena, &id(name), &[], &[value])
            .unwrap();
        assert_eq!(get::text(&arena, &value), Ok(expected));
        assert!(!side_effected);
    }
}

#[test]
fn test_builtin_interface_reports_side_effects_and_clears() {
    let mut arena = ValueArena::new();
    let _guard = FRESH_BUILTIN.lock().unwrap();
    let mut interface = p4spec_rust::interface::p4(&p4spec_rust::runner::Spec::Al(Vec::new()));
    interface.clear();
    let (value, side_effected) = interface
        .call_builtin(&mut arena, &id("fresh_typeId"), &[], &[])
        .unwrap();

    assert_eq!(get::text(&arena, &value), Ok("FRESH__0"));
    assert!(side_effected);

    interface.clear();
    let (value, side_effected) = interface
        .call_builtin(&mut arena, &id("fresh_typeId"), &[], &[])
        .unwrap();
    assert_eq!(get::text(&arena, &value), Ok("FRESH__0"));
    assert!(side_effected);
}

#[test]
fn test_builtin_interface_preserves_builtin_failures() {
    let mut arena = ValueArena::new();
    let error = p4spec_rust::interface::p4(&p4spec_rust::runner::Spec::Al(Vec::new()))
        .call_builtin(&mut arena, &id("sum_int"), &[], &[])
        .unwrap_err();

    let report = error.into_report();
    let ReportKind::Cause(diagnostic) = &report.kind else {
        panic!("expected a builtin diagnostic")
    };
    assert_eq!(diagnostic.code.as_deref(), Some("runtime/builtin-argument-arity-mismatch"));
    assert_eq!(diagnostic.message, "arity mismatch: expected 1, got 0");
}

#[test]
fn test_builtin_interface_prints_p4_values_without_side_effects() {
    let mut arena = ValueArena::new();
    let value = value::make::text(&mut arena, "a\n\"b".to_owned(), Span::default()).unwrap();
    let (printed, side_effected) =
        p4spec_rust::interface::p4(&p4spec_rust::runner::Spec::Al(Vec::new()))
            .call_builtin(&mut arena, &id("print_"), &[typ::make::text()], &[value])
            .unwrap();

    assert_eq!(get::text(&arena, &printed), Ok("a\\n\\\"b"));
    assert!(!side_effected);
}

#[test]
fn test_builtin_interface_print_validates_both_arities() {
    let mut arena = ValueArena::new();
    let mut interface = p4spec_rust::interface::p4(&p4spec_rust::runner::Spec::Al(Vec::new()));
    let typ = typ::make::text();
    let value = value::make::text(&mut arena, "value".to_owned(), Span::default()).unwrap();
    for (targs, values, actual) in [
        (vec![], vec![value], 0),
        (vec![typ.clone(), typ.clone()], vec![value], 2),
        (vec![typ.clone()], vec![], 0),
        (vec![typ], vec![value, value], 2),
    ] {
        let error = interface
            .call_builtin(&mut arena, &id("print_"), &targs, &values)
            .unwrap_err();
        let report = error.into_report();
        let ReportKind::Cause(diagnostic) = &report.kind else {
            panic!("expected a builtin diagnostic")
        };
        assert_eq!(diagnostic.code.as_deref(), Some("runtime/builtin-argument-arity-mismatch"));
        assert_eq!(diagnostic.message, format!("arity mismatch: expected 1, got {actual}"));
    }
}

#[test]
fn test_builtin_interface_print_preserves_unparse_failures() {
    let mut arena = ValueArena::new();
    let typ = typ::make::bool();
    let value =
        value::make::structure(&mut arena, typ.node.clone().into(), Vec::new(), Span::default())
            .unwrap();
    let error = p4spec_rust::interface::p4(&p4spec_rust::runner::Spec::Al(Vec::new()))
        .call_builtin(&mut arena, &id("print_"), &[typ], &[value])
        .unwrap_err();
    let report = error.into_report();
    let ReportKind::Cause(diagnostic) = &report.kind else {
        panic!("expected a builtin diagnostic")
    };
    assert_eq!(diagnostic.code.as_deref(), Some("runtime/builtin-print-unsupported"));
    assert_eq!(diagnostic.message, "cannot unparse runtime value kind Struct");
}

#[test]
fn test_builtin_interface_print_preserves_spec_hints_after_clear() {
    let mut arena = ValueArena::new();
    use p4spec_rust::lang::{
        al::ast,
        common::notation::{atom::Atom, mixfix::Mixfix},
        el, il,
    };

    let span = Span::default();
    let atom = phrase!(node: Atom::Keyword("WRAP".to_owned()), span: span.clone());
    let notation = Mixfix::Seq(vec![Mixfix::Atom(atom.clone()), Mixfix::Arg(typ::make::text())]);
    let hint = phrase!(node: el::ast::ExpKind::Seq(vec![
        phrase!(node: el::ast::ExpKind::Text("shown".to_owned()), span: span.clone()),
        phrase!(node: el::ast::ExpKind::Hole(el::ast::Hole::Next), span: span.clone()),
    ]), span: span.clone());
    let def_typ = phrase!(node: il::ast::DefTypKind::Variant(vec![il::ast::TypCase { not_typ: phrase!(node: notation, span: span.clone()), typ_origin: phrase!(node: il::ast::TypOriginKind { id: id("Origin"), targs: Vec::new() }, span: span.clone()), hints: vec![el::ast::Hint { id: id("print"), exp: hint }] }]), span: span.clone());
    let defined_typ =
        ast::DefinedTyp { id: id("Wrapper"), tparams: Vec::new(), def_typ, hints: Vec::new() };
    let def = ast::DefKind::Typ(ast::TypDef::Defined(Box::new(defined_typ)));
    let spec = vec![phrase!(node: def, span: span.clone())];
    let typ = typ::make::var(id("Wrapper"), Vec::new());
    let value = {
        let values = Mixfix::Seq(vec![
            Mixfix::Atom(atom),
            Mixfix::Arg(value::make::text(&mut arena, "payload".to_owned(), span.clone()).unwrap()),
        ]);
        value::make::case(&mut arena, typ.node.clone().into(), values, span).unwrap()
    };
    let _guard = FRESH_BUILTIN.lock().unwrap();
    let mut interface = p4spec_rust::interface::p4(&p4spec_rust::runner::Spec::Al(spec));
    for _ in 0..2 {
        let (printed, side_effected) = interface
            .call_builtin(
                &mut arena,
                &id("print_"),
                std::slice::from_ref(&typ),
                std::slice::from_ref(&value),
            )
            .unwrap();
        assert_eq!(get::text(&arena, &printed), Ok("shown payload"));
        assert!(!side_effected);
        interface.clear();
    }
}

#[test]
fn test_runner_statically_composes_its_components() {
    let mut runner = Runner::<FixtureInterpreter, NullInterface, NullExtern>::new(
        (),
        FixtureInterpreter { config: FixtureConfig::default() },
        NullInterface,
        NullExtern,
    );

    let value = runner.context().call_func("done", &[], &[]).unwrap();

    assert_eq!(get::text(runner.arena(), &value), Ok("done"));
}

#[test]
fn test_extern_can_reenter_the_interpreter() {
    let mut runner = Runner::<FixtureInterpreter, NullInterface, FixtureExtern>::new(
        (),
        FixtureInterpreter { config: FixtureConfig::default() },
        NullInterface,
        FixtureExtern::default(),
    );

    let value = runner.context().call_func("outer", &[], &[]).unwrap();

    assert_eq!(get::text(runner.arena(), &value), Ok("done"));
}

#[test]
fn test_extern_reports_side_effects_with_each_result() {
    let mut runner = Runner::<FixtureInterpreter, NullInterface, FixtureExtern>::new(
        (),
        FixtureInterpreter { config: FixtureConfig::default() },
        NullInterface,
        FixtureExtern::default(),
    );

    let value_pure = runner.context().call_func("pure_effect", &[], &[]).unwrap();
    let value_impure = runner
        .context()
        .call_func("impure_effect", &[], &[])
        .unwrap();

    assert_eq!(get::bool(runner.arena(), &value_pure), Ok(false));
    assert_eq!(get::bool(runner.arena(), &value_impure), Ok(true));
}

#[test]
fn test_null_extern_reports_configuration_failure() {
    let mut runner = Runner::<FixtureInterpreter, NullInterface, NullExtern>::new(
        (),
        FixtureInterpreter { config: FixtureConfig::default() },
        NullInterface,
        NullExtern,
    );

    let error = runner.context().call_func("extern", &[], &[]).unwrap_err();

    assert!(matches!(error, FixtureError::Extern(ExternError::ExternUnconfigured)));
}

fn eval_text(
    runner: &mut Runner<FixtureInterpreter, BuiltinInterface, FixtureExtern>,
    name: &str,
) -> String {
    let value = runner.context().call_func(name, &[], &[]).unwrap();
    get::text(runner.arena(), &value).unwrap().to_owned()
}

#[test]
fn test_runner_reset_releases_program_arena_and_resets_hosts() {
    let _guard = FRESH_BUILTIN.lock().unwrap();
    let mut runner = Runner::<FixtureInterpreter, BuiltinInterface, FixtureExtern>::new(
        (),
        FixtureInterpreter { config: FixtureConfig { label: "configured".to_owned() } },
        p4spec_rust::interface::p4(&p4spec_rust::runner::Spec::Al(Vec::new())),
        FixtureExtern::default(),
    );
    let typ = std::rc::Rc::new(p4spec_rust::lang::data::typ::TypKind::Text);
    let typ_weak = std::rc::Rc::downgrade(&typ);
    let value = value::make::new(
        runner.arena_mut(),
        value::ValueKind::Text("previous program".to_owned()),
        typ,
        Span::default(),
    )
    .unwrap();
    assert!(runner.eval_program("missing", value).is_err());
    assert_eq!(eval_text(&mut runner, "next_extern"), "0");
    assert_ne!(eval_text(&mut runner, "next_builtin"), eval_text(&mut runner, "next_builtin"));
    runner.reset();
    assert!(typ_weak.upgrade().is_none(), "previous program types must be released");
    assert_eq!(eval_text(&mut runner, "config"), "configured");
    assert_eq!(eval_text(&mut runner, "next_extern"), "0");
    let text = eval_text(&mut runner, "next_builtin");
    runner.reset();
    assert_eq!(eval_text(&mut runner, "next_builtin"), text);
}

#[test]
fn test_runner_dispatches_program_entry_and_errors() {
    let mut runner = Runner::<FixtureInterpreter, NullInterface, NullExtern>::new(
        (),
        FixtureInterpreter { config: FixtureConfig::default() },
        NullInterface,
        NullExtern,
    );
    let program =
        value::make::text(runner.arena_mut(), "program".to_owned(), Span::default()).unwrap();
    let values = runner.eval_program("identity", program).unwrap();
    assert_eq!(values.len(), 1);
    assert!((values[0] == program));
    let error = runner.eval_program("missing", program).unwrap_err();
    assert!(matches!(error, FixtureError::Unknown(name) if name == "missing"));
}

#[test]
fn test_registered_builtin_report_keeps_payload_and_is_fatal() {
    use p4spec_rust::{
        diagnostic::{Diagnostic, Label, Report, ReportKind, Severity},
        interp::shared::backtrack::Failure,
        lang::common::source::Position,
    };
    let span = Span { left: Position::new("host.p4", 2, 3), right: Position::new("host.p4", 2, 6) };
    let labels = vec![
        Label::primary(&span, "host location"),
        Label::secondary(&Span::default(), "host related"),
    ];
    let notes = vec!["host detail".to_owned(), "second detail".to_owned()];
    let mut report = Some(Box::new(
        Report::from(Diagnostic::new(
            "custom",
            Severity::Warning,
            Some("custom/check".to_owned()),
            "host message",
            labels.clone(),
            notes.clone(),
        ))
        .with_children(vec![Report::frame(
            span.clone(),
            "outer",
            vec![
                Report::frame(Span::default(), "first", vec![]),
                Report::frame(Span::default(), "second", vec![]),
            ],
        )]),
    ));
    let builtins = Builtins::with_extensions([(
        "custom",
        Box::new(move |_arena: &mut ValueArena, _targs: &[Typ], _values: &[Value]| {
            Err(report.take().expect("one host call").into())
        }) as p4spec_rust::interface::builtin::call::BuiltinImpl,
    )]);
    let mut interface = BuiltinInterface::new(builtins);
    let error = interface
        .call_builtin(&mut ValueArena::new(), &id("custom"), &[], &[])
        .unwrap_err();
    let failure = Failure::from(error).with_span(&Span::default());
    let Failure::Fatal(report) = failure else { panic!("builtin must be fatal") };
    let ReportKind::Cause(diagnostic) = &report.kind else { panic!("host cause missing") };
    assert_eq!(diagnostic.source, "custom");
    assert_eq!(diagnostic.severity, Severity::Warning);
    assert_eq!(diagnostic.code.as_deref(), Some("custom/check"));
    assert_eq!(diagnostic.message, "host message");
    assert_eq!(diagnostic.labels, labels);
    assert_eq!(diagnostic.notes, notes);
    assert_eq!(report.children.len(), 1);
    let report_outer = &report.children[0];
    assert!(
        matches!(&report_outer.kind, ReportKind::Frame { span: span_actual, message } if *span_actual == span && message == "outer")
    );
    assert_eq!(report_outer.children.len(), 2);
    for (report, message_expect) in report_outer.children.iter().zip(["first", "second"]) {
        assert!(
            matches!(&report.kind, ReportKind::Frame { span, message } if *span == Span::default() && message == message_expect)
        );
        assert!(report.children.is_empty());
    }
}

impl From<FixtureError> for ExternError {
    fn from(error: FixtureError) -> Self {
        match error {
            FixtureError::Interface(error) => ExternError::Report(error.into_report()),
            FixtureError::Extern(error) => error,
            FixtureError::Unknown(error) => ExternError::Message(error.to_string()),
        }
    }
}
