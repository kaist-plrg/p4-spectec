//! Program parsing through language-specific runner interfaces

use std::path::PathBuf;

use p4spectec::diagnostic::ReportKind;
use p4spectec::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        value::flat::{Value, get, make},
    },
    il::ast::{Id, Typ},
};
use p4spectec::runner::{
    self, Config, Eval, Extern, Interface, InterfaceError, Interpreter, NullExtern, NullInterface,
    Runner, RunnerContext, Spec,
};

/// Reads plain text files relative to one include directory.
struct TextInterface;

impl Interface for TextInterface {
    fn parse_program(
        &mut self,
        arena: &mut Arena,
        includes: &[PathBuf],
        paths: &[PathBuf],
    ) -> Result<Value, InterfaceError> {
        let text = paths
            .iter()
            .map(|path| std::fs::read_to_string(includes[0].join(path)).unwrap())
            .collect::<Vec<_>>()
            .join("");
        Ok(make::text(arena, text, Span::default()).unwrap())
    }

    fn call_builtin<Interp, Ext>(
        _ctx: &mut RunnerContext<'_, Interp, Self, Ext>,
        _id: &Id,
        _targs: &[Typ],
        _values: &[Value],
    ) -> Eval<Value, InterfaceError>
    where
        Ext: Extern,
        Interp: Interpreter<Self, Ext>,
    {
        panic!("text parsing does not call builtins")
    }

    fn clear(&mut self) {}
}

#[test]
fn parses_non_p4_inputs_in_the_supplied_arena() {
    for spec in [Spec::Al(vec![]), Spec::Sl(vec![]), Spec::Pl(vec![])] {
        let mut arena = Arena::new();
        let value_existing = make::text(&mut arena, "existing".into(), Span::default()).unwrap();
        let mut runner = Runner::new(
            &mut arena,
            spec,
            Config::new(false, false, false),
            TextInterface,
            NullExtern,
        )
        .unwrap();
        let includes = [PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures")];
        let paths = [PathBuf::from("first.txt"), PathBuf::from("second.txt")];

        let value = runner.parse_program(&mut arena, &includes, &paths).unwrap();

        assert_eq!(get::text(&arena, &value).unwrap(), "hello\nrunner\n");
        assert_eq!(get::text(&arena, &value_existing).unwrap(), "existing");
    }
}

#[test]
fn p4_rejects_zero_or_multiple_input_files() {
    let mut arena = Arena::new();
    let mut runner =
        runner::build_al(&mut arena, vec![], Config::new(false, false, false), NullExtern).unwrap();
    for paths in [vec![], vec![PathBuf::from("one.p4"), PathBuf::from("two.p4")]] {
        let report = runner
            .parse_program(&mut arena, &[], &paths)
            .unwrap_err()
            .into_report();
        let ReportKind::Cause(diagnostic) = &report.kind else {
            panic!("expected a parser diagnostic")
        };
        assert_eq!(diagnostic.code.as_deref(), Some("p4/input-count-invalid"));
    }
}

#[test]
fn p4_parse_errors_retain_their_code_and_location() {
    let mut arena = Arena::new();
    let mut runner =
        runner::build_al(&mut arena, vec![], Config::new(false, false, false), NullExtern).unwrap();
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("test-driver/expected/diagnostic/interp/p4-syntax.p4");

    let report = runner
        .parse_program(&mut arena, &[], &[path])
        .unwrap_err()
        .into_report();

    let ReportKind::Cause(diagnostic) = &report.kind else {
        panic!("expected a parser diagnostic")
    };
    assert_eq!(diagnostic.code.as_deref(), Some("p4/syntax-invalid"));
    assert_eq!(diagnostic.source, "p4");
    assert!(
        diagnostic
            .notes
            .iter()
            .any(|note| note.ends_with("p4-syntax.p4:1"))
    );
}

#[test]
fn unconfigured_interface_rejects_parsing() {
    let mut arena = Arena::new();
    let mut runner = Runner::new(
        &mut arena,
        Spec::Al(vec![]),
        Config::new(false, false, false),
        NullInterface,
        NullExtern,
    )
    .unwrap();

    let report = runner
        .parse_program(&mut arena, &[], &[])
        .unwrap_err()
        .into_report();

    let ReportKind::Cause(diagnostic) = &report.kind else {
        panic!("expected an interface diagnostic")
    };
    assert_eq!(diagnostic.code.as_deref(), Some("runtime/interface-unconfigured"));
}
