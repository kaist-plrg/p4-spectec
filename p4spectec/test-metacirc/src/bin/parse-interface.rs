//! Parsing and encoding checks for the AL program interface
//!
//! The checks call the interface and Runner parsing entry points
//! without evaluating an AL, SL, or PL program
//! Compiler errors retain their complete diagnostic trees

use std::{error::Error, path::PathBuf};

use p4spectec::{
    diagnostic::Report,
    interface,
    lang::{
        al::ast,
        common::source::Span,
        data::{
            arena::Arena,
            typ::TypKind,
            value::flat::{Value, get, make},
        },
        traits::eq::SyntaxEq,
    },
    metacirc::al::decode,
    runner::{Config, Interface, NullExtern, Runner, Spec},
};

type Result<T> = std::result::Result<T, Box<dyn Error>>;

/// Checks ordered inputs, arena ownership, and diagnostic propagation.
fn main() -> Result<()> {
    let mut arena = Arena::new();
    let mut iface = interface::al();
    let value_existing = make::bool(&mut arena, true, Span::default())?;
    let paths = [fixture("valid/0-type.watsup"), fixture("valid/1-function.watsup")];

    // Compile ordered files into one meta script in the supplied arena
    let value_meta = iface.parse_program(&mut arena, &[fixture("unused-include")], &paths)?;
    check_script(&arena, value_meta)?;
    let value_dir = iface.parse_program(&mut arena, &[], &[fixture("valid")])?;
    assert!(value_meta.view(&arena).syntax_eq(&value_dir.view(&arena)));

    // Clearing interface state must not invalidate the caller's values
    iface.clear();
    assert!(get::bool(&arena, &value_existing)?);
    check_script(&arena, value_meta)?;
    let value_empty = iface.parse_program(&mut arena, &[], &[])?;
    assert!(decode::spec(&arena, value_empty)?.is_empty());

    // Exercise Runner delegation without calling an evaluation entry point
    let mut runner = Runner::new(
        &mut arena,
        Spec::Al(vec![]),
        Config::new(false, false, false),
        iface,
        NullExtern,
    )?;
    let value_runner = runner.parse_program(&mut arena, &[], &paths)?;
    assert!(
        value_meta
            .view(&arena)
            .syntax_eq(&value_runner.view(&arena))
    );

    // Preserve every cause and location from parsing and elaboration failures
    for name in ["invalid-syntax.watsup", "invalid-type.watsup", "missing.watsup"] {
        let paths = [fixture(name)];
        let report_expect = p4spectec::algo(&paths).expect_err("invalid compiler input");
        let report = runner
            .parse_program(&mut arena, &[], &paths)
            .expect_err("invalid interface input")
            .into_report();
        check_report(&report, &report_expect);
    }
    eprintln!("AL interface parsing: ordered files, directory, empty input, arena, diagnostics");
    Ok(())
}

fn fixture(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("fixtures/parse-interface")
        .join(name)
}

/// Checks the meta annotation and source definitions after decoding.
fn check_script(arena: &Arena, value_meta: Value) -> Result<()> {
    assert!(
        matches!(arena.value.typ(value_meta.note).as_ref(), TypKind::Var(id, targs) if id.node.as_ref() == "script" && targs.is_empty())
    );
    let spec_al = decode::spec(arena, value_meta)?;
    let [def_typ_al, def_func_al] = spec_al.as_slice() else {
        panic!("expected one type and one function; variable declarations must be omitted");
    };
    assert!(
        matches!(&def_typ_al.node, ast::DefKind::Typ(ast::TypDef::Defined(typdef_al)) if typdef_al.id.node.as_ref() == "counter")
    );
    assert!(
        matches!(&def_func_al.node, ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func_al)) if func_al.id.node.as_ref() == "identity")
    );
    Ok(())
}

/// Compares diagnostic causes, labels, frames, and child ordering.
fn check_report(report: &Report, report_expect: &Report) {
    assert_eq!(format!("{:?}", report.kind), format!("{:?}", report_expect.kind));
    assert_eq!(report.children.len(), report_expect.children.len());
    for (report_child, report_expect_child) in report.children.iter().zip(&report_expect.children) {
        check_report(report_child, report_expect_child);
    }
}
