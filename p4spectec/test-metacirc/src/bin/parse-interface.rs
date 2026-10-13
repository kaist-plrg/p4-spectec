//! Parsing and encoding checks for the AL program interface
//!
//! The checks call the interface and Runner parsing entry points
//! without evaluating an AL, SL, or PL program
//! Compiler errors retain their complete diagnostic trees

use p4spectec::{
    interface::al::{decode::Decode, encode::Encode},
    lang::al::ast as al,
};
use std::rc::Rc;

use std::{error::Error, path::PathBuf};

use p4spectec::{
    diagnostic::Report,
    interface,
    lang::{
        al::ast,
        common::{notation::atom::Atom, source::Span},
        data::{
            arena::Arena,
            notation::tree::{Mixfix, Mixop},
            typ::{TypKind, make as make_typ},
            value::flat::{Value, get, make},
        },
        traits::eq::SyntaxEq,
    },
    phrase,
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
    check_script(&mut arena, value_meta)?;
    let value_dir = iface.parse_program(&mut arena, &[], &[fixture("valid")])?;
    assert!(value_meta.view(&arena).syntax_eq(&value_dir.view(&arena)));

    // Clearing interface state must not invalidate the caller's values
    iface.clear();
    assert!(get::bool(&arena, &value_existing)?);
    check_script(&mut arena, value_meta)?;
    let value_empty = iface.parse_program(&mut arena, &[], &[])?;
    assert!(<al::Spec as Decode>::decode(&mut arena, value_empty)?.is_empty());

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
    check_codecs(&mut arena)?;
    eprintln!("AL interface parsing: ordered files, directory, empty input, arena, diagnostics");
    Ok(())
}

fn fixture(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("fixtures/parse-interface")
        .join(name)
}

/// Checks the meta annotation and source definitions after decoding.
fn check_script(arena: &mut Arena, value_meta: Value) -> Result<()> {
    assert!(
        matches!(arena.value.typ(value_meta.note).as_ref(), TypKind::Var(id, targs) if id.node.as_ref() == "script" && targs.is_empty())
    );
    let spec_al = <al::Spec as Decode>::decode(arena, value_meta)?;
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

/// Checks source and runtime round trips without interpreting a specification.
fn check_codecs(arena: &mut Arena) -> Result<()> {
    // Exercise the definition and expression dispatchers on compiler output
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    for path in [root.join("fixtures/target.watsup"), root.join("../../spec-meta")] {
        let spec_al = p4spectec::algo(&[path])?;
        let value_meta = spec_al.encode(arena)?;
        let spec_decode_al = al::Spec::decode(arena, value_meta)?;
        let value_again = spec_decode_al.encode(arena)?;
        assert!(value_meta.view(arena).syntax_eq(&value_again.view(arena)));
    }

    // Retain complete polymorphic signatures through the type trait
    let id = phrase!(node: "X".into(), span: Span::default());
    let typ_param_al = make_typ::var(id.clone(), vec![]);
    let typ_al = make_typ::func(vec![id], vec![typ_param_al], make_typ::nat());
    let value_meta = typ_al.encode(arena)?;
    let typ_decode_al = al::Typ::decode(arena, value_meta)?;
    assert!(typ_al.syntax_eq(&typ_decode_al));

    // Equal constructors with different atom spans still decode successfully
    let mut span = Span::default();
    span.left.line = 7;
    span.right.line = 7;
    span.right.column = 4;
    let value_bool = make::bool(arena, true, span)?;
    let typ_meta = make_typ::var(phrase!(node: "val".into(), span: span), vec![]);
    let mixop_meta = Mixop::Seq(vec![
        Mixop::Atom(phrase!(node: Atom::Keyword("BOOL".into()), span: span)),
        Mixop::Arg,
    ]);
    let value_meta = make::case(
        arena,
        typ_meta.node.clone().into(),
        Mixfix::new(Rc::new(mixop_meta), vec![value_bool])?,
        span,
    )?;
    let value_decode = Value::decode(arena, value_meta)?;
    assert!(get::bool(arena, &value_decode)?);

    // Silent tags and quoted operators must not alias concrete keywords
    for atom in [Atom::tag("BOOL")?, Atom::operator("BOOL")?] {
        let mixop_meta = Mixop::Seq(vec![Mixop::Atom(phrase!(node: atom, span: span)), Mixop::Arg]);
        let value_bad = make::case(
            arena,
            typ_meta.node.clone().into(),
            Mixfix::new(Rc::new(mixop_meta), vec![value_bool])?,
            span,
        )?;
        assert!(Value::decode(arena, value_bad).is_err());
    }
    eprintln!("AL codecs: scripts, signatures, constructor spans and atom kinds");
    Ok(())
}
