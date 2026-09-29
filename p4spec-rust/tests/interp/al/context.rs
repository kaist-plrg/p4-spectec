use crate::interp::report::{IntoReport, ReportExt};
use p4spec_rust::interp::shared::context::{IterContext, ReadContext, WriteContext};
use p4spec_rust::interp::shared::error::EntityKind;
use p4spec_rust::lang::data::value::ValueArena;
use p4spec_rust::runtime::envs::interp::shared::{callable::Callable, frame::FrameLayout};
use std::rc::Rc;

use p4spec_rust::{
    interp::al::context::{Context, Global, Scope},
    lang::{
        al::ast,
        common::source::{Position, Span},
        data::{
            typ,
            value::{get, make},
        },
    },
    phrase,
    runtime::typdef::TypeDef,
};

fn id(name: &str, line: usize) -> ast::Id {
    phrase!(node: name.to_owned(), span: Span::new(
        Position::new("context.watsup", line, 0),
        Position::new("context.watsup", line, 1),
    ))
}

fn func(name: &str, line: usize) -> ast::MetaFuncDef {
    ast::MetaFuncDef::Extern(ast::ExternFunc {
        id: id(name, line),
        tparams: vec![],
        params: vec![],
        typ: typ::make::bool(),
        hints: vec![],
    })
}

fn def(node: ast::DefKind) -> ast::Def {
    phrase!(node: node, span: Span::default())
}

fn var(name: &str, iters: Vec<ast::Iter>) -> ast::Var {
    ast::Var { id: id(name, 1), typ: typ::make::bool(), iters }
}

#[test]
fn test_duplicate_global_definition_uses_second_identifier_span() {
    for (kind, def_a, def_b) in [
        (
            EntityKind::Type,
            ast::DefKind::Typ(ast::TypDef::Extern(ast::ExternTyp {
                id: id("x", 1),
                hints: vec![],
            })),
            ast::DefKind::Typ(ast::TypDef::Extern(ast::ExternTyp {
                id: id("x", 9),
                hints: vec![],
            })),
        ),
        (
            EntityKind::Function,
            ast::DefKind::MetaFunc(func("x", 1)),
            ast::DefKind::MetaFunc(func("x", 9)),
        ),
    ] {
        let error = Global::load(vec![def(def_a), def(def_b)])
            .unwrap_err()
            .into_report();
        assert_eq!(error.span(), id("x", 9).span);
        assert_eq!(
            error.diagnostic().message,
            p4spec_rust::interp::shared::error::context::binding_repeated(kind, "x".into()).message
        );
    }
}

#[test]
fn test_localize_with_layout_discards_locals_and_retains_global_lookup() {
    let mut arena = ValueArena::new();
    let func_global = func("global", 1);
    let global = Global::load(vec![def(ast::DefKind::MetaFunc(func_global.clone()))]).unwrap();
    let mut layout = FrameLayout::default();
    let slot = layout.resolve_var(var("x", vec![]));
    let layout = Rc::new(layout);
    let mut ctx = Context::new(&global).localize_with_layout(&layout);
    let func_local = Callable::prepare(func("local", 2));
    ctx.add_func(id("local", 2), func_local.clone().into())
        .unwrap();
    ctx.add_typdef(id("T", 3), TypeDef::Extern).unwrap();
    ctx.add_value_at_slot(slot.slot, make::bool(&mut arena, true, Span::default()).unwrap());
    assert_eq!(
        ctx.find_func_with_scope(&id("local", 8))
            .map(|(scope, func)| (scope, func.as_ref()))
            .unwrap(),
        (Scope::Local, &func_local)
    );
    let ctx_local = ctx.localize_with_layout(&layout);
    assert!(ctx_local.find_func_opt(&id("local", 8)).is_none());
    assert!(ctx_local.find_typdef_opt(&id("T", 8)).is_none());
    assert!(ctx_local.find_value_at_slot(slot.slot).is_none());
    assert_eq!(
        ctx_local
            .find_func_with_scope(&id("global", 8))
            .map(|(scope, func)| (scope, func.as_ref()))
            .unwrap(),
        (Scope::Global, &Callable::prepare(func_global))
    );
    assert!(ctx.find_value_at_slot(slot.slot).is_some());
}

#[test]
fn test_local_definition_duplicates_do_not_replace_bindings() {
    let global = Global::load(vec![
        def(ast::DefKind::MetaFunc(func("f", 1))),
        def(ast::DefKind::Typ(ast::TypDef::Extern(ast::ExternTyp {
            id: id("T", 1),
            hints: vec![],
        }))),
    ])
    .unwrap();
    let mut ctx = Context::new(&global);
    assert!(
        ctx.add_func(id("f", 7), Callable::prepare(func("f", 7)).into())
            .unwrap_err()
            .into_report()
            .code()
            == Some("runtime/binding-repeated")
    );
    assert_eq!(
        ctx.add_typdef(id("T", 7), TypeDef::Parameter)
            .unwrap_err()
            .into_report()
            .span(),
        id("T", 7).span
    );
    ctx.add_typdef(id("U", 2), TypeDef::Extern).unwrap();
    ctx.add_func(id("g", 2), Callable::prepare(func("g", 2)).into())
        .unwrap();
    assert!(ctx.add_typdef(id("U", 7), TypeDef::Parameter).is_err());
    assert!(
        ctx.add_func(id("g", 7), Callable::prepare(func("g", 7)).into())
            .is_err()
    );
    assert_eq!(ctx.find_typdef(&id("U", 8)).unwrap(), &TypeDef::Extern);
    assert_eq!(
        ctx.find_func_with_scope(&id("g", 8)).unwrap().1.as_ref(),
        &Callable::prepare(func("g", 2))
    );
}

#[test]
fn test_sibling_contexts_isolate_rebinding_and_iterator_paths() {
    let mut arena = ValueArena::new();
    let global = Global::load(vec![]).unwrap();
    let mut layout = FrameLayout::default();
    let slot = layout.resolve_var(var("x", vec![]));
    let slot_list = layout.resolve_var(p4spec_rust::lang::il::ast::Var {
        id: id("x", 1),
        typ: p4spec_rust::lang::data::typ::make::bool(),
        iters: vec![ast::Iter::List],
    });
    let mut ctx = Context::new(&global).localize_with_layout(&layout.into());
    ctx.add_value_at_slot(slot.slot, make::bool(&mut arena, false, Span::default()).unwrap());
    let mut ctx_a = ctx.clone();
    let ctx_b = ctx.clone();
    ctx_a.add_value_at_slot(slot.slot, make::bool(&mut arena, true, Span::default()).unwrap());
    ctx_a.add_value_at_slot(slot_list.slot, make::bool(&mut arena, true, Span::default()).unwrap());
    assert!(get::bool(&arena, ctx_a.find_value_at_slot(slot.slot).unwrap()).unwrap());
    assert!(!get::bool(&arena, ctx_b.find_value_at_slot(slot.slot).unwrap()).unwrap());
    assert!(ctx.find_value_at_slot(slot_list.slot).is_none());
}

#[test]
fn test_missing_value_reports_iterator_path_and_lookup_span() {
    let global = Global::load(vec![]).unwrap();
    let mut layout = FrameLayout::default();
    let slot = layout.resolve_var(p4spec_rust::lang::il::ast::Var {
        id: id("x", 9),
        typ: p4spec_rust::lang::data::typ::make::bool(),
        iters: vec![ast::Iter::List, ast::Iter::Opt],
    });
    let error = Context::new(&global)
        .localize_with_layout(&layout.into())
        .find_opt_values_by_var(&ValueArena::new(), &[slot])
        .unwrap_err()
        .into_report();
    assert_eq!(error.span(), id("x", 9).span);
    assert_eq!(
        error.diagnostic().message,
        p4spec_rust::interp::shared::error::context::binding_undefined(
            EntityKind::Value,
            "x*?".into()
        )
        .message
    );
}

#[test]
fn test_loaded_native_spec_preserves_definition_bodies_and_locations() {
    use p4spec_rust::{
        frontend::parse::parse_files,
        pass::{algo, elaborate},
    };
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../spec");
    let spec_el = parse_files([path]).unwrap();
    let spec_il = elaborate::convert(spec_el).unwrap();
    let spec_al = algo::convert(spec_il).unwrap();
    let global = Global::load(spec_al.clone()).unwrap();
    let mut layout = FrameLayout::default();
    for def in &spec_al {
        if let ast::DefKind::Var(var) = &def.node {
            layout.resolve_var(p4spec_rust::lang::il::ast::Var {
                id: var.id.clone(),
                typ: p4spec_rust::lang::data::typ::make::bool(),
                iters: vec![],
            });
        }
    }
    let ctx = Context::new(&global).localize_with_layout(&Rc::new(layout.clone()));
    let ctx_clone = ctx.clone();
    let ctx_local = ctx.localize();
    for def in &spec_al {
        match &def.node {
            ast::DefKind::Typ(ast::TypDef::Defined(typdef)) => {
                let (tparams, def_typ) = ctx.find_defined_typdef(&typdef.id).unwrap();
                assert_eq!(tparams, typdef.tparams);
                assert_eq!(def_typ, &typdef.def_typ);
            }
            ast::DefKind::Typ(ast::TypDef::Extern(typdef)) => {
                assert_eq!(ctx.find_typdef(&typdef.id).unwrap(), &TypeDef::Extern);
            }
            ast::DefKind::Rel(rel) => {
                let id = match rel {
                    ast::RelDef::Extern(rel) => &rel.id,
                    ast::RelDef::Defined(rel) => &rel.id,
                };
                let rel_global = ctx.find_rel(id).unwrap();
                assert!(std::ptr::eq(rel_global, ctx_clone.find_rel(id).unwrap()));
                assert!(std::ptr::eq(rel_global, ctx_local.find_rel(id).unwrap()));
            }
            ast::DefKind::MetaFunc(func) => {
                let id = match func {
                    ast::MetaFuncDef::Extern(func) => &func.id,
                    ast::MetaFuncDef::Builtin(func) => &func.id,
                    ast::MetaFuncDef::Table(func) => &func.id,
                    ast::MetaFuncDef::Defined(func) => &func.id,
                };
                let (scope, func_global) = ctx.find_func_with_scope(id).unwrap();
                assert_eq!(scope, Scope::Global);
                assert!(std::rc::Rc::ptr_eq(
                    func_global,
                    ctx_clone.find_func_with_scope(id).unwrap().1
                ));
                assert!(std::rc::Rc::ptr_eq(
                    func_global,
                    ctx_local.find_func_with_scope(id).unwrap().1
                ));
            }
            ast::DefKind::Var(var) => {
                assert!(
                    ctx.find_value_at_slot(
                        layout
                            .resolve_var(p4spec_rust::lang::il::ast::Var {
                                id: var.id.clone(),
                                typ: p4spec_rust::lang::data::typ::make::bool(),
                                iters: vec![]
                            })
                            .slot
                    )
                    .is_none()
                );
            }
        }
    }
}

#[test]
fn test_duplicate_relations_share_namespace_and_report_second_span() {
    use p4spec_rust::lang::hints::input::InputHint;
    let not_typ = phrase!(node: ast::NotTypKind::Arg(typ::make::bool()), span: id("r", 1).span);
    let rel = ast::RelDef::Extern(Box::new(ast::ExternRel {
        id: id("r", 1),
        not_typ: not_typ.clone(),
        input_hint: InputHint::new(vec![p4spec_rust::phrase!(node: 0, span: Default::default())]),
        hints: vec![],
    }));
    let rel_duplicate = ast::RelDef::Defined(Box::new(ast::DefinedRel {
        id: id("r", 9),
        not_typ,
        input_hint: InputHint::new(vec![p4spec_rust::phrase!(node: 0, span: Default::default())]),
        rule_groups: vec![],
        else_group: None,
        hints: vec![],
    }));
    let error =
        Global::load(vec![def(ast::DefKind::Rel(rel)), def(ast::DefKind::Rel(rel_duplicate))])
            .unwrap_err()
            .into_report();
    assert_eq!(error.span(), id("r", 9).span);
    assert_eq!(
        error.diagnostic().message,
        p4spec_rust::interp::shared::error::context::binding_repeated(
            EntityKind::Relation,
            "r".into()
        )
        .message
    );
}

#[test]
fn test_definition_lookup_errors_and_local_type_isolation() {
    let global = Global::load(vec![]).unwrap();
    let ctx = Context::new(&global);
    let id = id("missing", 8);
    for (kind, error) in [
        (EntityKind::Type, ctx.find_typdef(&id).unwrap_err().into_report()),
        (EntityKind::Relation, ctx.find_rel(&id).unwrap_err().into_report()),
        (EntityKind::Function, ctx.find_func_with_scope(&id).unwrap_err().into_report()),
    ] {
        assert_eq!(error.span(), id.span);
        assert_eq!(
            error.diagnostic().message,
            p4spec_rust::interp::shared::error::context::binding_undefined(kind, id.node.clone())
                .message
        );
    }
    let mut ctx_child = ctx.clone();
    ctx_child.add_typdef(id.clone(), TypeDef::Extern).unwrap();
    ctx_child
        .add_func(id.clone(), Callable::prepare(func("missing", 8)).into())
        .unwrap();
    assert!(ctx.find_typdef_opt(&id).is_none());
    assert!(ctx.find_func_opt(&id).is_none());
    assert_eq!(
        ctx_child
            .find_defined_typdef(&id)
            .unwrap_err()
            .into_report()
            .diagnostic()
            .message,
        p4spec_rust::interp::shared::error::context::binding_undefined(
            EntityKind::DefinedType,
            id.node
        )
        .message
    );
}
