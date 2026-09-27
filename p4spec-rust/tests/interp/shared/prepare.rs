use p4spec_rust::interp::shared::context::IterContext;
use p4spec_rust::interp::shared::prepare::Prepare;
use p4spec_rust::lang::data::var::{IdSlot, VarSlot};
use p4spec_rust::runtime::envs::interp::{
    shared::callable::Callable,
    shared::frame::{Frame, FrameLayout},
    sl::ast_prepared as sl,
};
use std::rc::Rc;

use p4spec_rust::{
    interp::shared::prepare::ast,
    lang::{
        al::ast as al_source,
        common::source::{Position, Span},
        il::ast as il_source,
        sl::ast as sl_source,
        traits::{eq::SyntaxEq, print::Print},
    },
    note_phrase, phrase,
};

// == Helpers

fn span(line: usize) -> Span {
    Span::new(Position::new("prepare", line, 0), Position::new("prepare", line, 1))
}

fn expression(name: &str, line: usize) -> il_source::Exp {
    p4spec_rust::note_phrase!(node: p4spec_rust::lang::il::ast::ExpKind::Id(phrase!(node: name.to_owned(), span: span(line))), note: Rc::new(il_source::TypKind::Bool), span: span(line))
}

fn variable(name: &str, iters: Vec<il_source::Iter>) -> il_source::Var {
    il_source::Var {
        id: phrase!(node: name.to_owned(), span: span(1)),
        typ: phrase!(node: il_source::TypKind::Bool, span: span(1)),
        iters,
    }
}

fn iter_expression(
    exp_inner: il_source::Exp,
    iter: il_source::Iter,
    vars: Vec<il_source::Var>,
    line: usize,
) -> il_source::Exp {
    let typ = il_source::TypKind::Iter(
        Box::new(phrase!(node: exp_inner.note.as_ref().clone(), span: exp_inner.span.clone())),
        iter,
    );
    note_phrase!(
        node: il_source::ExpKind::Iter(Box::new(exp_inner), il_source::ExpIter { iter, vars }),
        note: Rc::new(typ),
        span: span(line),
    )
}

// == Preparation

#[test]
fn preparation_preserves_occurrence_spans_and_type_allocations() {
    let exp_a: il_source::Exp = expression("x", 1);
    let exp_b = expression("x", 8);
    let typ_a = exp_a.note.clone();
    let typ_b = exp_b.note.clone();
    let mut layout = FrameLayout::default();
    let exp_prepared_a: il_source::Exp<IdSlot, VarSlot> = exp_a.clone().prepare(&mut layout);
    let exp_prepared_b = exp_b.clone().prepare(&mut layout);
    let (ast::ExpKind::Id(id_a), ast::ExpKind::Id(id_b)) =
        (&exp_prepared_a.node, &exp_prepared_b.node)
    else {
        panic!("expected prepared variables");
    };
    assert_eq!(id_a.slot, id_b.slot);
    assert_eq!(id_a.id.span, span(1));
    assert_eq!(id_b.id.span, span(8));
    assert!(Rc::ptr_eq(&typ_a, &exp_prepared_a.note));
    assert!(Rc::ptr_eq(&typ_b, &exp_prepared_b.note));
}

#[test]
fn binding_preparation_reserves_only_its_own_iterator_path() {
    let mut layout = FrameLayout::default();
    let var = variable("x", vec![il_source::Iter::List]).prepare(&mut layout);
    assert_eq!(layout.len(), 1);
    assert_eq!(var.var.iters, vec![il_source::Iter::List]);
    let frame = Frame::new(Rc::new(layout));
    assert!(frame.get(var.slot).is_none());
}

#[test]
fn syntax_equality_and_printing_ignore_callable_slot_order() {
    let exp_source = expression("x", 2);
    let mut layout_a = FrameLayout::default();
    let mut layout_b = FrameLayout::default();
    layout_b.resolve_var(p4spec_rust::lang::il::ast::Var {
        id: phrase!(node: "y".to_owned(), span: span(1)),
        typ: p4spec_rust::lang::data::typ::make::bool(),
        iters: vec![],
    });
    let exp_a = exp_source.clone().prepare(&mut layout_a);
    let exp_b = exp_source.clone().prepare(&mut layout_b);
    let (ast::ExpKind::Id(id_a), ast::ExpKind::Id(id_b)) = (&exp_a.node, &exp_b.node) else {
        panic!("expected prepared variables");
    };
    assert_ne!(id_a.slot, id_b.slot);
    assert_eq!(exp_a, exp_b);
    assert!(exp_a.syntax_eq(&exp_b));
    assert_eq!(Print::to_string(&exp_a), Print::to_string(&exp_source));
}

#[test]
fn algorithmic_clauses_and_else_clause_share_one_layout() {
    let clause = |name: &str| {
        phrase!(node: al_source::ClauseKind {
            args: vec![phrase!(node: il_source::ArgKind::Exp(Box::new(expression("x", 1))), span: span(1))],
            exp: expression(name, 2),
            prems: vec![],
        }, span: span(1))
    };
    let func_source = al_source::MetaFuncDef::Defined(Box::new(al_source::DefinedFunc {
        id: phrase!(node: "f".to_owned(), span: span(1)),
        tparams: vec![],
        params: vec![],
        typ: phrase!(node: il_source::TypKind::Bool, span: span(1)),
        clauses: vec![clause("a"), clause("b")],
        else_clause: Some(clause("c")),
        hints: vec![],
    }));
    let func = Callable::prepare(func_source.clone());
    let al_source::MetaFuncDef::<IdSlot, VarSlot>::Defined(func_defined) = &func.def else {
        panic!("expected defined function")
    };
    assert_eq!(func.layout.len(), 4);
    let slots = func_defined
        .clauses
        .iter()
        .chain(func_defined.else_clause.iter())
        .map(|clause| {
            let ast::ArgKind::Exp(exp) = &clause.node.args[0].node else {
                panic!("expected expression")
            };
            let ast::ExpKind::Id(slot) = &exp.node else { panic!("expected variable") };
            slot
        })
        .collect::<Vec<_>>();
    assert!(slots.windows(2).all(|slots| slots[0].slot == slots[1].slot));
}

#[test]
fn structured_parameters_and_case_guards_share_the_callable_layout() {
    let instr_source = phrase!(node: sl_source::InstrKind::Case(sl_source::CaseInstr {
        exp: expression("x", 2),
        cases: vec![sl_source::Case {
            guard: sl_source::Guard::Mem(expression("ys", 3)),
            block: vec![phrase!(node: sl_source::InstrKind::Return(sl_source::ReturnInstr {
                exp: expression("z", 4),
            }), span: span(4))],
        }],
        dangle: false,
    }), span: span(2));
    let func_source = sl_source::MetaFuncDef::Defined(sl_source::DefinedFunc {
        id: phrase!(node: "f".to_owned(), span: span(1)),
        tparams: vec![],
        params: vec![phrase!(node: sl_source::ParamKind::Exp(
            phrase!(node: il_source::TypKind::Bool, span: span(1)),
            Box::new(expression("x", 1)),
        ), span: span(1))],
        typ: phrase!(node: il_source::TypKind::Bool, span: span(1)),
        block: vec![instr_source.clone()],
        block_else: None,
        hints: vec![],
    });
    let func = Callable::prepare(func_source.clone());
    let sl_source::MetaFuncDef::<IdSlot, VarSlot>::Defined(func_defined) = &func.def else {
        panic!("expected defined function")
    };
    assert_eq!(func.layout.len(), 3);
    let sl::InstrKind::Case(instr) = &func_defined.block[0].node else { panic!("expected case") };
    let sl::ParamKind::Exp(_, exp_param) = &func_defined.params[0].node else {
        panic!("expected parameter pattern")
    };
    let (ast::ExpKind::Id(id_param), ast::ExpKind::Id(id_case)) =
        (&exp_param.node, &instr.exp.node)
    else {
        panic!("expected variables")
    };
    assert_eq!(id_param.slot, id_case.slot);
    assert_eq!(Print::to_string(&func_defined.block[0]), Print::to_string(&instr_source));
}

#[test]
fn lookup_errors_retain_leaf_spans_through_optional_and_list_bindings() {
    use p4spec_rust::interp::{
        al::context as al_context,
        shared::{
            error::{ContextErrorKind, EntityKind, ErrorKind},
            util::find_var,
        },
        sl::context as sl_context,
    };

    use p4spec_rust::{
        interp::{
            al::{AlInterp, Config as AlConfig},
            sl::{Config as SlConfig, SlInterp},
        },
        runner::{NullExtern, NullInterface, Runner},
    };

    let mut var_opt = variable("x", vec![]);
    var_opt.id.span = span(20);
    let exp_opt = iter_expression(expression("x", 7), il_source::Iter::Opt, vec![var_opt], 30);
    let mut var_list = variable("x", vec![il_source::Iter::Opt]);
    var_list.id.span = span(40);
    let exp_list = iter_expression(exp_opt.clone(), il_source::Iter::List, vec![var_list], 50);
    let global_al = al_context::Global::load(vec![]).unwrap();
    let global_sl = sl_context::Global::load(vec![]).unwrap();
    for (exp_source, iters, name) in [
        (expression("x", 7), vec![], "x"),
        (exp_opt, vec![il_source::Iter::Opt], "x?"),
        (exp_list, vec![il_source::Iter::Opt, il_source::Iter::List], "x?*"),
    ] {
        let mut layout = FrameLayout::default();
        let exp_prepared = exp_source.clone().prepare(&mut layout);
        let layout = Rc::new(layout);
        let ctx_al = al_context::Context::new(&global_al).localize_with_layout(&layout);
        let ctx_sl = sl_context::Context::new(&global_sl).localize_with_layout(&layout);
        let slot_lookup = find_var(&ctx_al, &exp_prepared).unwrap();
        assert_eq!(slot_lookup.var.id.span, span(7));
        assert_eq!(slot_lookup.var.iters, iters);
        let id = phrase!(node: "test".to_owned(), span: span(1));
        let typ = phrase!(node: exp_source.note.as_ref().clone(), span: span(1));
        let func_al = al_source::DefinedFunc {
            id: id.clone(),
            tparams: vec![],
            params: vec![],
            typ: typ.clone(),
            clauses: vec![phrase!(node: al_source::ClauseKind {
                args: vec![], exp: exp_source.clone(), prems: vec![],
            }, span: span(1))],
            else_clause: None,
            hints: vec![],
        };
        let func_sl = sl_source::DefinedFunc {
            id,
            tparams: vec![],
            params: vec![],
            typ,
            block: vec![phrase!(node: sl_source::InstrKind::Return(sl_source::ReturnInstr {
                exp: exp_source.clone(),
            }), span: span(1))],
            block_else: None,
            hints: vec![],
        };
        let mut runner_al = Runner::new(
            al_context::Global::load(vec![phrase!(node: al_source::DefKind::MetaFunc(
                al_source::MetaFuncDef::Defined(Box::new(func_al))
            ), span: span(1))])
            .unwrap(),
            AlInterp::new(AlConfig::new(false, false, false)),
            NullInterface,
            NullExtern,
        );
        let mut runner_sl = Runner::new(
            sl_context::Global::load(vec![phrase!(node: sl_source::DefKind::MetaFunc(
                sl_source::MetaFuncDef::Defined(func_sl)
            ), span: span(1))])
            .unwrap(),
            SlInterp::new(SlConfig::new(false, false, false)),
            NullInterface,
            NullExtern,
        );
        for error in [
            runner_al.context().call_func("test", &[], &[]).unwrap_err(),
            runner_sl.context().call_func("test", &[], &[]).unwrap_err(),
            ctx_al
                .find_list_values_by_var(
                    &p4spec_rust::lang::data::value::ValueArena::new(),
                    std::slice::from_ref(&slot_lookup),
                )
                .unwrap_err(),
            ctx_sl
                .find_list_values_by_var(
                    &p4spec_rust::lang::data::value::ValueArena::new(),
                    std::slice::from_ref(&slot_lookup),
                )
                .unwrap_err(),
        ] {
            let mut errors = vec![&error];
            let mut errors_undefined = Vec::new();
            while let Some(error) = errors.pop() {
                if matches!(*error.kind, ErrorKind::Context(ContextErrorKind::Undefined { .. })) {
                    errors_undefined.push(error);
                }
                errors.extend(&error.children);
            }
            assert_eq!(errors_undefined.len(), 1, "{error}");
            assert_eq!(errors_undefined[0].span, span(7));
            assert_eq!(
                *errors_undefined[0].kind,
                ErrorKind::Context(ContextErrorKind::Undefined {
                    kind: EntityKind::Value,
                    name: name.to_owned(),
                })
            );
        }
        assert_eq!(Print::to_string(&exp_prepared), Print::to_string(&exp_source));
    }
}

#[test]
fn iterated_variable_lookup_requires_matching_single_binders() {
    use p4spec_rust::interp::{
        al::context::{Context, Global},
        shared::util::{find_slot, find_var},
    };
    let global = Global::load(vec![]).unwrap();
    for vars in [
        vec![],
        vec![variable("x", vec![]), variable("y", vec![])],
        vec![variable("y", vec![])],
        vec![variable("x", vec![il_source::Iter::List])],
    ] {
        let exp_source = iter_expression(expression("x", 7), il_source::Iter::Opt, vars, 30);
        let mut layout = FrameLayout::default();
        let exp_prepared = exp_source.clone().prepare(&mut layout);
        let ctx = Context::new(&global).localize_with_layout(&layout.into());
        assert!(find_var(&ctx, &exp_prepared).is_none());
        assert!(find_slot(&ctx, &exp_prepared).is_none());
    }
    let exp_source = note_phrase!(
        node: il_source::ExpKind::Bool(true),
        note: Rc::new(il_source::TypKind::Bool),
        span: span(7),
    );
    let exp_source =
        iter_expression(exp_source, il_source::Iter::Opt, vec![variable("x", vec![])], 30);
    let mut layout = FrameLayout::default();
    let exp_prepared = exp_source.prepare(&mut layout);
    let ctx = Context::new(&global).localize_with_layout(&layout.into());
    assert!(find_var(&ctx, &exp_prepared).is_none());
    assert!(find_slot(&ctx, &exp_prepared).is_none());
}

#[test]
fn borrowed_printers_preserve_comparison_arguments_and_iterator_diagnostics() {
    let exp_source = note_phrase!(
        node: il_source::ExpKind::Cmp(
            il_source::CmpOp::Bool(p4spec_rust::lang::common::prim::bool::CmpOp::Eq),
            il_source::OpTyp::Bool,
            Box::new(expression("x", 1)),
            Box::new(expression("y", 2)),
        ),
        note: Rc::new(il_source::TypKind::Bool),
        span: span(1),
    );
    let mut layout = FrameLayout::default();
    let exp_prepared = exp_source.clone().prepare(&mut layout);
    assert_eq!(Print::to_string(&exp_prepared), Print::to_string(&exp_source));
    let args_source = vec![
        phrase!(node: il_source::ArgKind::Exp(Box::new(exp_source)), span: span(1)),
        phrase!(node: il_source::ArgKind::Def(phrase!(node: "f".to_owned(), span: span(3))), span: span(3)),
    ];
    let args_prepared = args_source
        .iter()
        .cloned()
        .map(|arg| arg.prepare(&mut layout))
        .collect::<Vec<_>>();
    assert_eq!(
        Print::to_string(args_prepared.as_slice()),
        Print::to_string(args_source.as_slice())
    );
    let prem_iter_source = il_source::PremIter {
        iter: il_source::Iter::List,
        vars_bound: vec![variable("x", vec![il_source::Iter::Opt])],
        vars_bind: vec![variable("y", vec![])],
    };
    let prem_iter_prepared = prem_iter_source.clone().prepare(&mut layout);
    assert_eq!(Print::to_string(&prem_iter_prepared), Print::to_string(&prem_iter_source));
}

#[test]
fn shared_syntax_ignores_slot_allocation_and_binder_order() {
    let exp_source =
        iter_expression(expression("x", 7), il_source::Iter::Opt, vec![variable("x", vec![])], 30);
    let mut layout = FrameLayout::default();
    let exp_prepared: il_source::Exp<IdSlot, VarSlot> = exp_source.clone().prepare(&mut layout);
    let mut layout_other = FrameLayout::default();
    variable("unused", vec![]).prepare(&mut layout_other);
    let exp_other = exp_source.clone().prepare(&mut layout_other);
    assert!(exp_prepared.syntax_eq(&exp_other));
    assert_eq!(Print::to_string(&exp_prepared), Print::to_string(&exp_other));

    let mut prem_iter_source: il_source::PremIter = il_source::PremIter {
        iter: il_source::Iter::List,
        vars_bound: vec![variable("x", vec![]), variable("y", vec![])],
        vars_bind: vec![variable("z", vec![il_source::Iter::Opt])],
    };
    let prem_iter_a: ast::PremIter = prem_iter_source
        .clone()
        .prepare(&mut FrameLayout::default());
    prem_iter_source.vars_bound.reverse();
    let prem_iter_b: ast::PremIter = prem_iter_source
        .clone()
        .prepare(&mut FrameLayout::default());
    assert!(prem_iter_a.syntax_eq(&prem_iter_b));
    assert_eq!(Print::to_string(&prem_iter_b), Print::to_string(&prem_iter_source));
}

#[test]
fn shared_mapping_preserves_nested_update_paths_and_call_arguments() {
    let typ = Rc::new(il_source::TypKind::Bool);
    let path_root = note_phrase!(node: il_source::PathKind::Root, note: typ.clone(), span: span(2));
    let path_idx = note_phrase!(
        node: il_source::PathKind::Idx(Box::new(path_root), Box::new(expression("idx", 3))),
        note: typ.clone(),
        span: span(3),
    );
    let path_slice = note_phrase!(
        node: il_source::PathKind::Slice(
            Box::new(path_idx),
            Box::new(expression("offset", 4)),
            Box::new(expression("len", 5)),
        ),
        note: typ.clone(),
        span: span(4),
    );
    let exp_arg = iter_expression(
        expression("value", 7),
        il_source::Iter::List,
        vec![variable("value", vec![])],
        8,
    );
    let exp_call = note_phrase!(
        node: il_source::ExpKind::Call(
            phrase!(node: "f".to_owned(), span: span(6)),
            vec![phrase!(node: il_source::TypKind::Bool, span: span(6))],
            vec![
                phrase!(node: il_source::ArgKind::Exp(Box::new(exp_arg)), span: span(7)),
                phrase!(node: il_source::ArgKind::Def(phrase!(node: "g".to_owned(), span: span(9))), span: span(9)),
            ],
        ),
        note: typ.clone(),
        span: span(6),
    );
    let exp_source: il_source::Exp = note_phrase!(
        node: il_source::ExpKind::Upd(
            Box::new(expression("base", 1)),
            Box::new(path_slice),
            Box::new(exp_call),
        ),
        note: typ.clone(),
        span: span(1),
    );
    let exp_prepared: il_source::Exp<IdSlot, VarSlot> =
        exp_source.clone().prepare(&mut FrameLayout::default());
    assert_eq!(Print::to_string(&exp_prepared), Print::to_string(&exp_source));
    let il_source::ExpKind::Upd(_, path_prepared, exp_call_prepared) = &exp_prepared.node else {
        panic!("expected update expression");
    };
    assert!(Rc::ptr_eq(&typ, &path_prepared.note));
    assert!(Rc::ptr_eq(&typ, &exp_call_prepared.note));
}

#[test]
fn nested_iteration_edges_share_only_the_required_binding_slots() {
    use p4spec_rust::interp::{
        al::context::{Context, Global},
        shared::util::find_var,
    };
    let global = Global::load(vec![]).unwrap();
    let mut exp_source = expression("x", 7);
    let mut iters = Vec::new();
    for iter in
        [il_source::Iter::List, il_source::Iter::List, il_source::Iter::Opt, il_source::Iter::List]
    {
        exp_source = iter_expression(exp_source, iter, vec![variable("x", iters.clone())], 20);
        iters.push(iter);
    }
    let mut layout = FrameLayout::default();
    let exp_prepared = exp_source.clone().prepare(&mut layout);
    assert_eq!(layout.len(), 5);
    let ctx = Context::new(&global).localize_with_layout(&Rc::new(layout.clone()));
    let mut exp_inner = &exp_prepared;
    while let ast::ExpKind::Iter(exp_next, exp_iter) = &exp_inner.node {
        let var = &exp_iter.vars[0];
        let slot_outer = layout.find_iter_var(&exp_iter.vars[0], exp_iter.iter);
        let mut iters_outer = var.var.iters.clone();
        iters_outer.push(exp_iter.iter);
        assert_eq!(slot_outer.var.iters, iters_outer);
        assert_eq!(var.slot, find_var(&ctx, exp_next).unwrap().slot);
        assert_eq!(slot_outer.slot, find_var(&ctx, exp_inner).unwrap().slot);
        exp_inner = exp_next;
    }
}

#[test]
fn premise_iteration_reuses_identical_bound_and_output_bindings() {
    let var = variable("x", vec![il_source::Iter::List, il_source::Iter::List]);
    let prem_iter = il_source::PremIter {
        iter: il_source::Iter::Opt,
        vars_bound: vec![var.clone()],
        vars_bind: vec![var],
    };
    let mut layout = FrameLayout::default();
    let prem_iter_prepared = prem_iter.clone().prepare(&mut layout);
    assert_eq!(layout.len(), 2);
    assert_eq!(prem_iter_prepared.vars_bound[0].slot, prem_iter_prepared.vars_bind[0].slot);
    assert_eq!(
        layout
            .find_iter_var(&prem_iter_prepared.vars_bound[0], prem_iter_prepared.iter)
            .slot,
        layout
            .find_iter_var(&prem_iter_prepared.vars_bind[0], prem_iter_prepared.iter)
            .slot
    );
    assert_eq!(
        layout
            .find_iter_var(&prem_iter_prepared.vars_bound[0], prem_iter_prepared.iter)
            .var
            .iters,
        vec![il_source::Iter::List, il_source::Iter::List, il_source::Iter::Opt]
    );
    assert_eq!(Print::to_string(&prem_iter_prepared), Print::to_string(&prem_iter));
}

#[test]
fn identifiers_and_binders_share_slots_without_sharing_occurrence_metadata() {
    let exp_source = expression("x", 7);
    let il_source::ExpKind::Id(id_source) = &exp_source.node else {
        panic!("expected identifier expression");
    };
    let var_source = variable("x", vec![]);
    let mut layout = FrameLayout::default();
    let exp_prepared = exp_source.clone().prepare(&mut layout);
    let var_prepared = var_source.clone().prepare(&mut layout);
    let ast::ExpKind::Id(id) = &exp_prepared.node else {
        panic!("expected prepared identifier expression");
    };
    assert_eq!(id.slot, var_prepared.slot);
    assert_eq!(&id.id, id_source);
    assert_eq!(id.id.span, span(7));
    assert_eq!(var_prepared.var, var_source);
    assert_eq!(var_prepared.var.id.span, span(1));
    assert_eq!(layout.len(), 1);
}

#[test]
fn preparation_of_nested_containers_preserves_annotations() {
    let exp_source = expression("x", 3);
    let span_source = exp_source.span.clone();
    let typ_note = exp_source.note.clone();
    let exps_source = vec![None, Some(Box::new(exp_source)), Some(Box::new(expression("y", 4)))];
    let mut layout = FrameLayout::default();
    let exps_prepared = exps_source.prepare(&mut layout);

    assert!(exps_prepared[0].is_none());
    let exp_prepared = exps_prepared[1].as_ref().unwrap();
    assert_eq!(exp_prepared.span, span_source);
    assert!(Rc::ptr_eq(&exp_prepared.note, &typ_note));
    let ast::ExpKind::Id(id) = &exp_prepared.node else {
        panic!("expected identifier");
    };
    assert_eq!(id.id.node, "x");
    assert_eq!(layout.len(), 2);
}

#[test]
fn preparation_of_deep_expressions_grows_the_stack() {
    std::thread::Builder::new()
        .stack_size(128 * 1024)
        .spawn(|| {
            let mut exp_source = expression("x", 1);
            for _ in 0..2048 {
                exp_source = note_phrase!(
                    node: il_source::ExpKind::Len(Box::new(exp_source)),
                    note: p4spec_rust::lang::data::typ::make::nat().node,
                    span: span(1),
                );
            }
            let mut exp_prepared = exp_source.prepare(&mut FrameLayout::default());
            for _ in 0..2048 {
                let ast::ExpKind::Len(exp_inner) = exp_prepared.node else {
                    panic!("expected nested length expression");
                };
                exp_prepared = *exp_inner;
            }
            let ast::ExpKind::Id(id) = exp_prepared.node else {
                panic!("expected identifier");
            };
            assert_eq!(id.id.node, "x");
        })
        .unwrap()
        .join()
        .unwrap();
}

#[test]
fn binding_list_equality_preserves_paths_and_duplicate_counts() {
    let var_plain = variable("x", vec![]);
    let var_list = variable("x", vec![il_source::Iter::List]);
    let mut var_other = var_plain.clone();
    var_other.id.span = span(9);
    var_other.typ = p4spec_rust::lang::data::typ::make::nat();
    for (vars_l, vars_r, equal) in [
        (
            vec![var_plain.clone(), var_list.clone()],
            vec![var_list.clone(), var_other.clone()],
            true,
        ),
        (vec![var_plain.clone()], vec![var_list.clone()], false),
        (vec![var_plain.clone(), var_plain.clone()], vec![var_other.clone()], false),
        (
            vec![var_plain.clone(), var_plain.clone(), var_list.clone()],
            vec![var_other, var_list.clone(), var_list],
            false,
        ),
        (
            vec![variable("x", vec![il_source::Iter::List, il_source::Iter::Opt])],
            vec![variable("x", vec![il_source::Iter::Opt, il_source::Iter::List])],
            false,
        ),
    ] {
        assert_eq!(vars_l.syntax_eq(&vars_r), equal);
        let mut layout_l = FrameLayout::default();
        let mut layout_r = FrameLayout::default();
        variable("unused", vec![]).prepare(&mut layout_r);
        let vars_l = vars_l.prepare(&mut layout_l);
        let vars_r = vars_r.prepare(&mut layout_r);
        assert_eq!(vars_l.syntax_eq(&vars_r), equal);
    }
}

#[test]
fn iteration_slots_resolve_in_either_registration_order() {
    use p4spec_rust::lang::common::Iter::{List, Opt};
    for reverse in [false, true] {
        let mut layout = FrameLayout::default();
        let mut iters = vec![vec![], vec![List], vec![Opt], vec![List, Opt]];
        if reverse {
            iters.reverse();
        }
        for iters in iters {
            layout.resolve_var(variable("x", iters));
        }
        let var = layout.resolve_var(variable("x", vec![]));
        let var_list = layout.resolve_var(variable("x", vec![List]));
        let var_opt = layout.resolve_var(variable("x", vec![Opt]));
        let var_list_opt = layout.resolve_var(variable("x", vec![List, Opt]));
        assert_eq!(layout.find_iter_slot(var.slot, List), var_list.slot);
        assert_eq!(layout.find_iter_slot(var.slot, Opt), var_opt.slot);
        assert_eq!(layout.find_iter_slot(var_list.slot, Opt), var_list_opt.slot);
        assert_eq!(layout.len(), 4);
    }
}

#[test]
fn slot_recognition_distinguishes_identity_iterations_from_computed_bodies() {
    use p4spec_rust::interp::{al::context, shared::util::find_slot};
    let exp = iter_expression(
        iter_expression(expression("x", 7), il_source::Iter::Opt, vec![variable("x", vec![])], 8),
        il_source::Iter::List,
        vec![variable("x", vec![il_source::Iter::Opt])],
        9,
    );
    let mut layout = FrameLayout::default();
    let exp = exp.prepare(&mut layout);
    let var = layout.resolve_var(variable("x", vec![il_source::Iter::Opt, il_source::Iter::List]));
    let exp_other = iter_expression(
        expression("y", 10),
        il_source::Iter::List,
        vec![variable("x", vec![])],
        11,
    )
    .prepare(&mut layout);
    let global = context::Global::load(vec![]).unwrap();
    let ctx = context::Context::new(&global).localize_with_layout(&Rc::new(layout));
    assert_eq!(find_slot(&ctx, &exp), Some(var.slot));
    assert_eq!(find_slot(&ctx, &exp_other), None);
}

#[test]
fn frame_snapshots_isolate_writes_and_wipe_all_bindings() {
    use p4spec_rust::lang::data::value::{ValueArena, make};
    let mut layout = FrameLayout::default();
    let vars: Vec<_> = (0..64)
        .map(|idx| layout.resolve_var(variable(&format!("x{idx}"), vec![])))
        .collect();
    let mut frame = Frame::new(Rc::new(layout));
    let mut arena = ValueArena::new();
    let value_a = make::bool(&mut arena, false, Span::default()).unwrap();
    let value_b = make::bool(&mut arena, true, Span::default()).unwrap();
    for var in &vars {
        frame.set(var.slot, value_a);
    }
    let mut frame_branch = frame.clone();
    for var in &vars {
        frame_branch.set(var.slot, value_b);
    }
    let frame_empty = frame_branch.wipe();
    for var in &vars {
        assert_eq!(frame.get(var.slot), Some(&value_a));
        assert_eq!(frame_branch.get(var.slot), Some(&value_b));
        assert_eq!(frame_empty.get(var.slot), None);
    }
    assert!(
        Frame::new(Rc::new(FrameLayout::default()))
            .layout()
            .is_empty()
    );
}
