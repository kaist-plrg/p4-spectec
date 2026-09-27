use p4spec_rust::{
    lang::{
        common::{
            notation::mixfix::Mixfix,
            source::{Position, Span},
        },
        el,
        hints::alter::{AlterationError, AlterationHint, Hole},
        hints::input::InputHint,
        il,
        pl::ast as pl,
        sl::ast as sl,
    },
    pass::prosify::{self, ProseErrorKind},
};

fn span(name: &str, column: usize) -> Span {
    Span::new(Position::new(name, 0, column), Position::new(name, 0, column))
}

fn id(name: &str) -> il::ast::Id {
    p4spec_rust::phrase! { node: name.to_owned(), span: span(name, 0) }
}

fn typ_bool() -> il::ast::Typ {
    p4spec_rust::phrase! { node: il::ast::TypKind::Bool, span: span("type", 0) }
}

fn exp_bool(value: bool, span: Span) -> il::ast::Exp {
    p4spec_rust::note_phrase! {
        node: il::ast::ExpKind::Bool(value),
        note: il::ast::TypKind::Bool,
        span: span,
    }
}

fn exp_var(name: &str, span: Span) -> il::ast::Exp {
    p4spec_rust::note_phrase! {
        node: il::ast::ExpKind::Id(id(name)),
        note: il::ast::TypKind::Bool,
        span: span,
    }
}

fn var(name: &str) -> il::ast::Var {
    il::ast::Var { id: id(name), typ: typ_bool(), iters: Vec::new() }
}

fn exp_call(name: &str, arg_exp: il::ast::Exp, span: Span) -> il::ast::Exp {
    exp_call_args(name, vec![arg_exp], span)
}

fn exp_call_args(name: &str, args: Vec<il::ast::Exp>, span: Span) -> il::ast::Exp {
    p4spec_rust::note_phrase! {
        node: il::ast::ExpKind::Call(
            id(name),
            Vec::new(),
            args
                .into_iter()
                .map(|exp| p4spec_rust::phrase! {
                    node: il::ast::ArgKind::Exp(Box::new(exp)),
                    span: span.clone(),
                })
                .collect(),
        ),
        note: il::ast::TypKind::Bool,
        span: span,
    }
}

fn prose_in_hint(hole: el::ast::Hole, span: Span) -> sl::Hint {
    el::ast::Hint {
        id: id("prose_in"),
        exp: p4spec_rust::phrase! { node: el::ast::ExpKind::Hole(hole), span: span },
    }
}

fn extern_func(name: &str, hints: Vec<sl::Hint>) -> sl::Def {
    p4spec_rust::phrase! {
        node: sl::DefKind::MetaFunc(sl::MetaFuncDef::Extern(sl::ExternFunc {
            id: id(name),
            tparams: Vec::new(),
            params: Vec::new(),
            typ: typ_bool(),
            hints,
        })),
        span: span("extern", 0),
    }
}

fn return_instr(value: bool, span: Span) -> sl::Instr {
    p4spec_rust::phrase! {
        node: sl::InstrKind::Return(sl::ReturnInstr {
            exp: exp_bool(value, span.clone()),
        }),
        span: span,
    }
}

fn defined_func(block: sl::Block) -> sl::Def {
    defined_func_with_else(block, None)
}

fn defined_func_with_else(block: sl::Block, block_else: Option<sl::Block>) -> sl::Def {
    p4spec_rust::phrase! {
        node: sl::DefKind::MetaFunc(sl::MetaFuncDef::Defined(sl::DefinedFunc {
            id: id("f"),
            tparams: Vec::new(),
            params: Vec::new(),
            typ: typ_bool(),
            block,
            block_else,
            hints: Vec::new(),
        })),
        span: span("function", 0),
    }
}

fn rel_signature() -> sl::RelSignature {
    sl::RelSignature {
        not_typ: p4spec_rust::phrase! {
            node: Mixfix::Arg(typ_bool()),
            span: span("signature", 0),
        },
        input_hint: InputHint::new(vec![p4spec_rust::phrase!(node: 0, span: Default::default())]),
    }
}

fn defined_rel(block: sl::Block) -> sl::Def {
    p4spec_rust::phrase! {
        node: sl::DefKind::Rel(sl::RelDef::Defined(sl::DefinedRel {
            id: id("relation"),
            rel_signature: rel_signature(),
            exps_input: vec![exp_var("input", span("relation-input", 0))],
            block,
            block_else: None,
            hints: Vec::new(),
        })),
        span: span("relation", 0),
    }
}

fn rule_instr(name: &str, column: usize) -> sl::Instr {
    let span_rule = span("rule", column);
    p4spec_rust::phrase! {
        node: sl::InstrKind::Rule(sl::RuleInstr {
            id: id(name),
            not_exp: Mixfix::Arg(exp_call(
                "partial",
                exp_bool(true, span_rule.clone()),
                span_rule.clone(),
            )),
            input_hint: InputHint::new(vec![p4spec_rust::phrase!(node: 0, span: Default::default())]),
            iter_instrs: Vec::new(),
            block: Vec::new(),
        }),
        span: span_rule,
    }
}

fn group_instr(name: &str, column: usize) -> sl::Instr {
    let span_group = span("group", column);
    p4spec_rust::phrase! {
        node: sl::InstrKind::Group(sl::GroupInstr {
            id: id(name),
            rel_signature: rel_signature(),
            exps: vec![exp_var("input", span_group.clone())],
            block: vec![rule_instr(name, column)],
        }),
        span: span_group,
    }
}

fn converted_func(block: sl::Block) -> pl::DefinedFunc {
    let mut spec_pl = prosify::convert(vec![defined_func(block)]).unwrap();
    let def_pl = spec_pl.pop().unwrap();
    match def_pl.node.node {
        pl::DefKind::MetaFunc(pl::MetaFuncDef::Defined(def_func_pl)) => def_func_pl,
        def_kind_pl => panic!("expected defined function, got {def_kind_pl:?}"),
    }
}

#[test]
fn test_multiple_group_instructions_become_backtrack_arms() {
    let span_a = span("arms", 1);
    let span_b = span("arms", 4);
    let def_func_pl = converted_func(vec![
        return_instr(true, span_a.clone()),
        return_instr(false, span_b.clone()),
    ]);

    assert_eq!(def_func_pl.block.len(), 1);
    let instr_pl = &def_func_pl.block[0];
    assert_eq!(instr_pl.node.span, Span::over(&[span_a, span_b]));
    let pl::InstrKind::Tier(pl::TierInstr {
        tier: pl::GroupInstr::Backtrack(pl::BacktrackInstr { blocks }),
    }) = &instr_pl.node.node
    else {
        panic!("expected backtracking alternatives, got {instr_pl:?}");
    };
    assert_eq!(blocks.len(), 2);
    assert_eq!(blocks[0].len(), 1);
    assert_eq!(blocks[1].len(), 1);
}

#[test]
fn test_let_and_debug_precede_their_converted_continuations() {
    let span_let = span("let", 0);
    let instr_let = p4spec_rust::phrase! {
        node: sl::InstrKind::Let(sl::LetInstr {
            exp_l: exp_var("x", span_let.clone()),
            exp_r: exp_bool(true, span_let.clone()),
            iter_instrs: Vec::new(),
            block: vec![
                return_instr(true, span("let", 2)),
                return_instr(false, span("let", 3)),
            ],
        }),
        span: span_let,
    };
    let def_func_pl = converted_func(vec![instr_let]);
    assert!(matches!(def_func_pl.block[0].node.node, pl::InstrKind::Let(_)));
    assert!(matches!(
        def_func_pl.block[1].node.node,
        pl::InstrKind::Tier(pl::TierInstr { tier: pl::GroupInstr::Backtrack(_) })
    ));

    let span_debug = span("debug", 0);
    let instr_debug = p4spec_rust::phrase! {
        node: sl::InstrKind::Debug(sl::DebugInstr {
            exp: exp_bool(true, span_debug.clone()),
            instr: Box::new(return_instr(false, span("debug", 2))),
        }),
        span: span_debug,
    };
    let def_func_pl = converted_func(vec![instr_debug]);
    assert!(matches!(def_func_pl.block[0].node.node, pl::InstrKind::Debug(_)));
    assert!(matches!(
        def_func_pl.block[1].node.node,
        pl::InstrKind::Tier(pl::TierInstr { tier: pl::GroupInstr::Return(_) })
    ));
}

#[test]
fn test_group_in_group_body_reports_the_instruction_span() {
    let span_group = span("invalid-group", 3);
    let instr_group = p4spec_rust::phrase! {
        node: sl::InstrKind::Group(sl::GroupInstr {
            id: id("g"),
            rel_signature: sl::RelSignature {
                not_typ: p4spec_rust::phrase! {
                    node: p4spec_rust::lang::common::notation::mixfix::Mixfix::Arg(typ_bool()),
                    span: span("signature", 0),
                },
                input_hint: p4spec_rust::lang::hints::input::InputHint::new(vec![p4spec_rust::phrase!(node: 0, span: Default::default())]),
            },
            exps: vec![exp_bool(true, span_group.clone())],
            block: Vec::new(),
        }),
        span: span_group.clone(),
    };

    let error = prosify::convert(vec![defined_func(vec![instr_group])]).unwrap_err();
    assert_eq!(error.kind, ProseErrorKind::InvalidGroupTier);
    assert_eq!(error.span, span_group);
}

#[test]
fn test_call_uses_hints_loaded_from_the_original_spec() {
    let span_call = span("call", 4);
    let def_func = defined_func(vec![p4spec_rust::phrase! {
        node: sl::InstrKind::Return(sl::ReturnInstr {
            exp: exp_call(
                "g",
                exp_bool(true, span("argument", 0)),
                span_call.clone(),
            ),
        }),
        span: span_call,
    }]);
    let spec_pl = prosify::convert(vec![
        extern_func("g", vec![prose_in_hint(el::ast::Hole::Next, span("hint", 0))]),
        def_func,
    ])
    .unwrap();
    let pl::DefKind::MetaFunc(pl::MetaFuncDef::Defined(def_func_pl)) = &spec_pl[1].node.node else {
        panic!("expected defined function");
    };
    let pl::InstrKind::Tier(pl::TierInstr {
        tier: pl::GroupInstr::Return(pl::ReturnInstr { exp: exp_pl }),
    }) = &def_func_pl.block[0].node.node
    else {
        panic!("expected return instruction");
    };
    assert_eq!(exp_pl.hints.prose_in, Some(AlterationHint::Hole(Hole::Next)));
}

#[test]
fn test_invalid_call_hint_reports_the_call_span() {
    let span_call = span("invalid-call", 7);
    let def_func = defined_func(vec![p4spec_rust::phrase! {
        node: sl::InstrKind::Return(sl::ReturnInstr {
            exp: exp_call(
                "g",
                exp_bool(true, span("argument", 0)),
                span_call.clone(),
            ),
        }),
        span: span_call.clone(),
    }]);
    let error = prosify::convert(vec![
        extern_func("g", vec![prose_in_hint(el::ast::Hole::Num(1), span("hint", 0))]),
        def_func,
    ])
    .unwrap_err();

    assert_eq!(
        error.kind,
        ProseErrorKind::Alteration(AlterationError::IndexOutOfBounds { index: 1, item_count: 1 })
    );
    assert_eq!(error.span, span_call);
}

#[test]
fn test_nested_calls_expand_left_to_right_and_preserve_outer_call() {
    let exp_inner_a =
        exp_call("inner_a", exp_bool(true, span("inner-a-arg", 0)), span("inner-a", 0));
    let exp_inner_b =
        exp_call("inner_b", exp_bool(false, span("inner-b-arg", 0)), span("inner-b", 0));
    let span_outer = span("outer", 0);
    let exp_outer = exp_call_args("outer", vec![exp_inner_a, exp_inner_b], span_outer.clone());
    let def_func_pl = converted_func(vec![p4spec_rust::phrase! {
        node: sl::InstrKind::Return(sl::ReturnInstr { exp: exp_outer }),
        span: span_outer,
    }]);

    assert_eq!(def_func_pl.block.len(), 3);
    let mut ids_bound = Vec::new();
    let mut ids_called = Vec::new();
    for instr_pl in &def_func_pl.block[..2] {
        let pl::InstrKind::Let(pl::LetInstr { exp_l, exp_r, .. }) = &instr_pl.node.node else {
            panic!("expected expanded let, got {instr_pl:?}");
        };
        let pl::ExpKind::Id(id_bound) = &exp_l.node.node else {
            panic!("expected fresh variable binding");
        };
        let pl::ExpKind::Call(id_called, _, _) = &exp_r.node.node else {
            panic!("expected lifted call");
        };
        ids_bound.push(id_bound.node.clone());
        ids_called.push(id_called.node.clone());
    }
    assert_eq!(ids_called, vec!["inner_a", "inner_b"]);

    let pl::InstrKind::Tier(pl::TierInstr {
        tier: pl::GroupInstr::Return(pl::ReturnInstr { exp: exp_outer_pl }),
    }) = &def_func_pl.block[2].node.node
    else {
        panic!("expected final return");
    };
    let pl::ExpKind::Call(id_outer, _, args_outer) = &exp_outer_pl.node.node else {
        panic!("expected outer call");
    };
    assert_eq!(id_outer.node, "outer");
    let ids_used = args_outer
        .iter()
        .map(|arg| match &arg.node {
            pl::ArgKind::Exp(exp) => match &exp.node.node {
                pl::ExpKind::Id(id) => id.node.clone(),
                exp_kind => panic!("expected variable argument, got {exp_kind:?}"),
            },
            arg_kind => panic!("expected expression argument, got {arg_kind:?}"),
        })
        .collect::<Vec<_>>();
    assert_eq!(ids_used, ids_bound);
}

#[test]
fn test_nested_call_in_update_path_is_lifted_before_return() {
    let span_call = span("path-call", 0);
    let path_root = p4spec_rust::note_phrase! {
        node: il::ast::PathKind::Root,
        note: il::ast::TypKind::Bool,
        span: span("path-root", 0),
    };
    let path = p4spec_rust::note_phrase! {
        node: il::ast::PathKind::Idx(
            Box::new(path_root),
            Box::new(exp_call(
                "index",
                exp_bool(true, span("path-arg", 0)),
                span_call.clone(),
            )),
        ),
        note: il::ast::TypKind::Bool,
        span: span("path", 0),
    };
    let exp_update = p4spec_rust::note_phrase! {
        node: il::ast::ExpKind::Upd(
            Box::new(exp_var("base", span("base", 0))),
            Box::new(path),
            Box::new(exp_bool(false, span("field", 0))),
        ),
        note: il::ast::TypKind::Bool,
        span: span("update", 0),
    };
    let def_func_pl = converted_func(vec![p4spec_rust::phrase! {
        node: sl::InstrKind::Return(sl::ReturnInstr { exp: exp_update }),
        span: span("return", 0),
    }]);

    let pl::InstrKind::Let(pl::LetInstr { exp_l, exp_r, .. }) = &def_func_pl.block[0].node.node
    else {
        panic!("expected lifted path call");
    };
    let pl::ExpKind::Id(id_fresh) = &exp_l.node.node else {
        panic!("expected fresh path variable");
    };
    assert!(matches!(&exp_r.node.node, pl::ExpKind::Call(id, _, _) if id.node == "index"));

    let pl::InstrKind::Tier(pl::TierInstr {
        tier: pl::GroupInstr::Return(pl::ReturnInstr { exp }),
    }) = &def_func_pl.block[1].node.node
    else {
        panic!("expected return after lifted path call");
    };
    let pl::ExpKind::Upd(_, path, _) = &exp.node.node else {
        panic!("expected update expression");
    };
    let pl::PathKind::Idx(_, exp_idx) = &path.node else {
        panic!("expected index path");
    };
    assert!(matches!(&exp_idx.node.node, pl::ExpKind::Id(id) if id.node == id_fresh.node));
}

#[test]
fn test_instruction_iterator_local_call_lifts_with_its_binding() {
    let span_let = span("iter-local", 0);
    let exp_local_call = exp_call("local", exp_var("x", span("x-use", 0)), span("local", 0));
    let exp_lifted_call =
        exp_call("independent", exp_bool(true, span("independent-arg", 0)), span("independent", 0));
    let exp_r = p4spec_rust::note_phrase! {
        node: il::ast::ExpKind::Tuple(vec![exp_local_call, exp_lifted_call]),
        note: il::ast::TypKind::Tuple(vec![typ_bool(), typ_bool()]),
        span: span_let.clone(),
    };
    let instr_let = p4spec_rust::phrase! {
        node: sl::InstrKind::Let(sl::LetInstr {
            exp_l: exp_var("result", span_let.clone()),
            exp_r,
            iter_instrs: vec![il::ast::PremIter {
                iter: il::ast::Iter::List,
                vars_bound: vec![var("x")],
                vars_bind: vec![var("result")],
            }],
            block: vec![return_instr(true, span("body", 0))],
        }),
        span: span_let,
    };

    let def_func_pl = converted_func(vec![instr_let]);
    assert_eq!(def_func_pl.block.len(), 4);

    let pl::InstrKind::Let(pl::LetInstr { exp_l, exp_r, iter_instrs, .. }) =
        &def_func_pl.block[0].node.node
    else {
        panic!("expected local call to be lifted");
    };
    let pl::ExpKind::Id(id_local) = &exp_l.node.node else {
        panic!("expected fresh variable for local call");
    };
    assert!(matches!(&exp_r.node.node, pl::ExpKind::Call(id, _, _) if id.node == "local"));
    assert_eq!(iter_instrs.len(), 1);
    assert_eq!(iter_instrs[0].iter, il::ast::Iter::List);
    assert_eq!(iter_instrs[0].vars_bound, vec![var("x")]);
    assert_eq!(iter_instrs[0].vars_bind[0].id.node, id_local.node);

    let pl::InstrKind::Let(pl::LetInstr { exp_r, iter_instrs, .. }) =
        &def_func_pl.block[1].node.node
    else {
        panic!("expected independent call to be lifted");
    };
    assert!(matches!(&exp_r.node.node, pl::ExpKind::Call(id, _, _) if id.node == "independent"));
    assert!(iter_instrs.is_empty());

    let pl::InstrKind::Let(pl::LetInstr { exp_r, iter_instrs, .. }) =
        &def_func_pl.block[2].node.node
    else {
        panic!("expected original iterated let");
    };
    assert_eq!(iter_instrs.len(), 1);
    assert_eq!(iter_instrs[0].vars_bound.len(), 1);
    assert_eq!(iter_instrs[0].vars_bound[0].id.node, id_local.node);
    let pl::ExpKind::Tuple(exps) = &exp_r.node.node else {
        panic!("expected tuple right-hand side");
    };
    assert!(matches!(&exps[0].node.node, pl::ExpKind::Id(id) if id.node == id_local.node));
    assert!(matches!(exps[1].node.node, pl::ExpKind::Id(_)));
}

#[test]
fn test_expression_iterator_lift_preserves_dimensions_and_outer_use() {
    let var_x = var("x");
    let exp_call = exp_call("each", exp_var("x", span("x-inner", 0)), span("each", 0));
    let exp_inner = p4spec_rust::note_phrase! {
        node: il::ast::ExpKind::Tuple(vec![exp_call, exp_var("x", span("x-sibling", 0))]),
        note: il::ast::TypKind::Tuple(vec![typ_bool(), typ_bool()]),
        span: span("iteration-inner", 0),
    };
    let exp_iter = p4spec_rust::note_phrase! {
        node: il::ast::ExpKind::Iter(
            Box::new(exp_inner),
            il::ast::ExpIter {
                iter: il::ast::Iter::List,
                vars: vec![var_x.clone()],
            },
        ),
        note: il::ast::TypKind::Iter(
            Box::new(p4spec_rust::phrase! {
                node: il::ast::TypKind::Tuple(vec![typ_bool(), typ_bool()]),
                span: span("tuple-type", 0),
            }),
            il::ast::Iter::List,
        ),
        span: span("iteration", 0),
    };
    let def_func_pl = converted_func(vec![p4spec_rust::phrase! {
        node: sl::InstrKind::Return(sl::ReturnInstr { exp: exp_iter }),
        span: span("return", 0),
    }]);

    let pl::InstrKind::Let(pl::LetInstr { exp_l, iter_instrs, .. }) =
        &def_func_pl.block[0].node.node
    else {
        panic!("expected lifted iterator call");
    };
    let pl::ExpKind::Id(id_fresh) = &exp_l.node.node else {
        panic!("expected fresh let variable");
    };
    assert_eq!(iter_instrs.len(), 1);
    assert_eq!(iter_instrs[0].iter, il::ast::Iter::List);
    assert_eq!(iter_instrs[0].vars_bound, vec![var_x.clone()]);
    assert_eq!(iter_instrs[0].vars_bind[0].id.node, id_fresh.node);

    let pl::InstrKind::Tier(pl::TierInstr {
        tier: pl::GroupInstr::Return(pl::ReturnInstr { exp }),
    }) = &def_func_pl.block[1].node.node
    else {
        panic!("expected return after lifted call");
    };
    let pl::ExpKind::Iter(exp_inner, il::ast::ExpIter { vars, .. }) = &exp.node.node else {
        panic!("expected iterated fresh result");
    };
    let pl::ExpKind::Tuple(exps) = &exp_inner.node.node else {
        panic!("expected iterated tuple");
    };
    assert!(matches!(&exps[0].node.node, pl::ExpKind::Id(id) if id.node == id_fresh.node));
    assert!(matches!(&exps[1].node.node, pl::ExpKind::Id(id) if id.node == "x"));
    assert_eq!(vars.len(), 2);
    assert_eq!(vars[0].id.node, id_fresh.node);
    assert_eq!(vars[1], var_x);
}

fn return_call(name: &str, column: usize) -> sl::Instr {
    let span_call = span("fallthrough", column);
    p4spec_rust::phrase! {
        node: sl::InstrKind::Return(sl::ReturnInstr {
            exp: exp_call(name, exp_bool(true, span_call.clone()), span_call.clone()),
        }),
        span: span_call,
    }
}

#[test]
fn test_failure_stamp_tracks_next_arm_then_final_failure() {
    let def_func_pl = converted_func(vec![return_call("a", 1), return_call("b", 2)]);
    let pl::InstrKind::Tier(pl::TierInstr {
        tier: pl::GroupInstr::Backtrack(pl::BacktrackInstr { blocks }),
    }) = &def_func_pl.block[0].node.node
    else {
        panic!("expected backtracking alternatives");
    };
    assert_eq!(blocks.len(), 2);
    assert_eq!(blocks[0][0].node.note, Some(pl::Fallthrough::Next));
    assert_eq!(blocks[1][0].node.note, Some(pl::Fallthrough::Fail));
}

#[test]
fn test_nonempty_else_changes_final_failure_destination() {
    let mut spec_pl = prosify::convert(vec![defined_func_with_else(
        vec![return_call("main", 1)],
        Some(vec![return_instr(false, span("else", 0))]),
    )])
    .unwrap();
    let def_pl = spec_pl.pop().unwrap();
    let pl::DefKind::MetaFunc(pl::MetaFuncDef::Defined(def_func_pl)) = def_pl.node.node else {
        panic!("expected defined function");
    };
    assert_eq!(def_func_pl.block[0].node.note, Some(pl::Fallthrough::Else));

    let mut spec_pl = prosify::convert(vec![defined_func_with_else(
        vec![return_call("main", 1)],
        Some(Vec::new()),
    )])
    .unwrap();
    let def_pl = spec_pl.pop().unwrap();
    let pl::DefKind::MetaFunc(pl::MetaFuncDef::Defined(def_func_pl)) = def_pl.node.node else {
        panic!("expected defined function");
    };
    assert_eq!(def_func_pl.block[0].node.note, Some(pl::Fallthrough::Fail));
}

#[test]
fn test_fresh_names_are_scoped_independently_across_else_blocks() {
    let return_nested_call = |name: &str, column: usize| {
        let span_outer = span("fresh-scope", column);
        let exp_inner = exp_call(name, exp_bool(true, span_outer.clone()), span_outer.clone());
        p4spec_rust::phrase! {
            node: sl::InstrKind::Return(sl::ReturnInstr {
                exp: exp_call("outer", exp_inner, span_outer.clone()),
            }),
            span: span_outer,
        }
    };
    let mut spec_pl = prosify::convert(vec![defined_func_with_else(
        vec![return_nested_call("main", 1)],
        Some(vec![return_nested_call("fallback", 2)]),
    )])
    .unwrap();
    let def_pl = spec_pl.pop().unwrap();
    let pl::DefKind::MetaFunc(pl::MetaFuncDef::Defined(def_func_pl)) = def_pl.node.node else {
        panic!("expected defined function");
    };
    let pl::InstrKind::Let(pl::LetInstr { exp_l: exp_main, .. }) = &def_func_pl.block[0].node.node
    else {
        panic!("expected lifted main call");
    };
    let pl::InstrKind::Let(pl::LetInstr { exp_l: exp_else, .. }) =
        &def_func_pl.block_else_opt.unwrap()[0].node.node
    else {
        panic!("expected lifted fallback call");
    };
    let (pl::ExpKind::Id(id_main), pl::ExpKind::Id(id_else)) =
        (&exp_main.node.node, &exp_else.node.node)
    else {
        panic!("expected fresh variables");
    };
    assert_eq!(id_main.node, id_else.node);
}

#[test]
fn test_context_rejects_duplicate_metavariables_at_the_new_binding() {
    let span_metavar = span("duplicate-metavar", 4);
    let def_var = p4spec_rust::phrase! {
        node: sl::DefKind::Var(sl::VarDef {
            id: p4spec_rust::phrase! { node: "bool".to_owned(), span: span_metavar.clone() },
            typ: typ_bool(),
            hints: Vec::new(),
        }),
        span: span_metavar.clone(),
    };
    let error = prosify::convert(vec![def_var]).unwrap_err();
    assert_eq!(error.kind, ProseErrorKind::DuplicateMetavariable);
    assert_eq!(error.span, span_metavar);
}

#[test]
fn test_zero_argument_nested_call_stays_in_the_original_expression() {
    let span_call = span("zero-argument", 3);
    let exp_zero = exp_call_args("zero", Vec::new(), span_call.clone());
    let exp_outer = exp_call("outer", exp_zero, span_call.clone());
    let def_func_pl = converted_func(vec![p4spec_rust::phrase! {
        node: sl::InstrKind::Return(sl::ReturnInstr { exp: exp_outer }),
        span: span_call,
    }]);

    assert_eq!(def_func_pl.block.len(), 1);
    let pl::InstrKind::Tier(pl::TierInstr {
        tier: pl::GroupInstr::Return(pl::ReturnInstr { exp }),
    }) = &def_func_pl.block[0].node.node
    else {
        panic!("expected direct return");
    };
    let pl::ExpKind::Call(_, _, args) = &exp.node.node else { panic!("expected outer call") };
    let pl::ArgKind::Exp(exp_zero) = &args[0].node else { panic!("expected expression argument") };
    assert!(
        matches!(&exp_zero.node.node, pl::ExpKind::Call(id, _, args) if id.node == "zero" && args.is_empty())
    );
}

#[test]
fn test_return_at_dispatch_level_reports_its_own_span() {
    let span_return = span("invalid-dispatch", 6);
    let error = prosify::convert(vec![defined_rel(vec![return_instr(true, span_return.clone())])])
        .unwrap_err();
    assert_eq!(error.kind, ProseErrorKind::InvalidDispatchTier);
    assert_eq!(error.span, span_return);
}

#[test]
fn test_relation_routes_stamp_each_group_toward_the_next_dispatch_arm() {
    let mut spec_pl = prosify::convert(vec![defined_rel(vec![
        group_instr("first", 1),
        group_instr("second", 2),
    ])])
    .unwrap();
    let def_pl = spec_pl.pop().unwrap();
    let pl::DefKind::Rel(pl::RelDef::Defined(def_rel_pl)) = def_pl.node.node else {
        panic!("expected defined relation");
    };
    let pl::InstrKind::Tier(pl::TierInstr {
        tier: pl::DispatchInstr::Route(pl::RouteInstr { blocks }),
    }) = &def_rel_pl.block[0].node.node
    else {
        panic!("expected dispatch route");
    };
    assert_eq!(blocks.len(), 2);
    let destinations = blocks
        .iter()
        .map(|block| {
            let pl::InstrKind::Tier(pl::TierInstr { tier: pl::DispatchInstr::Group(group) }) =
                &block[0].node.node
            else {
                panic!("expected group arm");
            };
            group.block[0].node.note.clone()
        })
        .collect::<Vec<_>>();
    assert_eq!(
        destinations,
        vec![Some(pl::Fallthrough::Group(id("second"))), Some(pl::Fallthrough::Fail),]
    );
}

#[test]
fn test_rule_input_call_is_lifted_inside_rulegroup() {
    let mut spec_pl = prosify::convert(vec![defined_rel(vec![group_instr("group", 1)])]).unwrap();
    let def_pl = spec_pl.pop().unwrap();
    let pl::DefKind::Rel(pl::RelDef::Defined(def_rel_pl)) = def_pl.node.node else {
        panic!("expected defined relation");
    };
    let pl::InstrKind::Tier(pl::TierInstr { tier: pl::DispatchInstr::Group(instr_group) }) =
        &def_rel_pl.block[0].node.node
    else {
        panic!("expected rulegroup");
    };
    let pl::InstrKind::Let(pl::LetInstr { exp_l, exp_r, .. }) = &instr_group.block[0].node.node
    else {
        panic!("expected lifted rule input call");
    };
    let pl::ExpKind::Id(id_fresh) = &exp_l.node.node else {
        panic!("expected fresh rule input variable");
    };
    assert!(matches!(&exp_r.node.node, pl::ExpKind::Call(id, _, _) if id.node == "partial"));

    let pl::InstrKind::Tier(pl::TierInstr { tier: pl::GroupInstr::Rule(instr_rule) }) =
        &instr_group.block[1].node.node
    else {
        panic!("expected rule after lifted call");
    };
    let Mixfix::Arg(exp_input) = &instr_rule.not_exp else {
        panic!("expected rule input expression");
    };
    assert!(matches!(&exp_input.node.node, pl::ExpKind::Id(id) if id.node == id_fresh.node));
}

fn check_let_candidate(exp_r: il::ast::Exp) -> sl::Instr {
    let span_check = span("check-let", 0);
    let exp_scrut = exp_var("scrutinee", span_check.clone());
    let exp_match = p4spec_rust::note_phrase! {
        node: il::ast::ExpKind::Match(
            Box::new(exp_scrut.clone()),
            il::ast::Pattern::Opt(il::ast::OptPattern::Some),
        ),
        note: il::ast::TypKind::Bool,
        span: span_check.clone(),
    };
    p4spec_rust::phrase! {
        node: sl::InstrKind::If(sl::IfInstr {
            exp: exp_match,
            iter_exps: Vec::new(),
            block: vec![p4spec_rust::phrase! {
                node: sl::InstrKind::Let(sl::LetInstr {
                    exp_l: exp_var("target", span_check.clone()),
                    exp_r,
                    iter_instrs: Vec::new(),
                    block: vec![return_instr(true, span("check-let", 2))],
                }),
                span: span_check.clone(),
            }],
            dangle: false,
        }),
        span: span_check,
    }
}

#[test]
fn test_check_let_shorthand_requires_a_scrutinee_alias_and_preserves_near_misses() {
    let def_func_pl =
        converted_func(vec![check_let_candidate(exp_var("scrutinee", span("check-let", 0)))]);
    let pl::InstrKind::CheckLetMatch(instr_check) = &def_func_pl.block[0].node.node else {
        panic!("expected check-let shorthand");
    };
    assert!(matches!(&instr_check.exp_l.node.node, pl::ExpKind::Id(id) if id.node == "target"));
    assert_eq!(instr_check.block.len(), 1);

    let def_func_pl =
        converted_func(vec![check_let_candidate(exp_var("different", span("check-let", 1)))]);
    let pl::InstrKind::If(instr_if) = &def_func_pl.block[0].node.node else {
        panic!("near miss must remain an if instruction");
    };
    let pl::InstrKind::Let(instr_let) = &instr_if.block[0].node.node else {
        panic!("near miss must preserve the leading let");
    };
    assert!(matches!(&instr_let.exp_r.node.node, pl::ExpKind::Id(id) if id.node == "different"));
    assert_eq!(instr_if.block.len(), 2);
}

#[test]
fn test_table_row_keeps_group_alternatives_separate() {
    let def_table = p4spec_rust::phrase! {
        node: sl::DefKind::MetaFunc(sl::MetaFuncDef::Table(sl::TableFunc {
            id: id("table"),
            params: Vec::new(),
            typ: typ_bool(),
            table_rows: vec![sl::TableRow {
                exps_input: Vec::new(),
                exp: exp_bool(true, span("table-condition", 0)),
                block: vec![
                    return_instr(true, span("table-arm", 1)),
                    return_instr(false, span("table-arm", 2)),
                ],
            }],
            hints: Vec::new(),
        })),
        span: span("table", 0),
    };
    let mut spec_pl = prosify::convert(vec![def_table]).unwrap();
    let def_pl = spec_pl.pop().unwrap();
    let pl::DefKind::MetaFunc(pl::MetaFuncDef::Table(def_table_pl)) = def_pl.node.node else {
        panic!("expected table function");
    };
    assert!(matches!(
        &def_table_pl.rows[0].block[0].node.node,
        pl::InstrKind::Tier(pl::TierInstr {
            tier: pl::GroupInstr::Backtrack(pl::BacktrackInstr { blocks })
        }) if blocks.len() == 2
    ));
}

#[test]
fn test_membership_guard_with_call_can_fail() {
    let span_case = span("case", 1);
    let exp_set = exp_call("a", exp_bool(true, span_case.clone()), span_case.clone());
    let instr = p4spec_rust::phrase! {
        node: sl::InstrKind::Case(sl::CaseInstr {
            exp: exp_var("x", span_case.clone()),
            cases: vec![sl::Case {
                guard: sl::Guard::Mem(exp_set),
                block: vec![return_instr(true, span_case.clone())],
            }],
            dangle: true,
        }),
        span: span_case,
    };
    let def_func_pl = converted_func(vec![instr]);
    assert_eq!(def_func_pl.block[0].node.note, Some(pl::Fallthrough::Fail));
}
