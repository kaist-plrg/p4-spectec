use crate::interp::report::{IntoReport, ReportExt};
use p4spec_rust::{diagnostic::ReportKind, interp::shared::backtrack::Failure};
use std::{cell::RefCell, rc::Rc};

use p4spec_rust::{
    interp::al::{AlInterp, Config, context::Global},
    lang::{
        al::ast,
        common::prim::{bool as boolean, num},
        common::{
            notation::{atom::Atom, mixfix::Mixfix},
            source::{Position, Span},
        },
        data::{
            typ,
            value::{Value, ValueArena, get, make},
        },
        il::ast::{ListPattern, OptPattern},
    },
    runner::{Interface, InterfaceError, NullExtern, NullInterface, Runner},
};

fn id(name: &str) -> ast::Id {
    p4spec_rust::phrase!(node: name.to_owned(), span: Span::default())
}

fn exp(kind: ast::ExpKind, typ: ast::Typ) -> ast::Exp {
    p4spec_rust::note_phrase!(node: kind, note: Rc::new(typ.node), span: typ.span)
}

fn int(num: i64) -> ast::Exp {
    exp(ast::ExpKind::Num(num::Number::Int(num.into())), typ::make::int())
}

fn text(value: &str) -> ast::Exp {
    exp(ast::ExpKind::Text(value.to_owned()), typ::make::text())
}

fn list(values: Vec<ast::Exp>) -> ast::Exp {
    exp(ast::ExpKind::List(values), typ::make::list(typ::make::int()))
}

fn function(name: &str, expression: ast::Exp) -> ast::Def {
    let typ =
        p4spec_rust::phrase!(node: expression.note.as_ref().clone(), span: expression.span.clone());
    p4spec_rust::phrase!(node: ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(Box::new(ast::DefinedFunc {
        id: id(name), tparams: vec![], params: vec![], typ,
        clauses: vec![p4spec_rust::phrase!(node: ast::ClauseKind { args: vec![], exp: expression, prems: vec![] }, span: Span::default())],
        else_clause: None, hints: vec![],
    }))), span: Span::default())
}

fn eval(
    expression: ast::Exp,
) -> Result<(ValueArena, Value), p4spec_rust::interp::shared::error::Error> {
    let global = Global::load(vec![function("test", expression)]).unwrap();
    let mut runner = Runner::<AlInterp, _, _>::new(
        global,
        AlInterp::new(Config::new(false, false, false)),
        NullInterface,
        NullExtern,
    );
    let value = runner
        .context()
        .call_func("test", &[], &[])
        .map_err(Failure::into_report)?;
    Ok((std::mem::take(runner.arena_mut()), value))
}

#[test]
#[should_panic(expected = "value must be bound")]
fn test_reading_an_unbound_slot_violates_the_ir_precondition() {
    let _ = eval(exp(ast::ExpKind::Id(id("missing")), typ::make::int()));
}

#[test]
#[should_panic(expected = "concatenation operands must have matching kinds")]
fn test_concatenation_requires_valid_operand_kinds() {
    let _ = eval(exp(ast::ExpKind::Cat(Box::new(text("x")), Box::new(int(1))), typ::make::text()));
}

#[test]
#[should_panic(expected = "length operand must be a text or list")]
fn test_length_requires_a_valid_operand_kind() {
    let _ = eval(exp(ast::ExpKind::Len(Box::new(int(1))), typ::make::nat()));
}

#[test]
fn test_optional_casts_require_option_values() {
    let typ = typ::make::opt(typ::make::int());
    let exp_inner = exp(ast::ExpKind::Bool(false), typ::make::bool());
    for kind in [
        ast::ExpKind::UpCast(Box::new(typ.clone()), Box::new(exp_inner.clone())),
        ast::ExpKind::DownCast(Box::new(typ.clone()), Box::new(exp_inner)),
    ] {
        let panic =
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| eval(exp(kind, typ.clone()))))
                .expect_err("optional casts require option operands");
        let message = panic
            .downcast_ref::<String>()
            .map(String::as_str)
            .or_else(|| panic.downcast_ref::<&str>().copied())
            .unwrap();
        assert!(message.contains("operand must be an option"), "{message}");
    }
}

#[test]
fn test_operand_shapes_are_ir_invariants() {
    let cases = [
        (
            "boolean",
            exp(
                ast::ExpKind::Un(
                    ast::UnOp::Bool(boolean::UnOp::Not),
                    ast::OpTyp::Bool,
                    Box::new(int(1)),
                ),
                typ::make::bool(),
            ),
        ),
        (
            "number",
            exp(
                ast::ExpKind::Bin(
                    ast::BinOp::Num(num::BinOp::Add),
                    ast::OpTyp::Int,
                    Box::new(text("x")),
                    Box::new(int(1)),
                ),
                typ::make::int(),
            ),
        ),
        (
            "membership",
            exp(ast::ExpKind::Mem(Box::new(int(1)), Box::new(int(2))), typ::make::bool()),
        ),
        ("index", exp(ast::ExpKind::Idx(Box::new(int(1)), Box::new(int(0))), typ::make::int())),
        (
            "slice",
            exp(
                ast::ExpKind::Slice(Box::new(int(1)), Box::new(int(0)), Box::new(int(1))),
                typ::make::list(typ::make::int()),
            ),
        ),
        (
            "tuple cast",
            exp(
                ast::ExpKind::UpCast(
                    Box::new(typ::make::tuple(vec![typ::make::int()])),
                    Box::new(exp(ast::ExpKind::Tuple(vec![]), typ::make::tuple(vec![]))),
                ),
                typ::make::tuple(vec![typ::make::int()]),
            ),
        ),
    ];
    let failures = cases
        .into_iter()
        .filter_map(|(name, expression)| {
            let result =
                std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| eval(expression)));
            result.is_ok().then_some(name)
        })
        .collect::<Vec<_>>();
    assert!(failures.is_empty(), "expected invariant panics: {failures:?}");
}

fn numbers(arena: &ValueArena, value: &Value) -> Vec<String> {
    get::list(arena, value)
        .unwrap()
        .iter()
        .map(|value| num::to_int(get::num(arena, value).unwrap()).to_string())
        .collect()
}

#[test]
fn test_repeated_evaluation_reuses_the_expression_type_allocation_without_call_caching() {
    let expression = list(vec![int(1), int(2)]);
    let typ = expression.note.clone();
    let global = Global::load(vec![function("test", expression)]).unwrap();
    let mut runner = Runner::<AlInterp, _, _>::new(
        global,
        AlInterp::new(Config::new(false, false, false)),
        NullInterface,
        NullExtern,
    );
    for _ in 0..2 {
        let value = runner.context().call_func("test", &[], &[]).unwrap();
        assert!(Rc::ptr_eq(runner.arena().typ(&value), &typ));
        assert_eq!(numbers(runner.arena(), &value), ["1", "2"]);
    }
}

fn path(kind: ast::PathKind, typ: ast::Typ) -> ast::Path {
    p4spec_rust::note_phrase!(node: kind, note: Rc::new(typ.node), span: typ.span)
}

#[test]
#[should_panic(expected = "condition must be a boolean")]
fn test_condition_typed_kind_precondition() {
    let mut def = function("test", int(1));
    let ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func)) = &mut def.node else {
        panic!("expected a defined function")
    };
    func.clauses[0].node.prems.push(p4spec_rust::phrase! {
        node: ast::PremKind::If(ast::IfPrem { exp: int(1) }),
        span: Span::default(),
    });
    let mut runner = Runner::new(
        Global::load(vec![def]).unwrap(),
        AlInterp::new(Config::new(false, false, false)),
        NullInterface,
        NullExtern,
    );
    let _ = runner.context().call_func("test", &[], &[]);
}

#[test]
#[should_panic(expected = "field update base must be a struct")]
fn test_field_update_typed_kind_precondition() {
    let atom = p4spec_rust::phrase!(node: Atom::Keyword("field".into()), span: Span::default());
    let path_root = path(ast::PathKind::Root, typ::make::int());
    let path_field = path(ast::PathKind::Dot(Box::new(path_root), atom), typ::make::int());
    let _ = eval(exp(
        ast::ExpKind::Upd(Box::new(int(1)), Box::new(path_field), Box::new(int(2))),
        typ::make::int(),
    ));
}

#[test]
fn test_optional_cons_tail_remains_a_runtime_error() {
    use p4spec_rust::pass::{algo, elaborate};

    let spec_el = crate::spec_fixture::parse(
        "syntax foo = | A\nvar x : foo\ndec $cons(foo?) : foo?\ndef $cons(x?) = A :: x?",
    )
    .unwrap();
    let spec_il = elaborate::convert(spec_el).unwrap();
    let spec_al = algo::convert(spec_il).unwrap();
    let mut runner = Runner::new(
        Global::load(spec_al).unwrap(),
        AlInterp::new(Config::new(false, false, true)),
        NullInterface,
        NullExtern,
    );
    let typ = typ::make::opt(typ::make::var(id("foo"), vec![]));
    let value = make::opt(runner.arena_mut(), typ.node.into(), None, Span::default()).unwrap();
    let report = runner
        .context()
        .call_func("cons", &[], &[value])
        .unwrap_err()
        .into_report();
    assert!(report.find_code("runtime/value-invalid").is_some());
}

#[test]
fn test_nested_updates_preserve_the_original_and_surrounding_fields() {
    let atom = p4spec_rust::phrase!(node: Atom::Keyword("items".into()), span: Span::default());
    let typ_list = typ::make::list(typ::make::int());
    let typ_struct = typ::make::var(id("record"), vec![]);
    let original = exp(
        ast::ExpKind::Str(vec![ast::ExpField {
            atom: atom.clone(),
            exp: list(vec![int(1), int(2), int(3)]),
        }]),
        typ_struct.clone(),
    );
    let root = path(ast::PathKind::Root, typ_struct);
    let field = path(ast::PathKind::Dot(Box::new(root), atom.clone()), typ_list.clone());
    let index = path(ast::PathKind::Idx(Box::new(field), Box::new(int(1))), typ::make::int());
    let updated = exp(
        ast::ExpKind::Upd(Box::new(original.clone()), Box::new(index), Box::new(int(9))),
        typ::make::var(id("record"), vec![]),
    );
    let updated = exp(ast::ExpKind::Dot(Box::new(updated), atom.clone()), typ_list.clone());
    let original = exp(ast::ExpKind::Dot(Box::new(original), atom), typ_list);
    let (arena, value_eval_1) = eval(updated).unwrap();
    assert_eq!(numbers(&arena, &value_eval_1), ["1", "9", "3"]);
    let (arena, value_eval_2) = eval(original).unwrap();
    assert_eq!(numbers(&arena, &value_eval_2), ["1", "2", "3"]);
}

#[test]
fn test_slice_updates_require_equal_lengths_and_support_text() {
    let typ_text = typ::make::text();
    let root = path(ast::PathKind::Root, typ_text.clone());
    let slice = path(
        ast::PathKind::Slice(Box::new(root), Box::new(int(1)), Box::new(int(2))),
        typ_text.clone(),
    );
    let update = |replacement| {
        exp(
            ast::ExpKind::Upd(
                Box::new(text("abcd")),
                Box::new(slice.clone()),
                Box::new(text(replacement)),
            ),
            typ_text.clone(),
        )
    };
    let (arena, value_eval_1) = eval(update("XY")).unwrap();
    assert_eq!(get::text(&arena, &value_eval_1).unwrap(), "aXYd");
    assert!(
        eval(update("X"))
            .unwrap_err()
            .into_report()
            .render()
            .contains("slice of length 2")
    );
}

#[test]
fn test_negative_slice_lengths_are_out_of_bounds() {
    let slice = |base, typ| {
        exp(ast::ExpKind::Slice(Box::new(base), Box::new(int(2)), Box::new(int(-3))), typ)
    };
    for exp_slice in [
        slice(list(vec![int(1), int(2), int(3)]), typ::make::list(typ::make::int())),
        slice(text("abc"), typ::make::text()),
    ] {
        let error = eval(exp_slice).unwrap_err().into_report().render();
        assert!(error.contains("slice [2, -1) out of bounds [0, 3)"), "{error}");
    }
}

#[test]
fn test_text_operations_use_byte_lengths_and_reject_split_utf8() {
    let length = exp(ast::ExpKind::Len(Box::new(text("é"))), typ::make::nat());
    let (arena, value_eval_1) = eval(length).unwrap();
    assert_eq!(num::to_int(get::num(&arena, &value_eval_1).unwrap()).to_string(), "2");
    let slice = exp(
        ast::ExpKind::Slice(Box::new(text("éa")), Box::new(int(0)), Box::new(int(2))),
        typ::make::text(),
    );
    let (arena, value_eval_2) = eval(slice).unwrap();
    assert_eq!(get::text(&arena, &value_eval_2).unwrap(), "é");
    let index = exp(ast::ExpKind::Idx(Box::new(text("é")), Box::new(int(0))), typ::make::text());
    assert!(
        eval(index)
            .unwrap_err()
            .into_report()
            .render()
            .contains("UTF-8 boundaries")
    );
}

#[test]
fn test_structural_equality_ignores_nested_spans_and_type_notes() {
    let left = list(vec![int(3)]);
    let mut right = list(vec![int(3)]);
    if let ast::ExpKind::List(exps) = &mut right.node {
        exps[0].span = Span {
            left: Position { file: "other".into(), line: 4, column: 2 },
            right: Position { file: "other".into(), line: 4, column: 3 },
        };
    }
    let equal = exp(
        ast::ExpKind::Cmp(
            ast::CmpOp::Bool(boolean::CmpOp::Eq),
            ast::OpTyp::Int,
            Box::new(left.clone()),
            Box::new(right.clone()),
        ),
        typ::make::bool(),
    );
    let (arena, value_eval_1) = eval(equal).unwrap();
    assert!(get::bool(&arena, &value_eval_1).unwrap());
    let membership =
        exp(ast::ExpKind::Mem(Box::new(left), Box::new(list(vec![right]))), typ::make::bool());
    let (arena, value_eval_2) = eval(membership).unwrap();
    assert!(get::bool(&arena, &value_eval_2).unwrap());
}

#[test]
fn test_recursive_casts_and_subtype_checks() {
    let nat = typ::make::nat();
    let cast = exp(
        ast::ExpKind::DownCast(
            Box::new(typ::make::tuple(vec![nat.clone(), typ::make::list(nat.clone())])),
            Box::new(exp(
                ast::ExpKind::Tuple(vec![int(3), list(vec![int(4)])]),
                typ::make::tuple(vec![typ::make::int(), typ::make::list(typ::make::int())]),
            )),
        ),
        typ::make::tuple(vec![nat.clone(), typ::make::list(nat.clone())]),
    );
    let (arena, value) = eval(cast).unwrap();
    let values = get::tuple(&arena, &value).unwrap();
    assert!(matches!(get::num(&arena, &values[0]).unwrap(), num::Number::Nat(_)));
    assert!(matches!(
        get::num(&arena, &get::list(&arena, &values[1]).unwrap()[0]).unwrap(),
        num::Number::Nat(_)
    ));
    let check = exp(
        ast::ExpKind::Sub(
            Box::new(int(-1)),
            Box::new(nat.clone()),
            Box::new(ast::Subcheck::Recurse(nat)),
        ),
        typ::make::bool(),
    );
    let (arena, value_eval_2) = eval(check).unwrap();
    assert!(!get::bool(&arena, &value_eval_2).unwrap());
}

#[test]
fn test_case_and_container_patterns() {
    let atom = p4spec_rust::phrase!(node: Atom::Keyword("SomeCase".into()), span: Span::default());
    let case = Mixfix::Seq(vec![Mixfix::Atom(atom), Mixfix::Arg(int(5))]);
    let pattern = ast::Pattern::Case(Box::new(case.to_mixop()));
    let case = exp(ast::ExpKind::Case(Box::new(case)), typ::make::var(id("case"), vec![]));
    let cases = [
        (case, pattern),
        (list(vec![int(1)]), ast::Pattern::List(ListPattern::Cons)),
        (list(vec![int(1), int(2)]), ast::Pattern::List(ListPattern::Fixed(2))),
        (list(vec![]), ast::Pattern::List(ListPattern::Nil)),
        (
            exp(ast::ExpKind::Opt(Some(Box::new(int(1)))), typ::make::opt(typ::make::int())),
            ast::Pattern::Opt(OptPattern::Some),
        ),
        (
            exp(ast::ExpKind::Opt(None), typ::make::opt(typ::make::int())),
            ast::Pattern::Opt(OptPattern::None),
        ),
    ];
    for (value, pattern) in cases {
        let matches = exp(ast::ExpKind::Match(Box::new(value), pattern), typ::make::bool());
        let (arena, value_eval_1) = eval(matches).unwrap();
        assert!(get::bool(&arena, &value_eval_1).unwrap());
    }
}

struct RecordingInterface(Rc<RefCell<Vec<String>>>);

impl Interface for RecordingInterface {
    fn call_builtin(
        &mut self,
        arena: &mut ValueArena,
        id: &ast::Id,
        _targs: &[ast::Typ],
        _values: &[Value],
    ) -> Result<(Value, bool), InterfaceError> {
        self.0.borrow_mut().push(id.node.clone());
        Ok((make::bool(arena, id.node == "right", Span::default()).unwrap(), true))
    }

    fn clear(&mut self) {
        self.0.borrow_mut().clear();
    }
}

#[test]
fn test_boolean_operators_evaluate_both_operands_in_order() {
    let call = |name| exp(ast::ExpKind::Call(id(name), vec![], vec![]), typ::make::bool());
    let expression = exp(
        ast::ExpKind::Bin(
            ast::BinOp::Bool(boolean::BinOp::And),
            ast::OpTyp::Bool,
            Box::new(call("left")),
            Box::new(call("right")),
        ),
        typ::make::bool(),
    );
    let mut defs = vec![function("test", expression)];
    for name in ["left", "right"] {
        defs.push(p4spec_rust::phrase!(node: ast::DefKind::MetaFunc(ast::MetaFuncDef::Builtin(ast::BuiltinFunc { id: id(name), tparams: vec![], params: vec![], typ: typ::make::bool(), hints: vec![] })), span: Span::default()));
    }
    let calls = Rc::new(RefCell::new(vec![]));
    let mut runner = Runner::<AlInterp, _, _>::new(
        Global::load(defs).unwrap(),
        AlInterp::new(Config::new(false, false, false)),
        RecordingInterface(calls.clone()),
        NullExtern,
    );
    assert!(
        !{
            let value = &runner.context().call_func("test", &[], &[]).unwrap();
            get::bool(runner.arena(), value)
        }
        .unwrap()
    );
    assert_eq!(*calls.borrow(), ["left", "right"]);
}

#[test]
fn test_numeric_errors_are_fatal_before_else_fallback() {
    let divide = exp(
        ast::ExpKind::Bin(
            ast::BinOp::Num(num::BinOp::Div),
            ast::OpTyp::Int,
            Box::new(int(1)),
            Box::new(int(0)),
        ),
        typ::make::int(),
    );
    let mut def = function("test", divide);
    if let ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func)) = &mut def.node {
        func.else_clause = Some(
            p4spec_rust::phrase!(node: ast::ClauseKind { args: vec![], exp: int(42), prems: vec![] }, span: Span::default()),
        );
    }
    let mut runner = Runner::<AlInterp, _, _>::new(
        Global::load(vec![def]).unwrap(),
        AlInterp::new(Config::new(false, false, false)),
        NullInterface,
        NullExtern,
    );
    assert!(
        runner
            .context()
            .call_func("test", &[], &[])
            .unwrap_err()
            .into_report()
            .render()
            .contains("zero divisor")
    );
}

#[test]
fn test_iteration_evaluates_each_bound_element_and_preserves_empty_options() {
    for iter in [ast::Iter::List, ast::Iter::Opt] {
        let typ_int = typ::make::int();
        let typ_iter = typ::make::iter(typ_int.clone(), iter);
        let var = ast::Var { id: id("x"), typ: typ_int.clone(), iters: vec![] };
        let exp_id = p4spec_rust::note_phrase!(node: p4spec_rust::lang::il::ast::ExpKind::Id(id("x")), note: typ_int.node.clone(), span: typ_int.span.clone());
        let signature = exp(
            ast::ExpKind::Iter(
                Box::new(exp_id.clone()),
                ast::ExpIter { iter, vars: vec![var.clone()] },
            ),
            typ_iter.clone(),
        );
        let add = exp(
            ast::ExpKind::Bin(
                ast::BinOp::Num(num::BinOp::Add),
                ast::OpTyp::Int,
                Box::new(exp_id),
                Box::new(int(10)),
            ),
            typ_int,
        );
        let expression = exp(
            ast::ExpKind::Iter(Box::new(add), ast::ExpIter { iter, vars: vec![var] }),
            typ_iter.clone(),
        );
        let mut def = function("test", expression);
        if let ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func)) = &mut def.node {
            func.params = vec![
                p4spec_rust::phrase!(node: ast::ParamKind::Exp(typ_iter.clone()), span: Span::default()),
            ];
            func.clauses[0].node.args = vec![
                p4spec_rust::phrase!(node: ast::ArgKind::Exp(Box::new(signature)), span: Span::default()),
            ];
        }
        let mut runner = Runner::<AlInterp, _, _>::new(
            Global::load(vec![def]).unwrap(),
            AlInterp::new(Config::new(false, false, false)),
            NullInterface,
            NullExtern,
        );
        let input = make::int(runner.arena_mut(), 2.into(), Span::default()).unwrap();
        match iter {
            ast::Iter::List => {
                let input = {
                    let values = vec![
                        input,
                        make::int(runner.arena_mut(), 4.into(), Span::default()).unwrap(),
                    ];
                    make::list(
                        runner.arena_mut(),
                        typ_iter.node.clone().into(),
                        values,
                        Span::default(),
                    )
                    .unwrap()
                };
                assert_eq!(
                    {
                        let value = runner.context().call_func("test", &[], &[input]).unwrap();
                        numbers(runner.arena(), &value)
                    },
                    ["12", "14"]
                );
            }
            ast::Iter::Opt => {
                let input = make::opt(
                    runner.arena_mut(),
                    typ_iter.node.clone().into(),
                    Some(input),
                    Span::default(),
                )
                .unwrap();
                let value = runner.context().call_func("test", &[], &[input]).unwrap();
                let value_inner = get::opt(runner.arena(), &value).unwrap().unwrap();
                assert_eq!(
                    num::to_int(get::num(runner.arena(), &value_inner).unwrap()).to_string(),
                    "12"
                );
                let input = make::opt(
                    runner.arena_mut(),
                    typ_iter.node.clone().into(),
                    None,
                    Span::default(),
                )
                .unwrap();
                assert!(
                    {
                        let value = &runner.context().call_func("test", &[], &[input]).unwrap();
                        get::opt(runner.arena(), value)
                    }
                    .unwrap()
                    .is_none()
                );
            }
        }
    }
}

#[test]
fn test_list_iteration_zips_values_without_rebinding_the_parent() {
    let typ_int = typ::make::int();
    let typ_list = typ::make::list(typ_int.clone());
    let vars: Vec<_> = ["x", "y"]
        .into_iter()
        .map(|name| ast::Var { id: id(name), typ: typ_int.clone(), iters: vec![] })
        .collect();
    let mut signatures: Vec<_> = vars
        .iter()
        .map(|var| {
            exp(
                ast::ExpKind::Iter(
                    Box::new(p4spec_rust::note_phrase!(node: p4spec_rust::lang::il::ast::ExpKind::Id(var.id.clone()), note: typ_int.node.clone(), span: typ_int.span.clone())),
                    ast::ExpIter { iter: ast::Iter::List, vars: vec![var.clone()] },
                ),
                typ_list.clone(),
            )
        })
        .collect();
    signatures.push(p4spec_rust::note_phrase!(node: p4spec_rust::lang::il::ast::ExpKind::Id(id("x")), note: typ_int.node.clone(), span: Span::default()));
    let exp_inner = exp(
        ast::ExpKind::Bin(
            ast::BinOp::Num(num::BinOp::Add),
            ast::OpTyp::Int,
            Box::new(
                p4spec_rust::note_phrase!(node: p4spec_rust::lang::il::ast::ExpKind::Id(id("x")), note: typ_int.node.clone(), span: Span::default()),
            ),
            Box::new(
                p4spec_rust::note_phrase!(node: p4spec_rust::lang::il::ast::ExpKind::Id(id("y")), note: typ_int.node.clone(), span: Span::default()),
            ),
        ),
        typ_int.clone(),
    );
    let expression = exp(
        ast::ExpKind::Tuple(vec![
            exp(
                ast::ExpKind::Iter(
                    Box::new(exp_inner),
                    ast::ExpIter { iter: ast::Iter::List, vars },
                ),
                typ_list.clone(),
            ),
            p4spec_rust::note_phrase!(node: p4spec_rust::lang::il::ast::ExpKind::Id(id("x")), note: typ_int.node.clone(), span: Span::default()),
        ]),
        typ::make::tuple(vec![typ_list.clone(), typ_int.clone()]),
    );
    let mut def = function("test", expression);
    if let ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func)) = &mut def.node {
        func.params = [typ_list.clone(), typ_list.clone(), typ_int]
            .into_iter()
            .map(|typ| p4spec_rust::phrase!(node: ast::ParamKind::Exp(typ), span: Span::default()))
            .collect();
        func.clauses[0].node.args = signatures.into_iter()
            .map(|exp| p4spec_rust::phrase!(node: ast::ArgKind::Exp(Box::new(exp)), span: Span::default()))
            .collect();
    }
    let mut runner = Runner::<AlInterp, _, _>::new(
        Global::load(vec![def]).unwrap(),
        AlInterp::new(Config::new(false, false, false)),
        NullInterface,
        NullExtern,
    );
    for width in [3, 0] {
        let mut inputs = Vec::new();
        for factor in [1, 10] {
            let values = (1..=width)
                .map(|value| {
                    make::int(runner.arena_mut(), (value * factor).into(), Span::default()).unwrap()
                })
                .collect();
            inputs.push(
                make::list(
                    runner.arena_mut(),
                    typ_list.node.clone().into(),
                    values,
                    Span::default(),
                )
                .unwrap(),
            );
        }
        let value_parent = make::int(runner.arena_mut(), 99.into(), Span::default()).unwrap();
        inputs.push(value_parent);
        let value = runner.context().call_func("test", &[], &inputs).unwrap();
        let values = get::tuple(runner.arena(), &value).unwrap();
        let expected: Vec<_> = (1..=width).map(|value| (value * 11).to_string()).collect();
        assert_eq!(numbers(runner.arena(), &values[0]), expected);
        assert_eq!(values[1], value_parent);
    }
}

struct TypeInterface(Rc<RefCell<Vec<ast::Typ>>>);

impl Interface for TypeInterface {
    fn call_builtin(
        &mut self,
        _arena: &mut ValueArena,
        _id: &ast::Id,
        targs: &[ast::Typ],
        values: &[Value],
    ) -> Result<(Value, bool), InterfaceError> {
        self.0.borrow_mut().extend_from_slice(targs);
        Ok((values[0], true))
    }

    fn clear(&mut self) {
        self.0.borrow_mut().clear();
    }
}

#[test]
fn test_call_arguments_substitute_local_types_and_pass_function_values() {
    let typ_t = typ::make::var(id("T"), vec![]);
    let typ_func = typ::make::func(vec![], vec![], typ::make::int());
    let call = exp(
        ast::ExpKind::Call(
            id("capture"),
            vec![typ_t],
            vec![
                p4spec_rust::phrase!(node: ast::ArgKind::Def(id("answer")), span: Span::default()),
            ],
        ),
        typ_func.clone(),
    );
    let mut outer = function("test", call);
    if let ast::DefKind::MetaFunc(ast::MetaFuncDef::Defined(func)) = &mut outer.node {
        func.tparams = vec![id("T")];
    }
    let capture = p4spec_rust::phrase!(node: ast::DefKind::MetaFunc(ast::MetaFuncDef::Builtin(ast::BuiltinFunc {
        id: id("capture"), tparams: vec![id("U")],
        params: vec![p4spec_rust::phrase!(node: ast::ParamKind::Def(id("f"), vec![], vec![], typ::make::int()), span: Span::default())],
        typ: typ_func, hints: vec![],
    })), span: Span::default());
    let seen = Rc::new(RefCell::new(vec![]));
    let mut runner = Runner::<AlInterp, _, _>::new(
        Global::load(vec![outer, capture, function("answer", int(42))]).unwrap(),
        AlInterp::new(Config::new(false, false, false)),
        TypeInterface(seen.clone()),
        NullExtern,
    );
    let value = runner
        .context()
        .call_func("test", &[typ::make::nat()], &[])
        .unwrap();
    assert_eq!(
        (match (runner.arena()).kind(&value) {
            p4spec_rust::lang::data::value::ValueKind::Func(id) => id,
            _ => panic!("expected function"),
        })
        .node,
        "answer"
    );
    assert!(matches!(seen.borrow()[0].node, ast::TypKind::Num(num::Typ::Nat)));
}

#[test]
fn test_index_failures_retain_the_index_expression_span() {
    fn contains_span(traces: &[p4spec_rust::diagnostic::Report], span: &Span) -> bool {
        traces
            .iter()
            .any(|trace| &trace.span() == span || contains_span(&trace.children, span))
    }
    fn contains_evaluation(traces: &[p4spec_rust::diagnostic::Report]) -> bool {
        traces.iter().any(|trace| {
            matches!(&trace.kind, ReportKind::Frame { message, .. } if message.starts_with("while evaluating "))
                || contains_evaluation(&trace.children)
        })
    }
    let mut index = int(3);
    index.span =
        Span::new(Position::new("index.watsup", 7, 8), Position::new("index.watsup", 7, 9));
    let span = index.span.clone();
    let expression =
        exp(ast::ExpKind::Idx(Box::new(list(vec![int(1)])), Box::new(index)), typ::make::int());
    let error = eval(expression).unwrap_err().into_report();
    let ReportKind::Frame { .. } = error.kind else {
        panic!("expected execution traces");
    };
    assert!(contains_span(&error.children, &span));
    assert!(contains_evaluation(&error.children));
}

#[test]
fn test_native_nested_update_and_list_destructuring() {
    use p4spec_rust::pass::{algo, elaborate};

    let source = r#"
var n : nat
var m : nat
var nss : nat**
dec $update(nat**) : nat**
def $update(nss) = nss[[0][1] = 9]
dec $sum_pair(nat*) : nat
def $sum_pair([n, m]) = n + m
dec $updated() : nat**
def $updated() = $update([[1, 2], [3, 4]])
dec $destructure() : nat
def $destructure() = $sum_pair(($updated())[0])
"#;
    let spec_el = crate::spec_fixture::parse(source).unwrap();
    let spec_il = elaborate::convert(spec_el).unwrap();
    let spec_al = algo::convert(spec_il).unwrap();
    let mut runner = Runner::<AlInterp, _, _>::new(
        Global::load(spec_al).unwrap(),
        AlInterp::new(Config::new(false, false, false)),
        NullInterface,
        NullExtern,
    );
    let value = runner.context().call_func("updated", &[], &[]).unwrap();
    let rows = get::list(runner.arena(), &value).unwrap();
    assert_eq!(numbers(runner.arena(), &rows[0]), ["1", "9"]);
    assert_eq!(numbers(runner.arena(), &rows[1]), ["3", "4"]);
    let value = runner.context().call_func("destructure", &[], &[]).unwrap();
    assert_eq!(num::to_int(get::num(runner.arena(), &value).unwrap()).to_string(), "10");
}

#[test]
fn test_generated_values_have_default_spans_and_access_preserves_input_spans() {
    use p4spec_rust::pass::{algo, elaborate};

    let source = r#"
var n : nat
var ns : nat*
dec $literal() : nat
def $literal() = 5
dec $increment(nat) : nat
def $increment(n) = n + 1
dec $identity(nat) : nat
def $identity(n) = n
dec $first(nat*) : nat
def $first(ns) = ns[0]
"#;
    let spec_el = crate::spec_fixture::parse(source).unwrap();
    let spec_il = elaborate::convert(spec_el).unwrap();
    let spec_al = algo::convert(spec_il).unwrap();
    let mut runner = Runner::<AlInterp, _, _>::new(
        Global::load(spec_al).unwrap(),
        AlInterp::new(Config::new(false, false, false)),
        NullInterface,
        NullExtern,
    );
    let span = Span::new(Position::new("input.watsup", 3, 4), Position::new("input.watsup", 3, 5));
    let input = make::nat(runner.arena_mut(), 7u64.into(), span.clone()).unwrap();
    let literal = runner.context().call_func("literal", &[], &[]).unwrap();
    assert_eq!(runner.arena().span(&literal).clone(), Span::default());
    let increment = runner
        .context()
        .call_func("increment", &[], std::slice::from_ref(&input))
        .unwrap();
    assert_eq!(runner.arena().span(&increment).clone(), Span::default());
    let identity = runner
        .context()
        .call_func("identity", &[], std::slice::from_ref(&input))
        .unwrap();
    assert_eq!(runner.arena().span(&identity).clone(), span);
    assert!((identity == input));
    let inputs = make::list(
        runner.arena_mut(),
        (typ::make::list(typ::make::nat())).node.clone().into(),
        vec![input],
        Span::default(),
    )
    .unwrap();
    let first = runner.context().call_func("first", &[], &[inputs]).unwrap();
    assert_eq!(runner.arena().span(&first).clone(), span);
    assert!((first == input));
}

#[test]
fn test_builtin_failure_remains_typed_in_public_error_tree() {
    let builtin = p4spec_rust::phrase!(node: ast::DefKind::MetaFunc(ast::MetaFuncDef::Builtin(ast::BuiltinFunc {
        id: id("missing_builtin"), tparams: vec![], params: vec![],
        typ: typ::make::int(), hints: vec![],
    })), span: Span::default());
    let call = exp(ast::ExpKind::Call(id("missing_builtin"), vec![], vec![]), typ::make::int());
    let mut runner = Runner::<AlInterp, _, _>::new(
        Global::load(vec![function("test", call), builtin]).unwrap(),
        AlInterp::new(Config::new(false, false, false)),
        p4spec_rust::interface::p4(&p4spec_rust::runner::Spec::Al(Vec::new())),
        NullExtern,
    );
    let error = runner
        .context()
        .call_func("test", &[], &[])
        .unwrap_err()
        .into_report();
    let diagnostic = error
        .find_code("runtime/builtin-implementation-missing")
        .expect("builtin cause")
        .diagnostic();
    assert!(diagnostic.message.contains("missing_builtin"));
}
