use std::rc::Rc;

use p4spec_rust::{
    lang::{
        common::{
            Iter,
            source::{Position, Span},
        },
        data::{
            value::{ValueArena, make},
            var::Var,
        },
    },
    phrase,
    runtime::envs::interp::shared::frame::{Frame, FrameLayout},
};

fn variable(name: &str, iters: Vec<Iter>, line: usize) -> Var {
    let span = Span::new(Position::new("slots", line, 0), Position::new("slots", line, 1));
    Var {
        id: phrase!(node: name.to_owned(), span: span),
        typ: p4spec_rust::lang::data::typ::make::bool(),
        iters,
    }
}

#[test]
fn slots_ignore_spans_but_distinguish_iterator_paths() {
    let mut layout = FrameLayout::default();
    let slot = layout.resolve_var(variable("x", vec![], 1));
    let slot_same = layout.resolve_var(variable("x", vec![], 9));
    let slot_list = layout.resolve_var(variable("x", vec![Iter::List], 1));
    let slot_opt = layout.resolve_var(variable("x", vec![Iter::Opt], 1));
    assert_eq!(slot.slot, slot_same.slot);
    assert_ne!(slot.slot, slot_list.slot);
    assert_ne!(slot_list.slot, slot_opt.slot);
    let slot_list_opt = layout.resolve_var(variable("x", vec![Iter::List, Iter::Opt], 1));
    let slot_opt_list = layout.resolve_var(variable("x", vec![Iter::Opt, Iter::List], 1));
    let slot_list_list = layout.resolve_var(variable("x", vec![Iter::List, Iter::List], 1));
    let slots = [&slot, &slot_list, &slot_opt, &slot_list_opt, &slot_opt_list, &slot_list_list];
    for (idx, slot_a) in slots.iter().enumerate() {
        for slot_b in &slots[idx + 1..] {
            assert_ne!(slot_a.slot, slot_b.slot);
        }
    }
    assert_eq!(layout.len(), slots.len());
    assert_eq!(slot_same.var.id.span.left.line, 9);
    let mut arena = ValueArena::new();
    let value = make::bool(&mut arena, true, Span::default()).unwrap();
    let mut frame = Frame::new(Rc::new(layout));
    assert!(frame.get(slot.slot).is_none());
    frame.set(slot.slot, value);
    assert_eq!(frame.get(slot_same.slot), Some(&value));
    for slot in &slots[1..] {
        assert!(frame.get(slot.slot).is_none());
    }
}

#[test]
fn branch_overwrites_and_new_bindings_leave_the_parent_and_sibling_unchanged() {
    let mut layout = FrameLayout::default();
    let slot_x = layout.resolve_var(variable("x", vec![], 1));
    let slot_y = layout.resolve_var(variable("y", vec![], 1));
    let mut arena = ValueArena::new();
    let value_true = make::bool(&mut arena, true, Span::default()).unwrap();
    let value_false = make::bool(&mut arena, false, Span::default()).unwrap();
    let mut frame = Frame::new(Rc::new(layout));
    frame.set(slot_x.slot, value_true);
    let mut frame_branch = frame.clone();
    let frame_sibling = frame.clone();
    frame_branch.set(slot_x.slot, value_false);
    frame_branch.set(slot_y.slot, value_true);
    assert_eq!(frame.get(slot_x.slot), Some(&value_true));
    assert_eq!(frame_sibling.get(slot_x.slot), Some(&value_true));
    assert!(frame.get(slot_y.slot).is_none());
    assert!(frame_sibling.get(slot_y.slot).is_none());
    assert_eq!(frame_branch.get(slot_x.slot), Some(&value_false));
    assert_eq!(frame_branch.get(slot_y.slot), Some(&value_true));
    let frame_empty = frame_branch.wipe();
    assert!(frame_empty.get(slot_x.slot).is_none());
    assert!(frame_empty.get(slot_y.slot).is_none());
    assert_eq!(frame_branch.get(slot_x.slot), Some(&value_false));
}

#[test]
fn independent_callable_layouts_bind_their_own_slots() {
    let mut layout_a = FrameLayout::default();
    let slot_x_a = layout_a.resolve_var(variable("x", vec![], 1));
    let mut layout_b = FrameLayout::default();
    let slot_y_b = layout_b.resolve_var(variable("y", vec![], 1));
    let slot_x_b = layout_b.resolve_var(variable("x", vec![], 1));
    let mut arena = ValueArena::new();
    let value_true = make::bool(&mut arena, true, Span::default()).unwrap();
    let value_false = make::bool(&mut arena, false, Span::default()).unwrap();
    let mut frame_a = Frame::new(Rc::new(layout_a));
    let mut frame_b = Frame::new(Rc::new(layout_b));
    frame_a.set(slot_x_a.slot, value_true);
    frame_b.set(slot_y_b.slot, value_true);
    frame_b.set(slot_x_b.slot, value_false);
    assert_eq!(frame_a.get(slot_x_a.slot), Some(&value_true));
    assert_eq!(frame_b.get(slot_x_b.slot), Some(&value_false));
    assert_eq!(frame_b.get(slot_y_b.slot), Some(&value_true));
}

#[test]
fn slots_share_identity_without_replacing_occurrence_types() {
    let var_a = variable("x", vec![Iter::List], 1);
    let mut var_b = variable("x", vec![Iter::List], 9);
    var_b.typ = p4spec_rust::lang::data::typ::make::nat();
    let mut layout = FrameLayout::default();
    let slot_a = layout.resolve_var(var_a.clone());
    let slot_b = layout.resolve_var(var_b.clone());
    assert_eq!(layout.len(), 1);
    assert_eq!(slot_a.slot, slot_b.slot);
    assert_eq!(slot_a.var, var_a);
    assert_eq!(slot_b.var, var_b);
    let mut var_outer_a = var_a;
    var_outer_a.iters.push(Iter::Opt);
    let slot_outer_a = layout.resolve_var(var_outer_a);
    let slot_outer_b = layout.find_var_slot_iterated(&slot_b, Iter::Opt);
    let mut var_outer_b = var_b;
    var_outer_b.iters.push(Iter::Opt);
    assert_eq!(layout.len(), 2);
    assert_eq!(slot_outer_a.slot, slot_outer_b.slot);
    assert_eq!(slot_outer_b.var, var_outer_b);
}
