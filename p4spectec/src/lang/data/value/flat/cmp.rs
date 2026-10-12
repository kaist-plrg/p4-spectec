//! Comparison of flat values
//!
//! Canonical equality combines body contents and child identities.
//! `ValueRef` requires both values to belong to the same arena.
//! Equality uses canonical ids; ordering compares contents structurally.

use std::cmp::Ordering;

use crate::lang::{
    common::prim::num,
    data::{
        intern::{CanonEq, CanonInterner},
        notation::MixopArena,
    },
    traits::{cmp::SyntaxCmp, eq::SyntaxEq},
};

use super::{Value, ValueField, ValueKind, ValueRef};

// = Canonical equality

impl CanonEq<MixopArena> for ValueKind {
    fn canon_eq(
        &self,
        interner: &CanonInterner<Self>,
        arena_mixop: &MixopArena,
        kind_r: &Self,
    ) -> bool {
        // Children compare by canonical id, computed when they were interned
        let eq_value = |value_l: &Value, value_r: &Value| {
            interner.canon_id(value_l.node) == interner.canon_id(value_r.node)
        };
        match (self, kind_r) {
            (ValueKind::Bool(value_l), ValueKind::Bool(value_r)) => value_l == value_r,
            (ValueKind::Num(value_l), ValueKind::Num(value_r)) => value_l == value_r,
            (ValueKind::Text(value_l), ValueKind::Text(value_r)) => value_l == value_r,
            (ValueKind::Struct(value_fields_l), ValueKind::Struct(value_fields_r)) => {
                value_fields_l.len() == value_fields_r.len()
                    && value_fields_l.iter().zip(value_fields_r).all(
                        |(
                            ValueField { atom: atom_l, value: value_l },
                            ValueField { atom: atom_r, value: value_r },
                        )| {
                            atom_l.node == atom_r.node && eq_value(value_l, value_r)
                        },
                    )
            }
            (ValueKind::Case(value_case_l), ValueKind::Case(value_case_r)) => {
                arena_mixop.canon_id(*value_case_l.mixop())
                    == arena_mixop.canon_id(*value_case_r.mixop())
                    && value_case_l.args().len() == value_case_r.args().len()
                    && value_case_l
                        .args()
                        .iter()
                        .zip(value_case_r.args())
                        .all(|(value_l, value_r)| eq_value(value_l, value_r))
            }
            (ValueKind::Tuple(values_l), ValueKind::Tuple(values_r))
            | (ValueKind::List(values_l), ValueKind::List(values_r)) => {
                values_l.len() == values_r.len()
                    && values_l
                        .iter()
                        .zip(values_r)
                        .all(|(value_l, value_r)| eq_value(value_l, value_r))
            }
            (ValueKind::Opt(value_l), ValueKind::Opt(value_r)) => match (value_l, value_r) {
                (Some(value_l), Some(value_r)) => eq_value(value_l, value_r),
                (None, None) => true,
                _ => false,
            },
            (ValueKind::Func(id_l), ValueKind::Func(id_r)) => id_l.node == id_r.node,
            (ValueKind::Extern(json_l), ValueKind::Extern(json_r)) => json_l == json_r,
            _ => false,
        }
    }
}

// = Syntax comparison

impl SyntaxEq for ValueRef<'_> {
    fn syntax_eq(&self, value_other: &Self) -> bool {
        assert!(
            std::ptr::eq(self.arena, value_other.arena),
            "values must belong to the same arena"
        );
        self.arena.value.canon_id(self.value.node)
            == self.arena.value.canon_id(value_other.value.node)
    }
}

impl SyntaxCmp for ValueRef<'_> {
    fn syntax_cmp(&self, value_other: &Self) -> Ordering {
        assert!(
            std::ptr::eq(self.arena, value_other.arena),
            "values must belong to the same arena"
        );
        // Compare child contents in the same arena
        let compare_value = |value_l: &Value, value_r: &Value| {
            value_l
                .view(self.arena)
                .syntax_cmp(&value_r.view(self.arena))
        };
        let compare_values = |values_l: &[Value], values_r: &[Value]| {
            values_l
                .iter()
                .zip(values_r)
                .map(|(value_l, value_r)| compare_value(value_l, value_r))
                .find(|order| !order.is_eq())
                .unwrap_or_else(|| values_l.len().cmp(&values_r.len()))
        };
        let kind_l = self.arena.value.kind(self.value.node);
        let kind_r = self.arena.value.kind(value_other.value.node);
        match (kind_l, kind_r) {
            (ValueKind::Bool(value_l), ValueKind::Bool(value_r)) => value_l.cmp(value_r),
            (ValueKind::Num(value_l), ValueKind::Num(value_r)) => num::compare(value_l, value_r),
            (ValueKind::Text(value_l), ValueKind::Text(value_r)) => value_l.cmp(value_r),
            (ValueKind::Struct(value_fields_l), ValueKind::Struct(value_fields_r)) => {
                value_fields_l
                    .iter()
                    .zip(value_fields_r)
                    .map(
                        |(
                            ValueField { atom: atom_l, value: value_l },
                            ValueField { atom: atom_r, value: value_r },
                        )| {
                            atom_l
                                .node
                                .cmp(&atom_r.node)
                                .then_with(|| compare_value(value_l, value_r))
                        },
                    )
                    .find(|order| !order.is_eq())
                    .unwrap_or_else(|| value_fields_l.len().cmp(&value_fields_r.len()))
            }
            (ValueKind::Case(value_case_l), ValueKind::Case(value_case_r)) => {
                value_case_l.cmp_by(&self.arena.mixop, value_case_r, compare_value)
            }
            (ValueKind::Tuple(values_l), ValueKind::Tuple(values_r))
            | (ValueKind::List(values_l), ValueKind::List(values_r)) => {
                compare_values(values_l, values_r)
            }
            (ValueKind::Opt(value_l), ValueKind::Opt(value_r)) => match (value_l, value_r) {
                (Some(value_l), Some(value_r)) => compare_value(value_l, value_r),
                (None, Some(_)) => Ordering::Less,
                (Some(_), None) => Ordering::Greater,
                (None, None) => Ordering::Equal,
            },
            (ValueKind::Func(id_l), ValueKind::Func(id_r)) => id_l.node.cmp(&id_r.node),
            (ValueKind::Extern(json_l), ValueKind::Extern(json_r)) => {
                crate::util::json::compare(json_l, json_r)
            }
            // Different kinds order by tag
            _ => kind_l.tag().cmp(&kind_r.tag()),
        }
    }
}
