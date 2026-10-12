//! Merge adjacent OL bindings with the same inputs and compatible outputs
//!
//! `collapse_bind` matches Let right-hand sides or relation ids and inputs,
//! then maps the later output names onto the earlier ones:
//!
//! ```text
//! let x = source { return x }; let y = source { return y }
//!
//! becomes
//!
//! let x = source { return x; return x }
//! ```
//!
//! Constructors and iterator metadata must match;
//! variable pairs build a renaming.
//! `downstream_block` checks only the next sibling:
//! for `let x = source`,
//! a following `let y = source { return y }` yields `{ return x }`.
//! `upstream_block` merges that body into the current Let and retries,
//! then rewrites the merged body; an intervening instruction stops merging.
//! A relation's input hint selects inputs and outputs.

use std::collections::VecDeque;

use crate::lang::{
    hints::input,
    traits::{eq::SyntaxEq, free::FreeIds},
};

use crate::lang::il::ast::{ExpField, ExpKind};

use crate::pass::structure::{ol::ast::*, opt::merge::merge_block, re::renamer::Renamer};

// == Bindings

/// An expression with only the iterator variables it uses.
///
/// `x` under `(x, y)*` keeps just `x*`.
struct ExpUnit<'a> {
    exp: &'a Exp,
    iter_exps: Vec<ExpIter>,
}

impl<'a> ExpUnit<'a> {
    fn new(exp: &'a Exp, iter_exps: &[ExpIter]) -> Self {
        // Keep only the iterator variables the expression mentions
        let ids = exp.free_ids();
        let iter_exps = iter_exps
            .iter()
            .map(|ExpIter { iter, vars }| {
                let vars = vars
                    .iter()
                    .filter(|var| ids.contains(&var.id))
                    .cloned()
                    .collect();
                ExpIter { iter: *iter, vars }
            })
            .collect();
        Self { exp, iter_exps }
    }
}

impl SyntaxEq for ExpUnit<'_> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.exp.syntax_eq(other.exp) && self.iter_exps.syntax_eq(&other.iter_exps)
    }
}

/// A binding instruction seen as its input and output units.
enum Bind<'a> {
    /// `let pattern = source`: the pattern and the source.
    Let(ExpUnit<'a>, ExpUnit<'a>),
    /// A relation call: its id, input units, and output units.
    Rule(&'a Id, Vec<ExpUnit<'a>>, Vec<ExpUnit<'a>>),
}

impl<'a> Bind<'a> {
    /// Views a let as its pattern and source units.
    fn from_let(instr_let: &'a LetInstr) -> Self {
        let LetInstr { exp_l, exp_r, iter_instrs, .. } = instr_let;
        let (iter_exps_bound, iter_exps_bind): (Vec<_>, Vec<_>) = iter_instrs
            .iter()
            .map(|iter_instr| {
                let exp_iter_bound =
                    ExpIter { iter: iter_instr.iter, vars: iter_instr.vars_bound.clone() };
                let exp_iter_bind =
                    ExpIter { iter: iter_instr.iter, vars: iter_instr.vars_bind.clone() };
                (exp_iter_bound, exp_iter_bind)
            })
            .unzip();
        // The pattern takes the binding iterators, the source the bound ones
        let expunit_l = ExpUnit::new(exp_l, &iter_exps_bind);
        let expunit_r = ExpUnit::new(exp_r, &iter_exps_bound);
        Self::Let(expunit_l, expunit_r)
    }

    /// Views a rule call, splitting its arguments by the input hint.
    fn from_rule(instr_rule: &'a RuleInstr) -> Self {
        let RuleInstr { id, not_exp, input_hint, iter_instrs, .. } = instr_rule;
        let exps = not_exp.args().iter().collect();
        // Elaboration validates hints; OL rewrites preserve notation arity
        let (exps_input, exps_output) =
            input::split(input_hint, exps).expect("validated relation hints and argument counts");
        let (iter_exps_bound, iter_exps_bind): (Vec<_>, Vec<_>) = iter_instrs
            .iter()
            .map(|iter_instr| {
                let exp_iter_bound =
                    ExpIter { iter: iter_instr.iter, vars: iter_instr.vars_bound.clone() };
                let exp_iter_bind =
                    ExpIter { iter: iter_instr.iter, vars: iter_instr.vars_bind.clone() };
                (exp_iter_bound, exp_iter_bind)
            })
            .unzip();
        // Inputs use the source iterators, outputs the binding ones
        let expunits_input = exps_input
            .into_iter()
            .map(|exp| ExpUnit::new(exp, &iter_exps_bound))
            .collect();
        let expunits_output = exps_output
            .into_iter()
            .map(|exp| ExpUnit::new(exp, &iter_exps_bind))
            .collect();
        Self::Rule(id, expunits_input, expunits_output)
    }
}

// == Binding comparison

/// Finds how to rename a later binding's outputs to the current binding's.
///
/// Let sources must match; rule calls need the same id and inputs,
/// including the input iterators, before the output patterns are compared.
///
/// ```text
/// current: let (x, y) = source
/// later:   let (a, b) = source
/// result:  Some({a -> x, b -> y})
/// ```
///
/// Different inputs, pattern shapes, or output iterators yield `None`.
/// This only builds a `Renamer`;
/// downstream renames the body and upstream merges it.
fn collapse_bind(bind: &Bind<'_>, bind_target: &Bind<'_>) -> Option<Renamer> {
    match (bind, bind_target) {
        // Lets need the same source; rules the same id and inputs
        (Bind::Let(expunit_l, expunit_r), Bind::Let(expunit_target_l, expunit_target_r))
            if expunit_r.syntax_eq(expunit_target_r) =>
        {
            collapse_expunit(Renamer::empty(), expunit_l, expunit_target_l)
        }
        (
            Bind::Rule(id, expunits_input, expunits_output),
            Bind::Rule(id_target, expunits_target_input, expunits_target_output),
        ) if id.syntax_eq(id_target) && expunits_input.syntax_eq(expunits_target_input) => {
            collapse_expunits(Renamer::empty(), expunits_output, expunits_target_output)
        }
        _ => None,
    }
}

/// Compares an output pattern, then its iterators under the resulting renaming.
///
/// For `x` with `x*` and `y` with `y*`, `y -> x` makes the iterators agree;
/// `x*` and `y?` still disagree after renaming, so the binding cannot merge.
fn collapse_expunit(
    renamer: Renamer,
    expunit: &ExpUnit<'_>,
    expunit_target: &ExpUnit<'_>,
) -> Option<Renamer> {
    let renamer = collapse_exp(renamer, expunit.exp, expunit_target.exp)?;
    let iter_exps_target = renamer.rename_iterexps(&mut false, expunit_target.iter_exps.clone());
    expunit
        .iter_exps
        .syntax_eq(&iter_exps_target)
        .then_some(renamer)
}

fn collapse_expunits(
    mut renamer: Renamer,
    expunits: &[ExpUnit<'_>],
    expunits_target: &[ExpUnit<'_>],
) -> Option<Renamer> {
    if expunits.len() != expunits_target.len() {
        return None;
    }
    for (expunit, expunit_target) in expunits.iter().zip(expunits_target) {
        renamer = collapse_expunit(renamer, expunit, expunit_target)?;
    }
    Some(renamer)
}

// - Expressions

/// Walks both output patterns together, collecting target-to-current pairs.
///
/// `(x, [y])` against `(a, [b])` records `a -> x` and `b -> y`;
/// unequal shapes fail.
/// Only the pattern forms below participate; even equal literals yield `None`.
/// Repeated target names follow traversal order:
/// `(x, y)` against `(a, a)` leaves `a -> y`.
fn collapse_exp(mut renamer: Renamer, exp: &Exp, exp_target: &Exp) -> Option<Renamer> {
    match (&exp.node, &exp_target.node) {
        (ExpKind::Id(id), ExpKind::Id(id_target)) => {
            // Matching x against y records y -> x for the later body
            if !id.syntax_eq(id_target) {
                renamer.add(id_target.clone(), id.clone());
            }
            Some(renamer)
        }
        // Structured patterns collapse componentwise
        (ExpKind::Tuple(exps), ExpKind::Tuple(exps_target))
        | (ExpKind::List(exps), ExpKind::List(exps_target)) => {
            let exps = exps.iter().collect();
            let exps_target = exps_target.iter().collect();
            collapse_exps(renamer, exps, exps_target)
        }
        (ExpKind::Case(not_exp), ExpKind::Case(not_exp_target)) => {
            collapse_case_exp(renamer, not_exp, not_exp_target)
        }
        (ExpKind::Str(exp_fields), ExpKind::Str(exp_fields_target)) => {
            collapse_str_exp(renamer, exp_fields, exp_fields_target)
        }
        (ExpKind::Opt(exp), ExpKind::Opt(exp_target)) => match (exp, exp_target) {
            (Some(exp), Some(exp_target)) => collapse_exp(renamer, exp, exp_target),
            (None, None) => Some(renamer),
            _ => None,
        },
        (ExpKind::Cons(exp_head, exp_tail), ExpKind::Cons(exp_head_target, exp_tail_target)) => {
            let renamer = collapse_exp(renamer, exp_head, exp_head_target)?;
            collapse_exp(renamer, exp_tail, exp_tail_target)
        }
        (ExpKind::Iter(exp, exp_iter), ExpKind::Iter(exp_target, exp_iter_target)) => {
            collapse_iter_exp(renamer, exp, exp_iter, exp_target, exp_iter_target)
        }
        // Literals and other forms never collapse
        _ => None,
    }
}

fn collapse_exps(mut renamer: Renamer, exps: Vec<&Exp>, exps_target: Vec<&Exp>) -> Option<Renamer> {
    if exps.len() != exps_target.len() {
        return None;
    }
    for (exp, exp_target) in exps.into_iter().zip(exps_target) {
        renamer = collapse_exp(renamer, exp, exp_target)?;
    }
    Some(renamer)
}

// - Case expression

/// Collapses the arguments of two cases with the same mixfix.
fn collapse_case_exp(
    renamer: Renamer,
    not_exp: &NotExp,
    not_exp_target: &NotExp,
) -> Option<Renamer> {
    if !not_exp.eq_mixop(not_exp_target) {
        return None;
    }
    let exps = not_exp.args().iter().collect();
    let exps_target = not_exp_target.args().iter().collect();
    collapse_exps(renamer, exps, exps_target)
}

// - Record expression

/// Collapses the fields of two structs with the same atoms.
fn collapse_str_exp(
    renamer: Renamer,
    exp_fields: &[ExpField],
    exp_fields_target: &[ExpField],
) -> Option<Renamer> {
    // Field atoms must agree pairwise
    if exp_fields.len() != exp_fields_target.len()
        || !exp_fields.iter().zip(exp_fields_target).all(
            |(ExpField { atom, .. }, ExpField { atom: atom_target, .. })| {
                atom.syntax_eq(atom_target)
            },
        )
    {
        return None;
    }
    let exps = exp_fields.iter().map(|ExpField { exp, .. }| exp).collect();
    let exps_target = exp_fields_target
        .iter()
        .map(|ExpField { exp, .. }| exp)
        .collect();
    collapse_exps(renamer, exps, exps_target)
}

// - Iterated expression

/// Collapses iterated patterns; the iterators must agree after renaming.
fn collapse_iter_exp(
    renamer: Renamer,
    exp: &Exp,
    exp_iter: &ExpIter,
    exp_target: &Exp,
    exp_iter_target: &ExpIter,
) -> Option<Renamer> {
    // x* against y* must also have equal iterators after renaming y -> x
    let renamer = collapse_exp(renamer, exp, exp_target)?;
    let exp_iter_target = renamer.rename_iterexp(&mut false, exp_iter_target.clone());
    exp_iter.syntax_eq(&exp_iter_target).then_some(renamer)
}

// == Downstream search

/// Takes the next sibling's body, renamed to this binding, if it collapses.
///
/// ```text
/// current binding:    let x = source
/// following siblings: [let y = source { return y }; return z]
///
/// returned body:      Some([return x])
/// remaining siblings: [return z]
/// ```
///
/// Upstream merges the returned body into the current binding's body.
/// A sibling that cannot merge stays in place and `None` is returned.
fn downstream_block(bind: &Bind<'_>, instrs_tail: &mut VecDeque<Instr>) -> Option<Block> {
    let instr_head = instrs_tail.front_mut()?;
    let block_merge = match &mut instr_head.node {
        InstrKind::Let(instr_let) => downstream_let_instr(bind, instr_let),
        InstrKind::Rule(instr_rule) => downstream_rule_instr(bind, instr_rule),
        _ => None,
    };
    if block_merge.is_some() {
        instrs_tail.pop_front();
    }
    block_merge
}

// - Let instruction

/// Takes a let's body, renamed to this binding's names, if it collapses.
fn downstream_let_instr(bind: &Bind<'_>, instr_let: &mut LetInstr) -> Option<Block> {
    let bind_target = Bind::from_let(instr_let);
    let renamer = collapse_bind(bind, &bind_target)?;
    let LetInstr { block, .. } = instr_let;
    let block = std::mem::take(block);
    let block = renamer.rename_block(&mut false, block);
    Some(block)
}

// - Rule instruction

/// Takes a rule call's body, renamed to this binding's names, if it collapses.
fn downstream_rule_instr(bind: &Bind<'_>, instr_rule: &mut RuleInstr) -> Option<Block> {
    let bind_target = Bind::from_rule(instr_rule);
    let renamer = collapse_bind(bind, &bind_target)?;
    let RuleInstr { block, .. } = instr_rule;
    let block = std::mem::take(block);
    let block = renamer.rename_block(&mut false, block);
    Some(block)
}

// == Upstream rewriting

/// Merges matching siblings into a binding, then rewrites its body.
fn upstream_instr_kind(
    changed: &mut bool,
    instrs_tail: &mut VecDeque<Instr>,
    instr_kind: InstrKind,
) -> InstrKind {
    match instr_kind {
        InstrKind::If(instr) => upstream_if_instr(changed, instr),
        InstrKind::Hold(instr) => upstream_hold_instr(changed, instr),
        InstrKind::Case(instr) => upstream_case_instr(changed, instr),
        InstrKind::Group(instr) => upstream_group_instr(changed, instr),
        InstrKind::Let(instr) => upstream_let_instr(changed, instrs_tail, instr),
        InstrKind::Rule(instr) => upstream_rule_instr(changed, instrs_tail, instr),
        instr_kind => instr_kind,
    }
}

/// Rewrites a block, letting each binding consume its following siblings.
fn upstream_block(changed: &mut bool, block: Block) -> Block {
    let mut instrs_tail: VecDeque<_> = block.into();
    let mut block = Vec::with_capacity(instrs_tail.len());
    while let Some(instr) = instrs_tail.pop_front() {
        let instr_kind = upstream_instr_kind(changed, &mut instrs_tail, instr.node);
        let instr = crate::phrase!(node: instr_kind, span: instr.span);
        block.push(instr);
    }
    block
}

// - If instruction

fn upstream_if_instr(changed: &mut bool, instr: IfInstr) -> InstrKind {
    let IfInstr { exp, iter_exps, block } = instr;
    let block = upstream_block(changed, block);
    let instr = IfInstr { exp, iter_exps, block };
    InstrKind::If(instr)
}

// - Hold instruction

fn upstream_hold_instr(changed: &mut bool, instr: HoldInstr) -> InstrKind {
    let HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold } = instr;
    let block_hold = upstream_block(changed, block_hold);
    let block_not_hold = upstream_block(changed, block_not_hold);
    let instr = HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold };
    InstrKind::Hold(instr)
}

// - Case instruction

fn upstream_case_instr(changed: &mut bool, instr: CaseInstr) -> InstrKind {
    let CaseInstr { exp, cases, total } = instr;
    let cases = cases
        .into_iter()
        .map(|case| {
            let Case { guard, block } = case;
            let block = upstream_block(changed, block);
            Case { guard, block }
        })
        .collect();
    let instr = CaseInstr { exp, cases, total };
    InstrKind::Case(instr)
}

// - Group instruction

fn upstream_group_instr(changed: &mut bool, instr: GroupInstr) -> InstrKind {
    let GroupInstr { id, rel_signature, exps, block } = instr;
    let block = upstream_block(changed, block);
    let instr = GroupInstr { id, rel_signature, exps, block };
    InstrKind::Group(instr)
}

// - Let instruction

/// Absorbs following lets with the same source, then rewrites the merged body.
fn upstream_let_instr(
    changed: &mut bool,
    instrs_tail: &mut VecDeque<Instr>,
    mut instr: LetInstr,
) -> InstrKind {
    // Keep absorbing while the next sibling collapses
    loop {
        let bind = Bind::from_let(&instr);
        let Some(block_merge) = downstream_block(&bind, instrs_tail) else {
            break;
        };
        *changed = true;
        instr.block = merge_block(instr.block, block_merge);
    }
    let LetInstr { exp_l, exp_r, iter_instrs, block } = instr;
    let block = upstream_block(changed, block);
    let instr = LetInstr { exp_l, exp_r, iter_instrs, block };
    InstrKind::Let(instr)
}

// - Rule instruction

/// Absorbs following calls with the same inputs, then rewrites the merged body.
fn upstream_rule_instr(
    changed: &mut bool,
    instrs_tail: &mut VecDeque<Instr>,
    mut instr: RuleInstr,
) -> InstrKind {
    // Keep absorbing while the next sibling collapses
    loop {
        let bind = Bind::from_rule(&instr);
        let Some(block_merge) = downstream_block(&bind, instrs_tail) else {
            break;
        };
        *changed = true;
        instr.block = merge_block(instr.block, block_merge);
    }
    let RuleInstr { id, not_exp, input_hint, iter_instrs, block } = instr;
    let block = upstream_block(changed, block);
    let instr = RuleInstr { id, not_exp, input_hint, iter_instrs, block };
    InstrKind::Rule(instr)
}

// == Entry point

/// Merges bindings throughout the block, flagging `changed` on any merge.
pub(crate) fn apply(changed: &mut bool, block: Block) -> Block {
    upstream_block(changed, block)
}
