//! Remove unused Let instructions
//!
//! `downstream_block({x}, block)` reports uses of x in the supplied block:
//!
//! ```text
//! [return y] -> {}
//! [return x] -> {x}
//! ```
//!
//! `upstream_let_instr` uses that result
//! to drop `let x = 1` around the first body and keep it around the second,
//! then recursively rewrites the body.
//! `let x = f() { return y }` stays because its right-hand side is a call.
//! Rule instructions also stay even when their outputs are unused.

use crate::lang::{
    common::{ds::set::IdSet, source::Span},
    hints::input,
    traits::{free::FreeIds, has_call::HasCall},
};

use crate::pass::structure::ol::ast::*;

// == Downstream uses

/// Reports which of `ids_defined` an instruction uses.
///
/// For `ids_defined = {x}`, `return (x, y)` contributes `{x}`.
fn downstream_instr(ids_defined: &IdSet, instr_ol: &Instr) -> IdSet {
    downstream_instr_kind(ids_defined, &instr_ol.node)
}

fn downstream_instr_kind(ids_defined: &IdSet, instr_kind_ol: &InstrKind) -> IdSet {
    match instr_kind_ol {
        InstrKind::If(instr_ol) => downstream_if_instr(ids_defined, instr_ol),
        InstrKind::Hold(instr_ol) => downstream_hold_instr(ids_defined, instr_ol),
        InstrKind::Case(instr_ol) => downstream_case_instr(ids_defined, instr_ol),
        InstrKind::Group(instr_ol) => downstream_group_instr(ids_defined, instr_ol),
        InstrKind::Let(instr_ol) => downstream_let_instr(ids_defined, instr_ol),
        InstrKind::Rule(instr_ol) => downstream_rule_instr(ids_defined, instr_ol),
        InstrKind::Result(instr_ol) => instr_ol.exps.free_ids().intersection(ids_defined),
        InstrKind::Return(instr_ol) => instr_ol.exp.free_ids().intersection(ids_defined),
        InstrKind::Debug(instr_ol) => downstream_debug_instr(ids_defined, instr_ol),
    }
}

/// Reports which of `ids_defined` a block uses before rebinding them.
///
/// With `ids_defined = {x}`, `[let x = 2 {}; return x]` yields `{}`,
/// but `[let x = 2 { return x }]` yields `{x}`: the body is inspected first.
/// Let bindings and rule outputs are excluded only from later instructions.
fn downstream_block(ids_defined: &IdSet, block: &Block) -> IdSet {
    let mut ids_defined = ids_defined.clone();
    let mut ids_used = IdSet::new();
    for instr_ol in block {
        let ids_used_instr = downstream_instr(&ids_defined, instr_ol);
        ids_used.append(ids_used_instr);
        // Count this instruction's uses before excluding its new definitions
        ids_defined = match &instr_ol.node {
            InstrKind::Let(instr_ol) => {
                let ids_bound = instr_ol.exp_l.free_ids();
                ids_defined.difference(&ids_bound)
            }
            InstrKind::Rule(instr_rule) => {
                let exps = instr_rule.not_exp.args().iter().collect();
                // Elaboration validates hints; OL rewrites preserve notation arity
                let (_, exps_output) = input::split(&instr_rule.input_hint, exps)
                    .expect("validated relation hints and argument counts");
                let mut ids_output = IdSet::new();
                for exp in exps_output {
                    exp.free_ids_into(&mut ids_output);
                }
                ids_defined.difference(&ids_output)
            }
            _ => ids_defined,
        };
    }
    ids_used
}

// - If instruction

fn downstream_if_instr(ids_defined: &IdSet, instr_ol: &IfInstr) -> IdSet {
    let IfInstr { exp, block, .. } = instr_ol;
    let ids_used = exp.free_ids().intersection(ids_defined);
    let ids_used_then = downstream_block(ids_defined, block);
    ids_used.union(ids_used_then)
}

// - Hold instruction

fn downstream_hold_instr(ids_defined: &IdSet, instr_ol: &HoldInstr) -> IdSet {
    let HoldInstr { not_exp, block_hold, block_not_hold, .. } = instr_ol;
    let ids_used = not_exp.free_ids().intersection(ids_defined);
    let ids_used_hold = downstream_block(ids_defined, block_hold);
    let ids_used_not_hold = downstream_block(ids_defined, block_not_hold);
    ids_used.union(ids_used_hold).union(ids_used_not_hold)
}

// - Case instruction

fn downstream_case_instr(ids_defined: &IdSet, instr_ol: &CaseInstr) -> IdSet {
    let CaseInstr { exp, cases, .. } = instr_ol;
    let mut ids_used = exp.free_ids().intersection(ids_defined);
    for case in cases {
        let Case { guard, block } = case;
        let ids_used_guard = guard.free_ids().intersection(ids_defined);
        ids_used.append(ids_used_guard);
        let ids_used_block = downstream_block(ids_defined, block);
        ids_used.append(ids_used_block);
    }
    ids_used
}

// - Group instruction

fn downstream_group_instr(ids_defined: &IdSet, instr_ol: &GroupInstr) -> IdSet {
    let GroupInstr { exps, block, .. } = instr_ol;
    let ids_used = exps.free_ids().intersection(ids_defined);
    let ids_used_block = downstream_block(ids_defined, block);
    ids_used.union(ids_used_block)
}

// - Let instruction

fn downstream_let_instr(ids_defined: &IdSet, instr_ol: &LetInstr) -> IdSet {
    let LetInstr { exp_r, block, .. } = instr_ol;
    let ids_used = exp_r.free_ids().intersection(ids_defined);
    let ids_used_block = downstream_block(ids_defined, block);
    ids_used.union(ids_used_block)
}

// - Rule instruction

fn downstream_rule_instr(ids_defined: &IdSet, instr_ol: &RuleInstr) -> IdSet {
    let RuleInstr { not_exp, input_hint, block, .. } = instr_ol;
    let exps = not_exp.args().iter().collect();
    // Elaboration validates hints; OL rewrites preserve notation arity
    let (exps_input, _) =
        input::split(input_hint, exps).expect("validated relation hints and argument counts");
    let mut ids_input = IdSet::new();
    for exp in exps_input {
        exp.free_ids_into(&mut ids_input);
    }
    let ids_used = ids_input.intersection(ids_defined);
    let ids_used_block = downstream_block(ids_defined, block);
    ids_used.union(ids_used_block)
}

// - Debug instruction

fn downstream_debug_instr(ids_defined: &IdSet, instr_ol: &DebugInstr) -> IdSet {
    let DebugInstr { exp, instr } = instr_ol;
    let ids_used = exp.free_ids().intersection(ids_defined);
    let ids_used_instr = downstream_instr(ids_defined, instr);
    ids_used.union(ids_used_instr)
}

// == Upstream binding removal

/// Rewrites an instruction, dropping removable lets whose bindings go unused.
fn upstream_instr(instr_ol: Instr) -> Block {
    upstream_instr_kind(instr_ol.node, instr_ol.span)
}

fn upstream_instr_kind(instr_kind_ol: InstrKind, span: Span) -> Block {
    match instr_kind_ol {
        InstrKind::If(instr_ol) => upstream_if_instr(instr_ol, span),
        InstrKind::Hold(instr_ol) => upstream_hold_instr(instr_ol, span),
        InstrKind::Case(instr_ol) => upstream_case_instr(instr_ol, span),
        InstrKind::Group(instr_ol) => upstream_group_instr(instr_ol, span),
        InstrKind::Let(instr_ol) => upstream_let_instr(instr_ol, span),
        InstrKind::Rule(instr_ol) => upstream_rule_instr(instr_ol, span),
        InstrKind::Result(_) | InstrKind::Return(_) | InstrKind::Debug(_) => {
            let instr = crate::phrase!(node: instr_kind_ol, span: span);
            vec![instr]
        }
    }
}

fn upstream_block(block: Block) -> Block {
    let mut block_rewritten = Vec::new();
    for instr_ol in block {
        let block = upstream_instr(instr_ol);
        block_rewritten.extend(block);
    }
    block_rewritten
}

// - If instruction

fn upstream_if_instr(instr_ol: IfInstr, span: Span) -> Block {
    let IfInstr { exp, iter_exps, block } = instr_ol;
    let block = upstream_block(block);
    let instr = IfInstr { exp, iter_exps, block };
    let instr = crate::phrase!(node: InstrKind::If(instr), span: span);
    vec![instr]
}

// - Hold instruction

fn upstream_hold_instr(instr_ol: HoldInstr, span: Span) -> Block {
    let HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold } = instr_ol;
    let block_hold = upstream_block(block_hold);
    let block_not_hold = upstream_block(block_not_hold);
    let instr = HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold };
    let instr = crate::phrase!(node: InstrKind::Hold(instr), span: span);
    vec![instr]
}

// - Case instruction

fn upstream_case_instr(instr_ol: CaseInstr, span: Span) -> Block {
    let CaseInstr { exp, cases, total } = instr_ol;
    let cases = cases
        .into_iter()
        .map(|case| {
            let Case { guard, block } = case;
            let block = upstream_block(block);
            Case { guard, block }
        })
        .collect();
    let instr = CaseInstr { exp, cases, total };
    let instr = crate::phrase!(node: InstrKind::Case(instr), span: span);
    vec![instr]
}

// - Group instruction

fn upstream_group_instr(instr_ol: GroupInstr, span: Span) -> Block {
    let GroupInstr { id, rel_signature, exps, block } = instr_ol;
    let block = upstream_block(block);
    let instr = GroupInstr { id, rel_signature, exps, block };
    let instr = crate::phrase!(node: InstrKind::Group(instr), span: span);
    vec![instr]
}

// - Let instruction

/// Drops the let when its pattern is unused in the body.
fn upstream_let_instr(instr_ol: LetInstr, span: Span) -> Block {
    let LetInstr { exp_l, exp_r, iter_instrs, block } = instr_ol;
    // Inspect uses before deleting inner Lets: `let x = 1 { let y = (x,) {} }`
    // becomes `let x = 1 {}`; the outer Let is not revisited in this pass
    if !exp_r.has_call() {
        let ids_defined = exp_l.free_ids();
        let ids_used = downstream_block(&ids_defined, &block);
        if ids_used.is_empty() {
            return upstream_block(block);
        }
    }
    let block = upstream_block(block);
    let instr = LetInstr { exp_l, exp_r, iter_instrs, block };
    let instr = crate::phrase!(node: InstrKind::Let(instr), span: span);
    vec![instr]
}

// - Rule instruction

fn upstream_rule_instr(instr_ol: RuleInstr, span: Span) -> Block {
    let RuleInstr { id, not_exp, input_hint, iter_instrs, block } = instr_ol;
    let block = upstream_block(block);
    let instr = RuleInstr { id, not_exp, input_hint, iter_instrs, block };
    let instr = crate::phrase!(node: InstrKind::Rule(instr), span: span);
    vec![instr]
}

// == Entry point

/// Removes dead lets throughout the block.
pub(crate) fn apply(block: Block) -> Block {
    upstream_block(block)
}
