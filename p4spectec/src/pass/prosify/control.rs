//! Normalize singleton cases, classify conditions, and stamp failure destinations
//!
//! `classify_spec` turns singleton cases into conditions
//! and walks blocks backwards, tracking what recovers from a failure
//!
//! For a definition body without an otherwise clause:
//!
//! ```text
//! If p { Return a }
//! If q { Return $f(b) }
//! ```
//!
//! becomes
//!
//! ```text
//! If p { Return a }
//! Check q { Return $f(b) [FAIL] }
//! ```
//!
//! The first condition can fail into the next instruction
//! The last condition must hold because no continuation remains
//! Later alternative arms and otherwise clauses also provide continuations
//! Other arms of a selected `Case` do not
//!
//! A `Fallthrough` note marks where control goes
//! when an instruction fails and no following instruction recovers:
//! a mismatch from a relation premise or function call skips every following instruction,
//! and a condition fails past the end of its alternative when nothing follows it
//! Table matching blocks are left unchanged

use crate::lang::{common::prim, pl::ast as pl, pl::rule_group, traits::has_call::HasCall};

// == Continuations

/// What recovers from a failure at one instruction.
#[derive(Clone)]
struct Continuation {
    /// A following instruction recovers from a failed condition.
    follows: bool,
    /// Where a mismatch goes, and a failed condition when nothing follows.
    fallthrough: pl::Fallthrough,
    /// The first rule group of the next route arm, for rule-group bodies.
    id_group_next: Option<pl::Id>,
}

impl Continuation {
    /// Starts a definition body that falls through to its otherwise clause or fails.
    fn of_body(has_else: bool) -> Self {
        let fallthrough = if has_else { pl::Fallthrough::Else } else { pl::Fallthrough::Fail };
        Continuation { follows: false, fallthrough, id_group_next: None }
    }

    /// Reports whether any instruction or alternative recovers from a failure.
    fn has_continuation(&self) -> bool {
        self.follows || self.fallthrough != pl::Fallthrough::Fail
    }

    /// Continues a block after one of its instructions.
    fn followed(&self) -> Self {
        Continuation { follows: true, ..self.clone() }
    }

    /// Enters a rule-group body, naming the group that a next route arm tries first.
    fn enter_rule_group(&self) -> Self {
        let fallthrough = match (&self.fallthrough, &self.id_group_next) {
            (pl::Fallthrough::Next, Some(id_group)) => pl::Fallthrough::Group(id_group.clone()),
            (pl::Fallthrough::Next, None) => unreachable!("route arms name their next group"),
            (fallthrough, _) => fallthrough.clone(),
        };
        Continuation { follows: self.follows, fallthrough, id_group_next: None }
    }

    // - Failure destinations

    /// Stamps a destination when evaluating `syntax` may mismatch.
    fn on_mismatch<T: HasCall + ?Sized>(&self, syntax: &T) -> Option<pl::Fallthrough> {
        may_mismatch(syntax).then(|| self.fallthrough.clone())
    }

    /// Stamps a destination when a failed condition leaves its alternative.
    fn on_condition(&self) -> Option<pl::Fallthrough> {
        (!self.follows).then(|| self.fallthrough.clone())
    }
}

/// Reports whether evaluating syntax may mismatch through a function call.
///
/// Every call counts as partial: a function whose clauses all fail mismatches,
/// and the mismatch abandons the caller's alternative.
fn may_mismatch<T: HasCall + ?Sized>(syntax: &T) -> bool {
    syntax.has_call()
}

// == Guards

/// Converts a nonbinding guard into a boolean predicate.
fn exp_of_guard(exp_scrut: pl::Exp, guard: pl::Guard) -> pl::Exp {
    let span = exp_scrut.node.span;
    let kind = match guard {
        pl::Guard::Bool(true) => return exp_scrut,
        pl::Guard::Bool(false) => {
            let exp_false = crate::annotated_note_phrase! {
                node: pl::ExpKind::Bool(false), note: pl::TypKind::Bool, span: span,
            };
            pl::ExpKind::Cmp(
                pl::CmpOp::Bool(prim::bool::CmpOp::Eq),
                pl::OpTyp::Bool,
                Box::new(exp_scrut),
                Box::new(exp_false),
            )
        }
        pl::Guard::Cmp(op, op_typ, exp_r) => {
            pl::ExpKind::Cmp(op, op_typ, Box::new(exp_scrut), Box::new(exp_r))
        }
        pl::Guard::Sub(typ) => pl::ExpKind::Sub(Box::new(exp_scrut), typ),
        pl::Guard::Match(pattern) => pl::ExpKind::Match(Box::new(exp_scrut), pattern),
        pl::Guard::LetSub(..) | pl::Guard::LetMatch(..) => {
            unreachable!("conditional bindings are introduced after classification")
        }
        pl::Guard::Mem(exp_list) => pl::ExpKind::Mem(Box::new(exp_scrut), Box::new(exp_list)),
    };
    crate::annotated_note_phrase! { node: kind, note: pl::TypKind::Bool, span: span }
}

// == Instructions

/// Classifies a tier instruction and stamps its own failure destination.
type ClassifyTier<Tier> = fn(&Continuation, Tier) -> (Tier, Option<pl::Fallthrough>);

/// A classified instruction with its own failure destination.
type Classified<Tier> = (pl::InstrKind<Tier>, Option<pl::Fallthrough>);

fn classify_instr<Tier>(
    cont: &Continuation,
    classify_tier: ClassifyTier<Tier>,
    mut instr: pl::Instr<Tier>,
) -> pl::Instr<Tier> {
    let (instr_kind, fallthrough) = classify_instr_kind(cont, classify_tier, instr.node.node);
    instr.node.node = instr_kind;
    instr.node.note = fallthrough;
    instr
}

fn classify_instr_kind<Tier>(
    cont: &Continuation,
    classify_tier: ClassifyTier<Tier>,
    instr_kind: pl::InstrKind<Tier>,
) -> Classified<Tier> {
    match instr_kind {
        pl::InstrKind::IfLetSub(_) | pl::InstrKind::IfLetMatch(_) => {
            unreachable!("conditional bindings are introduced after classification")
        }
        pl::InstrKind::If(instr) => classify_if_instr(cont, classify_tier, instr),
        pl::InstrKind::Check(instr) => classify_check_instr(cont, classify_tier, instr),
        pl::InstrKind::Hold(instr) => classify_hold_instr(cont, classify_tier, instr),
        pl::InstrKind::Case(instr) => classify_case_instr(cont, classify_tier, instr),
        pl::InstrKind::Let(instr) => classify_let_instr(cont, instr),
        pl::InstrKind::Debug(instr) => classify_debug_instr(cont, instr),
        pl::InstrKind::Destruct(instr) => classify_destruct_instr(cont, instr),
        pl::InstrKind::Tier(instr) => classify_tier_instr(cont, classify_tier, instr),
    }
}

// - If instruction

/// Classifies the body and requires the condition when no continuation remains.
///
/// A required condition fails the definition, as its keyword says,
/// so only a mismatch in it is stamped.
fn classify_if_instr<Tier>(
    cont: &Continuation,
    classify_tier: ClassifyTier<Tier>,
    mut instr_if: pl::IfInstr<Tier>,
) -> Classified<Tier> {
    instr_if.block = classify_block(cont, classify_tier, instr_if.block);
    // A failed condition continues when anything recovers
    if cont.has_continuation() {
        let fallthrough = cont.on_mismatch(&instr_if.exp).or(cont.on_condition());
        return (pl::InstrKind::If(instr_if), fallthrough);
    }
    // Otherwise the condition is required and fails the definition
    let fallthrough = cont.on_mismatch(&instr_if.exp);
    let instr_check =
        pl::CheckInstr { exp: instr_if.exp, iter_exps: instr_if.iter_exps, block: instr_if.block };
    (pl::InstrKind::Check(instr_check), fallthrough)
}

// - Check instruction

fn classify_check_instr<Tier>(
    cont: &Continuation,
    classify_tier: ClassifyTier<Tier>,
    mut instr_check: pl::CheckInstr<Tier>,
) -> Classified<Tier> {
    instr_check.block = classify_block(cont, classify_tier, instr_check.block);
    let fallthrough = cont.on_mismatch(&instr_check.exp);
    (pl::InstrKind::Check(instr_check), fallthrough)
}

// - Hold instruction

/// Classifies the branches; a one-sided hold fails like a condition.
fn classify_hold_instr<Tier>(
    cont: &Continuation,
    classify_tier: ClassifyTier<Tier>,
    mut instr_hold: pl::HoldInstr<Tier>,
) -> Classified<Tier> {
    instr_hold.hold_case = classify_hold_case(cont, classify_tier, instr_hold.hold_case);
    // Only a missing branch lets the relation's outcome fall through
    let fallthrough_condition = match &instr_hold.hold_case {
        pl::HoldCase::Both(..) => None,
        pl::HoldCase::Hold(..) | pl::HoldCase::NotHold(..) => cont.on_condition(),
    };
    let fallthrough = cont
        .on_mismatch(instr_hold.not_exp.args())
        .or(fallthrough_condition);
    (pl::InstrKind::Hold(instr_hold), fallthrough)
}

// - Case instruction

/// Normalizes singleton guards and classifies each selected case body.
///
/// The instruction's note covers its scrutinee;
/// each arm's note covers its guard, and the last arm of a partial case
/// also covers the failure of every guard.
fn classify_case_instr<Tier>(
    cont: &Continuation,
    classify_tier: ClassifyTier<Tier>,
    mut instr_case: pl::CaseInstr<Tier>,
) -> Classified<Tier> {
    // Normalize singleton guards before classifying their body once
    if instr_case.cases.len() == 1 {
        let case = instr_case.cases.pop().expect("singleton case");
        let instr_if = pl::IfInstr {
            exp: exp_of_guard(instr_case.exp, case.guard),
            iter_exps: vec![],
            block: case.block,
            dangle: instr_case.dangle,
        };
        return classify_if_instr(cont, classify_tier, instr_if);
    }
    // Case selection commits to its first matching guard
    let idx_last = instr_case.cases.len() - 1;
    let partial = instr_case.totality == pl::Totality::Partial;
    instr_case.cases = instr_case
        .cases
        .into_iter()
        .enumerate()
        .map(|(idx, case)| classify_case(cont, classify_tier, partial && idx == idx_last, case))
        .collect();
    let fallthrough = cont.on_mismatch(&instr_case.exp);
    (pl::InstrKind::Case(instr_case), fallthrough)
}

// - Let instruction

fn classify_let_instr<Tier>(cont: &Continuation, instr_let: pl::LetInstr) -> Classified<Tier> {
    let fallthrough = cont.on_mismatch(&instr_let.exp_r);
    (pl::InstrKind::Let(instr_let), fallthrough)
}

// - Debug instruction

fn classify_debug_instr<Tier>(
    cont: &Continuation,
    instr_debug: pl::DebugInstr,
) -> Classified<Tier> {
    let fallthrough = cont.on_mismatch(&instr_debug.exp);
    (pl::InstrKind::Debug(instr_debug), fallthrough)
}

// - Destruct instruction

fn classify_destruct_instr<Tier>(
    cont: &Continuation,
    instr_destruct: pl::DestructInstr,
) -> Classified<Tier> {
    let fallthrough = cont.on_mismatch(&instr_destruct.exp);
    (pl::InstrKind::Destruct(instr_destruct), fallthrough)
}

// - Tier instruction

fn classify_tier_instr<Tier>(
    cont: &Continuation,
    classify_tier: ClassifyTier<Tier>,
    instr_tier: pl::TierInstr<Tier>,
) -> Classified<Tier> {
    let (tier, fallthrough) = classify_tier(cont, instr_tier.tier);
    (pl::InstrKind::Tier(pl::TierInstr { tier }), fallthrough)
}

// == Holding conditions

fn classify_hold_case<Tier>(
    cont: &Continuation,
    classify_tier: ClassifyTier<Tier>,
    hold_case: pl::HoldCase<Tier>,
) -> pl::HoldCase<Tier> {
    match hold_case {
        pl::HoldCase::Both(block_hold, block_not_hold) => pl::HoldCase::Both(
            classify_block(cont, classify_tier, block_hold),
            classify_block(cont, classify_tier, block_not_hold),
        ),
        pl::HoldCase::Hold(block, dangle) => {
            pl::HoldCase::Hold(classify_block(cont, classify_tier, block), dangle)
        }
        pl::HoldCase::NotHold(block, dangle) => {
            pl::HoldCase::NotHold(classify_block(cont, classify_tier, block), dangle)
        }
    }
}

// == Cases

/// Classifies an arm body and stamps a failing guard, or every guard failing.
fn classify_case<Tier>(
    cont: &Continuation,
    classify_tier: ClassifyTier<Tier>,
    is_last_partial: bool,
    mut case: pl::Case<Tier>,
) -> pl::Case<Tier> {
    case.block = classify_block(cont, classify_tier, case.block);
    let fallthrough_condition = if is_last_partial { cont.on_condition() } else { None };
    case.fallthrough = cont.on_mismatch(&case.guard).or(fallthrough_condition);
    case
}

// == Group instructions

fn classify_group_tier(
    cont: &Continuation,
    tier: pl::GroupInstr,
) -> (pl::GroupInstr, Option<pl::Fallthrough>) {
    match tier {
        pl::GroupInstr::Backtrack(instr) => (classify_backtrack_instr(cont, instr), None),
        // A relation premise mismatches when the relation does not apply
        pl::GroupInstr::Rule(instr) => {
            let fallthrough = Some(cont.fallthrough.clone());
            (pl::GroupInstr::Rule(instr), fallthrough)
        }
        pl::GroupInstr::Result(instr) => {
            let fallthrough = cont.on_mismatch(instr.exps_output.as_slice());
            (pl::GroupInstr::Result(instr), fallthrough)
        }
        pl::GroupInstr::Return(instr) => {
            let fallthrough = cont.on_mismatch(&instr.exp);
            (pl::GroupInstr::Return(instr), fallthrough)
        }
    }
}

// - Backtrack instruction

fn classify_backtrack_instr(
    cont: &Continuation,
    mut instr_backtrack: pl::BacktrackInstr,
) -> pl::GroupInstr {
    instr_backtrack.blocks = classify_blocks(cont, classify_group_tier, instr_backtrack.blocks);
    pl::GroupInstr::Backtrack(instr_backtrack)
}

// == Dispatch instructions

fn classify_dispatch_tier(
    cont: &Continuation,
    tier: pl::DispatchInstr,
) -> (pl::DispatchInstr, Option<pl::Fallthrough>) {
    match tier {
        pl::DispatchInstr::Group(instr) => (classify_rule_group_instr(cont, instr), None),
        pl::DispatchInstr::Route(instr) => (classify_route_instr(cont, instr), None),
    }
}

// - Rule group instruction

fn classify_rule_group_instr(
    cont: &Continuation,
    mut instr_group: pl::RuleGroupInstr,
) -> pl::DispatchInstr {
    let cont_group = cont.enter_rule_group();
    instr_group.block = classify_group_block(&cont_group, instr_group.block);
    pl::DispatchInstr::Group(instr_group)
}

// - Route instruction

fn classify_route_instr(cont: &Continuation, mut instr_route: pl::RouteInstr) -> pl::DispatchInstr {
    instr_route.blocks = classify_blocks(cont, classify_dispatch_tier, instr_route.blocks);
    pl::DispatchInstr::Route(instr_route)
}

// == Blocks

/// Visits a block backwards, counting every following instruction as a continuation.
fn classify_block<Tier>(
    cont: &Continuation,
    classify_tier: ClassifyTier<Tier>,
    block: pl::Block<Tier>,
) -> pl::Block<Tier> {
    // Classify each instruction with all following instructions available
    let mut cont = cont.clone();
    let mut instrs = Vec::with_capacity(block.instrs.len());
    for instr in block.instrs.into_iter().rev() {
        instrs.push(classify_instr(&cont, classify_tier, instr));
        cont = cont.followed();
    }
    // Restore source order after propagating continuations backwards
    instrs.reverse();
    pl::Block { instrs }
}

/// Classifies alternatives with all later blocks included in their continuation.
///
/// Every arm but the last falls through to the next one;
/// a mismatch in the last arm ends the alternatives as a failed condition would.
fn classify_blocks<Tier: AltTier>(
    cont: &Continuation,
    classify_tier: ClassifyTier<Tier>,
    blocks: Vec<pl::Block<Tier>>,
) -> Vec<pl::Block<Tier>> {
    // A following instruction would take over a mismatch in the last arm
    assert!(!cont.follows, "alternatives end their block");
    // Propagate the availability of later alternatives backwards
    let mut cont = cont.clone();
    let mut blocks_classified: Vec<pl::Block<Tier>> = Vec::with_capacity(blocks.len());
    for block in blocks.into_iter().rev() {
        let block_classified = classify_block(&cont, classify_tier, block);
        cont = Continuation {
            follows: false,
            fallthrough: pl::Fallthrough::Next,
            id_group_next: Tier::id_group_first(&block_classified).or(cont.id_group_next),
        };
        blocks_classified.push(block_classified);
    }
    // Preserve alternative selection order
    blocks_classified.reverse();
    blocks_classified
}

/// A tier whose blocks can be alternatives.
trait AltTier: Sized {
    /// Names the rule group an alternative block tries first, if any.
    fn id_group_first(block: &pl::Block<Self>) -> Option<pl::Id>;
}

impl AltTier for pl::GroupInstr {
    fn id_group_first(_block: &pl::GroupBlock) -> Option<pl::Id> {
        None
    }
}

impl AltTier for pl::DispatchInstr {
    fn id_group_first(block: &pl::DispatchBlock) -> Option<pl::Id> {
        rule_group::collect_rule_groups(block)
            .first()
            .map(|rule_group| rule_group.id_group.clone())
    }
}

fn classify_group_block(cont: &Continuation, block: pl::GroupBlock) -> pl::GroupBlock {
    classify_block(cont, classify_group_tier, block)
}

fn classify_dispatch_block(cont: &Continuation, block: pl::DispatchBlock) -> pl::DispatchBlock {
    classify_block(cont, classify_dispatch_tier, block)
}

// == Relation definitions

fn classify_rel_def(def_rel: pl::RelDef) -> pl::RelDef {
    match def_rel {
        pl::RelDef::Defined(def_rel) => pl::RelDef::Defined(classify_defined_rel_def(def_rel)),
        pl::RelDef::Extern(def_rel) => pl::RelDef::Extern(def_rel),
    }
}

fn classify_defined_rel_def(mut def_rel: pl::DefinedRel) -> pl::DefinedRel {
    let cont = Continuation::of_body(def_rel.block_else_opt.is_some());
    def_rel.block = classify_dispatch_block(&cont, def_rel.block);
    def_rel.block_else_opt = def_rel
        .block_else_opt
        .map(|block| classify_dispatch_block(&Continuation::of_body(false), block));
    def_rel
}

// == Meta-function definitions

fn classify_func_def(def_func: pl::MetaFuncDef) -> pl::MetaFuncDef {
    match def_func {
        pl::MetaFuncDef::Defined(def_func) => {
            pl::MetaFuncDef::Defined(classify_defined_func_def(def_func))
        }
        def_func @ (pl::MetaFuncDef::Extern(_)
        | pl::MetaFuncDef::Builtin(_)
        | pl::MetaFuncDef::Table(_)) => def_func,
    }
}

fn classify_defined_func_def(mut def_func: pl::DefinedFunc) -> pl::DefinedFunc {
    let cont = Continuation::of_body(def_func.block_else_opt.is_some());
    def_func.block = classify_group_block(&cont, def_func.block);
    def_func.block_else_opt = def_func
        .block_else_opt
        .map(|block| classify_group_block(&Continuation::of_body(false), block));
    def_func
}

// == Definitions

fn classify_def(mut def: pl::Def) -> pl::Def {
    def.node.node = classify_def_kind(def.node.node);
    def
}

fn classify_def_kind(def_kind: pl::DefKind) -> pl::DefKind {
    match def_kind {
        pl::DefKind::Rel(def_rel) => pl::DefKind::Rel(classify_rel_def(def_rel)),
        pl::DefKind::MetaFunc(def_func) => pl::DefKind::MetaFunc(classify_func_def(def_func)),
        def_kind @ (pl::DefKind::Typ(_) | pl::DefKind::Var(_)) => def_kind,
    }
}

// == Entry point

/// Classifies canonical definitions before rendering or extracting rule groups.
pub(super) fn classify_spec(spec: pl::Spec) -> pl::Spec {
    spec.into_iter().map(classify_def).collect()
}
