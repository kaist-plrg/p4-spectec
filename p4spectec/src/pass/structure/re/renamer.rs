//! Capture-avoiding identifier renaming for OL instructions
//!
//! With `x -> y`, `let y = z { return x }` becomes
//! `let y' = z { return y }`, assuming `y'` is fresh.
//! The local binder is renamed so it does not capture the introduced `y`.

use std::rc::Rc;

use crate::lang::{
    common::ds::{map::IdMap, set::IdSet},
    data::notation::tree as notation,
    hints::input,
    traits::free::FreeIds,
};

use crate::lang::il::{ast::*, fresh};

use crate::{note_phrase, phrase};

use super::super::ol::ast as ol;

// == Environment

/// Identifier renaming `id -> id_renamed`, applied to OL without capture.
#[derive(Clone, Debug, Default)]
pub(crate) struct Renamer {
    ids: IdMap<Id>,
}

impl Renamer {
    pub(crate) fn empty() -> Self {
        Self::default()
    }

    pub(crate) fn dom(&self) -> IdSet {
        self.ids.domain()
    }

    pub(crate) fn values(&self) -> Vec<Id> {
        self.ids.iter().map(|(_, id)| id.clone()).collect()
    }

    pub(crate) fn singleton(id: Id, id_renamed: Id) -> Self {
        let mut renamer = Self::empty();
        renamer.add(id, id_renamed);
        renamer
    }

    pub(crate) fn add(&mut self, id: Id, id_renamed: Id) {
        self.ids.insert(id, id_renamed);
    }

    /// Keeps only the renames the predicate accepts.
    pub(crate) fn filter(&self, mut predicate: impl FnMut(&Id, &Id) -> bool) -> Self {
        Self {
            ids: self
                .ids
                .iter()
                .filter(|(id, id_renamed)| predicate(id, id_renamed))
                .map(|(id, id_renamed)| (id.clone(), id_renamed.clone()))
                .collect(),
        }
    }

    // == Capture avoidance

    /// Freshens the binders in `frees` that collide with rename targets.
    ///
    /// ```text
    /// Rename x -> y:
    ///   before: let y = w { return (x, y) }
    ///   after:  let y' = w { return (y, y') }
    /// ```
    ///
    /// Returns `y -> y'`, assuming `y'` is fresh;
    /// the caller applies this map to the binder and its uses
    /// along with the original renaming.
    /// Fresh names avoid the binders, the block's free names,
    /// both sides of this renamer, and names already chosen in this call.
    pub(crate) fn freshen_binders(&self, frees: &IdSet, block: &ol::Block) -> Self {
        // Binders that a rename target would capture
        let ids_collide: IdSet = self
            .values()
            .into_iter()
            .filter(|id| frees.contains(id))
            .collect();
        if ids_collide.is_empty() {
            return Self::empty();
        }
        // Avoid every name in play, then pick fresh ones
        let mut ids_avoid = frees
            .clone()
            .union(block.free_ids())
            .union(self.dom())
            .union(self.values().into_iter().collect());
        let mut renamer_fresh = Self::empty();
        for id in ids_collide.iter() {
            let id_fresh = fresh::id(&ids_avoid, id);
            renamer_fresh.add(id.clone(), id_fresh.clone());
            ids_avoid.insert(id_fresh);
        }
        renamer_fresh
    }

    // == Variables

    /// Renames one identifier, flagging `changed` when the spelling differs.
    fn rename_id(&self, changed: &mut bool, id: Id) -> Id {
        let Some(id_renamed) = self.ids.get(&id) else {
            return id;
        };
        if id.node != id_renamed.node {
            *changed = true;
        }
        id_renamed.clone()
    }

    fn rename_vars(&self, changed: &mut bool, vars: Vec<Var>) -> Vec<Var> {
        vars.into_iter()
            .map(|mut var| {
                var.id = self.rename_id(changed, var.id);
                var
            })
            .collect()
    }

    // == Expressions

    /// Renames identifiers throughout an expression.
    pub(crate) fn rename_exp(&self, changed: &mut bool, exp: Exp) -> Exp {
        // Nothing to rename
        if self.ids.is_empty() {
            return exp;
        }
        let exp_kind = match exp.node {
            ExpKind::Bool(_) | ExpKind::Num(_) | ExpKind::Text(_) => exp.node,
            // Renaming applies at identifiers; other nodes recurse
            ExpKind::Id(id) => ExpKind::Id(self.rename_id(changed, id)),
            ExpKind::Un(op, op_typ, exp) => {
                ExpKind::Un(op, op_typ, Box::new(self.rename_exp(changed, *exp)))
            }
            ExpKind::Bin(op, op_typ, exp_l, exp_r) => ExpKind::Bin(
                op,
                op_typ,
                Box::new(self.rename_exp(changed, *exp_l)),
                Box::new(self.rename_exp(changed, *exp_r)),
            ),
            ExpKind::Cmp(op, op_typ, exp_l, exp_r) => ExpKind::Cmp(
                op,
                op_typ,
                Box::new(self.rename_exp(changed, *exp_l)),
                Box::new(self.rename_exp(changed, *exp_r)),
            ),
            ExpKind::UpCast(typ, exp) => {
                ExpKind::UpCast(typ, Box::new(self.rename_exp(changed, *exp)))
            }
            ExpKind::DownCast(typ, exp) => {
                ExpKind::DownCast(typ, Box::new(self.rename_exp(changed, *exp)))
            }
            ExpKind::Sub(exp, typ, subcheck) => {
                ExpKind::Sub(Box::new(self.rename_exp(changed, *exp)), typ, subcheck)
            }
            ExpKind::Match(exp, pattern) => {
                ExpKind::Match(Box::new(self.rename_exp(changed, *exp)), pattern)
            }
            ExpKind::Tuple(exps) => ExpKind::Tuple(self.rename_exps(changed, exps)),
            ExpKind::Case(not_exp) => {
                ExpKind::Case(Box::new(not_exp.map(|exp| self.rename_exp(changed, exp.clone()))))
            }
            ExpKind::Str(exp_fields) => ExpKind::Str(
                exp_fields
                    .into_iter()
                    .map(|ExpField { atom, exp }| ExpField {
                        atom,
                        exp: self.rename_exp(changed, exp),
                    })
                    .collect(),
            ),
            ExpKind::Opt(exp) => {
                ExpKind::Opt(exp.map(|exp| Box::new(self.rename_exp(changed, *exp))))
            }
            ExpKind::List(exps) => ExpKind::List(self.rename_exps(changed, exps)),
            ExpKind::Cons(exp_head, exp_tail) => ExpKind::Cons(
                Box::new(self.rename_exp(changed, *exp_head)),
                Box::new(self.rename_exp(changed, *exp_tail)),
            ),
            ExpKind::Cat(exp_l, exp_r) => ExpKind::Cat(
                Box::new(self.rename_exp(changed, *exp_l)),
                Box::new(self.rename_exp(changed, *exp_r)),
            ),
            ExpKind::Mem(exp_elem, exp_set) => ExpKind::Mem(
                Box::new(self.rename_exp(changed, *exp_elem)),
                Box::new(self.rename_exp(changed, *exp_set)),
            ),
            ExpKind::Len(exp) => ExpKind::Len(Box::new(self.rename_exp(changed, *exp))),
            ExpKind::Dot(exp, atom) => ExpKind::Dot(Box::new(self.rename_exp(changed, *exp)), atom),
            ExpKind::Idx(exp_base, exp_idx) => ExpKind::Idx(
                Box::new(self.rename_exp(changed, *exp_base)),
                Box::new(self.rename_exp(changed, *exp_idx)),
            ),
            ExpKind::Slice(exp_base, exp_idx, exp_len) => ExpKind::Slice(
                Box::new(self.rename_exp(changed, *exp_base)),
                Box::new(self.rename_exp(changed, *exp_idx)),
                Box::new(self.rename_exp(changed, *exp_len)),
            ),
            ExpKind::Upd(exp_base, path, exp_field) => ExpKind::Upd(
                Box::new(self.rename_exp(changed, *exp_base)),
                Box::new(self.rename_path(changed, *path)),
                Box::new(self.rename_exp(changed, *exp_field)),
            ),
            ExpKind::Call(id, targs, args) => {
                ExpKind::Call(id, targs, self.rename_args(changed, args))
            }
            ExpKind::Iter(exp, exp_iter) => ExpKind::Iter(
                Box::new(self.rename_exp(changed, *exp)),
                self.rename_iterexp(changed, exp_iter),
            ),
        };
        note_phrase!(node: exp_kind, note: exp.note, span: exp.span)
    }

    pub(crate) fn rename_exps(&self, changed: &mut bool, exps: Vec<Exp>) -> Vec<Exp> {
        exps.into_iter()
            .map(|exp| self.rename_exp(changed, exp))
            .collect()
    }

    // == Expression iterators

    pub(crate) fn rename_iterexp(&self, changed: &mut bool, exp_iter: ExpIter) -> ExpIter {
        let ExpIter { iter, vars } = exp_iter;
        ExpIter { iter, vars: self.rename_vars(changed, vars) }
    }

    pub(crate) fn rename_iterexps(
        &self,
        changed: &mut bool,
        iter_exps: Vec<ExpIter>,
    ) -> Vec<ExpIter> {
        iter_exps
            .into_iter()
            .map(|exp_iter| self.rename_iterexp(changed, exp_iter))
            .collect()
    }

    // == Paths

    pub(crate) fn rename_path(&self, changed: &mut bool, path: Path) -> Path {
        let path_kind = match path.node {
            PathKind::Root => PathKind::Root,
            PathKind::Idx(path, exp) => PathKind::Idx(
                Box::new(self.rename_path(changed, *path)),
                Box::new(self.rename_exp(changed, *exp)),
            ),
            PathKind::Slice(path, exp_idx, exp_len) => PathKind::Slice(
                Box::new(self.rename_path(changed, *path)),
                Box::new(self.rename_exp(changed, *exp_idx)),
                Box::new(self.rename_exp(changed, *exp_len)),
            ),
            PathKind::Dot(path, atom) => {
                PathKind::Dot(Box::new(self.rename_path(changed, *path)), atom)
            }
        };
        note_phrase!(node: path_kind, note: path.note, span: path.span)
    }

    // == Arguments

    pub(crate) fn rename_arg(&self, changed: &mut bool, arg: Arg) -> Arg {
        let arg_kind = match arg.node {
            ArgKind::Exp(exp) => ArgKind::Exp(Box::new(self.rename_exp(changed, *exp))),
            ArgKind::Def(_) => arg.node,
        };
        phrase!(node: arg_kind, span: arg.span)
    }

    pub(crate) fn rename_args(&self, changed: &mut bool, args: Vec<Arg>) -> Vec<Arg> {
        args.into_iter()
            .map(|arg| self.rename_arg(changed, arg))
            .collect()
    }

    // == Cases

    pub(crate) fn rename_case(&self, changed: &mut bool, case: ol::Case) -> ol::Case {
        let ol::Case { guard, block } = case;
        let guard = self.rename_guard(changed, guard);
        let block = self.rename_block(changed, block);
        ol::Case { guard, block }
    }

    pub(crate) fn rename_cases(&self, changed: &mut bool, cases: Vec<ol::Case>) -> Vec<ol::Case> {
        cases
            .into_iter()
            .map(|case| self.rename_case(changed, case))
            .collect()
    }

    // - Guards

    /// Renames inside the guards that carry an expression.
    pub(crate) fn rename_guard(&self, changed: &mut bool, guard: ol::Guard) -> ol::Guard {
        match guard {
            ol::Guard::Bool(_) | ol::Guard::Sub(..) | ol::Guard::Match(_) => guard,
            ol::Guard::Cmp(op, op_typ, exp) => {
                ol::Guard::Cmp(op, op_typ, self.rename_exp(changed, exp))
            }
            ol::Guard::Mem(exp) => ol::Guard::Mem(self.rename_exp(changed, exp)),
        }
    }

    // == Instructions

    /// Renames identifiers throughout an instruction, avoiding capture.
    pub(crate) fn rename_instr(&self, changed: &mut bool, instr_ol: ol::Instr) -> ol::Instr {
        if self.ids.is_empty() {
            return instr_ol;
        }
        let instr_kind_ol = self.rename_instr_kind(changed, instr_ol.node);
        phrase!(node: instr_kind_ol, span: instr_ol.span)
    }

    fn rename_instr_kind(&self, changed: &mut bool, instr_kind_ol: ol::InstrKind) -> ol::InstrKind {
        match instr_kind_ol {
            ol::InstrKind::If(instr_ol) => self.rename_if_instr(changed, instr_ol),
            ol::InstrKind::Hold(instr_ol) => self.rename_hold_instr(changed, instr_ol),
            ol::InstrKind::Case(instr_ol) => self.rename_case_instr(changed, instr_ol),
            ol::InstrKind::Group(instr_ol) => self.rename_group_instr(changed, instr_ol),
            ol::InstrKind::Let(instr_ol) => self.rename_let_instr(changed, instr_ol),
            ol::InstrKind::Rule(instr_ol) => self.rename_rule_instr(changed, instr_ol),
            ol::InstrKind::Result(instr_ol) => self.rename_result_instr(changed, instr_ol),
            ol::InstrKind::Return(instr_ol) => self.rename_return_instr(changed, instr_ol),
            ol::InstrKind::Debug(instr_ol) => self.rename_debug_instr(changed, instr_ol),
        }
    }

    pub(crate) fn rename_instrs(
        &self,
        changed: &mut bool,
        instrs_ol: Vec<ol::Instr>,
    ) -> Vec<ol::Instr> {
        instrs_ol
            .into_iter()
            .map(|instr_ol| self.rename_instr(changed, instr_ol))
            .collect()
    }

    // - If instruction

    fn rename_if_instr(&self, changed: &mut bool, instr_ol: ol::IfInstr) -> ol::InstrKind {
        let ol::IfInstr { exp, iter_exps, block } = instr_ol;
        let exp = self.rename_exp(changed, exp);
        let iter_exps = self.rename_iterexps(changed, iter_exps);
        let block = self.rename_block(changed, block);
        ol::InstrKind::If(ol::IfInstr { exp, iter_exps, block })
    }

    // - Hold instruction

    fn rename_hold_instr(&self, changed: &mut bool, instr_ol: ol::HoldInstr) -> ol::InstrKind {
        let ol::HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold } = instr_ol;
        let not_exp = not_exp.map(|exp| self.rename_exp(changed, exp.clone()));
        let iter_exps = self.rename_iterexps(changed, iter_exps);
        let block_hold = self.rename_block(changed, block_hold);
        let block_not_hold = self.rename_block(changed, block_not_hold);
        ol::InstrKind::Hold(ol::HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold })
    }

    // - Case instruction

    fn rename_case_instr(&self, changed: &mut bool, instr_ol: ol::CaseInstr) -> ol::InstrKind {
        let ol::CaseInstr { exp, cases, total } = instr_ol;
        let exp = self.rename_exp(changed, exp);
        let cases = self.rename_cases(changed, cases);
        ol::InstrKind::Case(ol::CaseInstr { exp, cases, total })
    }

    // - Group instruction

    fn rename_group_instr(&self, changed: &mut bool, instr_ol: ol::GroupInstr) -> ol::InstrKind {
        let ol::GroupInstr { id, rel_signature, exps, block } = instr_ol;
        let exps = self.rename_exps(changed, exps);
        let block = self.rename_block(changed, block);
        ol::InstrKind::Group(ol::GroupInstr { id, rel_signature, exps, block })
    }

    // - Let instruction

    /// Renames a let; its binders shadow the renaming and may be freshened.
    fn rename_let_instr(&self, changed: &mut bool, instr_ol: ol::LetInstr) -> ol::InstrKind {
        let ol::LetInstr { exp_l, exp_r, iter_instrs, block } = instr_ol;
        let exp_r = self.rename_exp(changed, exp_r);
        // Names bound here are not renamed below, except to avoid capture
        let frees_l = exp_l.free_ids();
        let mut renamer = self.filter(|id, _| !frees_l.contains(id));
        let renamer_fresh = renamer.freshen_binders(&frees_l, &block);
        let exp_l = renamer_fresh.rename_exp(changed, exp_l);
        // The fresh binder names apply to the body as well
        renamer.ids.extend(
            renamer_fresh
                .ids
                .iter()
                .map(|(id, id_fresh)| (id.clone(), id_fresh.clone())),
        );
        let iter_instrs = renamer.rename_iterinstrs_bound(changed, iter_instrs);
        let block = renamer.rename_block(changed, block);
        ol::InstrKind::Let(ol::LetInstr { exp_l, exp_r, iter_instrs, block })
    }

    // - Rule instruction

    /// Renames a rule call; output binders shadow it and may be freshened.
    fn rename_rule_instr(&self, changed: &mut bool, instr_ol: ol::RuleInstr) -> ol::InstrKind {
        let ol::RuleInstr { id, not_exp, input_hint, iter_instrs, block } = instr_ol;
        // Split the arguments by the input hint
        let exps = not_exp.args().to_vec();
        // Elaboration validates hints; OL rewrites preserve notation arity
        let (exps_input, exps_output) =
            input::split(&input_hint, exps).expect("validated relation hints and argument counts");
        // Inputs are uses, outputs are binders
        let exps_input = self.rename_exps(changed, exps_input);
        let frees_output = exps_output.as_slice().free_ids();
        let mut renamer = self.filter(|id, _| !frees_output.contains(id));
        // Freshen output binders that would capture a rename target
        let renamer_fresh = renamer.freshen_binders(&frees_output, &block);
        let exps_output = renamer_fresh.rename_exps(changed, exps_output);
        renamer.ids.extend(
            renamer_fresh
                .ids
                .iter()
                .map(|(id, id_fresh)| (id.clone(), id_fresh.clone())),
        );
        // Rebuild the notation, then rename iterators and body
        // Renaming preserves the argument counts returned by input::split
        let exps = input::combine(&input_hint, exps_input, exps_output)
            .expect("validated relation hints and argument counts");
        let not_exp = notation::Mixfix::new(Rc::clone(not_exp.mixop()), exps)
            .expect("validated arguments preserve the mixfix arity");
        let iter_instrs = renamer.rename_iterinstrs_bound(changed, iter_instrs);
        let block = renamer.rename_block(changed, block);
        ol::InstrKind::Rule(ol::RuleInstr { id, not_exp, input_hint, iter_instrs, block })
    }

    // - Result instruction

    fn rename_result_instr(&self, changed: &mut bool, instr_ol: ol::ResultInstr) -> ol::InstrKind {
        let ol::ResultInstr { rel_signature, exps } = instr_ol;
        let exps = self.rename_exps(changed, exps);
        ol::InstrKind::Result(ol::ResultInstr { rel_signature, exps })
    }

    // - Return instruction

    fn rename_return_instr(&self, changed: &mut bool, instr_ol: ol::ReturnInstr) -> ol::InstrKind {
        let ol::ReturnInstr { exp } = instr_ol;
        let exp = self.rename_exp(changed, exp);
        ol::InstrKind::Return(ol::ReturnInstr { exp })
    }

    // - Debug instruction

    fn rename_debug_instr(&self, changed: &mut bool, instr_ol: ol::DebugInstr) -> ol::InstrKind {
        let ol::DebugInstr { exp, instr } = instr_ol;
        let exp = self.rename_exp(changed, exp);
        let instr = Box::new(self.rename_instr(changed, *instr));
        ol::InstrKind::Debug(ol::DebugInstr { exp, instr })
    }

    // == Blocks

    pub(crate) fn rename_block(&self, changed: &mut bool, block: ol::Block) -> ol::Block {
        if self.ids.is_empty() {
            return block;
        }
        self.rename_instrs(changed, block)
    }

    // == Instruction iterators

    // - Bound variables

    /// Renames the source variables of an instruction iterator.
    pub(crate) fn rename_iterinstr_bound(
        &self,
        changed: &mut bool,
        iter_instr: ol::InstrIter,
    ) -> ol::InstrIter {
        let ol::InstrIter { iter, vars_bound, vars_bind } = iter_instr;
        let vars_bound = self.rename_vars(changed, vars_bound);
        ol::InstrIter { iter, vars_bound, vars_bind }
    }

    pub(crate) fn rename_iterinstrs_bound(
        &self,
        changed: &mut bool,
        iter_instrs: Vec<ol::InstrIter>,
    ) -> Vec<ol::InstrIter> {
        iter_instrs
            .into_iter()
            .map(|iter_instr| self.rename_iterinstr_bound(changed, iter_instr))
            .collect()
    }

    // - Binding variables

    /// Renames the binding variables of an instruction iterator.
    pub(crate) fn rename_iterinstr_bind(
        &self,
        changed: &mut bool,
        iter_instr: ol::InstrIter,
    ) -> ol::InstrIter {
        let ol::InstrIter { iter, vars_bound, vars_bind } = iter_instr;
        let vars_bind = self.rename_vars(changed, vars_bind);
        ol::InstrIter { iter, vars_bound, vars_bind }
    }

    pub(crate) fn rename_iterinstrs_bind(
        &self,
        changed: &mut bool,
        iter_instrs: Vec<ol::InstrIter>,
    ) -> Vec<ol::InstrIter> {
        iter_instrs
            .into_iter()
            .map(|iter_instr| self.rename_iterinstr_bind(changed, iter_instr))
            .collect()
    }
}
