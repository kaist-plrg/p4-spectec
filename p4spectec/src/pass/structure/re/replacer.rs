//! Capture-avoiding expression replacement for OL instructions
//!
//! With `x -> y + z`, `let y = w { return x }` becomes
//! `let y' = w { return y + z }`, assuming `y'` is fresh.
//! `Renamer` first moves the local binder
//! away from the replacement's free names.

use std::rc::Rc;

use crate::lang::{
    common::{
        ds::{map::IdMap, set::IdSet},
        source::Span,
    },
    data::notation::tree as notation,
    hints::input,
    traits::free::FreeIds,
};

use crate::lang::il::{ast::*, fresh};

use crate::{note_phrase, phrase};

use super::super::ol::ast as ol;

use super::renamer::Renamer;

// == Environment

/// Expression substitution `id -> exp`, applied to OL without capture.
#[derive(Clone, Debug, Default)]
pub(crate) struct Replacer {
    exps: IdMap<Exp>,
}

impl Replacer {
    pub(crate) fn empty() -> Self {
        Self::default()
    }

    pub(crate) fn dom(&self) -> IdSet {
        self.exps.domain()
    }

    pub(crate) fn singleton(id: Id, exp: Exp) -> Self {
        let mut replacer = Self::empty();
        replacer.add(id, exp);
        replacer
    }

    pub(crate) fn add(&mut self, id: Id, exp: Exp) {
        self.exps.insert(id, exp);
    }

    /// Keeps only the substitutions the predicate accepts.
    pub(crate) fn filter(&self, mut predicate: impl FnMut(&Id, &Exp) -> bool) -> Self {
        Self {
            exps: self
                .exps
                .iter()
                .filter(|(id, exp)| predicate(id, exp))
                .map(|(id, exp)| (id.clone(), exp.clone()))
                .collect(),
        }
    }

    // == Capture avoidance

    /// Freshens the binders in `frees` that occur free in replacements.
    ///
    /// ```text
    /// Replace x -> y + z:
    ///   before: let y = w { return (x, y) }
    ///   after:  let y' = w { return (y + z, y') }
    /// ```
    ///
    /// Returns `y -> y'`, assuming `y'` is fresh;
    /// the caller renames the binder and its uses before replacing `x`,
    /// so the inserted `y` stays free.
    /// Fresh names avoid the binders, the block's free names,
    /// replacement keys and free names, and names already chosen in this call.
    pub(crate) fn freshen_binders(&self, frees: &IdSet, block: &ol::Block) -> Renamer {
        // Free names of the replacements
        let ids_codom = self
            .exps
            .iter()
            // Binders those names would capture
            .fold(IdSet::new(), |ids, (_, exp)| ids.union(exp.free_ids()));
        let ids_collide: IdSet = frees
            .iter()
            .filter(|id| ids_codom.contains(id))
            .cloned()
            .collect();
        // Avoid every name in play, then pick fresh ones
        let mut ids_avoid = frees
            .clone()
            .union(block.free_ids())
            .union(self.dom())
            .union(ids_codom);
        let mut renamer_fresh = Renamer::empty();
        for id in ids_collide.iter() {
            let id_fresh = fresh::id(&ids_avoid, id);
            renamer_fresh.add(id.clone(), id_fresh.clone());
            ids_avoid.insert(id_fresh);
        }
        renamer_fresh
    }

    // == Variables

    /// Drops substituted iteration variables, which are no longer variables.
    fn filter_vars(&self, vars: Vec<Var>) -> Vec<Var> {
        vars.into_iter()
            .filter(|var| !self.exps.contains_key(&var.id))
            .collect()
    }

    // == Expressions

    /// Substitutes expressions for identifiers throughout an expression.
    pub(crate) fn replace_exp(&self, exp: Exp) -> Exp {
        let exp_kind = match exp.node {
            ExpKind::Bool(_) | ExpKind::Num(_) | ExpKind::Text(_) => exp.node,
            // Substitution applies at identifiers; other nodes recurse
            ExpKind::Id(id) => return self.replace_id_exp(id, exp.note, exp.span),
            ExpKind::Un(op, op_typ, exp) => {
                ExpKind::Un(op, op_typ, Box::new(self.replace_exp(*exp)))
            }
            ExpKind::Bin(op, op_typ, exp_l, exp_r) => ExpKind::Bin(
                op,
                op_typ,
                Box::new(self.replace_exp(*exp_l)),
                Box::new(self.replace_exp(*exp_r)),
            ),
            ExpKind::Cmp(op, op_typ, exp_l, exp_r) => ExpKind::Cmp(
                op,
                op_typ,
                Box::new(self.replace_exp(*exp_l)),
                Box::new(self.replace_exp(*exp_r)),
            ),
            ExpKind::UpCast(typ, exp) => ExpKind::UpCast(typ, Box::new(self.replace_exp(*exp))),
            ExpKind::DownCast(typ, exp) => ExpKind::DownCast(typ, Box::new(self.replace_exp(*exp))),
            ExpKind::Sub(exp, typ, subcheck) => {
                ExpKind::Sub(Box::new(self.replace_exp(*exp)), typ, subcheck)
            }
            ExpKind::Match(exp, pattern) => {
                ExpKind::Match(Box::new(self.replace_exp(*exp)), pattern)
            }
            ExpKind::Tuple(exps) => ExpKind::Tuple(self.replace_exps(exps)),
            ExpKind::Case(not_exp) => {
                ExpKind::Case(Box::new(not_exp.map(|exp| self.replace_exp(exp.clone()))))
            }
            ExpKind::Str(exp_fields) => ExpKind::Str(
                exp_fields
                    .into_iter()
                    .map(|ExpField { atom, exp }| ExpField { atom, exp: self.replace_exp(exp) })
                    .collect(),
            ),
            ExpKind::Opt(exp) => ExpKind::Opt(exp.map(|exp| Box::new(self.replace_exp(*exp)))),
            ExpKind::List(exps) => ExpKind::List(self.replace_exps(exps)),
            ExpKind::Cons(exp_head, exp_tail) => ExpKind::Cons(
                Box::new(self.replace_exp(*exp_head)),
                Box::new(self.replace_exp(*exp_tail)),
            ),
            ExpKind::Cat(exp_l, exp_r) => {
                ExpKind::Cat(Box::new(self.replace_exp(*exp_l)), Box::new(self.replace_exp(*exp_r)))
            }
            ExpKind::Mem(exp_elem, exp_set) => ExpKind::Mem(
                Box::new(self.replace_exp(*exp_elem)),
                Box::new(self.replace_exp(*exp_set)),
            ),
            ExpKind::Len(exp) => ExpKind::Len(Box::new(self.replace_exp(*exp))),
            ExpKind::Dot(exp, atom) => ExpKind::Dot(Box::new(self.replace_exp(*exp)), atom),
            ExpKind::Idx(exp_base, exp_idx) => ExpKind::Idx(
                Box::new(self.replace_exp(*exp_base)),
                Box::new(self.replace_exp(*exp_idx)),
            ),
            ExpKind::Slice(exp_base, exp_idx, exp_len) => ExpKind::Slice(
                Box::new(self.replace_exp(*exp_base)),
                Box::new(self.replace_exp(*exp_idx)),
                Box::new(self.replace_exp(*exp_len)),
            ),
            ExpKind::Upd(exp_base, path, exp_field) => ExpKind::Upd(
                Box::new(self.replace_exp(*exp_base)),
                Box::new(self.replace_path(*path)),
                Box::new(self.replace_exp(*exp_field)),
            ),
            ExpKind::Call(id, targs, args) => ExpKind::Call(id, targs, self.replace_args(args)),
            ExpKind::Iter(exp, exp_iter) => {
                ExpKind::Iter(Box::new(self.replace_exp(*exp)), self.replace_iterexp(exp_iter))
            }
        };
        note_phrase!(node: exp_kind, note: exp.note, span: exp.span)
    }

    pub(crate) fn replace_exps(&self, exps: Vec<Exp>) -> Vec<Exp> {
        exps.into_iter().map(|exp| self.replace_exp(exp)).collect()
    }

    // - Identifier expression

    /// Substitutes an identifier in the domain, otherwise keeps it.
    fn replace_id_exp(&self, id: Id, note: std::rc::Rc<TypKind>, span: Span) -> Exp {
        self.exps.get(&id).cloned().unwrap_or(note_phrase!(
            node: ExpKind::Id(id),
            note: note,
            span: span,
        ))
    }

    // == Expression iterators

    pub(crate) fn replace_iterexp(&self, exp_iter: ExpIter) -> ExpIter {
        let ExpIter { iter, vars } = exp_iter;
        ExpIter { iter, vars: self.filter_vars(vars) }
    }

    pub(crate) fn replace_iterexps(&self, iter_exps: Vec<ExpIter>) -> Vec<ExpIter> {
        iter_exps
            .into_iter()
            .map(|exp_iter| self.replace_iterexp(exp_iter))
            .collect()
    }

    // == Paths

    pub(crate) fn replace_path(&self, path: Path) -> Path {
        let path_kind = match path.node {
            PathKind::Root => PathKind::Root,
            PathKind::Idx(path, exp) => {
                PathKind::Idx(Box::new(self.replace_path(*path)), Box::new(self.replace_exp(*exp)))
            }
            PathKind::Slice(path, exp_idx, exp_len) => PathKind::Slice(
                Box::new(self.replace_path(*path)),
                Box::new(self.replace_exp(*exp_idx)),
                Box::new(self.replace_exp(*exp_len)),
            ),
            PathKind::Dot(path, atom) => PathKind::Dot(Box::new(self.replace_path(*path)), atom),
        };
        note_phrase!(node: path_kind, note: path.note, span: path.span)
    }

    // == Arguments

    pub(crate) fn replace_arg(&self, arg: Arg) -> Arg {
        let arg_kind = match arg.node {
            ArgKind::Exp(exp) => ArgKind::Exp(Box::new(self.replace_exp(*exp))),
            ArgKind::Def(_) => arg.node,
        };
        phrase!(node: arg_kind, span: arg.span)
    }

    pub(crate) fn replace_args(&self, args: Vec<Arg>) -> Vec<Arg> {
        args.into_iter().map(|arg| self.replace_arg(arg)).collect()
    }

    // == Cases

    pub(crate) fn replace_case(&self, case: ol::Case) -> ol::Case {
        let ol::Case { guard, block } = case;
        let guard = self.replace_guard(guard);
        let block = self.replace_block(block);
        ol::Case { guard, block }
    }

    pub(crate) fn replace_cases(&self, cases: Vec<ol::Case>) -> Vec<ol::Case> {
        cases
            .into_iter()
            .map(|case| self.replace_case(case))
            .collect()
    }

    // - Guards

    /// Substitutes inside the guards that carry an expression.
    pub(crate) fn replace_guard(&self, guard: ol::Guard) -> ol::Guard {
        match guard {
            ol::Guard::Bool(_) | ol::Guard::Sub(..) | ol::Guard::Match(_) => guard,
            ol::Guard::Cmp(op, op_typ, exp) => ol::Guard::Cmp(op, op_typ, self.replace_exp(exp)),
            ol::Guard::Mem(exp) => ol::Guard::Mem(self.replace_exp(exp)),
        }
    }

    // == Instructions

    /// Substitutes throughout an instruction, avoiding capture at binders.
    pub(crate) fn replace_instr(&self, instr_ol: ol::Instr) -> ol::Instr {
        let instr_kind_ol = self.replace_instr_kind(instr_ol.node);
        phrase!(node: instr_kind_ol, span: instr_ol.span)
    }

    fn replace_instr_kind(&self, instr_kind_ol: ol::InstrKind) -> ol::InstrKind {
        match instr_kind_ol {
            ol::InstrKind::If(instr_ol) => self.replace_if_instr(instr_ol),
            ol::InstrKind::Hold(instr_ol) => self.replace_hold_instr(instr_ol),
            ol::InstrKind::Case(instr_ol) => self.replace_case_instr(instr_ol),
            ol::InstrKind::Group(instr_ol) => self.replace_group_instr(instr_ol),
            ol::InstrKind::Let(instr_ol) => self.replace_let_instr(instr_ol),
            ol::InstrKind::Rule(instr_ol) => self.replace_rule_instr(instr_ol),
            ol::InstrKind::Result(instr_ol) => self.replace_result_instr(instr_ol),
            ol::InstrKind::Return(instr_ol) => self.replace_return_instr(instr_ol),
            ol::InstrKind::Debug(instr_ol) => self.replace_debug_instr(instr_ol),
        }
    }

    pub(crate) fn replace_instrs(&self, instrs_ol: Vec<ol::Instr>) -> Vec<ol::Instr> {
        instrs_ol
            .into_iter()
            .map(|instr_ol| self.replace_instr(instr_ol))
            .collect()
    }

    // - If instruction

    fn replace_if_instr(&self, instr_ol: ol::IfInstr) -> ol::InstrKind {
        let ol::IfInstr { exp, iter_exps, block } = instr_ol;
        let exp = self.replace_exp(exp);
        let iter_exps = self.replace_iterexps(iter_exps);
        let block = self.replace_block(block);
        ol::InstrKind::If(ol::IfInstr { exp, iter_exps, block })
    }

    // - Hold instruction

    fn replace_hold_instr(&self, instr_ol: ol::HoldInstr) -> ol::InstrKind {
        let ol::HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold } = instr_ol;
        let not_exp = not_exp.map(|exp| self.replace_exp(exp.clone()));
        let iter_exps = self.replace_iterexps(iter_exps);
        let block_hold = self.replace_block(block_hold);
        let block_not_hold = self.replace_block(block_not_hold);
        ol::InstrKind::Hold(ol::HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold })
    }

    // - Case instruction

    fn replace_case_instr(&self, instr_ol: ol::CaseInstr) -> ol::InstrKind {
        let ol::CaseInstr { exp, cases, total } = instr_ol;
        let exp = self.replace_exp(exp);
        let cases = self.replace_cases(cases);
        ol::InstrKind::Case(ol::CaseInstr { exp, cases, total })
    }

    // - Group instruction

    fn replace_group_instr(&self, instr_ol: ol::GroupInstr) -> ol::InstrKind {
        let ol::GroupInstr { id, rel_signature, exps, block } = instr_ol;
        let exps = self.replace_exps(exps);
        let block = self.replace_block(block);
        ol::InstrKind::Group(ol::GroupInstr { id, rel_signature, exps, block })
    }

    // - Let instruction

    /// Substitutes in a let; its binders shadow it and are freshened first.
    fn replace_let_instr(&self, instr_ol: ol::LetInstr) -> ol::InstrKind {
        let ol::LetInstr { exp_l, exp_r, iter_instrs, block } = instr_ol;
        // Freshen colliding binders first so inserted names stay free
        let frees_l = exp_l.free_ids();
        let replacer = self.filter(|id, _| !frees_l.contains(id));
        let renamer_fresh = replacer.freshen_binders(&frees_l, &block);
        // Then substitute in the source, iterators, and body
        let exp_l = renamer_fresh.rename_exp(&mut false, exp_l);
        let iter_instrs = renamer_fresh.rename_iterinstrs_bound(&mut false, iter_instrs);
        let block = renamer_fresh.rename_block(&mut false, block);
        let exp_r = replacer.replace_exp(exp_r);
        let iter_instrs = replacer.replace_iterinstrs_bound(iter_instrs);
        let block = replacer.replace_block(block);
        ol::InstrKind::Let(ol::LetInstr { exp_l, exp_r, iter_instrs, block })
    }

    // - Rule instruction

    /// Substitutes in a rule call; output binders shadow it, freshened first.
    fn replace_rule_instr(&self, instr_ol: ol::RuleInstr) -> ol::InstrKind {
        let ol::RuleInstr { id, not_exp, input_hint, iter_instrs, block } = instr_ol;
        // Split the arguments by the input hint
        let exps = not_exp.args().to_vec();
        // Elaboration validates hints; OL rewrites preserve notation arity
        let (exps_input, exps_output) =
            input::split(&input_hint, exps).expect("validated relation hints and argument counts");
        // Inputs are uses, outputs are binders
        let exps_input = self.replace_exps(exps_input);
        let frees_output = exps_output.as_slice().free_ids();
        let replacer = self.filter(|id, _| !frees_output.contains(id));
        // Freshen output binders that would capture an inserted name
        let renamer_fresh = replacer.freshen_binders(&frees_output, &block);
        let exps_output = renamer_fresh.rename_exps(&mut false, exps_output);
        let iter_instrs = renamer_fresh.rename_iterinstrs_bound(&mut false, iter_instrs);
        let block = renamer_fresh.rename_block(&mut false, block);
        // Rebuild the notation, then substitute in iterators and body
        // Renaming preserves the argument counts returned by input::split
        let exps = input::combine(&input_hint, exps_input, exps_output)
            .expect("validated relation hints and argument counts");
        let not_exp = notation::Mixfix::new(Rc::clone(not_exp.mixop()), exps)
            .expect("validated arguments preserve the mixfix arity");
        let iter_instrs = replacer.replace_iterinstrs_bound(iter_instrs);
        let block = replacer.replace_block(block);
        ol::InstrKind::Rule(ol::RuleInstr { id, not_exp, input_hint, iter_instrs, block })
    }

    // - Result instruction

    fn replace_result_instr(&self, instr_ol: ol::ResultInstr) -> ol::InstrKind {
        let ol::ResultInstr { rel_signature, exps } = instr_ol;
        let exps = self.replace_exps(exps);
        ol::InstrKind::Result(ol::ResultInstr { rel_signature, exps })
    }

    // - Return instruction

    fn replace_return_instr(&self, instr_ol: ol::ReturnInstr) -> ol::InstrKind {
        let ol::ReturnInstr { exp } = instr_ol;
        let exp = self.replace_exp(exp);
        ol::InstrKind::Return(ol::ReturnInstr { exp })
    }

    // - Debug instruction

    fn replace_debug_instr(&self, instr_ol: ol::DebugInstr) -> ol::InstrKind {
        let ol::DebugInstr { exp, instr } = instr_ol;
        let exp = self.replace_exp(exp);
        let instr = Box::new(self.replace_instr(*instr));
        ol::InstrKind::Debug(ol::DebugInstr { exp, instr })
    }

    // == Blocks

    pub(crate) fn replace_block(&self, block: ol::Block) -> ol::Block {
        self.replace_instrs(block)
    }

    // == Instruction iterators

    /// Drops substituted identifiers from an iterator's binding variables.
    pub(crate) fn replace_iterinstr_bound(&self, iter_instr: ol::InstrIter) -> ol::InstrIter {
        let ol::InstrIter { iter, vars_bound, vars_bind } = iter_instr;
        let vars_bind = self.filter_vars(vars_bind);
        ol::InstrIter { iter, vars_bound, vars_bind }
    }

    pub(crate) fn replace_iterinstrs_bound(
        &self,
        iter_instrs: Vec<ol::InstrIter>,
    ) -> Vec<ol::InstrIter> {
        iter_instrs
            .into_iter()
            .map(|iter_instr| self.replace_iterinstr_bound(iter_instr))
            .collect()
    }
}
