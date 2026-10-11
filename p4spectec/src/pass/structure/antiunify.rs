//! Find shared input templates, then bind each original input to its template
//!
//! Inputs `(x, true)` and `(false, true)` share `(x', true)`.
//! Their paths start with `let x = x'` and `let false = x'`, respectively.
//!
//! Inputs -> shared template -> binding premises prepended to each path

use std::rc::Rc;

use crate::lang::{
    common::ds::{map::IdMap, set::IdSet},
    data::notation::tree as notation,
    traits::{
        at::At,
        eq::SyntaxEq,
        free::{FreeIds, FreeVars},
    },
};

use crate::lang::il;

use crate::lang::al::ast::*;

// == Unification environment

/// Maps original identifiers to sets of fresh unified identifiers.
#[derive(Default)]
struct UEnv {
    ids: IdMap<IdSet>,
}

impl UEnv {
    /// Checks whether `id` is a unified identifier from anti-unification.
    fn unified(&self, id: &Id) -> bool {
        self.ids
            .iter()
            .any(|(_, ids_unified)| ids_unified.contains(id))
    }

    fn insert(&mut self, id: Id, id_unified: Id) {
        if let Some(ids_unified) = self.ids.get_mut(&id) {
            ids_unified.insert(id_unified);
        } else {
            self.ids.insert(id, IdSet::from([id_unified]));
        }
    }

    /// Unions the unified identifiers collected at each input position.
    fn extend(&mut self, uenv: Self) {
        for (id, ids_unified) in uenv.ids.iter() {
            if let Some(ids) = self.ids.get_mut(id) {
                ids.append(ids_unified.clone());
            } else {
                self.ids.insert(id.clone(), ids_unified.clone());
            }
        }
    }
}

// == Arity checks

fn check_arity(expected: usize, actual: usize) {
    // Elaboration fixes input shapes; binding and templates preserve positions
    assert_eq!(expected, actual, "validated input arity");
}

// == Populating expression templates

// - Expression template

/// Emits the let premises binding one input to the shared template.
fn populate_exp_template(uenv: &UEnv, exp_template: &Exp, exp: &Exp) -> Vec<Prem> {
    if exp_template.syntax_eq(exp) {
        return vec![];
    }
    match (&exp_template.node, &exp.node) {
        // A unified identifier binds the whole input at this position
        (ExpKind::Id(id_template), _) if uenv.unified(id_template) => {
            let prem = populate_id_exp_template(exp_template, exp);
            vec![prem]
        }
        (ExpKind::Tuple(exps_template), ExpKind::Tuple(exps)) => {
            populate_exps_templates(uenv, exps_template.iter(), exps.iter())
        }
        (ExpKind::Case(not_exp_template), ExpKind::Case(not_exp))
            if not_exp_template.eq_mixop(not_exp) =>
        {
            populate_exps_templates(uenv, not_exp_template.args().iter(), not_exp.args().iter())
        }
        (ExpKind::Str(exp_fields_template), ExpKind::Str(exp_fields)) => {
            let exps_template = exp_fields_template.iter().map(|ExpField { exp, .. }| exp);
            let exps = exp_fields.iter().map(|ExpField { exp, .. }| exp);
            populate_exps_templates(uenv, exps_template, exps)
        }
        // Iterated inputs bind under their iteration
        (
            ExpKind::Iter(exp_body_template, exp_iter_template),
            ExpKind::Iter(exp_body, exp_iter),
        ) if exp_iter_template.iter.syntax_eq(&exp_iter.iter) => {
            let prem = populate_iter_exp_template(
                exp_body_template,
                exp_iter_template,
                exp_body,
                exp_iter,
            );
            vec![prem]
        }
        // Anti-unification preserves matching shapes and records every fresh leaf
        _ => unreachable!("input matches its anti-unified template"),
    }
}

fn populate_exps_templates<'a>(
    uenv: &UEnv,
    exps_template: impl ExactSizeIterator<Item = &'a Exp>,
    exps: impl ExactSizeIterator<Item = &'a Exp>,
) -> Vec<Prem> {
    check_arity(exps_template.len(), exps.len());
    let mut prems = vec![];
    for (exp_template, exp) in exps_template.zip(exps) {
        let prems_exp = populate_exp_template(uenv, exp_template, exp);
        prems.extend(prems_exp);
    }
    prems
}

// - Identifier expression

/// Builds `let exp = template`.
fn populate_id_exp_template(exp_template: &Exp, exp: &Exp) -> Prem {
    let span = [&exp, &exp_template].at();
    let prem = LetPrem { exp_l: exp.clone(), exp_r: exp_template.clone() };
    let prem_kind = PremKind::Let(prem);
    crate::phrase! {node: prem_kind, span: span}
}

// - Iterated expression

/// Builds `let exp = template` iterated over the variables of both sides.
fn populate_iter_exp_template(
    exp_template: &Exp,
    exp_iter_template: &ExpIter,
    exp: &Exp,
    exp_iter: &ExpIter,
) -> Prem {
    let ExpIter { iter, vars: vars_template } = exp_iter_template;
    let ExpIter { vars, .. } = exp_iter;
    let prem = populate_id_exp_template(exp_template, exp);
    let span = prem.span.clone();
    let prem_iter =
        PremIter { iter: *iter, vars_bound: vars_template.clone(), vars_bind: vars.clone() };
    let prem = Box::new(prem);
    let prem = IterPrem { prem, prem_iter };
    let prem_kind = PremKind::Iter(prem);
    crate::phrase! {node: prem_kind, span: span}
}

// == Anti-unification of expressions

// - Expression

/// Overlaps an input with the template, freshening identifiers on mismatch.
fn antiunify_exp(frees: &mut IdSet, uenv: &mut UEnv, exp_template: &Exp, exp: &Exp) -> Exp {
    // Identical inputs need no generalization
    if exp_template.syntax_eq(exp) {
        return exp_template.clone();
    }
    let exp_kind_template = match (&exp_template.node, &exp.node) {
        // An identifier on either side becomes a fresh unified identifier
        (ExpKind::Id(id_template), _) => antiunify_id_exp(frees, uenv, id_template),
        (_, ExpKind::Id(id)) => antiunify_fresh_id_exp(frees, uenv, id),
        (ExpKind::Tuple(exps_template), ExpKind::Tuple(exps)) => {
            let exps_template = antiunify_exps(frees, uenv, exps_template, exps);
            ExpKind::Tuple(exps_template)
        }
        (ExpKind::Case(not_exp_template), ExpKind::Case(not_exp))
            if not_exp_template.eq_mixop(not_exp) =>
        {
            antiunify_case_exp(frees, uenv, not_exp_template, not_exp)
        }
        (ExpKind::Str(exp_fields_template), ExpKind::Str(exp_fields)) => {
            antiunify_str_exp(frees, uenv, exp_fields_template, exp_fields)
        }
        (
            ExpKind::Iter(exp_body_template, exp_iter_template),
            ExpKind::Iter(exp_body, exp_iter),
        ) if exp_iter_template.iter.syntax_eq(&exp_iter.iter) => antiunify_iter_exp(
            frees,
            uenv,
            exp_body_template,
            exp_iter_template,
            exp_body,
            exp_iter,
        ),
        // Different shapes cannot be anti-unified
        _ => {
            // Binding analysis replaces incompatible pattern shapes with variables
            unreachable!("validated input pattern shapes");
        }
    };
    crate::note_phrase! {
        node: exp_kind_template,
        note: exp_template.note.clone(),
        span: exp_template.span.clone()
    }
}

fn antiunify_exps(
    frees: &mut IdSet,
    uenv: &mut UEnv,
    exps_template: &[Exp],
    exps: &[Exp],
) -> Vec<Exp> {
    check_arity(exps_template.len(), exps.len());
    exps_template
        .iter()
        .zip(exps)
        .map(|(exp_template, exp)| antiunify_exp(frees, uenv, exp_template, exp))
        .collect()
}

// - Identifier expression

/// Keeps an already unified template identifier, otherwise freshens it.
fn antiunify_id_exp(frees: &mut IdSet, uenv: &mut UEnv, id_template: &Id) -> ExpKind {
    if uenv.unified(id_template) {
        ExpKind::Id(id_template.clone())
    } else {
        antiunify_fresh_id_exp(frees, uenv, id_template)
    }
}

// - Fresh identifier expression

/// Introduces a fresh identifier and registers it under the original name.
fn antiunify_fresh_id_exp(frees: &mut IdSet, uenv: &mut UEnv, id: &Id) -> ExpKind {
    let id_fresh = il::fresh::id(frees, id);
    frees.insert(id_fresh.clone());
    uenv.insert(id.clone(), id_fresh.clone());
    ExpKind::Id(id_fresh)
}

// - Case expression

/// Overlaps the arguments of two case expressions with the same mixfix.
fn antiunify_case_exp(
    frees: &mut IdSet,
    uenv: &mut UEnv,
    not_exp_template: &NotExp,
    not_exp: &NotExp,
) -> ExpKind {
    let mut exps_unified = vec![];
    for (exp_template, exp) in not_exp_template.args().iter().zip(not_exp.args()) {
        let exp_unified = antiunify_exp(frees, uenv, exp_template, exp);
        exps_unified.push(exp_unified);
    }
    let not_exp_template = notation::Mixfix::new(Rc::clone(not_exp_template.mixop()), exps_unified)
        .expect("matching mixfix shapes have equal argument counts");
    let not_exp_template = Box::new(not_exp_template);
    ExpKind::Case(not_exp_template)
}

// - Record expression

/// Overlaps the fields of two struct expressions with the same atoms.
fn antiunify_str_exp(
    frees: &mut IdSet,
    uenv: &mut UEnv,
    exp_fields_template: &[ExpField],
    exp_fields: &[ExpField],
) -> ExpKind {
    check_arity(exp_fields_template.len(), exp_fields.len());
    // Field atoms must agree pairwise
    if !exp_fields_template.iter().zip(exp_fields).all(
        |(ExpField { atom: atom_template, .. }, ExpField { atom, .. })| {
            atom_template.syntax_eq(atom)
        },
    ) {
        // Elaboration orders record fields by their common declared type
        unreachable!("validated record field order");
    }
    let mut exp_fields_unified = vec![];
    for (ExpField { atom: atom_template, exp: exp_template }, ExpField { exp, .. }) in
        exp_fields_template.iter().zip(exp_fields)
    {
        let exp_template = antiunify_exp(frees, uenv, exp_template, exp);
        exp_fields_unified.push(ExpField { atom: atom_template.clone(), exp: exp_template });
    }
    ExpKind::Str(exp_fields_unified)
}

// - Iterated expression

/// Overlaps iterated expressions and maps their variables to unified names.
fn antiunify_iter_exp(
    frees: &mut IdSet,
    uenv: &mut UEnv,
    exp_template: &Exp,
    exp_iter_template: &ExpIter,
    exp: &Exp,
    exp_iter: &ExpIter,
) -> ExpKind {
    let ExpIter { iter, vars: vars_template } = exp_iter_template;
    let ExpIter { vars, .. } = exp_iter;
    let exp_template = antiunify_exp(frees, uenv, exp_template, exp);
    // Match both identifier and iteration depth in the resulting body
    let vars_free = exp_template.free_vars();
    let mut vars_unified = vec![];
    for var in vars_template.iter().chain(vars) {
        let Var { id, typ, iters } = var;
        let mut ids_unified = uenv.ids.get(id).cloned().unwrap_or_default();
        // Linear input bindings cannot retain an original and its fresh replacement
        if ids_unified.is_empty() {
            ids_unified.insert(id.clone());
        }
        // One original name can produce multiple unifiers within a tuple
        for id_unified in ids_unified.iter() {
            let var_unified =
                Var { id: id_unified.clone(), typ: typ.clone(), iters: iters.clone() };
            if vars_free.iter().any(|var| var.syntax_eq(&var_unified))
                && !vars_unified
                    .iter()
                    .any(|var: &Var| var.syntax_eq(&var_unified))
            {
                vars_unified.push(var_unified);
            }
        }
    }
    let exp_template = Box::new(exp_template);
    let exp_iter = ExpIter { iter: *iter, vars: vars_unified };
    ExpKind::Iter(exp_template, exp_iter)
}

// - Expressions across matches

/// Builds one template per input position across all rule matches.
fn antiunify_exps_across_matches(mut frees: IdSet, exps_by_match: &[&[Exp]]) -> (UEnv, Vec<Exp>) {
    let Some((exps_head, exps_tail)) = exps_by_match.split_first() else {
        return (UEnv::default(), vec![]);
    };
    for exps in exps_tail {
        check_arity(exps_head.len(), exps.len());
    }
    let mut uenv_acc = UEnv::default();
    let mut exps_template = vec![];
    // Share fresh names across input positions; keep a separate map for each
    for (num_idx, exp_head) in exps_head.iter().enumerate() {
        let mut uenv = UEnv::default();
        let mut exp_template = exp_head.clone();
        for exps in exps_tail {
            exp_template = antiunify_exp(&mut frees, &mut uenv, &exp_template, &exps[num_idx]);
        }
        uenv_acc.extend(uenv);
        exps_template.push(exp_template);
    }
    (uenv_acc, exps_template)
}

// == Populating argument templates

// - Argument template

/// Emits the let premises binding an argument to its template.
fn populate_arg_template(uenv: &UEnv, arg_template: &Arg, arg: &Arg) -> Vec<Prem> {
    match (&arg_template.node, &arg.node) {
        (ArgKind::Exp(exp_template), ArgKind::Exp(exp)) => {
            populate_exp_template(uenv, exp_template, exp)
        }
        (ArgKind::Def(id_template), ArgKind::Def(id)) if id_template.syntax_eq(id) => vec![],
        _ => {
            // Elaboration checks argument kinds and defining function names
            unreachable!("validated argument kinds and function names")
        }
    }
}

fn populate_args_templates(uenv: &UEnv, args_template: &[Arg], args: &[Arg]) -> Vec<Prem> {
    check_arity(args_template.len(), args.len());
    let mut prems = vec![];
    for (arg_template, arg) in args_template.iter().zip(args) {
        let prems_arg = populate_arg_template(uenv, arg_template, arg);
        prems.extend(prems_arg);
    }
    prems
}

// == Anti-unification of arguments

// - Argument

/// Overlaps an argument with its template; function arguments must agree.
fn antiunify_arg(frees: &mut IdSet, uenv: &mut UEnv, arg_template: &Arg, arg: &Arg) -> Arg {
    match (&arg_template.node, &arg.node) {
        (ArgKind::Exp(exp_template), ArgKind::Exp(exp)) => {
            let exp_template = antiunify_exp(frees, uenv, exp_template, exp);
            let exp_template = Box::new(exp_template);
            let arg_kind_template = ArgKind::Exp(exp_template);
            crate::phrase! {node: arg_kind_template, span: arg_template.span.clone()}
        }
        // Function arguments must name the same function
        (ArgKind::Def(id_template), ArgKind::Def(id)) if id_template.syntax_eq(id) => {
            arg_template.clone()
        }
        _ => {
            // Elaboration checks argument kinds and defining function names
            unreachable!("validated argument kinds and function names")
        }
    }
}

// - Arguments across clauses

/// Builds one argument template per position across all clauses.
fn antiunify_args_across_clauses(mut frees: IdSet, clauses: &[&Clause]) -> (UEnv, Vec<Arg>) {
    let Some((clause_head, clauses_tail)) = clauses.split_first() else {
        return (UEnv::default(), vec![]);
    };
    let args_head = &clause_head.node.args;
    for clause in clauses_tail {
        check_arity(args_head.len(), clause.node.args.len());
    }
    let mut uenv_acc = UEnv::default();
    let mut args_template = vec![];
    // Share fresh names across input positions; keep a separate map for each
    for (num_idx, arg_head) in args_head.iter().enumerate() {
        let mut uenv = UEnv::default();
        let mut arg_template = arg_head.clone();
        for clause in clauses_tail {
            arg_template =
                antiunify_arg(&mut frees, &mut uenv, &arg_template, &clause.node.args[num_idx]);
        }
        uenv_acc.extend(uenv);
        args_template.push(arg_template);
    }
    (uenv_acc, args_template)
}

// == Anti-unification of rule matches

/// Anti-unifies rule inputs into a template with per-group bindings.
pub(super) fn antiunify_rule_matches(
    frees: IdSet,
    exps_by_rule_group: &[Vec<Exp>],
    exps_else: Option<&[Exp]>,
) -> (Vec<Exp>, Vec<Vec<Prem>>, Option<Vec<Prem>>) {
    // The otherwise group joins the matches for the template
    let exps_by_match = exps_by_rule_group
        .iter()
        .map(Vec::as_slice)
        .chain(exps_else)
        .collect::<Vec<_>>();
    let (uenv, exps_template) = antiunify_exps_across_matches(frees, &exps_by_match);
    // Each group binds its inputs to the template
    let prems_by_rule_group = exps_by_rule_group
        .iter()
        .map(|exps| populate_exps_templates(&uenv, exps_template.iter(), exps.iter()))
        .collect();
    let prems_else =
        exps_else.map(|exps| populate_exps_templates(&uenv, exps_template.iter(), exps.iter()));
    (exps_template, prems_by_rule_group, prems_else)
}

// == Anti-unification of clauses

/// Prepends a clause's template bindings to its own premises.
fn populate_clause(uenv: &UEnv, args_template: &[Arg], clause: Clause) -> (Vec<Prem>, Exp) {
    let clause_kind = clause.node;
    let ClauseKind { args, exp, prems } = clause_kind;
    let mut prems_template = populate_args_templates(uenv, args_template, &args);
    prems_template.extend(prems);
    (prems_template, exp)
}

/// Anti-unifies clause arguments into a template with per-clause bindings.
#[expect(
    clippy::type_complexity,
    reason = "Destructured once per call site; a named result type would add no meaning"
)]
pub(super) fn antiunify_clauses(
    clauses: Vec<Clause>,
    clause_else: Option<Clause>,
) -> (Vec<Arg>, Vec<(Vec<Prem>, Exp)>, Option<(Vec<Prem>, Exp)>) {
    let clauses_all = clauses.iter().chain(clause_else.iter()).collect::<Vec<_>>();
    let mut frees = IdSet::new();
    for clause in &clauses_all {
        clause.free_ids_into(&mut frees);
    }
    let (uenv, args_template) = antiunify_args_across_clauses(frees, &clauses_all);
    let paths = clauses
        .into_iter()
        .map(|clause| populate_clause(&uenv, &args_template, clause))
        .collect();
    let path_else = clause_else.map(|clause| populate_clause(&uenv, &args_template, clause));
    (args_template, paths, path_else)
}
