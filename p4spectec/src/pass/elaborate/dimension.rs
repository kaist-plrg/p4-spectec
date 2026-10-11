//! Dimension analysis
//!
//! For each rule or clause, collect the dimension of all occurrences of every
//! identifier. The minimal dimension is the bound dimension of the
//! identifier in the rule or clause.
//!
//! ```text
//! -- if n_x* = [ 1, 1 ] ;; n_x : [ n_x* ]
//! -- if n_y = 1         ;; n_y : [ n_y ]
//! -- if (n_x = n_y)*    ;; n_x : [ n_x* ], n_y : [ n_y* ]
//!
//! Overall, n_x : [ n_x* ] and n_y : [ n_y, n_y* ]
//! Therefore, n_x : n_x* and n_y : n_y
//! ```
//!
//! Annotate iteration constructs with the variables they iterate over.
//!
//! - Variables with iterated dimensions at most the bound dimension
//! - Check that iteration is non-empty
//!
//! ```text
//! -- if n_x*{n_x <- n_x*} = [ 1, 1 ]
//! -- if n_y = 1
//! -- if (n_x = n_y)*{n_x <- n_x*}
//! ```

use std::{rc::Rc, slice};

use crate::lang::{
    common::{
        Id,
        ds::map::IdMap,
        source::{Phrase, Span},
    },
    data::notation::tree::Mixop,
    traits::eq::SyntaxEq,
};

use crate::lang::il::ast;

use crate::runtime::{dim::Dim, envs::elab::VEnv};

use crate::phrase;

use super::{ElabError, error};

// == Dimension inference

// - Context for dimension analysis

/// A dimension tagged with the span of the occurrence that produced it.
type DimPhrase = Phrase<Dim>;

/// Every dimension at which each identifier occurs in one rule or clause.
#[derive(Clone, Debug, Default)]
struct DimContext(IdMap<Vec<DimPhrase>>);

impl DimContext {
    // - Occurrence collection

    /// Records one occurrence of `id` at dimension `dim`.
    fn add(&mut self, id: &Id, dim: Dim) {
        let occurrence = phrase!(node: dim, span: id.span.clone());
        if let Some(occurrences) = self.0.get_mut(id) {
            occurrences.push(occurrence);
        } else {
            self.0.insert(id.clone(), vec![occurrence]);
        }
    }

    // - Bound inference

    /// Takes the minimal dimension of each identifier as its bound dimension.
    ///
    /// Every other occurrence must contain that minimum as a sub-dimension.
    fn into_bounds(self) -> Result<VEnv, ElabError> {
        let mut bounds = VEnv::new();
        for (id, occurrences) in self.0.iter() {
            // The fewest iterations give the bound dimension
            let dim_min = occurrences
                .iter()
                .min_by_key(|occurrence| occurrence.node.iters.len())
                .expect("identifier has an occurrence");
            // Every occurrence must extend the bound dimension
            if let Some(dim_conflict) = occurrences
                .iter()
                .find(|occurrence| !dim_min.node.sub(&occurrence.node))
            {
                // Label the later source occurrence independently of bound selection
                let (dim_earlier, dim_later) = if dim_min.span.left <= dim_conflict.span.left {
                    (dim_min, dim_conflict)
                } else {
                    (dim_conflict, dim_min)
                };
                return Err(error::dim::iteration_dimension_mismatch(
                    id,
                    &dim_later.node,
                    &dim_later.span,
                    &dim_earlier.node,
                    &dim_earlier.span,
                ));
            }
            bounds.insert(id.clone(), dim_min.node.clone());
        }
        Ok(bounds)
    }
}

// - Expression inference

/// Collects identifier occurrences of an expression under its iterations.
fn infer_exp(dim_ctx: &mut DimContext, exp: &ast::Exp, iters: &[ast::Iter]) {
    match &exp.node {
        // Literals mention no identifiers
        ast::ExpKind::Bool(_) | ast::ExpKind::Num(_) | ast::ExpKind::Text(_) => {}
        // An identifier occurs at its own type under the enclosing iterations
        ast::ExpKind::Id(id) => {
            let typ = phrase!(node: exp.note.as_ref().clone(), span: exp.span.clone());
            dim_ctx.add(id, Dim::new(typ, iters.to_vec()));
        }
        // Unary forms collect from the operand
        ast::ExpKind::Un(_, _, exp_inner)
        | ast::ExpKind::UpCast(_, exp_inner)
        | ast::ExpKind::DownCast(_, exp_inner)
        | ast::ExpKind::Sub(exp_inner, _, _)
        | ast::ExpKind::Match(exp_inner, _)
        | ast::ExpKind::Len(exp_inner)
        | ast::ExpKind::Dot(exp_inner, _) => infer_exp(dim_ctx, exp_inner, iters),
        // Binary forms collect from both operands
        ast::ExpKind::Bin(_, _, exp_l, exp_r)
        | ast::ExpKind::Cmp(_, _, exp_l, exp_r)
        | ast::ExpKind::Cons(exp_l, exp_r)
        | ast::ExpKind::Cat(exp_l, exp_r)
        | ast::ExpKind::Mem(exp_l, exp_r)
        | ast::ExpKind::Idx(exp_l, exp_r) => {
            infer_exp(dim_ctx, exp_l, iters);
            infer_exp(dim_ctx, exp_r, iters);
        }
        // Tuple or list: every element
        ast::ExpKind::Tuple(exps) | ast::ExpKind::List(exps) => {
            infer_exps(dim_ctx, exps, iters);
        }
        // Case: the arguments
        ast::ExpKind::Case(not_exp) => infer_not_exp(dim_ctx, not_exp, iters),
        // Struct: the fields
        ast::ExpKind::Str(fields) => {
            for ast::ExpField { exp, .. } in fields {
                infer_exp(dim_ctx, exp, iters);
            }
        }
        // Option: the payload, if any
        ast::ExpKind::Opt(exp_inner) => {
            if let Some(exp_inner) = exp_inner {
                infer_exp(dim_ctx, exp_inner, iters);
            }
        }
        // Slice: base, index, and length
        ast::ExpKind::Slice(exp_base, exp_idx, exp_len) => {
            infer_exp(dim_ctx, exp_base, iters);
            infer_exp(dim_ctx, exp_idx, iters);
            infer_exp(dim_ctx, exp_len, iters);
        }
        // Update: base, path, and field
        ast::ExpKind::Upd(exp_base, path, exp_field) => {
            infer_exp(dim_ctx, exp_base, iters);
            infer_path(dim_ctx, path, iters);
            infer_exp(dim_ctx, exp_field, iters);
        }
        // Call: the arguments
        ast::ExpKind::Call(_, _, args) => infer_args(dim_ctx, args, iters),
        // The innermost iteration comes first
        ast::ExpKind::Iter(exp_inner, ast::ExpIter { iter, .. }) => {
            let mut iters_inner = Vec::with_capacity(iters.len() + 1);
            iters_inner.push(*iter);
            iters_inner.extend_from_slice(iters);
            infer_exp(dim_ctx, exp_inner, &iters_inner);
        }
    }
}

fn infer_exps(dim_ctx: &mut DimContext, exps: &[ast::Exp], iters: &[ast::Iter]) {
    for exp in exps {
        infer_exp(dim_ctx, exp, iters);
    }
}

// - Notation expression inference

fn infer_not_exp(dim_ctx: &mut DimContext, not_exp: &ast::NotExp, iters: &[ast::Iter]) {
    for exp in not_exp.args() {
        infer_exp(dim_ctx, exp, iters);
    }
}

// - Path inference

fn infer_path(dim_ctx: &mut DimContext, path: &ast::Path, iters: &[ast::Iter]) {
    match &path.node {
        ast::PathKind::Root => {}
        ast::PathKind::Idx(path_inner, exp) => {
            infer_path(dim_ctx, path_inner, iters);
            infer_exp(dim_ctx, exp, iters);
        }
        ast::PathKind::Slice(path_inner, exp_idx, exp_len) => {
            infer_path(dim_ctx, path_inner, iters);
            infer_exp(dim_ctx, exp_idx, iters);
            infer_exp(dim_ctx, exp_len, iters);
        }
        ast::PathKind::Dot(path_inner, _) => infer_path(dim_ctx, path_inner, iters),
    }
}

// - Argument inference

fn infer_arg(dim_ctx: &mut DimContext, arg: &ast::Arg, iters: &[ast::Iter]) {
    if let ast::ArgKind::Exp(exp) = &arg.node {
        infer_exp(dim_ctx, exp, iters);
    }
}

fn infer_args(dim_ctx: &mut DimContext, args: &[ast::Arg], iters: &[ast::Iter]) {
    for arg in args {
        infer_arg(dim_ctx, arg, iters);
    }
}

// - Premise inference

/// Collects occurrences in a premise, rejecting pre-annotated iterations.
fn infer_prem(
    dim_ctx: &mut DimContext,
    prem: &ast::Prem,
    iters: &[ast::Iter],
) -> Result<(), ElabError> {
    match &prem.node {
        ast::PremKind::Rule(rule) => infer_not_exp(dim_ctx, &rule.not_exp, iters),
        ast::PremKind::If(if_prem) => infer_exp(dim_ctx, &if_prem.exp, iters),
        ast::PremKind::IfHold(if_prem) => infer_not_exp(dim_ctx, &if_prem.not_exp, iters),
        ast::PremKind::IfNotHold(if_prem) => infer_not_exp(dim_ctx, &if_prem.not_exp, iters),
        ast::PremKind::Iter(iter_prem) => {
            // Iterated premises must arrive without binding annotations
            if !iter_prem.prem_iter.vars_bound.is_empty()
                || !iter_prem.prem_iter.vars_bind.is_empty()
            {
                return Err(error::dim::iteration_annotation_invalid(&prem.span, "premise"));
            }
            let mut iters_inner = Vec::with_capacity(iters.len() + 1);
            iters_inner.push(iter_prem.prem_iter.iter);
            iters_inner.extend_from_slice(iters);
            infer_prem(dim_ctx, &iter_prem.prem, &iters_inner)?;
        }
        ast::PremKind::Debug(debug) => infer_exp(dim_ctx, &debug.exp, iters),
    }
    Ok(())
}

fn infer_prems(dim_ctx: &mut DimContext, prems: &[ast::Prem]) -> Result<(), ElabError> {
    for prem in prems {
        infer_prem(dim_ctx, prem, &[])?;
    }
    Ok(())
}

// - Rule inference

/// Collects occurrences over the conclusion and premises of a rule.
fn infer_rule(rule: &ast::Rule) -> Result<DimContext, ElabError> {
    let mut dim_ctx = DimContext::default();
    infer_not_exp(&mut dim_ctx, &rule.node.not_exp, &[]);
    infer_prems(&mut dim_ctx, &rule.node.prems)?;
    Ok(dim_ctx)
}

// - Clause inference

/// Collects occurrences over the arguments, premises, and body of a clause.
fn infer_clause(clause: &ast::Clause) -> Result<DimContext, ElabError> {
    let mut dim_ctx = DimContext::default();
    infer_args(&mut dim_ctx, &clause.node.args, &[]);
    infer_prems(&mut dim_ctx, &clause.node.prems)?;
    infer_exp(&mut dim_ctx, &clause.node.exp, &[]);
    Ok(dim_ctx)
}

// - Table row inference

/// Collects occurrences over the pattern and body of a table row.
fn infer_table_row(row: &ast::TableRow) -> DimContext {
    let mut dim_ctx = DimContext::default();
    infer_args(&mut dim_ctx, &row.node.args, &[]);
    infer_exp(&mut dim_ctx, &row.node.exp, &[]);
    dim_ctx
}

// == Dimension annotation

// - Occurrences

/// Identifiers occurring in a sub-term, with the dimension built up so far.
struct Occurrences(VEnv);

impl Occurrences {
    fn new() -> Self {
        Self(VEnv::new())
    }

    /// A single identifier occurrence at dimension zero.
    fn singleton(id: &Id, typ: ast::Typ) -> Self {
        let mut occurs = VEnv::new();
        occurs.insert(id.clone(), Dim::new(typ, vec![]));
        Self(occurs)
    }

    /// Merges occurrences, keeping the shallower dimension when shared.
    fn union(mut self, occurs_other: Self) -> Result<Self, ElabError> {
        for (id, dim_other) in occurs_other.0.iter() {
            if let Some(dim) = self.0.get(id) {
                // A shared identifier must have the same type on both sides
                if !dim.typ.syntax_eq(&dim_other.typ) {
                    return Err(error::dim::iteration_identifier_type_mismatch(
                        id,
                        &dim.typ,
                        &dim_other.typ,
                    ));
                }
                // Keep the occurrence with fewer iterations
                if dim_other.iters.len() <= dim.iters.len() {
                    self.0.insert(id.clone(), dim_other.clone());
                }
            } else {
                self.0.insert(id.clone(), dim_other.clone());
            }
        }
        Ok(self)
    }

    /// Extends the dimension of the iterated variables by one more `iter`.
    fn iterate(mut self, vars: &[ast::Var], iter: ast::Iter) -> Self {
        for var in vars {
            let mut iters = var.iters.clone();
            iters.push(iter);
            self.0
                .insert(var.id.clone(), Dim::new(var.typ.clone(), iters));
        }
        self
    }

    fn iter(&self) -> impl Iterator<Item = (&Id, &Dim)> {
        self.0.iter()
    }
}

// - Iteration variables

/// Selects the occurring variables that supply values for `iter`.
///
/// A variable is iterated
/// when adding `iter` to its current dimension
/// still fits within its bound dimension.
fn collect_iter_vars(bounds: &VEnv, occurs: &Occurrences, iter: ast::Iter) -> Vec<ast::Var> {
    occurs
        .iter()
        .filter_map(|(id, dim)| {
            let dim_bound = bounds
                .get(id)
                .expect("occurring variable has inferred bound");
            // Iterated once more, the variable must stay within its bound
            if dim.clone().add_iter(iter).sub(dim_bound) {
                Some(ast::Var { id: id.clone(), typ: dim.typ.clone(), iters: dim.iters.clone() })
            } else {
                None
            }
        })
        .collect()
}

// - Expression annotation

/// Annotates the iterations inside an expression and returns its occurrences.
fn annotate_exp(bounds: &VEnv, exp: &mut ast::Exp) -> Result<Occurrences, ElabError> {
    let span = &exp.span;
    let typ_kind = exp.note.as_ref();
    match &mut exp.node {
        ast::ExpKind::Bool(_) => Ok(annotate_bool_exp()),
        ast::ExpKind::Num(_) => Ok(annotate_num_exp()),
        ast::ExpKind::Text(_) => Ok(annotate_text_exp()),
        ast::ExpKind::Id(id) => Ok(annotate_id_exp(span, typ_kind, id)),
        ast::ExpKind::Un(_, _, exp_inner) => annotate_un_exp(bounds, exp_inner),
        ast::ExpKind::Bin(_, _, exp_l, exp_r) => annotate_bin_exp(bounds, exp_l, exp_r),
        ast::ExpKind::Cmp(_, _, exp_l, exp_r) => annotate_cmp_exp(bounds, exp_l, exp_r),
        ast::ExpKind::UpCast(_, exp_inner) => annotate_upcast_exp(bounds, exp_inner),
        ast::ExpKind::DownCast(_, exp_inner) => annotate_downcast_exp(bounds, exp_inner),
        ast::ExpKind::Sub(exp_inner, _, _) => annotate_sub_exp(bounds, exp_inner),
        ast::ExpKind::Match(exp_inner, _) => annotate_match_exp(bounds, exp_inner),
        ast::ExpKind::Tuple(exps) => annotate_tuple_exp(bounds, exps),
        ast::ExpKind::Case(not_exp) => annotate_case_exp(bounds, not_exp),
        ast::ExpKind::Str(fields) => annotate_str_exp(bounds, fields),
        ast::ExpKind::Opt(exp_inner) => annotate_opt_exp(bounds, exp_inner.as_deref_mut()),
        ast::ExpKind::List(exps) => annotate_list_exp(bounds, exps),
        ast::ExpKind::Cons(exp_l, exp_r) => annotate_cons_exp(bounds, exp_l, exp_r),
        ast::ExpKind::Cat(exp_l, exp_r) => annotate_cat_exp(bounds, exp_l, exp_r),
        ast::ExpKind::Mem(exp_l, exp_r) => annotate_mem_exp(bounds, exp_l, exp_r),
        ast::ExpKind::Len(exp_inner) => annotate_len_exp(bounds, exp_inner),
        ast::ExpKind::Dot(exp_inner, _) => annotate_dot_exp(bounds, exp_inner),
        ast::ExpKind::Idx(exp_l, exp_r) => annotate_idx_exp(bounds, exp_l, exp_r),
        ast::ExpKind::Slice(exp_base, exp_idx, exp_len) => {
            annotate_slice_exp(bounds, exp_base, exp_idx, exp_len)
        }
        ast::ExpKind::Upd(exp_base, path, exp_field) => {
            annotate_upd_exp(bounds, exp_base, path, exp_field)
        }
        ast::ExpKind::Call(_, _, args) => annotate_call_exp(bounds, args),
        ast::ExpKind::Iter(exp_inner, ast::ExpIter { iter, vars }) => {
            annotate_iter_exp(bounds, span, exp_inner, *iter, vars)
        }
    }
}

fn annotate_exps(bounds: &VEnv, exps: &mut [ast::Exp]) -> Result<Occurrences, ElabError> {
    let mut occurs = Occurrences::new();
    for exp in exps {
        let occurs_exp = annotate_exp(bounds, exp)?;
        occurs = occurs.union(occurs_exp)?;
    }
    Ok(occurs)
}

// - Boolean expressions

fn annotate_bool_exp() -> Occurrences {
    Occurrences::new()
}

// - Numeric expressions

fn annotate_num_exp() -> Occurrences {
    Occurrences::new()
}

// - Text expressions

fn annotate_text_exp() -> Occurrences {
    Occurrences::new()
}

// - Identifier expressions

fn annotate_id_exp(span: &Span, typ_kind: &ast::TypKind, id: &Id) -> Occurrences {
    let typ = phrase!(node: typ_kind.clone(), span: span.clone());
    Occurrences::singleton(id, typ)
}

// - Unary expressions

fn annotate_un_exp(bounds: &VEnv, exp_inner: &mut ast::Exp) -> Result<Occurrences, ElabError> {
    annotate_exp(bounds, exp_inner)
}

// - Binary expressions

fn annotate_bin_exp(
    bounds: &VEnv,
    exp_l: &mut ast::Exp,
    exp_r: &mut ast::Exp,
) -> Result<Occurrences, ElabError> {
    let occurs_l = annotate_exp(bounds, exp_l)?;
    let occurs_r = annotate_exp(bounds, exp_r)?;
    occurs_l.union(occurs_r)
}

// - Comparison expressions

fn annotate_cmp_exp(
    bounds: &VEnv,
    exp_l: &mut ast::Exp,
    exp_r: &mut ast::Exp,
) -> Result<Occurrences, ElabError> {
    let occurs_l = annotate_exp(bounds, exp_l)?;
    let occurs_r = annotate_exp(bounds, exp_r)?;
    occurs_l.union(occurs_r)
}

// - Upcast expressions

fn annotate_upcast_exp(bounds: &VEnv, exp_inner: &mut ast::Exp) -> Result<Occurrences, ElabError> {
    annotate_exp(bounds, exp_inner)
}

// - Downcast expressions

fn annotate_downcast_exp(
    bounds: &VEnv,
    exp_inner: &mut ast::Exp,
) -> Result<Occurrences, ElabError> {
    annotate_exp(bounds, exp_inner)
}

// - Subtype expressions

fn annotate_sub_exp(bounds: &VEnv, exp_inner: &mut ast::Exp) -> Result<Occurrences, ElabError> {
    annotate_exp(bounds, exp_inner)
}

// - Match expressions

fn annotate_match_exp(bounds: &VEnv, exp_inner: &mut ast::Exp) -> Result<Occurrences, ElabError> {
    annotate_exp(bounds, exp_inner)
}

// - Tuple expressions

fn annotate_tuple_exp(bounds: &VEnv, exps: &mut [ast::Exp]) -> Result<Occurrences, ElabError> {
    annotate_exps(bounds, exps)
}

// - Case expressions

fn annotate_case_exp(bounds: &VEnv, not_exp: &mut ast::NotExp) -> Result<Occurrences, ElabError> {
    annotate_not_exp(bounds, not_exp)
}

// - Struct expressions

fn annotate_str_exp(bounds: &VEnv, fields: &mut [ast::ExpField]) -> Result<Occurrences, ElabError> {
    let mut occurs = Occurrences::new();
    for ast::ExpField { exp, .. } in fields {
        let occurs_exp = annotate_exp(bounds, exp)?;
        occurs = occurs.union(occurs_exp)?;
    }
    Ok(occurs)
}

// - Optional expressions

fn annotate_opt_exp(
    bounds: &VEnv,
    exp_inner: Option<&mut ast::Exp>,
) -> Result<Occurrences, ElabError> {
    exp_inner.map_or_else(|| Ok(Occurrences::new()), |exp| annotate_exp(bounds, exp))
}

// - List expressions

fn annotate_list_exp(bounds: &VEnv, exps: &mut [ast::Exp]) -> Result<Occurrences, ElabError> {
    annotate_exps(bounds, exps)
}

// - List construction expressions

fn annotate_cons_exp(
    bounds: &VEnv,
    exp_l: &mut ast::Exp,
    exp_r: &mut ast::Exp,
) -> Result<Occurrences, ElabError> {
    let occurs_l = annotate_exp(bounds, exp_l)?;
    let occurs_r = annotate_exp(bounds, exp_r)?;
    occurs_l.union(occurs_r)
}

// - Concatenation expressions

fn annotate_cat_exp(
    bounds: &VEnv,
    exp_l: &mut ast::Exp,
    exp_r: &mut ast::Exp,
) -> Result<Occurrences, ElabError> {
    let occurs_l = annotate_exp(bounds, exp_l)?;
    let occurs_r = annotate_exp(bounds, exp_r)?;
    occurs_l.union(occurs_r)
}

// - Membership expressions

fn annotate_mem_exp(
    bounds: &VEnv,
    exp_l: &mut ast::Exp,
    exp_r: &mut ast::Exp,
) -> Result<Occurrences, ElabError> {
    let occurs_l = annotate_exp(bounds, exp_l)?;
    let occurs_r = annotate_exp(bounds, exp_r)?;
    occurs_l.union(occurs_r)
}

// - Length expressions

fn annotate_len_exp(bounds: &VEnv, exp_inner: &mut ast::Exp) -> Result<Occurrences, ElabError> {
    annotate_exp(bounds, exp_inner)
}

// - Field access expressions

fn annotate_dot_exp(bounds: &VEnv, exp_inner: &mut ast::Exp) -> Result<Occurrences, ElabError> {
    annotate_exp(bounds, exp_inner)
}

// - Index expressions

fn annotate_idx_exp(
    bounds: &VEnv,
    exp_l: &mut ast::Exp,
    exp_r: &mut ast::Exp,
) -> Result<Occurrences, ElabError> {
    let occurs_l = annotate_exp(bounds, exp_l)?;
    let occurs_r = annotate_exp(bounds, exp_r)?;
    occurs_l.union(occurs_r)
}

// - Slice expressions

fn annotate_slice_exp(
    bounds: &VEnv,
    exp_base: &mut ast::Exp,
    exp_idx: &mut ast::Exp,
    exp_len: &mut ast::Exp,
) -> Result<Occurrences, ElabError> {
    let occurs_base = annotate_exp(bounds, exp_base)?;
    let occurs_idx = annotate_exp(bounds, exp_idx)?;
    let occurs_len = annotate_exp(bounds, exp_len)?;
    let occurs = occurs_base.union(occurs_idx)?;
    occurs.union(occurs_len)
}

// - Update expressions

fn annotate_upd_exp(
    bounds: &VEnv,
    exp_base: &mut ast::Exp,
    path: &mut ast::Path,
    exp_field: &mut ast::Exp,
) -> Result<Occurrences, ElabError> {
    let occurs_base = annotate_exp(bounds, exp_base)?;
    let occurs_field = annotate_exp(bounds, exp_field)?;
    let occurs_path = annotate_path(bounds, path)?;
    let occurs = occurs_base.union(occurs_field)?;
    occurs.union(occurs_path)
}

// - Call expressions

fn annotate_call_exp(bounds: &VEnv, args: &mut [ast::Arg]) -> Result<Occurrences, ElabError> {
    annotate_args(bounds, args)
}

// - Iteration expressions

/// Fills in the source variables of an iterated expression.
fn annotate_iter_exp(
    bounds: &VEnv,
    span: &Span,
    exp_inner: &mut ast::Exp,
    iter: ast::Iter,
    vars: &mut Vec<ast::Var>,
) -> Result<Occurrences, ElabError> {
    // Iterated expressions must arrive without annotations
    if !vars.is_empty() {
        return Err(error::dim::iteration_annotation_invalid(span, "expression"));
    }
    let occurs = annotate_exp(bounds, exp_inner)?;
    // Variables that supply iteration values
    let vars_inner = collect_iter_vars(bounds, &occurs, iter);
    // An iteration needs at least one source variable
    if vars_inner.is_empty() {
        return Err(error::dim::iteration_expression_empty(span));
    }
    let occurs = occurs.iterate(&vars_inner, iter);
    *vars = vars_inner;
    Ok(occurs)
}

// - Notation expression annotation

/// Annotates the argument expressions of a notation expression.
///
/// Occurrences combine as the notation nests, left to right.
fn annotate_not_exp(bounds: &VEnv, not_exp: &mut ast::NotExp) -> Result<Occurrences, ElabError> {
    let mixop = Rc::clone(not_exp.mixop());
    annotate_mixop(bounds, &mixop, &mut not_exp.args_mut().iter_mut())
}

/// Annotates the arguments at a mixop's positions, taking them in order.
fn annotate_mixop(
    bounds: &VEnv,
    mixop: &Mixop,
    exps: &mut slice::IterMut<'_, ast::Exp>,
) -> Result<Occurrences, ElabError> {
    match mixop {
        Mixop::Arg => annotate_arg_not_exp(bounds, exps.next().expect("one argument per position")),
        Mixop::Atom(_) => Ok(annotate_atom_not_exp()),
        Mixop::Brack(_, mixop_inner, _) => annotate_brack_not_exp(bounds, mixop_inner, exps),
        Mixop::Infix(mixop_l, _, mixop_r) => annotate_infix_not_exp(bounds, mixop_l, mixop_r, exps),
        Mixop::Seq(mixops) => annotate_seq_not_exp(bounds, mixops, exps),
    }
}

// - Notation arguments

fn annotate_arg_not_exp(bounds: &VEnv, exp: &mut ast::Exp) -> Result<Occurrences, ElabError> {
    annotate_exp(bounds, exp)
}

// - Notation atoms

fn annotate_atom_not_exp() -> Occurrences {
    Occurrences::new()
}

// - Bracketed notation expressions

fn annotate_brack_not_exp(
    bounds: &VEnv,
    mixop_inner: &Mixop,
    exps: &mut slice::IterMut<'_, ast::Exp>,
) -> Result<Occurrences, ElabError> {
    annotate_mixop(bounds, mixop_inner, exps)
}

// - Infix notation expressions

fn annotate_infix_not_exp(
    bounds: &VEnv,
    mixop_l: &Mixop,
    mixop_r: &Mixop,
    exps: &mut slice::IterMut<'_, ast::Exp>,
) -> Result<Occurrences, ElabError> {
    let occurs_l = annotate_mixop(bounds, mixop_l, exps)?;
    let occurs_r = annotate_mixop(bounds, mixop_r, exps)?;
    occurs_l.union(occurs_r)
}

// - Notation sequences

fn annotate_seq_not_exp(
    bounds: &VEnv,
    mixops: &[Mixop],
    exps: &mut slice::IterMut<'_, ast::Exp>,
) -> Result<Occurrences, ElabError> {
    let mut occurs = Occurrences::new();
    for mixop in mixops {
        let occurs_exp = annotate_mixop(bounds, mixop, exps)?;
        occurs = occurs.union(occurs_exp)?;
    }
    Ok(occurs)
}

// - Path annotation

/// Annotates the index and slice expressions along a path.
fn annotate_path(bounds: &VEnv, path: &mut ast::Path) -> Result<Occurrences, ElabError> {
    match &mut path.node {
        ast::PathKind::Root => Ok(annotate_root_path()),
        ast::PathKind::Idx(path_inner, exp) => annotate_idx_path(bounds, path_inner, exp),
        ast::PathKind::Slice(path_inner, exp_idx, exp_len) => {
            annotate_slice_path(bounds, path_inner, exp_idx, exp_len)
        }
        ast::PathKind::Dot(path_inner, _) => annotate_dot_path(bounds, path_inner),
    }
}

// - Root paths

fn annotate_root_path() -> Occurrences {
    Occurrences::new()
}

// - Index paths

fn annotate_idx_path(
    bounds: &VEnv,
    path_inner: &mut ast::Path,
    exp: &mut ast::Exp,
) -> Result<Occurrences, ElabError> {
    let occurs_path = annotate_path(bounds, path_inner)?;
    let occurs_exp = annotate_exp(bounds, exp)?;
    occurs_path.union(occurs_exp)
}

// - Slice paths

fn annotate_slice_path(
    bounds: &VEnv,
    path_inner: &mut ast::Path,
    exp_idx: &mut ast::Exp,
    exp_len: &mut ast::Exp,
) -> Result<Occurrences, ElabError> {
    let occurs_path = annotate_path(bounds, path_inner)?;
    let occurs_idx = annotate_exp(bounds, exp_idx)?;
    let occurs_len = annotate_exp(bounds, exp_len)?;
    let occurs = occurs_path.union(occurs_idx)?;
    occurs.union(occurs_len)
}

// - Field paths

fn annotate_dot_path(bounds: &VEnv, path_inner: &mut ast::Path) -> Result<Occurrences, ElabError> {
    annotate_path(bounds, path_inner)
}

// - Argument annotation

fn annotate_arg(bounds: &VEnv, arg: &mut ast::Arg) -> Result<Occurrences, ElabError> {
    match &mut arg.node {
        ast::ArgKind::Exp(exp) => annotate_exp(bounds, exp),
        ast::ArgKind::Def(_) => Ok(Occurrences::new()),
    }
}

fn annotate_args(bounds: &VEnv, args: &mut [ast::Arg]) -> Result<Occurrences, ElabError> {
    let mut occurs = Occurrences::new();
    for arg in args {
        let occurs_arg = annotate_arg(bounds, arg)?;
        occurs = occurs.union(occurs_arg)?;
    }
    Ok(occurs)
}

// - Premise annotation

/// Annotates the iterations inside a premise and returns its occurrences.
fn annotate_prem(bounds: &VEnv, prem: &mut ast::Prem) -> Result<Occurrences, ElabError> {
    let span = &prem.span;
    match &mut prem.node {
        ast::PremKind::Rule(rule) => annotate_rule_prem(bounds, rule),
        ast::PremKind::If(if_prem) => annotate_if_prem(bounds, if_prem),
        ast::PremKind::IfHold(if_prem) => annotate_if_hold_prem(bounds, if_prem),
        ast::PremKind::IfNotHold(if_prem) => annotate_if_not_hold_prem(bounds, if_prem),
        ast::PremKind::Iter(iter_prem) => annotate_iter_prem(bounds, span, iter_prem),
        ast::PremKind::Debug(debug) => annotate_debug_prem(bounds, debug),
    }
}

fn annotate_prems(bounds: &VEnv, prems: &mut [ast::Prem]) -> Result<Occurrences, ElabError> {
    let mut occurs = Occurrences::new();
    for prem in prems {
        let occurs_prem = annotate_prem(bounds, prem)?;
        occurs = occurs.union(occurs_prem)?;
    }
    Ok(occurs)
}

// - Rule premises

fn annotate_rule_prem(bounds: &VEnv, rule: &mut ast::RulePrem) -> Result<Occurrences, ElabError> {
    annotate_not_exp(bounds, &mut rule.not_exp)
}

// - Conditional premises

fn annotate_if_prem(bounds: &VEnv, if_prem: &mut ast::IfPrem) -> Result<Occurrences, ElabError> {
    annotate_exp(bounds, &mut if_prem.exp)
}

// - Holding premises

fn annotate_if_hold_prem(
    bounds: &VEnv,
    if_prem: &mut ast::IfHoldPrem,
) -> Result<Occurrences, ElabError> {
    annotate_not_exp(bounds, &mut if_prem.not_exp)
}

// - Non-holding premises

fn annotate_if_not_hold_prem(
    bounds: &VEnv,
    if_prem: &mut ast::IfNotHoldPrem,
) -> Result<Occurrences, ElabError> {
    annotate_not_exp(bounds, &mut if_prem.not_exp)
}

// - Iteration premises

/// Fills in the source variables of an iterated premise.
fn annotate_iter_prem(
    bounds: &VEnv,
    span: &Span,
    iter_prem: &mut ast::IterPrem,
) -> Result<Occurrences, ElabError> {
    // Iterated premises must arrive without binding annotations
    if !iter_prem.prem_iter.vars_bound.is_empty() || !iter_prem.prem_iter.vars_bind.is_empty() {
        return Err(error::dim::iteration_annotation_invalid(span, "premise"));
    }
    let occurs = annotate_prem(bounds, &mut iter_prem.prem)?;
    let iter = iter_prem.prem_iter.iter;
    // Variables that supply iteration values
    let vars_bound = collect_iter_vars(bounds, &occurs, iter);
    // An iteration needs at least one source variable
    if vars_bound.is_empty() {
        return Err(error::dim::iteration_premise_empty(span));
    }
    let occurs = occurs.iterate(&vars_bound, iter);
    iter_prem.prem_iter.vars_bound = vars_bound;
    Ok(occurs)
}

// - Debug premises

fn annotate_debug_prem(
    bounds: &VEnv,
    debug: &mut ast::DebugPrem,
) -> Result<Occurrences, ElabError> {
    annotate_exp(bounds, &mut debug.exp)
}

// == Analysis

// - Rules

/// Infers the dimension bounds of a rule, then annotates its iterations.
fn analyze_rule(rule: &mut ast::Rule) -> Result<(), ElabError> {
    let bounds = infer_rule(rule)?.into_bounds()?;
    annotate_not_exp(&bounds, &mut rule.node.not_exp)?;
    annotate_prems(&bounds, &mut rule.node.prems)?;
    Ok(())
}

// - Rule groups

fn analyze_rule_group(group: &mut ast::RuleGroup) -> Result<(), ElabError> {
    for rule in &mut group.node.rules {
        analyze_rule(rule)?;
    }
    Ok(())
}

// - Otherwise groups

fn analyze_else_group(group: &mut ast::ElseGroup) -> Result<(), ElabError> {
    analyze_rule(&mut group.node.rule)
}

// - Clauses

/// Infers the dimension bounds of a clause, then annotates its iterations.
fn analyze_clause(clause: &mut ast::Clause) -> Result<(), ElabError> {
    let bounds = infer_clause(clause)?.into_bounds()?;
    annotate_args(&bounds, &mut clause.node.args)?;
    annotate_prems(&bounds, &mut clause.node.prems)?;
    annotate_exp(&bounds, &mut clause.node.exp)?;
    Ok(())
}

// - Table rows

/// Infers the dimension bounds of a table row, then annotates its iterations.
fn analyze_table_row(row: &mut ast::TableRow) -> Result<(), ElabError> {
    let bounds = infer_table_row(row).into_bounds()?;
    annotate_args(&bounds, &mut row.node.args)?;
    annotate_exp(&bounds, &mut row.node.exp)?;
    Ok(())
}

// - Relations

/// Analyzes every rule of a defined relation; extern relations have none.
fn analyze_rel_def(rel_def_il: &mut ast::RelDef) -> Result<(), ElabError> {
    let ast::RelDef::Defined(defined_rel_il) = rel_def_il else {
        return Ok(());
    };
    for rule_group_il in &mut defined_rel_il.rule_groups {
        analyze_rule_group(rule_group_il)?;
    }
    if let Some(else_group_il) = &mut defined_rel_il.else_group {
        analyze_else_group(else_group_il)?;
    }
    Ok(())
}

// - Meta-functions

/// Analyzes the rows or clauses of a function; externs and builtins have none.
fn analyze_meta_func_def(meta_func_def_il: &mut ast::MetaFuncDef) -> Result<(), ElabError> {
    match meta_func_def_il {
        ast::MetaFuncDef::Table(table_func_il) => analyze_table_func(table_func_il),
        ast::MetaFuncDef::Defined(defined_func_il) => analyze_defined_func(defined_func_il),
        ast::MetaFuncDef::Extern(_) | ast::MetaFuncDef::Builtin(_) => Ok(()),
    }
}

// - Table functions

fn analyze_table_func(table_func_il: &mut ast::TableFunc) -> Result<(), ElabError> {
    for table_row_il in &mut table_func_il.rows {
        analyze_table_row(table_row_il)?;
    }
    Ok(())
}

// - Defined functions

fn analyze_defined_func(defined_func_il: &mut ast::DefinedFunc) -> Result<(), ElabError> {
    for clause_il in &mut defined_func_il.clauses {
        analyze_clause(clause_il)?;
    }
    if let Some(else_clause_il) = &mut defined_func_il.else_clause {
        analyze_clause(else_clause_il)?;
    }
    Ok(())
}

// - Definitions

fn analyze_def(def_il: &mut ast::Def) -> Result<(), ElabError> {
    match &mut def_il.node {
        ast::DefKind::Rel(rel_def_il) => analyze_rel_def(rel_def_il),
        ast::DefKind::MetaFunc(meta_func_def_il) => analyze_meta_func_def(meta_func_def_il),
        _ => Ok(()),
    }
}

// - Specification

/// Annotates every iteration in the specification with its variables.
pub(super) fn analyze_spec(spec_il: &mut ast::Spec) -> Result<(), ElabError> {
    for def_il in spec_il {
        analyze_def(def_il)?;
    }
    Ok(())
}
