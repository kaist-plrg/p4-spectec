//! Explicit guard insertion for partial algorithmic expressions
//!
//! `insert_spec` inserts conditions before the operations that require them,
//! omitting conditions implied by earlier premises.
//!
//! - `a[i]` requires `i < |a|`
//! - `a[i : n]` requires `i + n <= |a|`
//! - `r[.VALUES[i] = v]` requires `i < |r.VALUES|`
//! - `a[[i : n] = b]` requires `i + n <= |a|` and `|b| = n`
//! - Text `t[[i] = t_new]` requires `i < |t|` and `|t_new| = 1`
//! - `a / b` and `a % b` require `b =/= 0`
//!
//! Iterations check source agreement before evaluating body guards.
//! For `a[x + y]*{x <- x*, y <- y*}`, insert:
//!
//! ```text
//! -- if |x*| = |y*|
//! -- if (x + y < |a|)*{x <- x*, y <- y*}
//! ```
//!
//! Optional iterations instead check `(x? = eps) <=> (y? = eps)`.
//! Body guards retain every original source, even those they do not reference:
//! `(a[i] + x)*{x <- x*}` requires `(i < |a|)*{x <- x*}`,
//! so an empty `x*` skips the index check.
//!
//! Binding premises also establish facts for subsequent guard elimination:
//!
//! ```text
//! -- let (x, y) = z *{x -> x*, y -> y*, z <- z*}
//! ```
//!
//! Guards checked before this binding may iterate over the input `z*`;
//! the output lists `x*` and `y*` are not yet bound.
//! Once the binding succeeds, `|x*| = |y*| = |z*|` is guaranteed,
//! so later guards requiring those equalities can be omitted.
//! Relation premises treat their inputs as values and their outputs as patterns.
//!
//! Guard elimination tracks facts established by earlier premises.
//! Equalities (`=`) and Boolean equivalences (`<=>`) form separate classes;
//! conjunctions contribute their conjuncts, and other conditions are recorded
//! for exact-match elimination.
//!
//! ```text
//! -- if a = b           ;; equality class: {a, b}
//! -- if b = c           ;; equality class: {a, b, c}
//! -- if a = c           ;; omitted: both sides belong to the same class
//!
//! -- if p <=> q         ;; equivalence class: {p, q}
//! -- if q <=> r         ;; equivalence class: {p, q, r}
//! -- if p <=> r         ;; omitted: both sides belong to the same class
//!
//! -- if i < |a|         ;; recorded as a condition
//! -- if i < |a|         ;; omitted: the same condition is already known
//! ```
//!
//! Newly collected guards are filtered in execution order:
//! each retained guard contributes facts for the following guards.
//! Facts established by a binding become available only after that binding.
//! Class membership compares whole expressions;
//! it does not substitute equal subexpressions inside other conditions.

use crate::lang::{
    common::prim,
    hints::input,
    traits::{at::At, eq::SyntaxEq},
};

use crate::lang::al::{self, ast};

use crate::runtime::{ops::typ::expand_typ, typdef::TypeDef};
use crate::{note_phrase, phrase};

use super::super::{AlgoError, binding::context::Context, error, otherwise};

// == Equivalence filtering

// - Equivalence classes

/// Which equivalence an `if` premise establishes.
#[derive(Clone, Copy)]
enum ClassKind {
    Equals,
    Equiv,
}

/// Expressions known equal, known equivalent, or a lone condition.
#[allow(clippy::large_enum_variant)]
enum Class<'a> {
    Equals(Vec<&'a ast::Exp>),
    Equiv(Vec<&'a ast::Exp>),
    Singleton(&'a ast::Exp),
}

impl<'a> Class<'a> {
    fn conditions(&self, kind: ClassKind) -> Option<&[&'a ast::Exp]> {
        match (kind, self) {
            (ClassKind::Equals, Self::Equals(conditions))
            | (ClassKind::Equiv, Self::Equiv(conditions)) => Some(conditions),
            _ => None,
        }
    }

    fn new(kind: ClassKind, conditions: Vec<&'a ast::Exp>) -> Self {
        match kind {
            ClassKind::Equals => Self::Equals(conditions),
            ClassKind::Equiv => Self::Equiv(conditions),
        }
    }
}

// - Equivalence table

/// Facts established by earlier premises, used to drop implied guards.
#[derive(Default)]
struct EquivalenceTable<'a> {
    classes: Vec<Class<'a>>,
}

impl<'a> EquivalenceTable<'a> {
    fn from_prems(prems_al: impl IntoIterator<Item = &'a ast::Prem>) -> Self {
        let mut table = Self::default();
        for prem_al in prems_al {
            table.add_prem(prem_al);
        }
        table
    }

    fn add_prem(&mut self, prem_al: &'a ast::Prem) {
        if let ast::PremKind::If(if_prem) = &prem_al.node {
            self.add_if_exp(&if_prem.exp);
        }
    }

    /// Records equalities, equivalences, conjunction halves, or lone facts.
    fn add_if_exp(&mut self, exp: &'a ast::Exp) {
        match &exp.node {
            ast::ExpKind::Cmp(ast::CmpOp::Bool(prim::bool::CmpOp::Eq), _, exp_l, exp_r) => {
                self.union(ClassKind::Equals, exp_l, exp_r)
            }
            ast::ExpKind::Bin(ast::BinOp::Bool(prim::bool::BinOp::Equiv), _, exp_l, exp_r) => {
                self.union(ClassKind::Equiv, exp_l, exp_r)
            }
            ast::ExpKind::Bin(ast::BinOp::Bool(prim::bool::BinOp::And), _, exp_l, exp_r) => {
                self.add_if_exp(exp_l);
                self.add_if_exp(exp_r);
            }
            _ => self.classes.insert(0, Class::Singleton(exp)),
        }
    }

    /// Finds the class containing a condition under the given kind.
    fn find(&self, kind: ClassKind, condition: &ast::Exp) -> Option<usize> {
        self.classes.iter().position(|class| {
            let Some(conditions) = class.conditions(kind) else {
                return false;
            };
            conditions
                .iter()
                .any(|candidate| condition.syntax_eq(candidate))
        })
    }

    fn take_conditions(&mut self, kind: ClassKind, index: usize) -> Vec<&'a ast::Exp> {
        let class = self.classes.remove(index);
        match (kind, class) {
            (ClassKind::Equals, Class::Equals(conditions))
            | (ClassKind::Equiv, Class::Equiv(conditions)) => conditions,
            _ => unreachable!("class lookup preserves the equivalence kind"),
        }
    }

    /// Merges the classes of two conditions, creating or extending as needed.
    fn union(&mut self, kind: ClassKind, condition_l: &'a ast::Exp, condition_r: &'a ast::Exp) {
        let idx_l = self.find(kind, condition_l);
        let idx_r = self.find(kind, condition_r);
        match (idx_l, idx_r) {
            (Some(idx_l), Some(idx_r)) if idx_l == idx_r => {}
            // Both known: merge the two classes
            (Some(idx_l), Some(idx_r)) => {
                let idx_high = idx_l.max(idx_r);
                let idx_low = idx_l.min(idx_r);
                let conditions_high = self.take_conditions(kind, idx_high);
                let conditions_low = self.take_conditions(kind, idx_low);
                let (mut conditions_l, mut conditions_r) = if idx_l == idx_low {
                    (conditions_low, conditions_high)
                } else {
                    (conditions_high, conditions_low)
                };
                conditions_l.append(&mut conditions_r);
                let class = Class::new(kind, conditions_l);
                self.classes.insert(0, class);
            }
            // One known: extend its class
            (Some(index), None) | (None, Some(index)) => {
                let condition_new = if idx_l.is_some() { condition_r } else { condition_l };
                let mut conditions = self.take_conditions(kind, index);
                conditions.insert(0, condition_new);
                let class = Class::new(kind, conditions);
                self.classes.insert(0, class);
            }
            // Neither known: start a class
            (None, None) => {
                let conditions = vec![condition_l, condition_r];
                let class = Class::new(kind, conditions);
                self.classes.insert(0, class);
            }
        }
    }

    /// Checks whether two conditions are known to be in the same class.
    fn contains(&self, kind: ClassKind, condition_l: &ast::Exp, condition_r: &ast::Exp) -> bool {
        if condition_l.syntax_eq(condition_r) {
            return true;
        }
        let Some(index) = self.find(kind, condition_l) else {
            return false;
        };
        let Some(conditions) = self.classes[index].conditions(kind) else {
            return false;
        };
        conditions
            .iter()
            .any(|condition| condition.syntax_eq(condition_r))
    }

    /// Checks whether the known facts entail a condition.
    fn implies_exp(&self, exp: &ast::Exp) -> bool {
        match &exp.node {
            ast::ExpKind::Cmp(ast::CmpOp::Bool(prim::bool::CmpOp::Eq), _, exp_l, exp_r) => {
                self.contains(ClassKind::Equals, exp_l, exp_r)
            }
            ast::ExpKind::Bin(ast::BinOp::Bool(prim::bool::BinOp::Equiv), _, exp_l, exp_r) => {
                self.contains(ClassKind::Equiv, exp_l, exp_r)
            }
            // A conjunction must be entailed on both sides
            ast::ExpKind::Bin(ast::BinOp::Bool(prim::bool::BinOp::And), _, exp_l, exp_r) => {
                self.implies_exp(exp_l) && self.implies_exp(exp_r)
            }
            // Other conditions must appear literally
            _ => self.classes.iter().any(
                |class| matches!(class, Class::Singleton(condition) if exp.syntax_eq(condition)),
            ),
        }
    }

    fn implies(&self, prem_al: &ast::Prem) -> bool {
        let ast::PremKind::If(if_prem) = &prem_al.node else {
            return false;
        };
        self.implies_exp(&if_prem.exp)
    }
}

// == Collection result

/// Guards collected from one premise.
///
/// Must-premises are established by the premise itself
/// and only filter later guards;
/// insert-premises are checked before it.
struct Collected {
    prems_must: Vec<ast::Prem>,
    prems_insert: Vec<ast::Prem>,
}

impl Collected {
    fn compose(mut self, mut other: Self) -> Self {
        self.prems_must.append(&mut other.prems_must);
        self.prems_insert.append(&mut other.prems_insert);
        self
    }
}

/// Drops insert-premises implied by or repeated among the known premises.
fn filter_prems_insert(
    prems_base: &[&[ast::Prem]],
    prems_derived: &[ast::Prem],
    prems_output: &[ast::Prem],
    prems_insert: Vec<ast::Prem>,
) -> Vec<ast::Prem> {
    // Facts known before this premise
    let mut prems_must: Vec<_> = prems_base
        .iter()
        .flat_map(|prems| prems.iter())
        .chain(prems_derived)
        .chain(prems_output)
        .collect();
    let mut table = EquivalenceTable::from_prems(prems_must.iter().copied());
    // Check each guard using only premises that will execute before it
    let keep: Vec<_> = prems_insert
        .iter()
        .map(|prem_al| {
            let implied = table.implies(prem_al);
            let duplicated = prems_must
                .iter()
                .any(|prem_must_al| prem_al.syntax_eq(prem_must_al));
            // Keep the first establishing guard, including its iteration sources
            if implied || duplicated {
                return false;
            }
            table.add_prem(prem_al);
            prems_must.push(prem_al);
            true
        })
        .collect();
    // Move retained premises after the table's borrowed expressions are released
    prems_insert
        .into_iter()
        .zip(keep)
        .filter_map(|(prem_al, keep)| keep.then_some(prem_al))
        .collect()
}

/// Wraps a premise over every source, preserving empty-iteration behavior.
fn iterate_prem(iter: ast::Iter, vars: &[ast::Var], prem_al: ast::Prem) -> ast::Prem {
    let span = prem_al.span;
    let prem_iter = ast::PremIter { iter, vars_bound: vars.to_vec(), vars_bind: vec![] };
    let prem_kind = ast::PremKind::Iter(ast::IterPrem { prem: Box::new(prem_al), prem_iter });
    phrase!(node: prem_kind, span: span)
}

fn iterate_prems(iter: ast::Iter, vars: &[ast::Var], prems_al: Vec<ast::Prem>) -> Vec<ast::Prem> {
    prems_al
        .into_iter()
        .map(|prem_al| iterate_prem(iter, vars, prem_al))
        .collect()
}

/// Iterates must- and insert-premises over their respective variables.
fn iterate_collected(
    iter: ast::Iter,
    vars_must: &[ast::Var],
    vars_insert: &[ast::Var],
    collected: Collected,
) -> Collected {
    Collected {
        prems_must: iterate_prems(iter, vars_must, collected.prems_must),
        prems_insert: iterate_prems(iter, vars_insert, collected.prems_insert),
    }
}

// == Guard generation

/// Builds a length expression with the operand's location.
fn gen_len_exp(exp_al: &ast::Exp) -> ast::Exp {
    note_phrase! {
        node: ast::ExpKind::Len(Box::new(exp_al.clone())),
        note: ast::TypKind::Num(prim::num::Typ::Nat),
        span: exp_al.span,
    }
}

/// Builds the read expression whose guards protect each update path prefix.
///
/// Path notes retain the type at each step: `r[.VALUES[i] = v]`
/// checks the index against `r.VALUES`, not against `r`.
fn gen_access_exp(exp_base_al: &ast::Exp, path_al: &ast::Path) -> ast::Exp {
    let exp_kind = match &path_al.node {
        ast::PathKind::Root => return exp_base_al.clone(),
        ast::PathKind::Idx(path_inner_al, exp_idx_al) => ast::ExpKind::Idx(
            Box::new(gen_access_exp(exp_base_al, path_inner_al)),
            exp_idx_al.clone(),
        ),
        ast::PathKind::Slice(path_inner_al, exp_idx_al, exp_len_al) => ast::ExpKind::Slice(
            Box::new(gen_access_exp(exp_base_al, path_inner_al)),
            exp_idx_al.clone(),
            exp_len_al.clone(),
        ),
        ast::PathKind::Dot(path_inner_al, atom) => {
            ast::ExpKind::Dot(Box::new(gen_access_exp(exp_base_al, path_inner_al)), atom.clone())
        }
    };
    note_phrase!(node: exp_kind, note: path_al.note.clone(), span: path_al.span)
}

/// Builds a comparison premise at the guarded expression's location.
fn gen_cmp_guard_prem(
    exp_al: &ast::Exp,
    op: ast::CmpOp,
    op_typ: ast::OpTyp,
    exp_l_al: ast::Exp,
    exp_r_al: ast::Exp,
) -> Vec<ast::Prem> {
    let span = exp_al.span;
    let exp_guard_al = note_phrase! {
        node: ast::ExpKind::Cmp(op, op_typ, Box::new(exp_l_al), Box::new(exp_r_al)),
        note: ast::TypKind::Bool,
        span: span,
    };
    let prem_kind = ast::PremKind::If(ast::IfPrem { exp: exp_guard_al });
    vec![phrase!(node: prem_kind, span: span)]
}

/// Builds `if idx < |base|` for an index expression.
fn gen_index_guard_prem(
    exp_al: &ast::Exp,
    exp_base_al: &ast::Exp,
    exp_idx_al: &ast::Exp,
) -> Vec<ast::Prem> {
    gen_cmp_guard_prem(
        exp_al,
        ast::CmpOp::Num(prim::num::CmpOp::Lt),
        ast::OpTyp::Nat,
        exp_idx_al.clone(),
        gen_len_exp(exp_base_al),
    )
}

/// Builds `if idx + len <= |base|`, including the empty slice at the end.
fn gen_slice_guard_prem(
    exp_al: &ast::Exp,
    exp_base_al: &ast::Exp,
    exp_idx_al: &ast::Exp,
    exp_len_al: &ast::Exp,
) -> Vec<ast::Prem> {
    // Both operands are naturals and addition is arbitrary precision
    let exp_end_al = note_phrase! {
        node: ast::ExpKind::Bin(
            ast::BinOp::Num(prim::num::BinOp::Add),
            ast::OpTyp::Nat,
            Box::new(exp_idx_al.clone()),
            Box::new(exp_len_al.clone()),
        ),
        note: ast::TypKind::Num(prim::num::Typ::Nat),
        span: exp_al.span,
    };
    // Check the exclusive end after the operands' own guards
    gen_cmp_guard_prem(
        exp_al,
        ast::CmpOp::Num(prim::num::CmpOp::Le),
        ast::OpTyp::Nat,
        exp_end_al,
        gen_len_exp(exp_base_al),
    )
}

/// Builds a nonzero-divisor guard with the arithmetic operand's numeric kind.
fn gen_divisor_guard_prem(
    exp_al: &ast::Exp,
    op_typ: ast::OpTyp,
    exp_r_al: &ast::Exp,
) -> Vec<ast::Prem> {
    // Equality distinguishes natural and integer values
    let num_zero = match op_typ {
        ast::OpTyp::Nat => prim::num::Number::Nat(0u64.into()),
        ast::OpTyp::Int => prim::num::Number::Int(0.into()),
        ast::OpTyp::Bool => unreachable!("arithmetic operands must be numeric"),
    };
    let typ_zero = ast::TypKind::Num(prim::num::to_typ(&num_zero));
    let exp_zero_al = note_phrase! {
        node: ast::ExpKind::Num(num_zero),
        note: typ_zero,
        span: exp_r_al.span,
    };
    // Generic inequality compares two values of the same kind
    gen_cmp_guard_prem(
        exp_al,
        ast::CmpOp::Bool(prim::bool::CmpOp::Ne),
        ast::OpTyp::Bool,
        exp_r_al.clone(),
        exp_zero_al,
    )
}

/// Requires length-preserving replacements for slices and text indices.
fn gen_update_guard_prem(
    ctx: &Context,
    path_al: &ast::Path,
    exp_field_al: &ast::Exp,
) -> Vec<ast::Prem> {
    let exp_len_al = match &path_al.node {
        // A slice replacement must retain the selected length
        ast::PathKind::Slice(_, _, exp_len_al) => exp_len_al.as_ref().clone(),
        // Only text indexing constrains the replacement's length
        ast::PathKind::Idx(path_inner_al, _) => {
            let typ = phrase!(node: path_inner_al.note.as_ref().clone(), span: path_inner_al.span);
            let typ = expand_typ(&ctx.tdenv, &typ)
                .expect("an elaborated index base must have a defined sequence type");
            if !matches!(typ.node, ast::TypKind::Text) {
                return vec![];
            }
            note_phrase! {
                node: ast::ExpKind::Num(prim::num::Number::Nat(1u64.into())),
                note: ast::TypKind::Num(prim::num::Typ::Nat),
                span: path_al.span,
            }
        }
        // Whole values and record fields may change their length
        ast::PathKind::Root | ast::PathKind::Dot(_, _) => return vec![],
    };
    // Inner updates preserve enclosing sequence lengths by construction
    gen_cmp_guard_prem(
        exp_field_al,
        ast::CmpOp::Bool(prim::bool::CmpOp::Eq),
        ast::OpTyp::Bool,
        gen_len_exp(exp_field_al),
        exp_len_al,
    )
}

/// Builds `x? = eps` for an option variable.
fn gen_exp_eq_epsilon(iter: ast::Iter, var: &ast::Var) -> ast::Exp {
    let mut var = var.clone();
    var.iters.push(iter);
    let exp_al = al::var::as_exp(true, &var);
    let span = exp_al.span;
    let typ = exp_al.note.clone();
    // Compare the iterated option against the empty option
    let exp_epsilon_al = note_phrase! {
        node: ast::ExpKind::Opt(None),
        note: typ,
        span: span,
    };
    note_phrase! {
        node: ast::ExpKind::Cmp(
            ast::CmpOp::Bool(prim::bool::CmpOp::Eq),
            ast::OpTyp::Bool,
            Box::new(exp_al),
            Box::new(exp_epsilon_al),
        ),
        note: ast::TypKind::Bool,
        span: span,
    }
}

/// Builds `|x*|` for a list variable.
fn gen_exp_len(iter: ast::Iter, var: &ast::Var) -> ast::Exp {
    let mut var = var.clone();
    var.iters.push(iter);
    let exp_al = al::var::as_exp(true, &var);
    let span = exp_al.span;
    note_phrase! {
        node: ast::ExpKind::Len(Box::new(exp_al)),
        note: ast::TypKind::Num(prim::num::Typ::Nat),
        span: span,
    }
}

/// Pairs two guards: `<=>` for options, `=` for lists.
fn gen_exp_pair(iter: ast::Iter, exp_l_al: ast::Exp, exp_r_al: ast::Exp) -> ast::Exp {
    let span = [&exp_l_al, &exp_r_al].at();
    let exp_kind = match iter {
        ast::Iter::Opt => ast::ExpKind::Bin(
            ast::BinOp::Bool(prim::bool::BinOp::Equiv),
            ast::OpTyp::Bool,
            Box::new(exp_l_al),
            Box::new(exp_r_al),
        ),
        ast::Iter::List => ast::ExpKind::Cmp(
            ast::CmpOp::Bool(prim::bool::CmpOp::Eq),
            ast::OpTyp::Bool,
            Box::new(exp_l_al),
            Box::new(exp_r_al),
        ),
    };
    note_phrase!(node: exp_kind, note: ast::TypKind::Bool, span: span)
}

fn gen_exp_and(exp_l_al: ast::Exp, exp_r_al: ast::Exp) -> ast::Exp {
    let span = [&exp_l_al, &exp_r_al].at();
    note_phrase! {
        node: ast::ExpKind::Bin(
            ast::BinOp::Bool(prim::bool::BinOp::And),
            ast::OpTyp::Bool,
            Box::new(exp_l_al),
            Box::new(exp_r_al),
        ),
        note: ast::TypKind::Bool,
        span: span,
    }
}

/// Requires all iterated variables to agree in length or emptiness, pairwise.
fn gen_iter_guard(exp_iter: &ast::ExpIter) -> Vec<ast::Prem> {
    let ast::ExpIter { iter, vars } = exp_iter;
    // One variable iterates freely
    if vars.len() < 2 {
        return vec![];
    }
    let mut exps_al = vars.iter().map(|var| match iter {
        ast::Iter::Opt => gen_exp_eq_epsilon(*iter, var),
        ast::Iter::List => gen_exp_len(*iter, var),
    });
    let Some(exp_first_al) = exps_al.next() else {
        return vec![];
    };
    let Some(mut exp_prev_al) = exps_al.next() else {
        return vec![];
    };
    // Chain pairwise: (v1 ~ v2) /\ (v2 ~ v3) and so on
    let mut exp_guard_al = gen_exp_pair(*iter, exp_first_al, exp_prev_al.clone());
    for exp_al in exps_al {
        let exp_pair_al = gen_exp_pair(*iter, exp_prev_al, exp_al.clone());
        exp_guard_al = gen_exp_and(exp_guard_al, exp_pair_al);
        exp_prev_al = exp_al;
    }
    let span = exp_guard_al.span;
    let prem_kind = ast::PremKind::If(ast::IfPrem { exp: exp_guard_al });
    let prem_guard_al = phrase!(node: prem_kind, span: span);
    vec![prem_guard_al]
}

// == Guard collection

// - Expressions

/// Collects the guards an expression needs before evaluation.
fn collect_exp(ctx: &Context, exp_al: &ast::Exp) -> Vec<ast::Prem> {
    match &exp_al.node {
        // Literals and variables need no guards
        ast::ExpKind::Bool(_)
        | ast::ExpKind::Num(_)
        | ast::ExpKind::Text(_)
        | ast::ExpKind::Id(_) => vec![],
        // Unary forms collect the guards of the operand
        ast::ExpKind::Un(_, _, exp_inner_al)
        | ast::ExpKind::UpCast(_, exp_inner_al)
        | ast::ExpKind::DownCast(_, exp_inner_al)
        | ast::ExpKind::Sub(exp_inner_al, _)
        | ast::ExpKind::Match(exp_inner_al, _)
        | ast::ExpKind::Len(exp_inner_al)
        | ast::ExpKind::Dot(exp_inner_al, _) => collect_exp(ctx, exp_inner_al),
        // Division and modulo require a nonzero divisor after operand guards
        ast::ExpKind::Bin(
            ast::BinOp::Num(prim::num::BinOp::Div | prim::num::BinOp::Mod),
            op_typ,
            exp_l_al,
            exp_r_al,
        ) => {
            let mut prems_insert = collect_exp(ctx, exp_l_al);
            prems_insert.extend(collect_exp(ctx, exp_r_al));
            prems_insert.extend(gen_divisor_guard_prem(exp_al, *op_typ, exp_r_al));
            prems_insert
        }
        // Binary forms collect from both sides
        ast::ExpKind::Bin(_, _, exp_l_al, exp_r_al)
        | ast::ExpKind::Cmp(_, _, exp_l_al, exp_r_al)
        | ast::ExpKind::Cons(exp_l_al, exp_r_al)
        | ast::ExpKind::Cat(exp_l_al, exp_r_al)
        | ast::ExpKind::Mem(exp_l_al, exp_r_al) => {
            let mut prems_insert = collect_exp(ctx, exp_l_al);
            let prems_r_insert = collect_exp(ctx, exp_r_al);
            prems_insert.extend(prems_r_insert);
            prems_insert
        }
        // Tuple or list: guards of every element
        ast::ExpKind::Tuple(exps_al) | ast::ExpKind::List(exps_al) => {
            collect_exps(ctx, exps_al.iter())
        }
        // Case: guards of the arguments
        ast::ExpKind::Case(not_exp) => collect_exps(ctx, not_exp.args()),
        // Struct: guards of the fields
        ast::ExpKind::Str(fields) => {
            collect_exps(ctx, fields.iter().map(|ast::ExpField { exp, .. }| exp))
        }
        // Option: guards of the payload
        ast::ExpKind::Opt(Some(exp_inner_al)) => collect_exp(ctx, exp_inner_al),
        // Empty option needs no guard
        ast::ExpKind::Opt(None) => vec![],
        // Indexing needs a bounds guard
        ast::ExpKind::Idx(exp_base_al, exp_idx_al) => {
            let mut prems_insert = collect_exp(ctx, exp_base_al);
            let prems_idx_insert = collect_exp(ctx, exp_idx_al);
            let prems_guard = gen_index_guard_prem(exp_al, exp_base_al, exp_idx_al);
            prems_insert.extend(prems_idx_insert);
            prems_insert.extend(prems_guard);
            prems_insert
        }
        // Slice: guards of base, index, and length
        ast::ExpKind::Slice(exp_base_al, exp_idx_al, exp_len_al) => {
            let mut prems_insert = collect_exp(ctx, exp_base_al);
            let prems_idx_insert = collect_exp(ctx, exp_idx_al);
            let prems_len_insert = collect_exp(ctx, exp_len_al);
            prems_insert.extend(prems_idx_insert);
            prems_insert.extend(prems_len_insert);
            prems_insert.extend(gen_slice_guard_prem(exp_al, exp_base_al, exp_idx_al, exp_len_al));
            prems_insert
        }
        // Update: guard each accessed prefix, then the replacement
        ast::ExpKind::Upd(exp_base_al, path_al, exp_field_al) => {
            let exp_target_al = gen_access_exp(exp_base_al, path_al);
            let mut prems_insert = collect_exp(ctx, &exp_target_al);
            prems_insert.extend(collect_exp(ctx, exp_field_al));
            prems_insert.extend(gen_update_guard_prem(ctx, path_al, exp_field_al));
            prems_insert
        }
        // Call: guards of the arguments
        ast::ExpKind::Call(_, _, args) => collect_args(ctx, args),
        // Check source agreement before evaluating any iterated guard
        ast::ExpKind::Iter(exp_inner_al, exp_iter) => {
            let prems_inner_insert = collect_exp(ctx, exp_inner_al);
            let mut prems_insert = gen_iter_guard(exp_iter);
            prems_insert.extend(iterate_prems(exp_iter.iter, &exp_iter.vars, prems_inner_insert));
            prems_insert
        }
    }
}

fn collect_exps<'a>(ctx: &Context, exps: impl IntoIterator<Item = &'a ast::Exp>) -> Vec<ast::Prem> {
    let mut prems_insert = Vec::new();
    for exp_al in exps {
        let prems_exp_insert = collect_exp(ctx, exp_al);
        prems_insert.extend(prems_exp_insert);
    }
    prems_insert
}

// - Arguments

fn collect_arg(ctx: &Context, arg: &ast::Arg) -> Vec<ast::Prem> {
    match &arg.node {
        ast::ArgKind::Exp(exp_al) => collect_exp(ctx, exp_al),
        ast::ArgKind::Def(_) => vec![],
    }
}

fn collect_args(ctx: &Context, args: &[ast::Arg]) -> Vec<ast::Prem> {
    let mut prems_insert = Vec::new();
    for arg in args {
        let prems_arg_insert = collect_arg(ctx, arg);
        prems_insert.extend(prems_arg_insert);
    }
    prems_insert
}

// - Premises

fn collect_prem(ctx: &Context, prem_al: &ast::Prem) -> Collected {
    match &prem_al.node {
        ast::PremKind::Rule(rule_prem) => collect_rule_prem(ctx, rule_prem),
        ast::PremKind::If(if_prem) => collect_if_prem(ctx, if_prem),
        ast::PremKind::IfHold(if_prem) => collect_if_hold_prem(ctx, if_prem),
        ast::PremKind::IfNotHold(if_prem) => collect_if_not_hold_prem(ctx, if_prem),
        ast::PremKind::Let(let_prem) => collect_let_prem(ctx, let_prem),
        ast::PremKind::Iter(iter_prem) => collect_iter_prem(ctx, iter_prem),
        ast::PremKind::Debug(debug_prem) => collect_debug_prem(ctx, debug_prem),
    }
}

/// The output patterns establish their guards; the inputs need them checked.
fn collect_rule_prem(ctx: &Context, rule_prem: &ast::RulePrem) -> Collected {
    let exps_al = rule_prem.not_exp.args().iter().collect();
    // Binding analysis validates hints; later rewrites preserve notation arity
    let (exps_input_al, exps_output_al) = input::split(&rule_prem.input_hint, exps_al)
        .expect("validated relation hints and argument counts");
    let prems_must = collect_exps(ctx, exps_output_al);
    let prems_insert = collect_exps(ctx, exps_input_al);
    Collected { prems_must, prems_insert }
}

fn collect_if_prem(ctx: &Context, if_prem: &ast::IfPrem) -> Collected {
    let prems_insert = collect_exp(ctx, &if_prem.exp);
    Collected { prems_must: vec![], prems_insert }
}

fn collect_if_hold_prem(ctx: &Context, if_prem: &ast::IfHoldPrem) -> Collected {
    let prems_insert = collect_exps(ctx, if_prem.not_exp.args());
    Collected { prems_must: vec![], prems_insert }
}

fn collect_if_not_hold_prem(ctx: &Context, if_prem: &ast::IfNotHoldPrem) -> Collected {
    let prems_insert = collect_exps(ctx, if_prem.not_exp.args());
    Collected { prems_must: vec![], prems_insert }
}

/// The pattern side establishes its guards; the value side needs them checked.
fn collect_let_prem(ctx: &Context, let_prem: &ast::LetPrem) -> Collected {
    let prems_must = collect_exp(ctx, &let_prem.exp_l);
    let prems_insert = collect_exp(ctx, &let_prem.exp_r);
    Collected { prems_must, prems_insert }
}

/// Collects guards under an iteration premise.
fn collect_iter_prem(ctx: &Context, iter_prem: &ast::IterPrem) -> Collected {
    let prem_iter = &iter_prem.prem_iter;
    let mut vars_must = prem_iter.vars_bound.clone();
    vars_must.extend(prem_iter.vars_bind.clone());

    // Musts iterate over all variables, inserts use source variables only
    let collected = collect_prem(ctx, &iter_prem.prem);
    let collected = iterate_collected(prem_iter.iter, &vars_must, &prem_iter.vars_bound, collected);
    // The iteration itself requires its variables to agree in length
    let collected_guard = Collected {
        prems_must: gen_iter_guard(&ast::ExpIter { iter: prem_iter.iter, vars: vars_must }),
        prems_insert: gen_iter_guard(&ast::ExpIter {
            iter: prem_iter.iter,
            vars: prem_iter.vars_bound.clone(),
        }),
    };
    collected_guard.compose(collected)
}

fn collect_debug_prem(ctx: &Context, debug_prem: &ast::DebugPrem) -> Collected {
    let prems_insert = collect_exp(ctx, &debug_prem.exp);
    Collected { prems_must: vec![], prems_insert }
}

// == Guard insertion

// - Premises

/// Premises produced by inserting guards into one premise list.
struct InsertedPrems {
    /// Facts the premises establish, usable to filter later guards.
    derived: Vec<ast::Prem>,
    /// The premises with their guards inserted before them.
    output: Vec<ast::Prem>,
}

/// Inserts the guards a premise needs, skipping those already known.
fn insert_prem(
    ctx: &Context,
    prems_base: &[&[ast::Prem]],
    prems_derived: &mut Vec<ast::Prem>,
    prems_output: &mut Vec<ast::Prem>,
    prem_al: ast::Prem,
) {
    let collected = collect_prem(ctx, &prem_al);
    let mut prems_insert =
        filter_prems_insert(prems_base, prems_derived, prems_output, collected.prems_insert);
    // Guards go before the premise, its own facts after
    prems_insert.push(prem_al);
    prems_derived.extend(collected.prems_must);
    prems_output.extend(prems_insert);
}

fn insert_prems(
    ctx: &Context,
    prems_base: &[&[ast::Prem]],
    prems_al: Vec<ast::Prem>,
) -> InsertedPrems {
    let mut prems_derived = Vec::new();
    let mut prems_output = Vec::new();
    for prem_al in prems_al {
        insert_prem(ctx, prems_base, &mut prems_derived, &mut prems_output, prem_al);
    }
    InsertedPrems { derived: prems_derived, output: prems_output }
}

// - Rule groups

/// Inserts guards into the shared match and then into each rule path.
fn insert_rule_group(ctx: &Context, mut rule_group_al: ast::RuleGroup) -> ast::RuleGroup {
    // Guards of the inputs are assumed, not inserted
    let prems_input = collect_exps(ctx, rule_group_al.node.rule_match.exps_input.iter());
    let prems_match_al = std::mem::take(&mut rule_group_al.node.rule_match.prems);
    let prems_match = insert_prems(ctx, &[&prems_input], prems_match_al);
    rule_group_al.node.rule_match.prems = prems_match.output;

    // Each path builds on the facts of the shared match
    for rule_path_al in &mut rule_group_al.node.rule_paths {
        let prems_base = [
            prems_input.as_slice(),
            prems_match.derived.as_slice(),
            rule_group_al.node.rule_match.prems.as_slice(),
        ];
        let prems_path_al = std::mem::take(&mut rule_path_al.prems);
        let prems_path = insert_prems(ctx, &prems_base, prems_path_al);
        rule_path_al.prems = prems_path.output;

        // Outputs need guards too, after the path premises
        let prems_output = collect_exps(ctx, rule_path_al.exps_output.iter());
        let prems_output = filter_prems_insert(
            &prems_base,
            &prems_path.derived,
            &rule_path_al.prems,
            prems_output,
        );
        rule_path_al.prems.extend(prems_output);
    }
    rule_group_al
}

/// Inserts guards into an otherwise group, like a one-path rule group.
fn insert_else_group(
    ctx: &Context,
    mut else_group_al: ast::ElseGroup,
) -> Result<ast::ElseGroup, AlgoError> {
    // Guards of the inputs are assumed, not inserted
    let prems_input = collect_exps(ctx, else_group_al.node.rule_match.exps_input.iter());
    let prems_match_al = std::mem::take(&mut else_group_al.node.rule_match.prems);
    let prems_match = insert_prems(ctx, &[&prems_input], prems_match_al);
    else_group_al.node.rule_match.prems = prems_match.output;

    // The path builds on the facts of the match
    let prems_path_al = std::mem::take(&mut else_group_al.node.rule_path.prems);
    let prems_base = [
        prems_input.as_slice(),
        prems_match.derived.as_slice(),
        else_group_al.node.rule_match.prems.as_slice(),
    ];
    let prems_path = insert_prems(ctx, &prems_base, prems_path_al);
    else_group_al.node.rule_path.prems = prems_path.output;

    // Outputs need guards too, after the path premises
    let prems_output = collect_exps(ctx, else_group_al.node.rule_path.exps_output.iter());
    let prems_output = filter_prems_insert(
        &prems_base,
        &prems_path.derived,
        &else_group_al.node.rule_path.prems,
        prems_output,
    );
    else_group_al.node.rule_path.prems.extend(prems_output);
    // Only the final premises count after implied guards have been omitted
    otherwise::check_prems(
        ctx,
        else_group_al
            .node
            .rule_match
            .prems
            .iter()
            .chain(&else_group_al.node.rule_path.prems),
        &else_group_al.node.otherwise,
    )?;
    Ok(else_group_al)
}

// - Clauses

/// Inserts guards into a clause's premises and body.
fn insert_clause(ctx: &Context, mut clause_al: ast::Clause) -> ast::Clause {
    // Guards of the arguments are assumed, not inserted
    let prems_args = collect_args(ctx, &clause_al.node.args);
    let prems_clause_al = std::mem::take(&mut clause_al.node.prems);
    let prems_clause = insert_prems(ctx, &[&prems_args], prems_clause_al);
    clause_al.node.prems = prems_clause.output;

    // The body's guards go after the premises
    let prems_output = collect_exp(ctx, &clause_al.node.exp);
    let prems_output = filter_prems_insert(
        &[&prems_args],
        &prems_clause.derived,
        &clause_al.node.prems,
        prems_output,
    );
    clause_al.node.prems.extend(prems_output);
    clause_al
}

/// Inserts guards and checks all retained premises of an otherwise clause.
fn insert_else_clause(
    ctx: &Context,
    clause_al: ast::ElseClause,
) -> Result<ast::ElseClause, AlgoError> {
    let span = clause_al.span;
    let ast::ElseClauseKind { otherwise, clause } = clause_al.node;
    let clause_al = insert_clause(ctx, phrase!(node: clause, span: span));
    otherwise::check_prems(ctx, &clause_al.node.prems, &otherwise)?;
    Ok(phrase!(
        node: ast::ElseClauseKind { otherwise, clause: clause_al.node },
        span: span,
    ))
}

// - Table rows

/// Rejects a table row that would need a guard, computed as for a clause.
///
/// Rows are exclusive and exhaustive,
/// so a failing guard must not skip a row and fall through to the next.
fn check_table_row(ctx: &Context, table_row_al: &ast::TableRow) -> Result<(), AlgoError> {
    // Guards of the arguments are assumed, not inserted
    let prems_args = collect_args(ctx, &table_row_al.node.args);
    let prems_row_al = &table_row_al.node.prems;
    // No guard is inserted, so the premises before each one stay unchanged
    let mut prems_derived = Vec::new();
    for (idx, prem_al) in prems_row_al.iter().enumerate() {
        let collected = collect_prem(ctx, prem_al);
        let prems_insert = filter_prems_insert(
            &[&prems_args],
            &prems_derived,
            &prems_row_al[..idx],
            collected.prems_insert,
        );
        if let Some(prem_guard_al) = prems_insert.first() {
            return Err(error::table::table_row_guard_required(table_row_al, prem_guard_al));
        }
        prems_derived.extend(collected.prems_must);
    }
    // The body's guards follow the premises
    let prems_output = collect_exp(ctx, &table_row_al.node.exp);
    let prems_output =
        filter_prems_insert(&[&prems_args], &prems_derived, prems_row_al, prems_output);
    if let Some(prem_guard_al) = prems_output.first() {
        return Err(error::table::table_row_guard_required(table_row_al, prem_guard_al));
    }
    Ok(())
}

// - Definitions

fn insert_def(ctx: &Context, def_al: ast::Def) -> Result<ast::Def, AlgoError> {
    let span = def_al.span;
    let def_kind_al = match def_al.node {
        ast::DefKind::Rel(rel_def_al) => {
            let rel_def_al = insert_rel_def(ctx, rel_def_al)?;
            ast::DefKind::Rel(rel_def_al)
        }
        ast::DefKind::MetaFunc(meta_func_def_al) => {
            let meta_func_def_al = insert_meta_func_def(ctx, meta_func_def_al)?;
            ast::DefKind::MetaFunc(meta_func_def_al)
        }
        def_kind_al => def_kind_al,
    };
    Ok(phrase!(node: def_kind_al, span: span))
}

/// Inserts guards into every rule group of a defined relation.
fn insert_rel_def(ctx: &Context, rel_def_al: ast::RelDef) -> Result<ast::RelDef, AlgoError> {
    let ast::RelDef::Defined(mut defined_rel_al) = rel_def_al else {
        return Ok(rel_def_al);
    };
    defined_rel_al.rule_groups = defined_rel_al
        .rule_groups
        .into_iter()
        .map(|rule_group| insert_rule_group(ctx, rule_group))
        .collect();
    defined_rel_al.else_group = defined_rel_al
        .else_group
        .map(|else_group| insert_else_group(ctx, else_group))
        .transpose()?;
    Ok(ast::RelDef::Defined(defined_rel_al))
}

/// Inserts guards into every clause of a defined function.
///
/// Table rows take no guards; a row that would need one is rejected.
fn insert_meta_func_def(
    ctx: &Context,
    meta_func_def_al: ast::MetaFuncDef,
) -> Result<ast::MetaFuncDef, AlgoError> {
    let mut defined_func_al = match meta_func_def_al {
        ast::MetaFuncDef::Defined(defined_func_al) => defined_func_al,
        ast::MetaFuncDef::Table(table_func_al) => {
            for table_row_al in &table_func_al.table_rows {
                check_table_row(ctx, table_row_al)?;
            }
            return Ok(ast::MetaFuncDef::Table(table_func_al));
        }
        meta_func_def_al => return Ok(meta_func_def_al),
    };
    // Ordinary clauses only need guard insertion
    defined_func_al.clauses = defined_func_al
        .clauses
        .into_iter()
        .map(|clause| insert_clause(ctx, clause))
        .collect();
    if let Some(clause) = defined_func_al.else_clause {
        // Type parameters remain opaque when rechecking singleton conditions
        let mut ctx = ctx.clone();
        for tparam in &defined_func_al.tparams {
            ctx.tdenv.insert(tparam.clone(), TypeDef::Parameter);
        }
        defined_func_al.else_clause = Some(insert_else_clause(&ctx, clause)?);
    }
    Ok(ast::MetaFuncDef::Defined(defined_func_al))
}

// == Entry point

/// Inserts guards and checks otherwise purity after redundant guards are omitted.
pub(in crate::pass::algo) fn insert_spec(
    ctx: &Context,
    spec_al: ast::Spec,
) -> Result<ast::Spec, AlgoError> {
    spec_al
        .into_iter()
        .map(|def| insert_def(ctx, def))
        .collect()
}
