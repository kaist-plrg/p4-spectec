//! Rewriting of repeated binding occurrences
//!
//! The leftmost occurrence keeps its identifier.
//! Each later occurrence is renamed,
//! and a side condition requires every renamed occurrence
//! to equal the leftmost one.
//!
//! -- let (int, int, int) = ...
//!
//! becomes
//!
//! -- let (int, int', int'') = ...,
//! -- if int = int' && int = int''

use crate::lang::{
    common::{
        Id,
        ds::{map::IdMap, set::IdSet},
        prim,
    },
    traits::free::FreeIds,
};

use crate::lang::il::ast;

use crate::lang::al;

use crate::runtime::{dim::Dim, envs::algo::VEnv};

use crate::{note_phrase, phrase};

use super::{
    bind::{BEnv, Binding},
    context::Context,
    iteration::{ICtx, Iteration},
};

// == Rename environment

/// Ordered renamed occurrences for each repeated source identifier.
#[derive(Clone, Debug)]
pub struct RenameEnv {
    renames: IdMap<Vec<Id>>,
    dimensions: IdMap<Dim>,
}

impl RenameEnv {
    /// Starts an empty rename list for every `Multiple` binding.
    pub fn from_bindings(benv: &BEnv) -> Self {
        let mut renames = IdMap::new();
        let mut dimensions = IdMap::new();
        for (id, binding) in benv.iter() {
            let Binding::Multiple(dim) = binding else {
                continue;
            };
            renames.insert(id.clone(), Vec::new());
            dimensions.insert(id.clone(), dim.clone());
        }
        Self { renames, dimensions }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Id, &Vec<Id>)> {
        self.renames.iter()
    }

    fn get(&self, id: &Id) -> Option<&Vec<Id>> {
        self.renames.get(id)
    }

    fn get_mut(&mut self, id: &Id) -> Option<&mut Vec<Id>> {
        self.renames.get_mut(id)
    }

    fn dimension(&self, id: &Id) -> Option<&Dim> {
        self.dimensions.get(id)
    }
}

// == Binding renaming

/// Appends primes to `id` until no identifier with the same base clashes.
fn fresh_id(ids: &IdSet, id: &Id) -> Id {
    // Only identifiers with the same base can clash
    let base = id.strip_suffix().node;
    let ids_same_base = ids
        .iter()
        .filter(|id_other| id_other.strip_suffix().node == base)
        .cloned()
        .collect::<IdSet>();
    let mut id_fresh = id.clone();
    while ids_same_base.contains(&id_fresh) {
        id_fresh.node.push('\'');
    }
    id_fresh
}

/// Keeps the first occurrence of a repeated identifier, renaming later ones.
fn rename_id_exp(ctx: &mut Context, renv: &mut RenameEnv, exp: &ast::Exp, id: &Id) -> ast::Exp {
    let Some(ids_rename) = renv.get_mut(id) else {
        return exp.clone();
    };
    // The first occurrence keeps its name, later ones get a fresh prime
    let id_rename = if ids_rename.is_empty() { id.clone() } else { fresh_id(&ctx.frees, id) };
    ctx.add_free(id_rename.clone());
    ids_rename.push(id_rename.clone());
    crate::note_phrase!(node: crate::lang::il::ast::ExpKind::Id(id_rename), note: exp.note.clone(), span: exp.span.clone())
}

/// Renames repeated binders inside an invertible expression.
pub fn rename_exp(ctx: &mut Context, renv: &mut RenameEnv, exp: &ast::Exp) -> ast::Exp {
    let kind = match &exp.node {
        // A repeated identifier gets its rename
        ast::ExpKind::Id(id) => return rename_id_exp(ctx, renv, exp, id),
        // Upcast: rename inside
        ast::ExpKind::UpCast(typ, exp_inner) => {
            let exp_inner = rename_exp(ctx, renv, exp_inner);
            ast::ExpKind::UpCast(typ.clone(), Box::new(exp_inner))
        }
        // Tuple: rename every component
        ast::ExpKind::Tuple(exps) => ast::ExpKind::Tuple(rename_exps(ctx, renv, exps)),
        // Case: rename the arguments
        ast::ExpKind::Case(not_exp) => {
            let not_exp = not_exp.map(|exp| rename_exp(ctx, renv, exp));
            ast::ExpKind::Case(Box::new(not_exp))
        }
        // Struct: rename the fields
        ast::ExpKind::Str(fields) => {
            let fields = fields
                .iter()
                .map(|ast::ExpField { atom, exp }| ast::ExpField {
                    atom: atom.clone(),
                    exp: rename_exp(ctx, renv, exp),
                })
                .collect();
            ast::ExpKind::Str(fields)
        }
        // Option: rename the payload
        ast::ExpKind::Opt(Some(exp_inner)) => {
            let exp_inner = rename_exp(ctx, renv, exp_inner);
            ast::ExpKind::Opt(Some(Box::new(exp_inner)))
        }
        // Empty option has nothing to rename
        ast::ExpKind::Opt(None) => return exp.clone(),
        // List: rename every element
        ast::ExpKind::List(exps) => ast::ExpKind::List(rename_exps(ctx, renv, exps)),
        // Cons: rename head and tail
        ast::ExpKind::Cons(exp_head, exp_tail) => {
            let exp_head = rename_exp(ctx, renv, exp_head);
            let exp_tail = rename_exp(ctx, renv, exp_tail);
            ast::ExpKind::Cons(Box::new(exp_head), Box::new(exp_tail))
        }
        // Iteration: rename inside, then its variables
        ast::ExpKind::Iter(exp_inner, ast::ExpIter { iter, vars }) => {
            let exp_inner = rename_exp(ctx, renv, exp_inner);
            // Iteration variables follow renamed occurrences still used inside
            let frees = exp_inner.free_ids();
            let mut vars_renamed = Vec::new();
            for var in vars {
                match renv.get(&var.id) {
                    None => vars_renamed.push(var.clone()),
                    Some(ids_rename) if ids_rename.is_empty() => vars_renamed.push(var.clone()),
                    Some(ids_rename) => {
                        vars_renamed.extend(
                            ids_rename
                                .iter()
                                .filter(|id_rename| frees.contains(id_rename))
                                .map(|id_rename| ast::Var {
                                    id: id_rename.clone(),
                                    typ: var.typ.clone(),
                                    iters: var.iters.clone(),
                                }),
                        );
                    }
                }
            }
            ast::ExpKind::Iter(
                Box::new(exp_inner),
                ast::ExpIter { iter: *iter, vars: vars_renamed },
            )
        }
        // Non-invertible nodes hold no binders
        _ => return exp.clone(),
    };
    note_phrase!(node: kind, note: exp.note.clone(), span: exp.span.clone())
}

pub fn rename_exps(ctx: &mut Context, renv: &mut RenameEnv, exps: &[ast::Exp]) -> Vec<ast::Exp> {
    exps.iter().map(|exp| rename_exp(ctx, renv, exp)).collect()
}

pub fn rename_arg(ctx: &mut Context, renv: &mut RenameEnv, arg: &ast::Arg) -> ast::Arg {
    let ast::ArgKind::Exp(exp) = &arg.node else {
        return arg.clone();
    };
    let exp = rename_exp(ctx, renv, exp);
    phrase!(node: ast::ArgKind::Exp(Box::new(exp)), span: arg.span.clone())
}

pub fn rename_args(ctx: &mut Context, renv: &mut RenameEnv, args: &[ast::Arg]) -> Vec<ast::Arg> {
    args.iter().map(|arg| rename_arg(ctx, renv, arg)).collect()
}

// == Side-condition generation

/// Source identifiers involved in a repeated binding.
pub struct Origin {
    pub id_bound: Id,
    pub id_repeated: Id,
}

/// A multibind check and the identifiers that caused it.
pub struct AnalyzedPrem {
    pub prem_al: al::ast::Prem,
    pub origin: Origin,
}

/// Builds `id = id_rename`.
fn gen_exp_equality(id: &Id, id_rename: &Id, typ: &ast::Typ) -> ast::Exp {
    let exp_l = crate::note_phrase!(node: crate::lang::il::ast::ExpKind::Id(id.clone()), note: typ.node.clone(), span: id.span.clone());
    let exp_r = crate::note_phrase!(node: crate::lang::il::ast::ExpKind::Id(id_rename.clone()), note: typ.node.clone(), span: id.span.clone());
    note_phrase! {
        node: ast::ExpKind::Cmp(
            ast::CmpOp::Bool(prim::bool::CmpOp::Eq),
            ast::OpTyp::Bool,
            Box::new(exp_l),
            Box::new(exp_r),
        ),
        note: ast::TypKind::Bool,
        span: id.span.clone(),
    }
}

/// Builds `x = x' /\ x = x''` for one repeated identifier under its iterations.
///
/// Returns `None` when the identifier occurred only once.
fn generate_side_condition(
    iter_ctx: &ICtx,
    dim: &Dim,
    id: &Id,
    ids_rename: &[Id],
) -> Option<AnalyzedPrem> {
    let mut ids_repeated = ids_rename.iter().skip(1);
    let id_rename = ids_repeated.next()?;
    let id_bound = ids_rename.first()?.clone();
    // Locate the condition at the last occurrence
    let mut id_repeated = id.clone();
    id_repeated.span = ids_rename.last()?.span.clone();
    let mut exp = gen_exp_equality(&id_repeated, id_rename, &dim.typ);
    // Conjoin one equality per renamed occurrence
    for id_rename in ids_repeated {
        let exp_r = gen_exp_equality(&id_repeated, id_rename, &dim.typ);
        exp = note_phrase! {
            node: ast::ExpKind::Bin(
                ast::BinOp::Bool(prim::bool::BinOp::And),
                ast::OpTyp::Bool,
                Box::new(exp),
                Box::new(exp_r),
            ),
            note: ast::TypKind::Bool,
            span: id_repeated.span.clone(),
        };
    }
    let prem = phrase! {
        node: al::ast::PremKind::If(al::ast::IfPrem { exp }),
        span: id_repeated.span.clone(),
    };

    // Iterate over the identifier's own dimension and the enclosing iterations
    let mut iterations = dim.iters.clone();
    iterations.extend(iter_ctx.iters());
    let mut iter_ctx_side = ICtx::from_iterations(
        iterations
            .into_iter()
            .map(|iter| Iteration { iter, vars_bound: vec![], vars_bind: vec![] })
            .collect(),
    );
    let venv = std::iter::once(&id_repeated)
        .chain(ids_rename)
        .map(|id| (id.clone(), Dim::new(dim.typ.clone(), vec![])))
        .collect::<VEnv>();
    iter_ctx_side.add_vars_bound(venv);
    Some(AnalyzedPrem {
        prem_al: iter_ctx_side.iterate_prem(prem),
        origin: Origin { id_bound, id_repeated },
    })
}

/// Builds one check per repeated identifier with its source identifiers.
pub fn generate_side_conditions(iter_ctx: &ICtx, renv: &RenameEnv) -> Vec<AnalyzedPrem> {
    renv.iter()
        .filter_map(|(id, ids_rename)| {
            let dim = renv.dimension(id)?;
            generate_side_condition(iter_ctx, dim, id, ids_rename)
        })
        .collect()
}
