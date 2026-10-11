//! Elaboration-language validation and conversion to internal syntax
//!
//! `elaborate` walks EL definitions in source order
//! and builds the `Context` as declarations appear;
//!
//! `populate_defs` then moves the collected rule groups and clauses
//! into their relation and function definitions;
//!
//! `dimension::analyze_spec` annotates iterations.
//!
//! Expressions elaborate bidirectionally:
//! `infer_exp` synthesizes a type bottom-up,
//! while `elab_exp` checks against an expected type and inserts casts,
//! so a `nat` variable where `int` is expected becomes an upcast.

use std::rc::Rc;

use crate::lang::{
    common::{
        Id,
        ds::map::IdMap,
        prim,
        source::{Phrase, Span},
    },
    data::notation::tree::{self as notation, MixfixRef, MixfixView},
    hints::input,
    traits::{at::At, free::FreeIds, print::Print},
};

use crate::lang::el::ast as el;

use crate::lang::il::{ast as il, fresh, var};

use crate::diagnostic::Report;

use crate::runtime::{
    ops::typ::{
        Theta, equiv_func_typ, equiv_typ, expand_typ, optimize_sub_typ, sub_typ, subst_not_typ,
        subst_params, subst_typ, subst_typs,
    },
    typdef::TypeDef,
};

use crate::{note_phrase, phrase};

use super::{
    backtrack::{
        Backtrack, choose_sequential, fatal, finish, mismatch, success, unavailable, unwrap,
        unwrap_from_result,
    },
    context::Context,
    dimension,
    error::{self, ElabError},
    expect::{ExpExpect, NotExpect, StructExpect},
};

// == Checks

// - Identifiers

/// Checks that an identifier carries no suffix, as a type identifier must.
fn valid_tid(id: &Id) -> bool {
    id.strip_suffix().node == id.node
}

/// Finds the first repeated parameter while retaining the earlier occurrence.
fn find_repeated_tparam(tparams: &[el::TParam]) -> Option<(&Id, &Span)> {
    let mut spans_seen = IdMap::new();
    for tparam in tparams {
        // Stop at the first duplicate in source order
        if let Some(span_previous) = spans_seen.get(tparam) {
            return Some((tparam, *span_previous));
        }
        spans_seen.insert(tparam.clone(), &tparam.span);
    }
    None
}

// == Types

// - Type destructuring

/// Requires the expanded type to be `text`.
fn as_text_typ_unavailable(ctx: &Context, typ_il: &il::Typ) -> Backtrack<()> {
    let typ_il = unwrap_from_result!(
        expand_typ(&ctx.tdenv, typ_il)
            .map_err(|error| { error::typ::type_operation_invalid("cannot expand type", error) })
    );
    match &typ_il.node {
        il::TypKind::Text => success!(()),
        _ => unavailable!(error: error::typ::type_shape_mismatch("text", &typ_il.span)),
    }
}

/// Requires text type, using the caller's diagnostic on mismatch.
fn as_text_typ_mismatch(
    ctx: &Context,
    typ_il: &il::Typ,
    on_mismatch: impl FnOnce() -> ElabError,
) -> Backtrack<()> {
    match as_text_typ_unavailable(ctx, typ_il) {
        unavailable!(_) => mismatch!(error: on_mismatch()),
        result => result,
    }
}

/// Destructs the expanded type into its element type and iteration.
fn as_iter_typ_unavailable(ctx: &Context, typ_il: &il::Typ) -> Backtrack<(il::Typ, il::Iter)> {
    let typ_il = unwrap_from_result!(
        expand_typ(&ctx.tdenv, typ_il)
            .map_err(|error| { error::typ::type_operation_invalid("cannot expand type", error) })
    )
    .into_owned();
    let span = typ_il.span;
    let il::TypKind::Iter(typ_inner_il, iter_il) = typ_il.node else {
        return unavailable!(error: error::typ::type_shape_mismatch("an iteration", &span));
    };
    success!((*typ_inner_il, iter_il))
}

/// Requires an iteration type, using the caller's diagnostic on mismatch.
fn as_iter_typ_mismatch(
    ctx: &Context,
    typ_il: &il::Typ,
    on_mismatch: impl FnOnce() -> ElabError,
) -> Backtrack<(il::Typ, il::Iter)> {
    match as_iter_typ_unavailable(ctx, typ_il) {
        unavailable!(_) => mismatch!(error: on_mismatch()),
        result => result,
    }
}

/// Destructs the expanded type into its tuple component types.
fn as_tuple_typ_unavailable(ctx: &Context, typ_il: &il::Typ) -> Backtrack<Vec<il::Typ>> {
    let typ_il = unwrap_from_result!(
        expand_typ(&ctx.tdenv, typ_il)
            .map_err(|error| { error::typ::type_operation_invalid("cannot expand type", error) })
    )
    .into_owned();
    let span = typ_il.span;
    let il::TypKind::Tuple(typs_il) = typ_il.node else {
        return unavailable!(error: error::typ::type_shape_mismatch("a tuple", &span));
    };
    success!(typs_il)
}

/// Requires a tuple type, using the caller's diagnostic on mismatch.
fn as_tuple_typ_mismatch(
    ctx: &Context,
    typ_il: &il::Typ,
    on_mismatch: impl FnOnce() -> ElabError,
) -> Backtrack<Vec<il::Typ>> {
    match as_tuple_typ_unavailable(ctx, typ_il) {
        unavailable!(_) => mismatch!(error: on_mismatch()),
        result => result,
    }
}

/// Destructs the expanded type into the element type of a list.
fn as_list_typ_unavailable(ctx: &Context, typ_il: &il::Typ) -> Backtrack<il::Typ> {
    let typ_il = unwrap_from_result!(
        expand_typ(&ctx.tdenv, typ_il)
            .map_err(|error| { error::typ::type_operation_invalid("cannot expand type", error) })
    )
    .into_owned();
    let span = typ_il.span;
    let il::TypKind::Iter(typ_inner_il, il::Iter::List) = typ_il.node else {
        return unavailable!(error: error::typ::type_shape_mismatch("a list", &span));
    };
    success!(*typ_inner_il)
}

/// Requires a list type, using the caller's diagnostic on mismatch.
fn as_list_typ_mismatch(
    ctx: &Context,
    typ_il: &il::Typ,
    on_mismatch: impl FnOnce() -> ElabError,
) -> Backtrack<il::Typ> {
    match as_list_typ_unavailable(ctx, typ_il) {
        unavailable!(_) => mismatch!(error: on_mismatch()),
        result => result,
    }
}

/// Destructs the expanded type into the fields of the struct type it names.
fn as_struct_typ_unavailable(
    ctx: &Context,
    typ_il: &il::Typ,
) -> Backtrack<(Span, Vec<il::TypField>)> {
    let typ_il = unwrap_from_result!(
        expand_typ(&ctx.tdenv, typ_il)
            .map_err(|error| { error::typ::type_operation_invalid("cannot expand type", error) })
    );
    // Struct types are always named types with a definition
    let il::TypKind::Var(id, _) = &typ_il.node else {
        return unavailable!(error: error::typ::type_shape_mismatch("a struct", &typ_il.span));
    };
    let Some(TypeDef::Defined(_, def_typ_il)) = ctx.find_typdef_opt(id) else {
        return unavailable!(error: error::typ::type_shape_mismatch("a struct", &typ_il.span));
    };
    match &def_typ_il.node {
        il::DefTypKind::Struct(typ_fields_il) => {
            success!((def_typ_il.span.clone(), typ_fields_il.clone()))
        }
        _ => unavailable!(error: error::typ::type_shape_mismatch("a struct", &typ_il.span)),
    }
}

/// Requires a struct type, using the caller's diagnostic on mismatch.
fn as_struct_typ_mismatch(
    ctx: &Context,
    typ_il: &il::Typ,
    on_mismatch: impl FnOnce() -> ElabError,
) -> Backtrack<(Span, Vec<il::TypField>)> {
    match as_struct_typ_unavailable(ctx, typ_il) {
        unavailable!(_) => mismatch!(error: on_mismatch()),
        result => result,
    }
}

// - Plain types

/// Elaborates a plain type, checking the type-argument arity of named types.
fn elab_plain_typ(ctx: &Context, plain_typ: &el::PlainTyp) -> Result<il::Typ, ElabError> {
    let typ_kind_il = match &plain_typ.node {
        // Primitive types map directly
        el::PlainTypKind::Bool => il::TypKind::Bool,
        el::PlainTypKind::Num(num_typ) => il::TypKind::Num(*num_typ),
        el::PlainTypKind::Text => il::TypKind::Text,
        // Named types must be defined and fully applied
        el::PlainTypKind::Var(id, targs) => {
            let (id_declaration, typdef) = ctx
                .tdenv
                .get_key_value(id)
                .ok_or_else(|| error::decl::type_undefined(id))?;
            let tparams = typdef.tparams();
            if tparams.len() != targs.len() {
                let span = targs
                    .get(tparams.len())
                    .map_or_else(|| id.span.clone(), |targ| targ.span.clone());
                return Err(error::typ::type_argument_arity_mismatch(
                    id,
                    tparams.len(),
                    targs.len(),
                    &span,
                    Some(&id_declaration.span),
                ));
            }
            let mut targs_il = Vec::with_capacity(targs.len());
            for targ in targs {
                let targ_il = elab_plain_typ(ctx, targ)?;
                targs_il.push(targ_il);
            }
            il::TypKind::Var(id.clone(), targs_il)
        }
        // Parentheses leave no trace in IL
        el::PlainTypKind::Paren(plain_typ) => {
            let typ_il = elab_plain_typ(ctx, plain_typ)?;
            typ_il.node
        }
        // Tuples elaborate componentwise
        el::PlainTypKind::Tuple(plain_typs) => {
            let mut typs_il = Vec::with_capacity(plain_typs.len());
            for plain_typ in plain_typs {
                let typ_il = elab_plain_typ(ctx, plain_typ)?;
                typs_il.push(typ_il);
            }
            il::TypKind::Tuple(typs_il)
        }
        // Iterations elaborate the element type
        el::PlainTypKind::Iter(plain_typ, iter) => {
            let typ_il = elab_plain_typ(ctx, plain_typ)?;
            il::TypKind::Iter(Box::new(typ_il), *iter)
        }
    };
    let typ_il = phrase!(node: typ_kind_il, span: plain_typ.span.clone());
    Ok(typ_il)
}

// - Notation types

/// Elaborates a plain or notation type into mixfix notation.
fn elab_not_typ(ctx: &Context, typ: &el::Typ) -> Result<il::NotTyp, ElabError> {
    match typ {
        // A plain type is a single notation argument
        el::Typ::Plain(plain_typ) => {
            let typ_il = elab_plain_typ(ctx, plain_typ)?;
            let not_typ_il = notation::make::arg(typ_il);
            let not_typ_il = phrase!(node: not_typ_il, span: plain_typ.span.clone());
            Ok(not_typ_il)
        }
        // Notation types mirror the mixfix shape
        el::Typ::Notation(not_typ) => {
            let not_typ_kind_il = match &not_typ.node {
                el::NotTypKind::Atom(atom) => notation::make::atom(atom.clone()),
                el::NotTypKind::Seq(typs) => {
                    let mut not_typs_il = Vec::with_capacity(typs.len());
                    for typ in typs {
                        let not_typ_il = elab_not_typ(ctx, typ)?;
                        not_typs_il.push(not_typ_il.node);
                    }
                    notation::make::seq(not_typs_il)
                }
                el::NotTypKind::Infix(typ_l, atom, typ_r) => {
                    let not_typ_l_il = elab_not_typ(ctx, typ_l)?;
                    let not_typ_r_il = elab_not_typ(ctx, typ_r)?;
                    notation::make::infix(not_typ_l_il.node, atom.clone(), not_typ_r_il.node)
                }
                el::NotTypKind::Brack(atom_l, typ, atom_r) => {
                    let not_typ_il = elab_not_typ(ctx, typ)?;
                    notation::make::brack(atom_l.clone(), not_typ_il.node, atom_r.clone())
                }
            };
            let not_typ_il = phrase!(node: not_typ_kind_il, span: not_typ.span.clone());
            Ok(not_typ_il)
        }
    }
}

// - Definition types

/// Expands a plain type used as a variant case into the cases it names.
///
/// With `syntax u = A | B`,
/// the case `u` in `syntax t = u | C` contributes `A` and `B`,
/// with the type arguments of `u` substituted.
fn elab_typ_case_plain(ctx: &Context, typ_il: &il::Typ) -> Result<Vec<il::TypCase>, ElabError> {
    let typ_il = expand_typ(&ctx.tdenv, typ_il).map_err(|error| {
        error::typ::type_operation_invalid("cannot expand variant extension", error)
    })?;
    let il::TypKind::Var(id, targs_il) = &typ_il.node else {
        return Err(error::typ::type_extension_primitive_unsupported(&*typ_il, &typ_il.span));
    };
    let span_declaration = ctx
        .tdenv
        .get_key_value(id)
        .map(|(id_declaration, _)| id_declaration.span.clone());
    match ctx.find_typdef(id)? {
        // Only a completed variant type can be extended
        TypeDef::Defining(_) => Err(error::typ::type_extension_incomplete(
            &*typ_il,
            &typ_il.span,
            span_declaration.as_ref().unwrap_or(&id.span),
        )),
        TypeDef::Defined(tparams, def_typ_il) => {
            let il::DefTypKind::Variant(typ_cases_il) = &def_typ_il.node else {
                return Err(error::typ::type_extension_struct_unsupported(
                    &*typ_il,
                    &typ_il.span,
                    span_declaration.as_ref().unwrap_or(&id.span),
                ));
            };
            // Substitute the type arguments into each inherited case
            let theta = Theta::from_lists(tparams, targs_il).map_err(|mismatch| {
                error::typ::type_argument_arity_mismatch(
                    id,
                    mismatch.expected,
                    mismatch.actual,
                    &typ_il.span,
                    Some(span_declaration.as_ref().unwrap_or(&id.span)),
                )
            })?;
            typ_cases_il
                .iter()
                .map(|il::TypCase { not_typ: not_typ_il, typ_origin: typ_origin_il, hints }| {
                    let find_subst = |id: &il::Id| theta.get(id);
                    let not_typ_il =
                        subst_not_typ(&find_subst, not_typ_il).map_err(|error| {
                            error::typ::type_operation_invalid("cannot substitute variant notation", error)
                        })?;
                    let targs_il =
                        subst_typs(&find_subst, &typ_origin_il.node.targs).map_err(|error| {
                            error::typ::type_operation_invalid("cannot substitute variant origin", error)
                        })?;
                    let typ_origin_il = phrase! {
                        node: il::TypOriginKind { id: typ_origin_il.node.id.clone(), targs: targs_il },
                        span: typ_origin_il.span.clone(),
                    };
                    Ok(il::TypCase { not_typ: not_typ_il, typ_origin: typ_origin_il, hints: hints.clone() })
                })
                .collect()
        }
        // Parameter and extern types have no cases to inherit
        TypeDef::Parameter => Err(error::typ::type_extension_parameter_unsupported(
            &*typ_il,
            &typ_il.span,
            span_declaration.as_ref().unwrap_or(&id.span),
        )),
        TypeDef::Extern => Err(error::typ::type_extension_extern_unsupported(
            &*typ_il,
            &typ_il.span,
            span_declaration.as_ref().unwrap_or(&id.span),
        )),
    }
}

/// Elaborates the body of a type definition and builds its stored `TypeDef`.
fn elab_def_typ(
    ctx: &Context,
    id: &Id,
    tparams: &[el::TParam],
    def_typ: &el::DefTyp,
) -> Result<(TypeDef, il::DefTyp), ElabError> {
    let def_typ_il = match &def_typ.node {
        el::DefTypKind::Plain(plain_typ) => {
            let typ_il = elab_plain_typ(ctx, plain_typ)?;
            let def_typ_kind_il = il::DefTypKind::Plain(typ_il);
            phrase!(node: def_typ_kind_il, span: plain_typ.span.clone())
        }
        el::DefTypKind::Struct(fields) => {
            let mut typ_fields_il = Vec::with_capacity(fields.len());
            for el::TypField { atom, typ: plain_typ, .. } in fields {
                let typ_il = elab_plain_typ(ctx, plain_typ)?;
                typ_fields_il.push(il::TypField { atom: atom.clone(), typ: typ_il });
            }
            let def_typ_kind_il = il::DefTypKind::Struct(typ_fields_il);
            phrase!(node: def_typ_kind_il, span: def_typ.span.clone())
        }
        // Cases originate from this type applied to its own parameters
        el::DefTypKind::Variant(cases) => {
            let targs_il = tparams
                .iter()
                .map(|tparam| {
                    let typ_kind_il = il::TypKind::Var(tparam.clone(), vec![]);
                    phrase!(node: typ_kind_il, span: tparam.span.clone())
                })
                .collect();
            let typ_origin_kind_il = il::TypOriginKind { id: id.clone(), targs: targs_il };
            let typ_origin_il = phrase!(node: typ_origin_kind_il, span: id.span.clone());
            let mut typ_cases_il = vec![];
            // Plain cases inherit another variant, notation cases are new
            for el::TypCase { typ, hints } in cases {
                match typ {
                    el::Typ::Plain(plain_typ) => {
                        let typ_il = elab_plain_typ(ctx, plain_typ)?;
                        let typ_cases_plain_il = elab_typ_case_plain(ctx, &typ_il)?;
                        typ_cases_il.extend(typ_cases_plain_il);
                    }
                    el::Typ::Notation(_) => {
                        let not_typ_il = elab_not_typ(ctx, typ)?;
                        typ_cases_il.push(il::TypCase {
                            not_typ: not_typ_il,
                            typ_origin: typ_origin_il.clone(),
                            hints: hints.clone(),
                        });
                    }
                }
            }
            // Two cases with the same mixfix shape would be ambiguous
            for (idx, typ_case_il) in typ_cases_il.iter().enumerate() {
                let mixop = typ_case_il.not_typ.node.mixop();
                if let Some(typ_case_previous_il) = typ_cases_il[..idx]
                    .iter()
                    .find(|typ_case_other_il| typ_case_other_il.not_typ.node.mixop() == mixop)
                {
                    return Err(error::typ::variant_case_shape_repeated(
                        &Print::to_string(mixop.as_ref()),
                        &typ_case_il.not_typ.span,
                        &typ_case_previous_il.not_typ.span,
                    ));
                }
            }
            let def_typ_kind_il = il::DefTypKind::Variant(typ_cases_il);
            phrase!(node: def_typ_kind_il, span: def_typ.span.clone())
        }
    };
    let typdef = TypeDef::Defined(tparams.to_vec(), Box::new(def_typ_il.clone()));
    Ok((typdef, def_typ_il))
}

// == Elaboration helpers

/// Wraps a type kind with the span of the term it describes.
fn typ_at(typ_kind_il: il::TypKind, span: &Span) -> il::Typ {
    phrase!(node: typ_kind_il, span: span.clone())
}

// == Expressions

// - Expression type inference

/// Fails inference of a construct whose type cannot be synthesized.
fn unavailable_infer<T>(span: &Span, construct: &str) -> Backtrack<T> {
    unavailable!(error: error::exp::expression_inference_invalid(span, construct))
}

/// Synthesizes the type of an expression bottom-up.
///
/// Constructs such as `eps`, structs, and notation
/// only elaborate against an expected type and fail here.
fn infer_exp(ctx: &mut Context, exp: &el::Exp) -> Backtrack<il::Exp> {
    let result = match &exp.node {
        // Inferable constructs dispatch to their rule
        el::ExpKind::Bool(value) => infer_bool_exp(ctx, &exp.span, *value),
        el::ExpKind::Num(_, value) => infer_num_exp(ctx, &exp.span, value),
        el::ExpKind::Text(value) => infer_text_exp(ctx, &exp.span, value),
        el::ExpKind::Id(id) => return infer_id_exp(ctx, &exp.span, id),
        el::ExpKind::Un(op, exp_inner) => infer_un_exp(ctx, &exp.span, *op, exp_inner),
        el::ExpKind::Bin(exp_l, op, exp_r) => infer_bin_exp(ctx, &exp.span, exp_l, *op, exp_r),
        el::ExpKind::Cmp(exp_l, op, exp_r) => infer_cmp_exp(ctx, &exp.span, exp_l, *op, exp_r),
        el::ExpKind::Arith(exp_inner) => infer_arith_exp(ctx, &exp.span, exp_inner),
        el::ExpKind::List(exps) => return infer_list_exp(ctx, &exp.span, exps),
        el::ExpKind::Cons(exp_head, exp_tail) => infer_cons_exp(ctx, &exp.span, exp_head, exp_tail),
        el::ExpKind::Cat(exp_l, exp_r) => infer_cat_exp(ctx, &exp.span, exp_l, exp_r),
        el::ExpKind::Idx(exp_base, exp_idx) => infer_idx_exp(ctx, &exp.span, exp_base, exp_idx),
        el::ExpKind::Slice(exp_base, exp_idx, exp_len) => {
            infer_slice_exp(ctx, &exp.span, exp_base, exp_idx, exp_len)
        }
        el::ExpKind::Tuple(exps) => infer_tuple_exp(ctx, &exp.span, exps),
        el::ExpKind::Len(exp_inner) => infer_len_exp(ctx, &exp.span, exp_inner),
        el::ExpKind::Mem(exp_elem, exp_set) => infer_mem_exp(ctx, &exp.span, exp_elem, exp_set),
        el::ExpKind::Dot(exp_inner, atom) => infer_dot_exp(ctx, &exp.span, exp_inner, atom),
        el::ExpKind::Upd(exp_base, path, exp_field) => {
            infer_upd_exp(ctx, &exp.span, exp_base, path, exp_field)
        }
        el::ExpKind::Paren(exp_inner) => return infer_paren_exp(ctx, &exp.span, exp_inner),
        el::ExpKind::Call(id, targs, args) => infer_call_exp(ctx, &exp.span, id, targs, args),
        el::ExpKind::Sub(exp_inner, plain_typ) => {
            infer_sub_exp(ctx, &exp.span, exp_inner, plain_typ)
        }
        el::ExpKind::Iter(exp_inner, iter) => infer_iter_exp(ctx, &exp.span, exp_inner, *iter),
        // Constructs that need an expected type cannot be inferred
        el::ExpKind::Eps => return unavailable_infer(&exp.span, "empty sequence"),
        el::ExpKind::Str(_) => return unavailable_infer(&exp.span, "struct expression"),
        el::ExpKind::Atom(_) => return unavailable_infer(&exp.span, "atom"),
        el::ExpKind::Seq(_) => return unavailable_infer(&exp.span, "sequence expression"),
        el::ExpKind::Infix(_, _, _) => return unavailable_infer(&exp.span, "infix expression"),
        el::ExpKind::Brack(_, _, _) => return unavailable_infer(&exp.span, "bracket expression"),
        el::ExpKind::Hole(_) => fatal!(error: error::exp::hole_outside_hint_unsupported(&exp.span)),
        el::ExpKind::Fuse(_, op, _) => {
            fatal!(error: error::exp::fuse_outside_hint_unsupported(&op.span))
        }
        el::ExpKind::Unparen(_) => {
            fatal!(error: error::exp::unparen_outside_hint_unsupported(&exp.span))
        }
        el::ExpKind::Latex(_) => {
            fatal!(error: error::exp::latex_outside_hint_unsupported(&exp.span))
        }
    };
    // A selected inference rule owns failures from its operands
    result.unavailable_as_mismatch()
}

fn infer_exps(ctx: &mut Context, exps: &[el::Exp]) -> Backtrack<Vec<il::Exp>> {
    let mut exps_il = Vec::with_capacity(exps.len());
    for exp in exps {
        let exp_il = unwrap!(infer_exp(ctx, exp));
        exps_il.push(exp_il);
    }
    success!(exps_il)
}

// - Boolean expression inference

fn infer_bool_exp(_ctx: &mut Context, span: &Span, value: bool) -> Backtrack<il::Exp> {
    let exp_il = note_phrase! {
        node: il::ExpKind::Bool(value),
        note: il::TypKind::Bool,
        span: span.clone(),
    };
    success!(exp_il)
}

// - Number expression inference

fn infer_num_exp(_ctx: &mut Context, span: &Span, value: &el::Num) -> Backtrack<il::Exp> {
    let exp_il = note_phrase! {
        node: il::ExpKind::Num(value.clone()),
        note: il::TypKind::Num(prim::num::to_typ(value)),
        span: span.clone(),
    };
    success!(exp_il)
}

// - Text expression inference

fn infer_text_exp(_ctx: &mut Context, span: &Span, value: &el::Text) -> Backtrack<il::Exp> {
    let exp_il = note_phrase! {
        node: il::ExpKind::Text(value.clone()),
        note: il::TypKind::Text,
        span: span.clone(),
    };
    success!(exp_il)
}

// - Identifier expression inference

/// Looks up the type of a variable through its meta-variable.
fn infer_id_exp(ctx: &mut Context, span: &Span, id: &Id) -> Backtrack<il::Exp> {
    // The suffix is dropped to find the governing meta-variable
    let tid = id.strip_suffix();
    let Some(typ_il) = ctx.find_metavar_opt(&tid) else {
        return mismatch!(error: error::exp::expression_inference_invalid(&id.span, "variable"));
    };
    let exp_il = crate::note_phrase!(node: crate::lang::il::ast::ExpKind::Id(id.clone()), note: typ_il.node.clone(), span: span.clone());
    success!(exp_il)
}

// - Unary expression inference

/// Infers a unary expression by trying each operand type the operator accepts.
fn infer_un_exp(ctx: &mut Context, span: &Span, op: el::UnOp, exp: &el::Exp) -> Backtrack<il::Exp> {
    // Infer the type of the operand
    let exp_il = unwrap!(infer_exp(ctx, exp));
    // Candidates of (operator type, operand type, result type)
    let candidates_il = match op {
        el::UnOp::Bool(_) => vec![(il::OpTyp::Bool, il::TypKind::Bool, il::TypKind::Bool)],
        el::UnOp::Num(_) => vec![
            (
                il::OpTyp::Nat,
                il::TypKind::Num(prim::num::Typ::Nat),
                il::TypKind::Num(prim::num::Typ::Nat),
            ),
            (
                il::OpTyp::Int,
                il::TypKind::Num(prim::num::Typ::Int),
                il::TypKind::Num(prim::num::Typ::Int),
            ),
        ],
    };
    // Try elaboration for each candidate
    for (op_typ_il, typ_operand_il, typ_result_il) in candidates_il {
        // Check if the operand can be cast to the expected type
        let typ_expect_il = typ_at(typ_operand_il, &exp_il.span);
        match cast_exp(ctx, &ExpExpect::plain(&typ_expect_il), exp_il.clone()) {
            success!(exp_il) => {
                let exp_il = note_phrase! {
                    node: il::ExpKind::Un(op, op_typ_il, Box::new(exp_il)),
                    note: typ_result_il,
                    span: span.clone(),
                };
                return success!(exp_il);
            }
            fatal!(reports) => return fatal!(reports),
            unavailable!(_) | mismatch!(_) => {}
        }
    }
    mismatch!(error: error::exp::operator_unop_type_mismatch(&op, &exp_il))
}

// - Binary expression inference

/// Infers a binary expression by trying each accepted pair of operand types.
fn infer_bin_exp(
    ctx: &mut Context,
    span: &Span,
    exp_l: &el::Exp,
    op: el::BinOp,
    exp_r: &el::Exp,
) -> Backtrack<il::Exp> {
    // Infer the types of both operands
    let exp_l_il = unwrap!(infer_exp(ctx, exp_l));
    let exp_r_il = unwrap!(infer_exp(ctx, exp_r));
    // Candidates of (operator type, left type, right type, result type)
    let candidates_il = match op {
        el::BinOp::Bool(_) => {
            vec![(il::OpTyp::Bool, il::TypKind::Bool, il::TypKind::Bool, il::TypKind::Bool)]
        }
        // Subtraction on naturals yields an integer
        el::BinOp::Num(prim::num::BinOp::Sub) => vec![
            (
                il::OpTyp::Int,
                il::TypKind::Num(prim::num::Typ::Nat),
                il::TypKind::Num(prim::num::Typ::Nat),
                il::TypKind::Num(prim::num::Typ::Int),
            ),
            (
                il::OpTyp::Int,
                il::TypKind::Num(prim::num::Typ::Int),
                il::TypKind::Num(prim::num::Typ::Int),
                il::TypKind::Num(prim::num::Typ::Int),
            ),
        ],
        el::BinOp::Num(_) => vec![
            (
                il::OpTyp::Nat,
                il::TypKind::Num(prim::num::Typ::Nat),
                il::TypKind::Num(prim::num::Typ::Nat),
                il::TypKind::Num(prim::num::Typ::Nat),
            ),
            (
                il::OpTyp::Int,
                il::TypKind::Num(prim::num::Typ::Int),
                il::TypKind::Num(prim::num::Typ::Int),
                il::TypKind::Num(prim::num::Typ::Int),
            ),
        ],
    };
    // Try each candidate, casting both operands to its operand types
    for (op_typ_il, typ_expect_l_il, typ_expect_r_il, typ_result_il) in candidates_il {
        let typ_expect_l_il = typ_at(typ_expect_l_il, &exp_l_il.span);
        let typ_expect_r_il = typ_at(typ_expect_r_il, &exp_r_il.span);
        let exp_l_il = match cast_exp(ctx, &ExpExpect::plain(&typ_expect_l_il), exp_l_il.clone()) {
            success!(exp_l_il) => exp_l_il,
            fatal!(reports) => return fatal!(reports),
            unavailable!(_) | mismatch!(_) => continue,
        };
        let exp_r_il = match cast_exp(ctx, &ExpExpect::plain(&typ_expect_r_il), exp_r_il.clone()) {
            success!(exp_r_il) => exp_r_il,
            fatal!(reports) => return fatal!(reports),
            unavailable!(_) | mismatch!(_) => continue,
        };
        let exp_il = note_phrase! {
            node: il::ExpKind::Bin(op, op_typ_il, Box::new(exp_l_il), Box::new(exp_r_il)),
            note: typ_result_il,
            span: span.clone(),
        };
        return success!(exp_il);
    }
    mismatch!(error: error::exp::operator_binop_type_mismatch(&op, &exp_l_il, &exp_r_il))
}

// - Comparison expression inference

/// Infers a comparison.
///
/// Equality checks one side against the other;
/// ordering tries each numeric type for both sides.
fn infer_cmp_exp(
    ctx: &mut Context,
    span: &Span,
    exp_l: &el::Exp,
    op: el::CmpOp,
    exp_r: &el::Exp,
) -> Backtrack<il::Exp> {
    match op {
        // Equality: infer one side and check the other against it
        el::CmpOp::Bool(_) => choose_sequential(
            ctx,
            // Infer the right side, check the left against it
            |ctx| {
                let exp_r_il = unwrap!(infer_exp(ctx, exp_r));
                let typ_expect_l_il =
                    phrase!(node: exp_r_il.note.as_ref().clone(), span: exp_r_il.span.clone());
                let exp_l_il = unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_expect_l_il), exp_l));
                let exp_il = note_phrase! {
                    node: il::ExpKind::Cmp(op, il::OpTyp::Bool, Box::new(exp_l_il), Box::new(exp_r_il)),
                    note: il::TypKind::Bool,
                    span: span.clone(),
                };
                success!(exp_il)
            },
            // Infer the left side, check the right against it
            |ctx| {
                let exp_l_il = unwrap!(infer_exp(ctx, exp_l));
                let typ_expect_r_il =
                    phrase!(node: exp_l_il.note.as_ref().clone(), span: exp_l_il.span.clone());
                let exp_r_il = unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_expect_r_il), exp_r));
                let exp_il = note_phrase! {
                    node: il::ExpKind::Cmp(op, il::OpTyp::Bool, Box::new(exp_l_il), Box::new(exp_r_il)),
                    note: il::TypKind::Bool,
                    span: span.clone(),
                };
                success!(exp_il)
            },
        ),
        // Ordering: both sides must cast to the same numeric type
        el::CmpOp::Num(_) => {
            let exp_l_il = unwrap!(infer_exp(ctx, exp_l));
            let exp_r_il = unwrap!(infer_exp(ctx, exp_r));
            for (op_typ_il, typ_expect_kind_il) in [
                (il::OpTyp::Nat, il::TypKind::Num(prim::num::Typ::Nat)),
                (il::OpTyp::Int, il::TypKind::Num(prim::num::Typ::Int)),
            ] {
                let typ_expect_l_il = typ_at(typ_expect_kind_il.clone(), &exp_l_il.span);
                let typ_expect_r_il = typ_at(typ_expect_kind_il, &exp_r_il.span);
                let exp_l_il =
                    match cast_exp(ctx, &ExpExpect::plain(&typ_expect_l_il), exp_l_il.clone()) {
                        success!(exp_l_il) => exp_l_il,
                        fatal!(reports) => return fatal!(reports),
                        unavailable!(_) | mismatch!(_) => continue,
                    };
                let exp_r_il =
                    match cast_exp(ctx, &ExpExpect::plain(&typ_expect_r_il), exp_r_il.clone()) {
                        success!(exp_r_il) => exp_r_il,
                        fatal!(reports) => return fatal!(reports),
                        unavailable!(_) | mismatch!(_) => continue,
                    };
                let exp_il = note_phrase! {
                    node: il::ExpKind::Cmp(op, op_typ_il, Box::new(exp_l_il), Box::new(exp_r_il)),
                    note: il::TypKind::Bool,
                    span: span.clone(),
                };
                return success!(exp_il);
            }
            mismatch!(error: error::exp::operator_cmpop_type_mismatch(&op, &exp_l_il, &exp_r_il))
        }
    }
}

// - Arithmetic expression inference

/// Infers the inner expression and widens its span to the arithmetic brackets.
fn infer_arith_exp(ctx: &mut Context, span: &Span, exp: &el::Exp) -> Backtrack<il::Exp> {
    let mut exp_il = unwrap!(infer_exp(ctx, exp));
    exp_il.span = span.clone();
    success!(exp_il)
}

// - List expression inference

/// Infers a non-empty list from its first element; the rest must agree.
fn infer_list_exp(ctx: &mut Context, span: &Span, exps: &[el::Exp]) -> Backtrack<il::Exp> {
    let Some((exp_first, exps_rest)) = exps.split_first() else {
        return unavailable_infer(span, "empty list");
    };
    // The element type comes from the first element
    let exp_first_il = unwrap!(infer_exp(ctx, exp_first).unavailable_as_mismatch());
    let typ_first_il =
        phrase!(node: exp_first_il.note.as_ref().clone(), span: exp_first_il.span.clone());
    let mut exps_rest_il = unwrap!(infer_exps(ctx, exps_rest).unavailable_as_mismatch());
    // Remaining elements must have an equivalent type
    for (idx, exp_il) in exps_rest_il.iter().enumerate() {
        let typ_il = phrase!(node: exp_il.note.as_ref().clone(), span: exp_il.span.clone());
        let equivalent =
            unwrap_from_result!(equiv_typ(&ctx.tdenv, &typ_first_il, &typ_il).map_err(|error| {
                error::typ::type_operation_invalid("cannot compare list element types", error)
            }));
        if !equivalent {
            return mismatch!(error: error::exp::list_element_type_mismatch(idx + 1, &typ_first_il, &typ_il));
        }
    }
    let mut exps_il = vec![exp_first_il];
    exps_il.append(&mut exps_rest_il);
    let exp_il = note_phrase! {
        node: il::ExpKind::List(exps_il),
        note: il::TypKind::Iter(Box::new(typ_first_il), il::Iter::List),
        span: span.clone(),
    };
    success!(exp_il)
}

// - Cons expression inference

/// Infers a cons from its head and checks the tail against that list type.
fn infer_cons_exp(
    ctx: &mut Context,
    span: &Span,
    exp_head: &el::Exp,
    exp_tail: &el::Exp,
) -> Backtrack<il::Exp> {
    let exp_head_il = unwrap!(infer_exp(ctx, exp_head));
    let typ_head_il =
        phrase!(node: exp_head_il.note.as_ref().clone(), span: exp_head_il.span.clone());
    // The tail must be a list of the head's type
    let typ_list_kind_il = il::TypKind::Iter(Box::new(typ_head_il), il::Iter::List);
    let typ_list_il = phrase!(node: typ_list_kind_il, span: exp_head_il.span.clone());
    let exp_tail_il = unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_list_il), exp_tail));
    let exp_il = note_phrase! {
        node: il::ExpKind::Cons(Box::new(exp_head_il), Box::new(exp_tail_il)),
        note: typ_list_il.node,
        span: span.clone(),
    };
    success!(exp_il)
}

// - Concatenation expression inference

/// Infers a concatenation as lists first, then as texts.
fn infer_cat_exp(
    ctx: &mut Context,
    span: &Span,
    exp_l: &el::Exp,
    exp_r: &el::Exp,
) -> Backtrack<il::Exp> {
    choose_sequential(
        ctx,
        // Lists: the right side must match the list type of the left
        |ctx| {
            let exp_l_il = unwrap!(infer_exp(ctx, exp_l));
            let typ_l_il =
                phrase!(node: exp_l_il.note.as_ref().clone(), span: exp_l_il.span.clone());
            let typ_base_il = unwrap!(as_list_typ_mismatch(ctx, &typ_l_il, || {
                error::exp::expression_type_shape_mismatch(&typ_l_il.span, &typ_l_il, "a list")
            }));
            let typ_list_kind_il = il::TypKind::Iter(Box::new(typ_base_il.clone()), il::Iter::List);
            let typ_list_il = phrase!(node: typ_list_kind_il, span: typ_base_il.span);
            let exp_r_il = unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_list_il), exp_r));
            let exp_il = note_phrase! {
                node: il::ExpKind::Cat(Box::new(exp_l_il), Box::new(exp_r_il)),
                note: typ_list_il.node,
                span: span.clone(),
            };
            success!(exp_il)
        },
        // Texts: both sides must elaborate as text
        |ctx| {
            let typ_text_l_il = typ_at(il::TypKind::Text, &exp_l.span);
            let exp_l_il = unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_text_l_il), exp_l));
            let typ_text_r_il = typ_at(il::TypKind::Text, &exp_r.span);
            let exp_r_il = unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_text_r_il), exp_r));
            let exp_il = note_phrase! {
                node: il::ExpKind::Cat(Box::new(exp_l_il), Box::new(exp_r_il)),
                note: il::TypKind::Text,
                span: span.clone(),
            };
            success!(exp_il)
        },
    )
}

// - Tuple expression inference

/// Infers a tuple from the types of its components.
fn infer_tuple_exp(ctx: &mut Context, span: &Span, exps: &[el::Exp]) -> Backtrack<il::Exp> {
    let exps_il = unwrap!(infer_exps(ctx, exps));
    let typs_il = exps_il
        .iter()
        .map(|exp_il| phrase!(node: exp_il.note.as_ref().clone(), span: exp_il.span.clone()))
        .collect();
    let exp_il = note_phrase! {
        node: il::ExpKind::Tuple(exps_il),
        note: il::TypKind::Tuple(typs_il),
        span: span.clone(),
    };
    success!(exp_il)
}

// - Length expression inference

/// Infers a length of a list first, then of a text.
fn infer_len_exp(ctx: &mut Context, span: &Span, exp: &el::Exp) -> Backtrack<il::Exp> {
    choose_sequential(
        ctx,
        // Length of a list
        |ctx| {
            let exp_il = unwrap!(infer_exp(ctx, exp));
            let typ_il = phrase!(node: exp_il.note.as_ref().clone(), span: exp_il.span.clone());
            unwrap!(as_list_typ_mismatch(ctx, &typ_il, || {
                error::exp::expression_type_shape_mismatch(&typ_il.span, &typ_il, "a list")
            }));
            let exp_il = note_phrase! {
                node: il::ExpKind::Len(Box::new(exp_il)),
                note: il::TypKind::Num(prim::num::Typ::Nat),
                span: span.clone(),
            };
            success!(exp_il)
        },
        // Length of a text
        |ctx| {
            let typ_text_il = typ_at(il::TypKind::Text, &exp.span);
            let exp_il = unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_text_il), exp));
            let exp_il = note_phrase! {
                node: il::ExpKind::Len(Box::new(exp_il)),
                note: il::TypKind::Num(prim::num::Typ::Nat),
                span: span.clone(),
            };
            success!(exp_il)
        },
    )
}

// - Membership expression inference

/// Infers a membership test from the element first, then from the list.
fn infer_mem_exp(
    ctx: &mut Context,
    span: &Span,
    exp_elem: &el::Exp,
    exp_set: &el::Exp,
) -> Backtrack<il::Exp> {
    choose_sequential(
        ctx,
        // Element first: the list must hold elements of its type
        |ctx| {
            let exp_elem_il = unwrap!(infer_exp(ctx, exp_elem));
            let typ_elem_il = phrase! {
                node: exp_elem_il.note.as_ref().clone(),
                span: exp_elem_il.span.clone(),
            };
            let typ_list_kind_il = il::TypKind::Iter(Box::new(typ_elem_il), il::Iter::List);
            let typ_list_il = phrase!(node: typ_list_kind_il, span: exp_set.span.clone());
            let exp_set_il = unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_list_il), exp_set));
            let exp_il = note_phrase! {
                node: il::ExpKind::Mem(Box::new(exp_elem_il), Box::new(exp_set_il)),
                note: il::TypKind::Bool,
                span: span.clone(),
            };
            success!(exp_il)
        },
        // List first: the element must have its element type
        |ctx| {
            let exp_set_il = unwrap!(infer_exp(ctx, exp_set));
            let typ_set_il =
                phrase!(node: exp_set_il.note.as_ref().clone(), span: exp_set_il.span.clone());
            let typ_elem_il = unwrap!(as_list_typ_mismatch(ctx, &typ_set_il, || {
                error::exp::expression_type_shape_mismatch(&typ_set_il.span, &typ_set_il, "a list")
            }));
            let exp_elem_il = unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_elem_il), exp_elem));
            let exp_il = note_phrase! {
                node: il::ExpKind::Mem(Box::new(exp_elem_il), Box::new(exp_set_il)),
                note: il::TypKind::Bool,
                span: span.clone(),
            };
            success!(exp_il)
        },
    )
}

// - Index expression inference

/// Infers an index into a list first, then into a text.
fn infer_idx_exp(
    ctx: &mut Context,
    span: &Span,
    exp_base: &el::Exp,
    exp_idx: &el::Exp,
) -> Backtrack<il::Exp> {
    choose_sequential(
        ctx,
        // Index into a list: the element type is the result
        |ctx| {
            let exp_base_il = unwrap!(infer_exp(ctx, exp_base));
            let typ_base_il =
                phrase!(node: exp_base_il.note.as_ref().clone(), span: exp_base_il.span.clone());
            let typ_elem_il = unwrap!(as_list_typ_mismatch(ctx, &typ_base_il, || {
                error::exp::expression_type_shape_mismatch(
                    &typ_base_il.span,
                    &typ_base_il,
                    "a list",
                )
            }));
            let typ_nat_il = typ_at(il::TypKind::Num(prim::num::Typ::Nat), &exp_idx.span);
            let exp_idx_il = unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_nat_il), exp_idx));
            let exp_il = note_phrase! {
                node: il::ExpKind::Idx(Box::new(exp_base_il), Box::new(exp_idx_il)),
                note: typ_elem_il.node,
                span: span.clone(),
            };
            success!(exp_il)
        },
        // Index into a text yields a text
        |ctx| {
            let typ_text_il = typ_at(il::TypKind::Text, &exp_base.span);
            let exp_base_il = unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_text_il), exp_base));
            let typ_nat_il = typ_at(il::TypKind::Num(prim::num::Typ::Nat), &exp_idx.span);
            let exp_idx_il = unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_nat_il), exp_idx));
            let exp_il = note_phrase! {
                node: il::ExpKind::Idx(Box::new(exp_base_il), Box::new(exp_idx_il)),
                note: il::TypKind::Text,
                span: span.clone(),
            };
            success!(exp_il)
        },
    )
}

// - Slice expression inference

/// Infers a slice of a list first, then of a text.
fn infer_slice_exp(
    ctx: &mut Context,
    span: &Span,
    exp_base: &el::Exp,
    exp_idx: &el::Exp,
    exp_len: &el::Exp,
) -> Backtrack<il::Exp> {
    choose_sequential(
        ctx,
        // Slice of a list keeps the list type
        |ctx| {
            let exp_base_il = unwrap!(infer_exp(ctx, exp_base));
            let typ_base_il =
                phrase!(node: exp_base_il.note.as_ref().clone(), span: exp_base_il.span.clone());
            unwrap!(as_list_typ_mismatch(ctx, &typ_base_il, || {
                error::exp::expression_type_shape_mismatch(
                    &typ_base_il.span,
                    &typ_base_il,
                    "a list",
                )
            }));
            let typ_nat_il = typ_at(il::TypKind::Num(prim::num::Typ::Nat), &exp_idx.span);
            let exp_idx_il = unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_nat_il), exp_idx));
            let typ_nat_il = typ_at(il::TypKind::Num(prim::num::Typ::Nat), &exp_len.span);
            let exp_len_il = unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_nat_il), exp_len));
            let exp_il = note_phrase! { node: il::ExpKind::Slice(
                Box::new(exp_base_il),
                Box::new(exp_idx_il),
                Box::new(exp_len_il),
            ), note: typ_base_il.node, span: span.clone() };
            success!(exp_il)
        },
        // Slice of a text
        |ctx| {
            let typ_text_il = typ_at(il::TypKind::Text, &exp_base.span);
            let exp_base_il = unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_text_il), exp_base));
            let typ_nat_il = typ_at(il::TypKind::Num(prim::num::Typ::Nat), &exp_idx.span);
            let exp_idx_il = unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_nat_il), exp_idx));
            let typ_nat_il = typ_at(il::TypKind::Num(prim::num::Typ::Nat), &exp_len.span);
            let exp_len_il = unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_nat_il), exp_len));
            let exp_il = note_phrase! { node: il::ExpKind::Slice(
                Box::new(exp_base_il),
                Box::new(exp_idx_il),
                Box::new(exp_len_il),
            ), note: il::TypKind::Text, span: span.clone() };
            success!(exp_il)
        },
    )
}

// - Dot expression inference

/// Infers a field access by looking the field up in the struct type.
fn infer_dot_exp(
    ctx: &mut Context,
    span: &Span,
    exp: &el::Exp,
    atom: &el::Atom,
) -> Backtrack<il::Exp> {
    let exp_il = unwrap!(infer_exp(ctx, exp));
    let typ_il = phrase!(node: exp_il.note.as_ref().clone(), span: exp_il.span.clone());
    // The field must exist in the struct type
    let (span_declaration, typ_fields_il) = unwrap!(as_struct_typ_mismatch(ctx, &typ_il, || {
        error::exp::expression_type_shape_mismatch(&typ_il.span, &typ_il, "a struct")
    }));
    let Some(il::TypField { typ: typ_field_il, .. }) = typ_fields_il
        .iter()
        .find(|il::TypField { atom: atom_field, .. }| atom_field.node == atom.node)
    else {
        return mismatch!(error: error::exp::struct_field_undefined(&typ_il, atom, &typ_fields_il, &span_declaration));
    };
    let exp_il = note_phrase! {
        node: il::ExpKind::Dot(Box::new(exp_il), atom.clone()),
        note: typ_field_il.node.clone(),
        span: span.clone(),
    };
    success!(exp_il)
}

// - Update expression inference

/// Infers a field update, checking the new value against the type at the path.
fn infer_upd_exp(
    ctx: &mut Context,
    span: &Span,
    exp_base: &el::Exp,
    path: &el::Path,
    exp_field: &el::Exp,
) -> Backtrack<il::Exp> {
    let exp_base_il = unwrap!(infer_exp(ctx, exp_base));
    let typ_base_il =
        phrase!(node: exp_base_il.note.as_ref().clone(), span: exp_base_il.span.clone());
    // The path from the base type gives the field type
    let path_il = unwrap!(elab_path(ctx, &typ_base_il, path));
    let typ_field_il = typ_at(path_il.note.as_ref().clone(), &path_il.span);
    let exp_field_il = unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_field_il), exp_field));
    let exp_il = note_phrase! {
        node: il::ExpKind::Upd(Box::new(exp_base_il), Box::new(path_il), Box::new(exp_field_il)),
        note: typ_base_il.node,
        span: span.clone(),
    };
    success!(exp_il)
}

// - Parenthesized expression inference

/// Infers the inner expression and widens its span to the parentheses.
fn infer_paren_exp(ctx: &mut Context, span: &Span, exp: &el::Exp) -> Backtrack<il::Exp> {
    let mut exp_il = unwrap!(infer_exp(ctx, exp));
    exp_il.span = span.clone();
    success!(exp_il)
}

// - Call expression inference

/// Infers a call by instantiating the signature with the type arguments.
fn infer_call_exp(
    ctx: &mut Context,
    span: &Span,
    id: &Id,
    targs: &[el::Targ],
    args: &[el::Arg],
) -> Backtrack<il::Exp> {
    let (span_declaration, tparams_il, params_il, typ_ret_il) = match ctx.find_func_signature(id) {
        Ok((id_declaration, tparams, params, typ_ret)) => {
            (id_declaration.span.clone(), tparams.to_vec(), params.to_vec(), typ_ret.clone())
        }
        Err(error) => return fatal!(error: error),
    };
    // Type arguments must match the declared type parameters
    if tparams_il.len() != targs.len() {
        let span_error = targs
            .get(tparams_il.len())
            .map_or_else(|| id.span.clone(), |targ| targ.span.clone());
        return fatal!(error: error::arg::function_call_type_argument_arity_mismatch(
            id,
            tparams_il.len(),
            targs.len(),
            &span_error,
            &span_declaration,
        ));
    }
    let mut targs_il = Vec::with_capacity(targs.len());
    for targ in targs {
        let targ_il = match elab_plain_typ(ctx, targ) {
            Ok(targ_il) => targ_il,
            Err(error) => return fatal!(error: error),
        };
        targs_il.push(targ_il);
    }
    let theta = match Theta::from_lists(&tparams_il, &targs_il) {
        Ok(theta) => theta,
        Err(mismatch) => {
            return fatal!(error: error::arg::function_call_type_argument_arity_mismatch(
                id,
                mismatch.expected,
                mismatch.actual,
                &id.span,
                &span_declaration,
            ));
        }
    };
    // Substitute the type arguments into the parameters and return type
    let find_subst = |id: &il::Id| theta.get(id);
    let params_il = unwrap_from_result!(subst_params(&find_subst, &params_il).map_err(|error| {
        error::typ::type_operation_invalid("cannot instantiate call parameters", error)
    }));
    let typ_ret_il = unwrap_from_result!(subst_typ(&find_subst, &typ_ret_il).map_err(|error| {
        error::typ::type_operation_invalid("cannot instantiate call result", error)
    }));
    // Check the arguments against the instantiated parameters
    let args_il =
        unwrap!(elab_args(ctx, &params_il, args, ArgMode::Call, span, id, &span_declaration));
    let exp_il = note_phrase! {
        node: il::ExpKind::Call(id.clone(), targs_il, args_il),
        note: typ_ret_il.node,
        span: span.clone(),
    };
    success!(exp_il)
}

// - Iterated expression inference

/// Infers an iterated expression, leaving its variables to dimension analysis.
fn infer_iter_exp(
    ctx: &mut Context,
    span: &Span,
    exp: &el::Exp,
    iter: el::Iter,
) -> Backtrack<il::Exp> {
    let exp_il = unwrap!(infer_exp(ctx, exp));
    let typ_il = phrase!(node: exp_il.note.as_ref().clone(), span: exp_il.span.clone());
    let iter_il = iter;
    let exp_il = note_phrase! {
        node: il::ExpKind::Iter(Box::new(exp_il), il::ExpIter { iter: iter_il, vars: vec![] }),
        note: il::TypKind::Iter(Box::new(typ_il), iter_il),
        span: span.clone(),
    };
    success!(exp_il)
}

// - Subtype expression inference

/// Infers a subtype test, which requires the two types to be comparable.
fn infer_sub_exp(
    ctx: &mut Context,
    span: &Span,
    exp: &el::Exp,
    plain_typ: &el::PlainTyp,
) -> Backtrack<il::Exp> {
    let exp_il = unwrap!(infer_exp(ctx, exp));
    let typ_source_il = phrase!(node: exp_il.note.as_ref().clone(), span: exp_il.span.clone());
    let typ_target_il = match elab_plain_typ(ctx, plain_typ) {
        Ok(typ_il) => typ_il,
        Err(error) => return fatal!(error: error),
    };
    // The types must be related in at least one direction
    let source_sub =
        unwrap_from_result!(sub_typ(&ctx.tdenv, &typ_source_il, &typ_target_il).map_err(|error| {
            error::typ::type_operation_invalid("cannot compare subtype expression", error)
        }));
    let target_sub =
        unwrap_from_result!(sub_typ(&ctx.tdenv, &typ_target_il, &typ_source_il).map_err(|error| {
            error::typ::type_operation_invalid("cannot compare subtype expression", error)
        }));
    if !source_sub && !target_sub {
        return mismatch!(error: error::exp::expression_type_mismatch(
            &exp_il.span,
            "subtype expression compares incomparable types",
        ));
    }
    // Precompute the runtime check the test performs
    let check =
        unwrap_from_result!(optimize_sub_typ(&ctx.tdenv, &typ_source_il, &typ_target_il).map_err(
            |error| error::typ::type_operation_invalid("cannot optimize subtype expression", error)
        ));
    let exp_il = note_phrase! {
        node: il::ExpKind::Sub(Box::new(exp_il), Box::new(typ_target_il), Box::new(check)),
        note: il::TypKind::Bool,
        span: span.clone(),
    };
    success!(exp_il)
}

// - Expected-type expression elaboration

/// Checks a cast, using declaration context only for a type mismatch.
fn cast_exp(ctx: &Context, expect: &ExpExpect<'_>, exp_il: il::Exp) -> Backtrack<il::Exp> {
    let typ_expect_il = expect.typ_il;
    let typ_infer_il = phrase!(node: exp_il.note.as_ref().clone(), span: exp_il.span.clone());
    // Equivalent types need no cast
    let equivalent =
        unwrap_from_result!(equiv_typ(&ctx.tdenv, typ_expect_il, &typ_infer_il).map_err(|error| {
            error::typ::type_operation_invalid("cannot compare expression types", error)
        }));
    if equivalent {
        return success!(exp_il);
    }
    // A subtype is upcast to the expected type
    let subtype =
        unwrap_from_result!(sub_typ(&ctx.tdenv, &typ_infer_il, typ_expect_il).map_err(|error| {
            error::typ::type_operation_invalid("cannot compare expression types", error)
        }));
    if subtype {
        let span = exp_il.span.clone();
        let exp_il = note_phrase! {
            node: il::ExpKind::UpCast(Box::new(typ_expect_il.clone()), Box::new(exp_il)),
            note: typ_expect_il.node.clone(),
            span: span,
        };
        return success!(exp_il);
    }
    // Refine a failed tuple cast without changing the cast or failure state
    if let Ok(typ_infer_expanded_il) = expand_typ(&ctx.tdenv, &typ_infer_il)
        && let il::TypKind::Tuple(typs_infer_il) = &typ_infer_expanded_il.node
        && let Ok(typ_expanded_il) = expand_typ(&ctx.tdenv, typ_expect_il)
        && let il::TypKind::Tuple(typs_expect_il) = &typ_expanded_il.node
        && typs_expect_il.len() != typs_infer_il.len()
    {
        return mismatch!(error: error::exp::tuple_arity_mismatch(
            typs_expect_il.len(), typs_infer_il.len(), &exp_il.span, expect,
        ));
    }
    // Diagnose the failed type relationship before discarding inference data
    let error = error::exp::expected_type_mismatch(expect, &typ_infer_il);
    mismatch!(error: error)
}

/// Moves the span of an expression and its casts to the enclosing parentheses.
fn respan_parenthesized_exp(exp_il: &mut il::Exp, span: &Span) {
    exp_il.span = span.clone();
    match &mut exp_il.node {
        il::ExpKind::UpCast(_, exp_inner_il) | il::ExpKind::DownCast(_, exp_inner_il) => {
            respan_parenthesized_exp(exp_inner_il, span);
        }
        _ => {}
    }
}

/// Checks an expression and links any inner cause to its declaration.
fn elab_exp(ctx: &mut Context, expect: &ExpExpect<'_>, exp: &el::Exp) -> Backtrack<il::Exp> {
    // A parenthesized result takes the span of the parentheses
    let parenthesized = matches!(exp.node, el::ExpKind::Paren(_));
    let span = exp.span.clone();
    let result = elab_exp_inner(ctx, expect, exp).map(move |mut exp_il| {
        if parenthesized {
            respan_parenthesized_exp(&mut exp_il, &span);
        }
        exp_il
    });
    // Preserve the failure state while connecting causes to the declaration
    let result = match result {
        unavailable!(mut reports) => {
            error::exp::annotate_expected_type(expect, &mut reports);
            unavailable!(reports)
        }
        fatal!(mut reports) => {
            error::exp::annotate_expected_type(expect, &mut reports);
            fatal!(reports)
        }
        mismatch!(mut reports) => {
            error::exp::annotate_expected_type(expect, &mut reports);
            mismatch!(reports)
        }
        result => result,
    };
    // A single cause already identifies the failed expression
    match result {
        unavailable!(reports) if reports.len() == 1 => unavailable!(reports),
        mismatch!(reports) if reports.len() == 1 => mismatch!(reports),
        result => result.nest(exp.span.clone(), "expression elaboration failed"),
    }
}

/// Preserves expression alternatives while carrying expected-type diagnostics.
fn elab_exp_inner(ctx: &mut Context, expect: &ExpExpect<'_>, exp: &el::Exp) -> Backtrack<il::Exp> {
    let typ_expect_il = expect.typ_il;
    // Keep the singleton candidate ahead of the normal reading
    match as_iter_typ_unavailable(ctx, typ_expect_il) {
        success!((typ_base_il, iter_expect_il)) => {
            elab_iter_exp_alternatives(ctx, expect, &typ_base_il, iter_expect_il, exp)
        }
        fatal!(reports) => fatal!(reports),
        unavailable!(_) | mismatch!(_) => elab_exp_normal(ctx, expect, exp),
    }
}

/// Tries both iteration readings while reporting the applicable checking rule.
fn elab_iter_exp_alternatives(
    ctx: &mut Context,
    expect: &ExpExpect<'_>,
    typ_base_il: &il::Typ,
    iter_expect_il: il::Iter,
    exp: &el::Exp,
) -> Backtrack<il::Exp> {
    let typ_expect_il = expect.typ_il;
    // Commit only a successful singleton candidate and stop on fatal errors
    let mut ctx_candidate = ctx.clone();
    let result_singleton = match elab_singleton_iter_exp(
        &mut ctx_candidate,
        typ_expect_il,
        typ_base_il,
        iter_expect_il,
        exp,
    ) {
        // Commit the successful singleton reading
        success!(exp_il) => {
            *ctx = ctx_candidate;
            return success!(exp_il);
        }
        // A fatal singleton failure forbids the normal reading
        fatal!(reports) => return fatal!(reports),
        result => result,
    };
    // Retry the normal reading from the original context
    let mut ctx_candidate = ctx.clone();
    match elab_exp_normal(&mut ctx_candidate, expect, exp) {
        // Commit only the successful normal reading
        success!(exp_il) => {
            *ctx = ctx_candidate;
            success!(exp_il)
        }
        // An unavailable normal rule leaves the singleton's cause applicable
        unavailable!(reports) => match result_singleton {
            unavailable!(reports_singleton) | mismatch!(reports_singleton)
                if reports_singleton.is_empty() =>
            {
                unavailable!(reports)
            }
            result => result,
        },
        // Applicable mismatches and fatal errors keep their original state
        result => result,
    }
}

// - Singleton iteration expression elaboration

/// Elaborates a `t` expression as a singleton where `t*` or `t?` is expected.
fn elab_singleton_iter_exp(
    ctx: &mut Context,
    typ_expect_il: &il::Typ,
    typ_base_il: &il::Typ,
    iter_expect_il: il::Iter,
    exp: &el::Exp,
) -> Backtrack<il::Exp> {
    // Wildcards and empty sequences are never singletons
    if matches!(&exp.node, el::ExpKind::Id(id) if id.node == "_")
        || matches!(&exp.node, el::ExpKind::Eps)
        || matches!(&exp.node, el::ExpKind::List(exps) if exps.is_empty())
    {
        return unavailable!(vec![]);
    }
    let exp_inner_il =
        unwrap!(elab_exp(ctx, &ExpExpect::plain(typ_base_il), exp).unavailable_as_mismatch());
    let exp_kind_il = match iter_expect_il {
        il::Iter::Opt => il::ExpKind::Opt(Some(Box::new(exp_inner_il))),
        il::Iter::List => il::ExpKind::List(vec![exp_inner_il]),
    };
    success!(note_phrase! {
        node: exp_kind_il,
        note: typ_expect_il.node.clone(),
        span: exp.span.clone(),
    })
}

// - Normal expression elaboration

/// Elaborates by inference and cast, falling back to contextual elaboration.
///
/// When inference mismatches,
/// a wildcard becomes a fresh variable,
/// a named expected type is unfolded into its plain, struct, or variant body,
/// and other constructs elaborate against the expected type directly.
fn elab_exp_normal(ctx: &mut Context, expect: &ExpExpect<'_>, exp: &el::Exp) -> Backtrack<il::Exp> {
    // Try inference first, keeping its context only on success
    let mut ctx_candidate = ctx.clone();
    match infer_exp(&mut ctx_candidate, exp) {
        success!(exp_il) => match cast_exp(&ctx_candidate, expect, exp_il) {
            success!(exp_il) => {
                *ctx = ctx_candidate;
                success!(exp_il)
            }
            result => result,
        },
        // Fatal inference forbids contextual retry
        fatal!(reports) => fatal!(reports),
        // An unavailable fallback must not obscure an inference cause
        mismatch!(reports_infer) => match elab_exp_normal_fallback(ctx, expect, exp) {
            unavailable!(_) => mismatch!(reports_infer),
            result => result,
        },
        // A contextual rule supersedes the absence of an inference rule
        unavailable!(reports_infer) => match elab_exp_normal_fallback(ctx, expect, exp) {
            unavailable!(reports) if reports.is_empty() => unavailable!(reports_infer),
            result => result,
        },
    }
}

/// Elaborates an expression against the shape of its expected type.
fn elab_exp_normal_fallback(
    ctx: &mut Context,
    expect: &ExpExpect<'_>,
    exp: &el::Exp,
) -> Backtrack<il::Exp> {
    let typ_expect_il = expect.typ_il;
    // A wildcard `_` becomes a fresh variable of the expected type
    if matches!(&exp.node, el::ExpKind::Id(id) if id.node == "_") {
        return elab_wildcard_exp(ctx, typ_expect_il, exp);
    }
    // Unfold a named expected type into its definition
    if let il::TypKind::Var(id, targs_il) = &typ_expect_il.node
        && let Some(TypeDef::Defined(tparams, def_typ_il)) = ctx.find_typdef_opt(id)
    {
        let theta = match Theta::from_lists(tparams, targs_il) {
            Ok(theta) => theta,
            Err(mismatch) => {
                return fatal!(error: error::typ::type_argument_arity_mismatch(
                    id,
                    mismatch.expected,
                    mismatch.actual,
                    &typ_expect_il.span,
                    ctx.tdenv
                        .get_key_value(id)
                        .map(|(id_declaration, _)| &id_declaration.span),
                ));
            }
        };
        match &def_typ_il.node {
            // Alias: elaborate against the aliased type
            il::DefTypKind::Plain(typ_il) => {
                let typ_il =
                    unwrap_from_result!(subst_typ(&|id| theta.get(id), typ_il).map_err(|error| {
                        error::typ::type_operation_invalid("cannot instantiate type alias", error)
                    }));
                let expect = ExpExpect { typ_il: &typ_il, ..*expect };
                return elab_exp_normal(ctx, &expect, exp);
            }
            // Struct: match the fields
            il::DefTypKind::Struct(typ_fields_il) => {
                let mut typ_fields_subst_il = Vec::with_capacity(typ_fields_il.len());
                for il::TypField { atom, typ: typ_il } in typ_fields_il {
                    let span_declaration = typ_il.span.clone();
                    let typ_il = unwrap_from_result!(
                        subst_typ(&|id| theta.get(id), typ_il).map_err(|error| {
                            error::typ::type_operation_invalid(
                                "cannot instantiate struct field",
                                error,
                            )
                        })
                    );
                    typ_fields_subst_il
                        .push((span_declaration, il::TypField { atom: atom.clone(), typ: typ_il }));
                }
                let expect = StructExpect {
                    typ_il: typ_expect_il,
                    span_declaration: def_typ_il.span.clone(),
                    typ_fields_il: typ_fields_subst_il,
                };
                return elab_struct_exp(ctx, &expect, exp);
            }
            // Variant: match exactly one case
            il::DefTypKind::Variant(typ_cases_il) => {
                let mut typ_cases_subst_il = Vec::with_capacity(typ_cases_il.len());
                for il::TypCase { not_typ: not_typ_il, typ_origin: typ_origin_il, hints } in
                    typ_cases_il
                {
                    let find_subst = |id: &il::Id| theta.get(id);
                    let not_typ_il = unwrap_from_result!(
                        subst_not_typ(&find_subst, not_typ_il).map_err(|error| {
                            error::typ::type_operation_invalid(
                                "cannot instantiate variant case",
                                error,
                            )
                        })
                    );
                    let targs_il = unwrap_from_result!(
                        subst_typs(&find_subst, &typ_origin_il.node.targs).map_err(|error| {
                            error::typ::type_operation_invalid(
                                "cannot instantiate variant origin",
                                error,
                            )
                        })
                    );
                    let typ_origin_il = phrase! {
                        node: il::TypOriginKind { id: typ_origin_il.node.id.clone(), targs: targs_il },
                        span: typ_origin_il.span.clone(),
                    };
                    typ_cases_subst_il.push(il::TypCase {
                        not_typ: not_typ_il,
                        typ_origin: typ_origin_il,
                        hints: hints.clone(),
                    });
                }
                return elab_variant_exp(ctx, typ_expect_il, &typ_cases_subst_il, exp);
            }
        }
    }
    elab_plain_exp(ctx, expect, exp)
}

// - Wildcard expression elaboration

/// Replaces `_` with a fresh variable of the expected type, recorded as free.
fn elab_wildcard_exp(
    ctx: &mut Context,
    typ_expect_il: &il::Typ,
    exp: &el::Exp,
) -> Backtrack<il::Exp> {
    let var_il =
        fresh::var_from_typ_wildcard(&ctx.menv, &ctx.frees, exp.span.clone(), typ_expect_il);
    let exp_il = var::as_exp(false, &var_il);
    ctx.add_free(var_il.id);
    success!(exp_il)
}

// - Plain expression elaboration

/// Elaborates constructs whose shape is fixed by the expected type.
fn elab_plain_exp(ctx: &mut Context, expect: &ExpExpect<'_>, exp: &el::Exp) -> Backtrack<il::Exp> {
    let typ_expect_il = expect.typ_il;
    let exp_kind_il = match &exp.node {
        el::ExpKind::Eps => unwrap!(elab_eps_exp(ctx, typ_expect_il, exp)),
        el::ExpKind::List(exps) => unwrap!(elab_list_exp(ctx, typ_expect_il, exp, exps)),
        el::ExpKind::Cons(exp_head, exp_tail) => {
            unwrap!(elab_cons_exp(ctx, typ_expect_il, exp, exp_head, exp_tail))
        }
        el::ExpKind::Cat(exp_l, exp_r) => unwrap!(elab_cat_exp(ctx, typ_expect_il, exp_l, exp_r)),
        el::ExpKind::Tuple(exps) => unwrap!(elab_tuple_exp(ctx, expect, exp, exps)),
        el::ExpKind::Paren(exp_inner) => unwrap!(elab_paren_exp(ctx, expect, exp_inner)),
        el::ExpKind::Iter(exp_inner, iter) => {
            unwrap!(elab_iter_exp(ctx, typ_expect_il, exp, exp_inner, *iter))
        }
        // Anything else needed inference
        _ => {
            return unavailable!(Vec::new());
        }
    };
    success!(note_phrase! {
        node: exp_kind_il,
        note: typ_expect_il.node.clone(),
        span: exp.span.clone(),
    })
}

// - Epsilon expression elaboration

/// Elaborates `eps` as the empty option or list the expected type requires.
fn elab_eps_exp(ctx: &Context, typ_expect_il: &il::Typ, exp: &el::Exp) -> Backtrack<il::ExpKind> {
    let (_, iter_expect_il) = unwrap!(as_iter_typ_mismatch(ctx, typ_expect_il, || {
        error::exp::expected_expression_mismatch(typ_expect_il, exp)
    }));
    success!(match iter_expect_il {
        il::Iter::Opt => il::ExpKind::Opt(None),
        il::Iter::List => il::ExpKind::List(vec![]),
    })
}

// - List expression elaboration

/// Elaborates a list literal element-wise against an expected list type.
fn elab_list_exp(
    ctx: &mut Context,
    typ_expect_il: &il::Typ,
    exp: &el::Exp,
    exps: &[el::Exp],
) -> Backtrack<il::ExpKind> {
    // A list literal requires a list rather than an optional type
    let typ_base_il = unwrap!(as_list_typ_mismatch(ctx, typ_expect_il, || {
        error::exp::expected_expression_mismatch(typ_expect_il, exp)
    }));
    let mut exps_il = Vec::with_capacity(exps.len());
    for exp in exps {
        exps_il.push(unwrap!(
            elab_exp(ctx, &ExpExpect::plain(&typ_base_il), exp).unavailable_as_mismatch()
        ));
    }
    success!(il::ExpKind::List(exps_il))
}

// - Cons expression elaboration

/// Elaborates a cons against an expected iteration type.
fn elab_cons_exp(
    ctx: &mut Context,
    typ_expect_il: &il::Typ,
    exp: &el::Exp,
    exp_head: &el::Exp,
    exp_tail: &el::Exp,
) -> Backtrack<il::ExpKind> {
    let (typ_base_il, iter_expect_il) = unwrap!(as_iter_typ_mismatch(ctx, typ_expect_il, || {
        error::exp::expected_expression_mismatch(typ_expect_il, exp)
    }));
    let exp_head_il =
        unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_base_il), exp_head).unavailable_as_mismatch());
    let typ_tail_kind_il = il::TypKind::Iter(Box::new(typ_base_il), iter_expect_il);
    let typ_tail_il = phrase!(node: typ_tail_kind_il, span: typ_expect_il.span.clone());
    let exp_tail_il =
        unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_tail_il), exp_tail).unavailable_as_mismatch());
    success!(il::ExpKind::Cons(Box::new(exp_head_il), Box::new(exp_tail_il)))
}

// - Concatenation expression elaboration

/// Elaborates a concatenation as iterations first, then as texts.
fn elab_cat_exp(
    ctx: &mut Context,
    typ_expect_il: &il::Typ,
    exp_l: &el::Exp,
    exp_r: &el::Exp,
) -> Backtrack<il::ExpKind> {
    choose_sequential(
        ctx,
        // Iterations: both sides at the expected type
        |ctx| {
            let (typ_base_il, iter_expect_il) =
                unwrap!(as_iter_typ_unavailable(ctx, typ_expect_il));
            let typ_iter_kind_il = il::TypKind::Iter(Box::new(typ_base_il.clone()), iter_expect_il);
            let typ_iter_il = phrase!(node: typ_iter_kind_il, span: typ_base_il.span);
            let exp_l_il = unwrap!(
                elab_exp(ctx, &ExpExpect::plain(&typ_iter_il), exp_l).unavailable_as_mismatch()
            );
            let exp_r_il = unwrap!(
                elab_exp(ctx, &ExpExpect::plain(&typ_iter_il), exp_r).unavailable_as_mismatch()
            );
            success!(il::ExpKind::Cat(Box::new(exp_l_il), Box::new(exp_r_il)))
        },
        // Texts
        |ctx| {
            let typ_text_il = typ_at(il::TypKind::Text, &exp_l.span);
            let exp_l_il = unwrap!(
                elab_exp(ctx, &ExpExpect::plain(&typ_text_il), exp_l).unavailable_as_mismatch()
            );
            let typ_text_il = typ_at(il::TypKind::Text, &exp_r.span);
            let exp_r_il = unwrap!(
                elab_exp(ctx, &ExpExpect::plain(&typ_text_il), exp_r).unavailable_as_mismatch()
            );
            success!(il::ExpKind::Cat(Box::new(exp_l_il), Box::new(exp_r_il)))
        },
    )
}

// - Tuple expression elaboration

/// Elaborates a tuple component-wise against an expected tuple type.
fn elab_tuple_exp(
    ctx: &mut Context,
    expect: &ExpExpect<'_>,
    exp: &el::Exp,
    exps: &[el::Exp],
) -> Backtrack<il::ExpKind> {
    // Component count must match the tuple type
    let typs_expect_il = unwrap!(as_tuple_typ_mismatch(ctx, expect.typ_il, || {
        error::exp::expected_expression_mismatch(expect.typ_il, exp)
    }));
    if typs_expect_il.len() != exps.len() {
        return mismatch!(error: error::exp::tuple_arity_mismatch(
            typs_expect_il.len(), exps.len(), &exp.span, expect,
        ));
    }
    let mut exps_il = Vec::with_capacity(exps.len());
    for (typ_expect_il, exp) in typs_expect_il.iter().zip(exps) {
        exps_il.push(unwrap!(
            elab_exp(ctx, &ExpExpect::plain(typ_expect_il), exp).unavailable_as_mismatch()
        ));
    }
    success!(il::ExpKind::Tuple(exps_il))
}

// - Parenthesized expression elaboration

fn elab_paren_exp(
    ctx: &mut Context,
    expect: &ExpExpect<'_>,
    exp: &el::Exp,
) -> Backtrack<il::ExpKind> {
    let exp_il = unwrap!(elab_exp(ctx, expect, exp));
    success!(exp_il.node)
}

// - Iterated expression elaboration

/// Elaborates an iterated expression, requiring the expected iteration.
fn elab_iter_exp(
    ctx: &mut Context,
    typ_expect_il: &il::Typ,
    exp: &el::Exp,
    exp_inner: &el::Exp,
    iter: el::Iter,
) -> Backtrack<il::ExpKind> {
    let (typ_base_il, iter_expect_il) = unwrap!(as_iter_typ_mismatch(ctx, typ_expect_il, || {
        error::exp::expected_expression_mismatch(typ_expect_il, exp)
    }));
    // The iteration must match the expected one
    let iter_il = iter;
    if iter_il != iter_expect_il {
        return mismatch!(error: error::exp::expression_iteration_mismatch(
            &exp.span,
            format!("expected iteration '{}', but found '{}'", iter_expect_il.to_string(), iter_il.to_string()),
        ));
    }
    let exp_il = unwrap!(
        elab_exp(ctx, &ExpExpect::plain(&typ_base_il), exp_inner).unavailable_as_mismatch()
    );
    success!(il::ExpKind::Iter(Box::new(exp_il), il::ExpIter { iter: iter_il, vars: vec![] }))
}

// - Notation expression elaboration

/// Checks token correspondence without elaborating or accepting input.
fn notation_shape_matches(not_typ_il: MixfixRef<'_, il::Typ>, exp: &el::Exp) -> bool {
    // Match the same transparent parentheses as notation elaboration
    if let el::ExpKind::Paren(exp) = &exp.node {
        return notation_shape_matches(not_typ_il, exp);
    }
    match (not_typ_il.view(), &exp.node) {
        // Argument types are checked only by elaboration
        (MixfixView::Arg(_), _) => true,
        // Token spelling does not affect structural correspondence
        (MixfixView::Atom(_), el::ExpKind::Atom(_)) => true,
        // Every sequence position must have a corresponding subtree
        (MixfixView::Seq(not_typs_il), el::ExpKind::Seq(exps)) => {
            not_typs_il.len() == exps.len()
                && not_typs_il
                    .into_iter()
                    .zip(exps)
                    .all(|(not_typ_il, exp)| notation_shape_matches(not_typ_il, exp))
        }
        // Both infix operands must correspond
        (MixfixView::Infix(not_typ_l_il, _, not_typ_r_il), el::ExpKind::Infix(exp_l, _, exp_r)) => {
            notation_shape_matches(not_typ_l_il, exp_l)
                && notation_shape_matches(not_typ_r_il, exp_r)
        }
        // Bracket contents must correspond
        (MixfixView::Brack(_, not_typ_inner_il, _), el::ExpKind::Brack(_, exp_inner, _)) => {
            notation_shape_matches(not_typ_inner_il, exp_inner)
        }
        // Other syntax does not establish token correspondence
        _ => false,
    }
}

/// Elaborates notation with its declaration and zero-based argument positions.
///
/// The expression shares the declaration's mixop:
/// matching takes atoms and brackets from the declaration,
/// so only the arguments come from the expression.
fn elab_not_exp(ctx: &mut Context, expect: &NotExpect<'_>, exp: &el::Exp) -> Backtrack<il::NotExp> {
    let not_typ_il = &expect.not_typ_il.node;
    let mut exps_il = Vec::with_capacity(not_typ_il.arity());
    unwrap!(elab_not_exp_inner(ctx, not_typ_il.as_ref(), exp, expect, &mut exps_il));
    let not_exp_il = notation::Mixfix::new(Rc::clone(not_typ_il.mixop()), exps_il)
        .expect("notation elaboration fills every argument position");
    success!(not_exp_il)
}

/// Matches notation in source order without changing candidate boundaries.
///
/// Elaborated arguments are appended to `exps_il`,
/// whose length numbers the next argument position.
fn elab_not_exp_inner(
    ctx: &mut Context,
    not_typ_il: MixfixRef<'_, il::Typ>,
    exp: &el::Exp,
    expect: &NotExpect<'_>,
    exps_il: &mut Vec<il::Exp>,
) -> Backtrack<()> {
    // Parentheses around notation are transparent
    if let el::ExpKind::Paren(exp) = &exp.node {
        return elab_not_exp_inner(ctx, not_typ_il, exp, expect, exps_il);
    }
    match (not_typ_il.view(), &exp.node) {
        // Count only argument slots and retain a nested failure's own cause
        (MixfixView::Arg(typ_il), _) => {
            let exp_expect = ExpExpect::not_arg(expect.kind, exps_il.len(), typ_il);
            match elab_exp_inner(ctx, &exp_expect, exp) {
                // Keep the elaborated argument
                success!(exp_il) => {
                    exps_il.push(exp_il);
                    success!(())
                }
                // Preserve a fatal inner cause with its declaration context
                fatal!(mut reports) => {
                    error::exp::annotate_expected_type(&exp_expect, &mut reports);
                    fatal!(reports)
                }
                // Preserve mismatches for later notation candidates
                unavailable!(mut reports) | mismatch!(mut reports) => {
                    error::exp::annotate_expected_type(&exp_expect, &mut reports);
                    mismatch!(reports)
                }
            }
        }
        // Atoms must agree literally
        (MixfixView::Atom(atom_expect), el::ExpKind::Atom(atom))
            if atom_expect.node == atom.node =>
        {
            success!(())
        }
        // Sequences match element-wise without guessing omitted positions
        (MixfixView::Seq(not_typs_il), el::ExpKind::Seq(exps)) => {
            if not_typs_il.len() != exps.len() {
                return unavailable!(error: error::not::notation_shape_mismatch(&exp.span, expect));
            }
            for (not_typ_il, exp) in not_typs_il.into_iter().zip(exps) {
                unwrap!(elab_not_exp_inner(ctx, not_typ_il, exp, expect, exps_il));
            }
            success!(())
        }
        // Infix: the atom agrees, both sides match their notation
        (
            MixfixView::Infix(not_typ_l_il, atom_expect, not_typ_r_il),
            el::ExpKind::Infix(exp_l, atom, exp_r),
        ) if atom_expect.node == atom.node => {
            unwrap!(elab_not_exp_inner(ctx, not_typ_l_il, exp_l, expect, exps_il));
            unwrap!(elab_not_exp_inner(ctx, not_typ_r_il, exp_r, expect, exps_il));
            success!(())
        }
        // Brackets: both atoms agree, the inside matches its notation
        (
            MixfixView::Brack(atom_expect_l, not_typ_inner_il, atom_expect_r),
            el::ExpKind::Brack(atom_l, exp_inner, atom_r),
        ) if atom_expect_l.node == atom_l.node && atom_expect_r.node == atom_r.node => {
            unwrap!(elab_not_exp_inner(ctx, not_typ_inner_il, exp_inner, expect, exps_il));
            success!(())
        }
        // Explain corresponding tokens only after establishing their shape
        _ => {
            let error = match (not_typ_il.view(), &exp.node) {
                // Two literal atoms occupy the same matched slot
                (MixfixView::Atom(atom_expect), el::ExpKind::Atom(atom)) => {
                    error::not::notation_token_mismatch(atom_expect, atom, expect)
                }
                // Equal operand shapes identify the differing infix token
                (MixfixView::Infix(_, atom_expect, _), el::ExpKind::Infix(_, atom, _))
                    if notation_shape_matches(not_typ_il, exp) =>
                {
                    error::not::notation_token_mismatch(atom_expect, atom, expect)
                }
                // Equal contents identify the differing bracket delimiter
                (
                    MixfixView::Brack(atom_expect_l, _, atom_expect_r),
                    el::ExpKind::Brack(atom_l, _, atom_r),
                ) if notation_shape_matches(not_typ_il, exp) => {
                    let (atom_expect, atom) = if atom_expect_l.node != atom_l.node {
                        (atom_expect_l, atom_l)
                    } else {
                        (atom_expect_r, atom_r)
                    };
                    error::not::notation_token_mismatch(atom_expect, atom, expect)
                }
                // Different tree shapes do not establish a missing token
                _ => {
                    return unavailable!(error: error::not::notation_shape_mismatch(&exp.span, expect));
                }
            };
            mismatch!(error: error)
        }
    }
}

// - Struct expression elaboration

/// Elaborates a struct literal field by field in declaration order.
fn elab_struct_exp(
    ctx: &mut Context,
    expect: &StructExpect<'_>,
    exp: &el::Exp,
) -> Backtrack<il::Exp> {
    let el::ExpKind::Str(exp_fields) = &exp.node else {
        return unavailable!(Vec::new());
    };
    // Field count must match the struct type
    if expect.typ_fields_il.len() != exp_fields.len() {
        return mismatch!(error: error::exp::struct_field_arity_mismatch(
            expect, exp_fields.len(), &exp.span,
        ));
    }
    let mut exp_fields_il = Vec::with_capacity(exp_fields.len());
    for ((span_decl, il::TypField { atom: atom_expect, typ: typ_il }), (atom, exp_field)) in
        expect.typ_fields_il.iter().zip(exp_fields)
    {
        // Fields must appear in declaration order
        if atom_expect.node != atom.node {
            return mismatch!(error: error::exp::struct_field_name_mismatch(
                atom_expect, atom,
            ));
        }
        let exp_field_il = unwrap!(
            elab_exp(ctx, &ExpExpect::struct_field(atom_expect, typ_il, span_decl), exp_field)
                .unavailable_as_mismatch()
        );
        exp_fields_il.push(il::ExpField { atom: atom_expect.clone(), exp: exp_field_il });
    }
    success!(note_phrase! {
        node: il::ExpKind::Str(exp_fields_il),
        note: expect.typ_il.node.clone(),
        span: exp.span.clone(),
    })
}

// - Variant expression elaboration

/// Elaborates an expression against a variant type; one case must match.
fn elab_variant_exp(
    ctx: &mut Context,
    typ_expect_il: &il::Typ,
    typ_cases_il: &[il::TypCase],
    exp: &el::Exp,
) -> Backtrack<il::Exp> {
    // Try each case on a copy of the context
    let mut ctx_match = ctx.clone();
    let mut exps_match_il = Vec::new();
    let mut reports_unavailable = Vec::new();
    let mut reports_mismatch = Vec::new();
    let mut has_mismatch = false;
    for il::TypCase { not_typ: not_typ_il, typ_origin: typ_origin_il, .. } in typ_cases_il {
        let mut ctx_candidate = ctx_match.clone();
        let not_exp_il =
            match elab_not_exp(&mut ctx_candidate, &NotExpect::variant(not_typ_il), exp) {
                success!(not_exp_il) => not_exp_il,
                fatal!(reports) => return fatal!(reports),
                unavailable!(mut reports) => {
                    reports_unavailable.append(&mut reports);
                    continue;
                }
                mismatch!(mut reports) => {
                    has_mismatch = true;
                    reports_mismatch.append(&mut reports);
                    continue;
                }
            };
        let typ_case_kind_il =
            il::TypKind::Var(typ_origin_il.node.id.clone(), typ_origin_il.node.targs.clone());
        let typ_case_il = phrase!(node: typ_case_kind_il, span: typ_origin_il.span.clone());
        let exp_case_kind_il = il::ExpKind::Case(Box::new(not_exp_il));
        let exp_case_il = note_phrase! {
            node: exp_case_kind_il,
            note: typ_case_il.node.clone(),
            span: exp.span.clone(),
        };
        // The case type must still cast to the expected type
        let exp_case_il =
            match cast_exp(&ctx_candidate, &ExpExpect::plain(typ_expect_il), exp_case_il) {
                success!(exp_case_il) => exp_case_il,
                unavailable!(reports) | fatal!(reports) | mismatch!(reports) => {
                    return fatal!(reports);
                }
            };
        ctx_match = ctx_candidate;
        exps_match_il.push(exp_case_il);
    }
    // Exactly one case may match
    match exps_match_il.len() {
        1 => {
            *ctx = ctx_match;
            success!(exps_match_il.pop().expect("single variant match"))
        }
        0 => {
            // Keep all applicable candidates' failures so their causes are not
            // buried under unrelated candidates' shape errors
            // If none applies, keep the shape errors to show expected notations
            let reports = if has_mismatch { reports_mismatch } else { reports_unavailable };
            let count = reports.len();
            let result = if has_mismatch { mismatch!(reports) } else { unavailable!(reports) };
            if count == 1 {
                result
            } else {
                result.nest(exp.span.clone(), "expression does not match any variant case")
            }
        }
        _ => mismatch!(error: error::exp::variant_expression_match_repeated(&exp.span)),
    }
}

// == Paths

// - Path elaboration

fn elab_path(ctx: &mut Context, typ_expect_il: &il::Typ, path: &el::Path) -> Backtrack<il::Path> {
    match &path.node {
        el::PathKind::Root => success!(elab_root_path(&path.span, typ_expect_il)),
        el::PathKind::Idx(path_inner, exp_idx) => {
            elab_idx_path(ctx, &path.span, typ_expect_il, path_inner, exp_idx)
        }
        el::PathKind::Slice(path_inner, exp_idx, exp_len) => {
            elab_slice_path(ctx, &path.span, typ_expect_il, path_inner, exp_idx, exp_len)
        }
        el::PathKind::Dot(path_inner, atom) => {
            elab_dot_path(ctx, &path.span, typ_expect_il, path_inner, atom)
        }
    }
}

// - Root path elaboration

fn elab_root_path(span: &Span, typ_expect_il: &il::Typ) -> il::Path {
    note_phrase! {
        node: il::PathKind::Root,
        note: typ_expect_il.node.clone(),
        span: span.clone(),
    }
}

// - Index path elaboration

/// Elaborates an index path into a list first, then into a text.
fn elab_idx_path(
    ctx: &mut Context,
    span: &Span,
    typ_expect_il: &il::Typ,
    path_inner: &el::Path,
    exp_idx: &el::Exp,
) -> Backtrack<il::Path> {
    choose_sequential(
        ctx,
        // Index into a list: the path continues at the element type
        |ctx| {
            let path_inner_il = unwrap!(elab_path(ctx, typ_expect_il, path_inner));
            let typ_inner_il = typ_at(path_inner_il.note.as_ref().clone(), &path_inner_il.span);
            let typ_nat_il = typ_at(il::TypKind::Num(prim::num::Typ::Nat), &exp_idx.span);
            let exp_idx_il = unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_nat_il), exp_idx));
            let path_kind_il = il::PathKind::Idx(Box::new(path_inner_il), Box::new(exp_idx_il));
            let typ_elem_il = unwrap!(as_list_typ_mismatch(ctx, &typ_inner_il, || {
                error::exp::expression_type_shape_mismatch(
                    &typ_inner_il.span,
                    &typ_inner_il,
                    "a list",
                )
            }));
            success!(note_phrase! {
                node: path_kind_il,
                note: typ_elem_il.node,
                span: span.clone(),
            })
        },
        // Index into a text stays a text
        |ctx| {
            let path_inner_il = unwrap!(elab_path(ctx, typ_expect_il, path_inner));
            let typ_inner_il = typ_at(path_inner_il.note.as_ref().clone(), &path_inner_il.span);
            let typ_nat_il = typ_at(il::TypKind::Num(prim::num::Typ::Nat), &exp_idx.span);
            let exp_idx_il = unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_nat_il), exp_idx));
            let path_kind_il = il::PathKind::Idx(Box::new(path_inner_il), Box::new(exp_idx_il));
            unwrap!(as_text_typ_mismatch(ctx, &typ_inner_il, || {
                error::exp::expression_type_shape_mismatch(
                    &typ_inner_il.span,
                    &typ_inner_il,
                    "text",
                )
            }));
            success!(note_phrase! {
                node: path_kind_il,
                note: typ_inner_il.node,
                span: span.clone(),
            })
        },
    )
}

// - Slice path elaboration

/// Elaborates a slice path, which keeps the type of the list or text it slices.
fn elab_slice_path(
    ctx: &mut Context,
    span: &Span,
    typ_expect_il: &il::Typ,
    path_inner: &el::Path,
    exp_idx: &el::Exp,
    exp_len: &el::Exp,
) -> Backtrack<il::Path> {
    choose_sequential(
        ctx,
        // Slice a list after elaborating both bounds
        |ctx| {
            let path_inner_il = unwrap!(elab_path(ctx, typ_expect_il, path_inner));
            let typ_inner_il = typ_at(path_inner_il.note.as_ref().clone(), &path_inner_il.span);
            let typ_nat_il = typ_at(il::TypKind::Num(prim::num::Typ::Nat), &exp_idx.span);
            let exp_idx_il = unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_nat_il), exp_idx));
            let typ_nat_il = typ_at(il::TypKind::Num(prim::num::Typ::Nat), &exp_len.span);
            let exp_len_il = unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_nat_il), exp_len));
            let path_kind_il = il::PathKind::Slice(
                Box::new(path_inner_il),
                Box::new(exp_idx_il),
                Box::new(exp_len_il),
            );
            unwrap!(as_list_typ_mismatch(ctx, &typ_inner_il, || {
                error::exp::expression_type_shape_mismatch(
                    &typ_inner_il.span,
                    &typ_inner_il,
                    "a list",
                )
            }));
            success!(note_phrase! {
                node: path_kind_il,
                note: typ_inner_il.node,
                span: span.clone(),
            })
        },
        // Slice a text after elaborating both bounds
        |ctx| {
            let path_inner_il = unwrap!(elab_path(ctx, typ_expect_il, path_inner));
            let typ_inner_il = typ_at(path_inner_il.note.as_ref().clone(), &path_inner_il.span);
            let typ_nat_il = typ_at(il::TypKind::Num(prim::num::Typ::Nat), &exp_idx.span);
            let exp_idx_il = unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_nat_il), exp_idx));
            let typ_nat_il = typ_at(il::TypKind::Num(prim::num::Typ::Nat), &exp_len.span);
            let exp_len_il = unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_nat_il), exp_len));
            let path_kind_il = il::PathKind::Slice(
                Box::new(path_inner_il),
                Box::new(exp_idx_il),
                Box::new(exp_len_il),
            );
            unwrap!(as_text_typ_mismatch(ctx, &typ_inner_il, || {
                error::exp::expression_type_shape_mismatch(
                    &typ_inner_il.span,
                    &typ_inner_il,
                    "text",
                )
            }));
            success!(note_phrase! {
                node: path_kind_il,
                note: typ_inner_il.node,
                span: span.clone(),
            })
        },
    )
}

// - Dot path elaboration

/// Elaborates a field path by looking the field up in the struct type.
fn elab_dot_path(
    ctx: &mut Context,
    span: &Span,
    typ_expect_il: &il::Typ,
    path_inner: &el::Path,
    atom: &el::Atom,
) -> Backtrack<il::Path> {
    let path_inner_il = unwrap!(elab_path(ctx, typ_expect_il, path_inner));
    let typ_inner_il = typ_at(path_inner_il.note.as_ref().clone(), &path_inner_il.span);
    // The field must exist in the struct type
    let (span_declaration, typ_fields_il) =
        unwrap!(as_struct_typ_mismatch(ctx, &typ_inner_il, || {
            error::exp::expression_type_shape_mismatch(
                &typ_inner_il.span,
                &typ_inner_il,
                "a struct",
            )
        }));
    let Some(il::TypField { typ: typ_field_il, .. }) = typ_fields_il
        .iter()
        .find(|il::TypField { atom: atom_field, .. }| atom_field.node == atom.node)
    else {
        return mismatch!(error: error::exp::struct_field_undefined(&typ_inner_il, atom, &typ_fields_il, &span_declaration));
    };
    let path_kind_il = il::PathKind::Dot(Box::new(path_inner_il), atom.clone());
    success!(note_phrase! {
        node: path_kind_il,
        note: typ_field_il.node.clone(),
        span: span.clone(),
    })
}

// == Parameters and arguments

// - Parameter elaboration

/// Elaborates a parameter; a function parameter scopes its type parameters.
fn elab_param(ctx: &mut Context, param: &el::Param) -> Result<il::Param, ElabError> {
    let param_kind_il = match &param.node {
        el::ParamKind::Exp(plain_typ) => {
            let typ_il = elab_plain_typ(ctx, plain_typ)?;
            il::ParamKind::Exp(typ_il)
        }
        el::ParamKind::Def(id, tparams, params, plain_typ_ret) => {
            if let Some((tparam, span_previous)) = find_repeated_tparam(tparams) {
                return Err(error::arg::function_parameter_type_parameter_repeated(
                    tparam,
                    span_previous,
                ));
            }
            // Type parameters scope over the parameters and return type only
            let (params_il, typ_ret_il) = {
                let mut ctx_local = ctx.clone();
                ctx_local.add_tparams(tparams)?;
                let params_il = params
                    .iter()
                    .map(|param| elab_param(&mut ctx_local, param))
                    .collect::<Result<Vec<_>, _>>()?;
                let typ_ret_il = elab_plain_typ(&ctx_local, plain_typ_ret)?;
                (params_il, typ_ret_il)
            };
            il::ParamKind::Def(id.clone(), tparams.clone(), params_il, typ_ret_il)
        }
    };
    let param_il = phrase!(node: param_kind_il, span: param.span.clone());
    Ok(param_il)
}

/// Gives the type a parameter binds: its plain type, or a function type.
fn typ_of_param(param_il: &il::Param) -> il::Typ {
    match &param_il.node {
        il::ParamKind::Exp(typ_il) => typ_il.clone(),
        // A function parameter has a function type
        il::ParamKind::Def(_, tparams_il, params_il, typ_ret_il) => {
            let func_typ_il = il::FuncTyp {
                tparams: tparams_il.clone(),
                typs_params: params_il.iter().map(typ_of_param).collect(),
                typ_ret: Box::new(typ_ret_il.clone()),
            };
            let typ_kind_il = il::TypKind::Func(func_typ_il);
            phrase!(node: typ_kind_il, span: param_il.span.clone())
        }
    }
}

// - Argument elaboration

/// Selects argument checking at a call or binding in a definition.
#[derive(Clone, Copy, PartialEq, Eq)]
enum ArgMode {
    Call,
    Def,
}

/// Elaborates an argument against its parameter.
///
/// A function argument in a defining clause (`ArgMode::Def`)
/// declares the parameter as a local function;
/// elsewhere it must name a function with an equivalent signature.
fn elab_arg(
    ctx: &mut Context,
    param_il: &il::Param,
    arg: &el::Arg,
    mode: ArgMode,
    id_func: &Id,
    idx: usize,
) -> Backtrack<il::Arg> {
    match (&param_il.node, &arg.node) {
        // Expression arguments elaborate against the parameter type
        (il::ParamKind::Exp(typ_il), el::ArgKind::Exp(exp)) => {
            let expect = ExpExpect::func_arg(id_func, idx, typ_il, &param_il.span);
            let exp_il = unwrap!(elab_exp(ctx, &expect, exp).recoverable_as_failure());
            let arg_il = il::ArgKind::Exp(Box::new(exp_il));
            let arg_il = phrase!(node: arg_il, span: arg.span.clone());
            success!(arg_il)
        }
        // Clause definition: bind the function parameter under its own name
        (
            il::ParamKind::Def(id_param, tparams_il, params_il, typ_ret_il),
            el::ArgKind::Def(id_arg),
        ) if mode == ArgMode::Def => {
            if id_param.node != id_arg.node {
                return fatal!(error: error::arg::function_argument_name_mismatch(id_arg, id_param));
            }
            let defined_func_il = il::DefinedFunc {
                id: id_param.clone(),
                tparams: tparams_il.clone(),
                params: params_il.clone(),
                typ: typ_ret_il.clone(),
                clauses: vec![],
                else_clause: None,
                hints: vec![],
            };
            if let Err(error) = ctx.add_defined_func(defined_func_il) {
                return fatal!(error: error);
            }
            let arg_il = il::ArgKind::Def(id_arg.clone());
            let arg_il = phrase!(node: arg_il, span: arg.span.clone());
            success!(arg_il)
        }
        // Call: the named function must have an equivalent signature
        (
            il::ParamKind::Def(id_param, tparams_il, params_il, typ_ret_il),
            el::ArgKind::Def(id_arg),
        ) => {
            let (id_arg_declaration, tparams_arg_il, params_arg_il, typ_ret_arg_il) =
                match ctx.find_func_signature(id_arg) {
                    Ok(signature) => signature,
                    Err(error) => return fatal!(error: error),
                };
            let span_arg_declaration = &id_arg_declaration.span;
            if tparams_il.len() != tparams_arg_il.len() {
                return fatal!(error: error::arg::function_argument_type_parameter_arity_mismatch(
                    id_param,
                    id_arg,
                    tparams_il.len(),
                    tparams_arg_il.len(),
                    &arg.span,
                    span_arg_declaration,
                ));
            }
            if params_il.len() != params_arg_il.len() {
                return fatal!(error: error::arg::function_argument_parameter_arity_mismatch(
                    id_param,
                    id_arg,
                    params_il.len(),
                    params_arg_il.len(),
                    &arg.span,
                    span_arg_declaration,
                ));
            }
            let typ_param_il = il::FuncTyp {
                tparams: tparams_il.clone(),
                typs_params: params_il.iter().map(typ_of_param).collect(),
                typ_ret: Box::new(typ_ret_il.clone()),
            };
            let typ_arg_il = il::FuncTyp {
                tparams: tparams_arg_il.to_vec(),
                typs_params: params_arg_il.iter().map(typ_of_param).collect(),
                typ_ret: Box::new(typ_ret_arg_il.clone()),
            };
            let find_typdef_opt = |id: &il::Id| ctx.tdenv.get(id);
            let equivalent = unwrap_from_result!(
                equiv_func_typ(&find_typdef_opt, &arg.span, &typ_param_il, &typ_arg_il,).map_err(
                    |type_error| error::typ::type_operation_invalid(
                        "cannot compare function argument signatures",
                        type_error,
                    )
                )
            );
            if !equivalent {
                return fatal!(error: error::arg::function_argument_signature_mismatch(
                    id_param,
                    id_arg,
                    &arg.span,
                    span_arg_declaration,
                ));
            }
            let arg_il = il::ArgKind::Def(id_arg.clone());
            let arg_il = phrase!(node: arg_il, span: arg.span.clone());
            success!(arg_il)
        }
        (il::ParamKind::Exp(_), el::ArgKind::Def(_)) => {
            fatal!(error: error::arg::function_argument_kind_mismatch(
                "an expression",
                "a function",
                &arg.span,
                &param_il.span,
            ))
        }
        (il::ParamKind::Def(_, _, _, _), el::ArgKind::Exp(_)) => {
            fatal!(error: error::arg::function_argument_kind_mismatch(
                "a function",
                "an expression",
                &arg.span,
                &param_il.span,
            ))
        }
    }
}

/// Elaborates arguments pairwise against parameters of matching count.
fn elab_args(
    ctx: &mut Context,
    params_il: &[il::Param],
    args: &[el::Arg],
    mode: ArgMode,
    span: &Span,
    id_func: &Id,
    span_declaration: &Span,
) -> Backtrack<Vec<il::Arg>> {
    // Argument count must match parameter count
    if params_il.len() != args.len() {
        let span_error = args
            .get(params_il.len())
            .map_or_else(|| span.clone(), |arg| arg.span.clone());
        return fatal!(error: error::arg::function_call_argument_arity_mismatch(
            id_func,
            params_il.len(),
            args.len(),
            &span_error,
            span_declaration,
        ));
    }
    let mut args_il = Vec::with_capacity(args.len());
    for (idx, (param_il, arg)) in params_il.iter().zip(args).enumerate() {
        let arg_il = unwrap!(elab_arg(ctx, param_il, arg, mode, id_func, idx));
        args_il.push(arg_il);
    }
    success!(args_il)
}

// == Premises

/// An elaborated premise, or a marker for premises that produce no IL premise.
#[allow(clippy::large_enum_variant)]
enum PremInternal {
    Some(il::Prem),
    /// A variable premise, which only extends the context.
    Var,
    /// An otherwise premise, which marks the fallback rule or clause.
    Else,
}

// - Premise elaboration

/// Elaborates a premise into an IL premise or a marker.
fn elab_prem(ctx: &mut Context, prem: &el::Prem) -> Backtrack<PremInternal> {
    let prem_kind_il = match &prem.node {
        // Variable and otherwise premises leave no IL premise
        el::PremKind::Var(var_prem) => {
            unwrap!(elab_var_prem(ctx, var_prem));
            return success!(PremInternal::Var);
        }
        el::PremKind::Rule(rule_prem) => unwrap!(elab_rule_prem(ctx, rule_prem)),
        el::PremKind::RuleNot(rule_not_prem) => unwrap!(elab_rule_not_prem(ctx, rule_not_prem)),
        el::PremKind::If(if_prem) => unwrap!(elab_if_prem(ctx, if_prem)),
        // An otherwise premise marks the fallback
        el::PremKind::Else => return success!(PremInternal::Else),
        el::PremKind::Iter(iter_prem) => unwrap!(elab_iter_prem(ctx, iter_prem)),
        el::PremKind::Debug(debug_prem) => unwrap!(elab_debug_prem(ctx, debug_prem)),
    };
    let prem_il = phrase!(node: prem_kind_il, span: prem.span.clone());
    success!(PremInternal::Some(prem_il))
}

/// Elaborates premises in order and retains the otherwise marker location.
fn elab_prems(
    ctx: &mut Context,
    prems: &[el::Prem],
    _span: &Span,
) -> Backtrack<(Vec<il::Prem>, Option<il::Otherwise>)> {
    let mut prems_il = Vec::new();
    let mut span_else_previous_opt = None;
    let mut span_else_repeated_opt = None;
    for prem in prems {
        let prem_internal = unwrap!(elab_prem(ctx, prem));
        match prem_internal {
            PremInternal::Some(prem_il) => prems_il.push(prem_il),
            PremInternal::Var => {}
            PremInternal::Else => {
                if span_else_previous_opt.is_none() {
                    span_else_previous_opt = Some(&prem.span);
                } else if span_else_repeated_opt.is_none() {
                    span_else_repeated_opt = Some(&prem.span);
                }
            }
        }
    }
    // At most one otherwise premise
    if let (Some(span_else_previous), Some(span_else_repeated)) =
        (span_else_previous_opt, span_else_repeated_opt)
    {
        return fatal!(error: error::prem::premise_otherwise_repeated(span_else_repeated, span_else_previous));
    }
    let otherwise_opt = span_else_previous_opt
        .map(|span_else| phrase!(node: il::OtherwiseKind, span: span_else.clone()));
    success!((prems_il, otherwise_opt))
}

// - Variable premise elaboration

/// Binds the meta-variable declared by a variable premise.
fn elab_var_prem(ctx: &mut Context, prem: &el::VarPrem) -> Backtrack<()> {
    if !valid_tid(&prem.id) {
        return fatal!(error: error::prem::premise_variable_identifier_invalid(&prem.id));
    }
    // A meta-variable name must not clash with a type
    if let Some((id_previous, _)) = ctx.tdenv.get_key_value(&prem.id) {
        return fatal!(error: error::prem::premise_variable_type_repeated(&prem.id, &id_previous.span));
    }
    let typ_il = match elab_plain_typ(ctx, &prem.plain_typ) {
        Ok(typ_il) => typ_il,
        Err(error) => return fatal!(error: error),
    };
    if let Err(error) = ctx.add_metavar(prem.id.clone(), typ_il) {
        return fatal!(error: error);
    }
    success!(())
}

// - Rule premise elaboration

/// Elaborates a rule premise; one without outputs becomes a holding check.
fn elab_rule_prem(ctx: &mut Context, prem: &el::RulePrem) -> Backtrack<il::PremKind> {
    let (not_typ_il, input_hint) = match ctx.find_rel_signature(&prem.id) {
        Ok((not_typ_il, input_hint)) => (not_typ_il.clone(), input_hint.clone()),
        Err(error) => return fatal!(error: error),
    };
    let not_exp_il = unwrap!(
        elab_not_exp(ctx, &NotExpect::rel(&prem.id, &not_typ_il), &prem.exp)
            .recoverable_as_failure()
    );
    let exps_il = not_exp_il.args();
    let conditional = match input::is_conditional(&input_hint, exps_il) {
        Ok(conditional) => conditional,
        Err(error) => {
            return fatal!(error: error::prem::relation_input_hint_mismatch(&prem.exp.span, error.to_string()));
        }
    };
    // A premise without outputs only checks that the relation holds
    if conditional {
        success!(il::PremKind::IfHold(il::IfHoldPrem { id: prem.id.clone(), not_exp: not_exp_il }))
    } else {
        success!(il::PremKind::Rule(il::RulePrem {
            id: prem.id.clone(),
            not_exp: not_exp_il,
            input_hint,
        }))
    }
}

// - Negated rule premise elaboration

/// Elaborates a negated rule premise, which may not yield outputs.
fn elab_rule_not_prem(ctx: &mut Context, prem: &el::RuleNotPrem) -> Backtrack<il::PremKind> {
    let (not_typ_il, input_hint) = match ctx.find_rel_signature(&prem.id) {
        Ok((not_typ_il, input_hint)) => (not_typ_il.clone(), input_hint.clone()),
        Err(error) => return fatal!(error: error),
    };
    let not_exp_il = unwrap!(
        elab_not_exp(ctx, &NotExpect::rel(&prem.id, &not_typ_il), &prem.exp)
            .recoverable_as_failure()
    );
    let exps_il = not_exp_il.args().iter().collect();
    let (_, exps_output_il) = match input::split(&input_hint, exps_il) {
        Ok(parts) => parts,
        Err(error) => {
            return fatal!(error: error::prem::relation_input_hint_mismatch(&prem.exp.span, error.to_string()));
        }
    };
    // A negated premise cannot bind outputs
    if let Some(exp_output_il) = exps_output_il.first() {
        return fatal!(error: error::prem::premise_negated_output_unsupported(
            &prem.id,
            &exp_output_il.span,
            &not_typ_il.span,
        ));
    }
    success!(il::PremKind::IfNotHold(il::IfNotHoldPrem {
        id: prem.id.clone(),
        not_exp: not_exp_il
    }))
}

// - Conditional premise elaboration

fn elab_if_prem(ctx: &mut Context, prem: &el::IfPrem) -> Backtrack<il::PremKind> {
    let typ_bool_il = typ_at(il::TypKind::Bool, &prem.exp.span);
    let exp_il =
        unwrap!(elab_exp(ctx, &ExpExpect::plain(&typ_bool_il), &prem.exp).recoverable_as_failure());
    success!(il::PremKind::If(il::IfPrem { exp: exp_il }))
}

// - Iteration premise elaboration

/// Elaborates an iterated premise, leaving its variables to dimension analysis.
fn elab_iter_prem(ctx: &mut Context, prem: &el::IterPrem) -> Backtrack<il::PremKind> {
    let prem_inner_il = unwrap!(elab_prem(ctx, &prem.prem));
    // Only premises that produce an IL premise can be iterated
    let prem_inner_il = match prem_inner_il {
        PremInternal::Some(prem_inner_il) => prem_inner_il,
        PremInternal::Var => {
            return fatal!(error: error::prem::premise_variable_iteration_unsupported(&prem.prem.span));
        }
        PremInternal::Else => {
            return fatal!(error: error::prem::premise_otherwise_iteration_unsupported(&prem.prem.span));
        }
    };
    let prem_iter_il = il::PremIter { iter: prem.iter, vars_bound: vec![], vars_bind: vec![] };
    success!(il::PremKind::Iter(il::IterPrem {
        prem: Box::new(prem_inner_il),
        prem_iter: prem_iter_il
    }))
}

// - Debug premise elaboration

fn elab_debug_prem(ctx: &mut Context, prem: &el::DebugPrem) -> Backtrack<il::PremKind> {
    let exp_il = unwrap!(infer_exp(ctx, &prem.exp).recoverable_as_failure());
    success!(il::PremKind::Debug(il::DebugPrem { exp: exp_il }))
}

// == Rules and clauses

// - Rule elaboration

/// Elaborates one rule and retains its otherwise marker location.
fn elab_rule(
    ctx: &mut Context,
    rule: &el::Rule,
    id_rel: &Id,
    not_typ_il: &il::NotTyp,
) -> Result<(il::Rule, Option<il::Otherwise>), ElabError> {
    let el::RuleKind { id_rel: id_rel_rule, id_rule, exp, prems } = &rule.node;
    if id_rel_rule.node != id_rel.node {
        return Err(error::prem::relation_rule_group_name_mismatch(id_rel_rule, id_rel));
    }
    // Elaborate under a local context seeded with the rule's free identifiers
    let mut ctx_local = ctx.clone();
    ctx_local.reset_frees();
    let frees = rule.free_ids();
    ctx_local.add_frees(&frees);
    let not_exp_il =
        finish(elab_not_exp(&mut ctx_local, &NotExpect::rel(id_rel, not_typ_il), exp))?;
    let (prems_il, otherwise_opt) = finish(elab_prems(&mut ctx_local, prems, &id_rule.span))?;
    let rule_kind_il = il::RuleKind { id: id_rule.clone(), not_exp: not_exp_il, prems: prems_il };
    let rule_il = phrase!(node: rule_kind_il, span: rule.span.clone());
    Ok((rule_il, otherwise_opt))
}

/// Elaborates a rule group into ordinary rules or a single otherwise rule.
fn elab_rule_group(
    ctx: &mut Context,
    def: &Phrase<&el::RuleGroupDef>,
) -> Result<(Option<il::RuleGroup>, Option<il::ElseGroup>), ElabError> {
    let span = &def.span;
    let def = def.node;
    let defined_rel_il = ctx.find_defined_rel(&def.relid)?;
    let not_typ_il = defined_rel_il.not_typ.clone();
    let mut rules_il = Vec::with_capacity(def.rules.len());
    let mut rules_else_il = Vec::new();
    // Sort the rules into ordinary and otherwise rules
    for rule in &def.rules {
        let (rule_il, otherwise_opt) = elab_rule(ctx, rule, &def.relid, &not_typ_il)?;
        if let Some(otherwise) = otherwise_opt {
            rules_else_il.push((rule_il, otherwise));
        } else {
            rules_il.push(rule_il);
        }
    }
    // An otherwise rule must be alone in its group
    match rules_else_il.len() {
        0 => {
            let rule_group_kind_il = il::RuleGroupKind { id: def.groupid.clone(), rules: rules_il };
            let rule_group_il = phrase!(node: rule_group_kind_il, span: span.clone());
            Ok((Some(rule_group_il), None))
        }
        1 if def.rules.len() == 1 => {
            let (rule_il, otherwise) = rules_else_il.remove(0);
            let else_group_kind_il =
                il::ElseGroupKind { id: def.groupid.clone(), rule: rule_il, otherwise };
            let else_group_il = phrase!(node: else_group_kind_il, span: span.clone());
            Ok((None, Some(else_group_il)))
        }
        _ if rules_else_il.len() > 1 => Err(error::prem::relation_rule_otherwise_repeated(
            &rules_else_il[1].0.span,
            &rules_else_il[0].0.span,
        )),
        _ => {
            let rule_else_il = &rules_else_il[0].0;
            let rule_other_il = &rules_il[0];
            if rule_else_il.span.left < rule_other_il.span.left {
                Err(error::prem::relation_rule_otherwise_invalid(
                    &rule_other_il.span,
                    &rule_else_il.span,
                    "`otherwise` rule here",
                ))
            } else {
                Err(error::prem::relation_rule_otherwise_invalid(
                    &rule_else_il.span,
                    &rule_other_il.span,
                    "other rule here",
                ))
            }
        }
    }
}

// - Clause elaboration

/// Elaborates one clause and reports whether it is the otherwise clause.
fn elab_clause(
    ctx: &mut Context,
    def: &Phrase<&el::FuncDef>,
) -> Result<(il::Clause, bool), ElabError> {
    let span = &def.span;
    let def = def.node;
    let il::DefinedFunc {
        id: id_declaration,
        tparams: tparams_expect_il,
        params: params_il,
        typ: typ_ret_il,
        ..
    } = ctx.find_defined_func(&def.id)?;
    let span_declaration = id_declaration.span.clone();
    let span_tparams_declaration = match (tparams_expect_il.first(), tparams_expect_il.last()) {
        (Some(tparam_first), Some(tparam_last)) => {
            Span::new(tparam_first.span.left.clone(), tparam_last.span.right.clone())
        }
        _ => span_declaration.clone(),
    };
    // Type parameters must repeat the declaration exactly
    if def.tparams.len() != tparams_expect_il.len()
        || def
            .tparams
            .iter()
            .zip(tparams_expect_il)
            .any(|(tparam, tparam_expect_il)| tparam.node != tparam_expect_il.node)
    {
        let span_tparams = match (def.tparams.first(), def.tparams.last()) {
            (Some(tparam_first), Some(tparam_last)) => {
                Span::new(tparam_first.span.left.clone(), tparam_last.span.right.clone())
            }
            _ => def.id.span.clone(),
        };
        return Err(error::arg::function_clause_type_parameter_mismatch(
            &def.id,
            tparams_expect_il,
            &def.tparams,
            &span_tparams,
            &span_tparams_declaration,
        ));
    }
    let params_il = params_il.to_vec();
    let typ_ret_il = typ_ret_il.clone();
    if params_il.len() != def.args.len() {
        let span_args = def
            .args
            .get(params_il.len())
            .map_or_else(|| span.clone(), |arg| arg.span.clone());
        return Err(error::arg::function_clause_argument_arity_mismatch(
            &def.id,
            params_il.len(),
            def.args.len(),
            &span_args,
            &span_declaration,
        ));
    }
    // Local context with the clause's free identifiers and type parameters
    let mut ctx_local = ctx.clone();
    ctx_local.reset_frees();
    let frees = def.free_ids();
    ctx_local.add_frees(&frees);
    ctx_local.add_tparams(&def.tparams)?;
    let args_il = finish(elab_args(
        &mut ctx_local,
        &params_il,
        &def.args,
        ArgMode::Def,
        span,
        &def.id,
        &span_declaration,
    ))?;
    let (prems_il, otherwise_opt) = finish(elab_prems(&mut ctx_local, &def.prems, span))?;
    let expect = ExpExpect::func_return(&def.id, &typ_ret_il);
    let exp_il = finish(elab_exp(&mut ctx_local, &expect, &def.exp))?;
    let is_else = otherwise_opt.is_some();
    let clause_kind_il =
        il::ClauseKind { args: args_il, exp: exp_il, prems: prems_il, otherwise_opt };
    let clause_il = phrase!(node: clause_kind_il, span: span.clone());
    Ok((clause_il, is_else))
}

// == Definitions

// - Definition dispatch

/// Elaborates a definition; bodies for earlier declarations yield nothing.
fn elab_def(
    ctx: &mut Context,
    warnings: &mut Vec<Report>,
    def_el: el::Def,
) -> Result<Option<il::Def>, ElabError> {
    let span = def_el.span;
    match def_el.node {
        // An extern type becomes an IL definition
        el::DefKind::ExternSyntax(extern_syntax_def) => {
            let def_kind_il = elab_extern_syntax_def(ctx, extern_syntax_def)?;
            let def_il = phrase!(node: def_kind_il, span: span);
            Ok(Some(def_il))
        }
        // Forward declarations only extend the context
        el::DefKind::Syntax(syntax_def) => {
            elab_syntax_def(ctx, &syntax_def)?;
            Ok(None)
        }
        // A type body completes its forward declaration
        el::DefKind::Typ(typ_def) => {
            let span = typ_def.def_typ.span.clone();
            let def_kind_il = elab_typ_def(ctx, typ_def)?;
            let def_il = phrase!(node: def_kind_il, span: span);
            Ok(Some(def_il))
        }
        // A meta-variable declaration
        el::DefKind::Var(var_def) => {
            let span = var_def.id.span.clone();
            let def_kind_il = elab_var_def(ctx, var_def)?;
            let def_il = phrase!(node: def_kind_il, span: span);
            Ok(Some(def_il))
        }
        // An extern relation declaration
        el::DefKind::ExternRel(extern_rel_def) => {
            let def_kind_il = elab_extern_rel_def(ctx, &span, warnings, extern_rel_def)?;
            let def_il = phrase!(node: def_kind_il, span: span);
            Ok(Some(def_il))
        }
        // A relation declaration; its rules arrive later
        el::DefKind::Rel(rel_def) => {
            let def_kind_il = elab_rel_def(ctx, &span, warnings, rel_def)?;
            let def_il = phrase!(node: def_kind_il, span: span);
            Ok(Some(def_il))
        }
        // Rule groups attach to their relation and yield no definition
        el::DefKind::RuleGroup(rule_group_def) => {
            let rule_group_def = crate::phrase! {
                node: &rule_group_def,
                span: span,
            };
            elab_rule_group_def(ctx, &rule_group_def)?;
            Ok(None)
        }
        // An extern function declaration
        el::DefKind::ExternDec(extern_dec_def) => {
            let def_kind_il = elab_extern_dec_def(ctx, extern_dec_def)?;
            let def_il = phrase!(node: def_kind_il, span: span);
            Ok(Some(def_il))
        }
        // A builtin function declaration
        el::DefKind::BuiltinDec(builtin_dec_def) => {
            let def_kind_il = elab_builtin_dec_def(ctx, builtin_dec_def)?;
            let def_il = phrase!(node: def_kind_il, span: span);
            Ok(Some(def_il))
        }
        // A table declaration; its rows arrive later
        el::DefKind::TableDec(table_dec_def) => {
            let def_kind_il = elab_table_dec_def(ctx, table_dec_def)?;
            let def_il = phrase!(node: def_kind_il, span: span);
            Ok(Some(def_il))
        }
        // A function declaration; its clauses arrive later
        el::DefKind::FuncDec(func_dec_def) => {
            let def_kind_il = elab_func_dec_def(ctx, func_dec_def)?;
            let def_il = phrase!(node: def_kind_il, span: span);
            Ok(Some(def_il))
        }
        // Table rows attach to their declared table
        el::DefKind::TableDef(table_def) => {
            elab_table_def(ctx, &table_def)?;
            Ok(None)
        }
        // A clause attaches to its declared function
        el::DefKind::FuncDef(func_def) => {
            let func_def = crate::phrase! {
                node: &func_def,
                span: span,
            };
            elab_func_def(ctx, &func_def)?;
            Ok(None)
        }
        // Separators carry nothing
        el::DefKind::Sep => Ok(None),
    }
}

// - Type declarations

/// Declares an extern type together with a meta-variable of that type.
fn elab_extern_syntax_def(
    ctx: &mut Context,
    def: el::ExternSyntaxDef,
) -> Result<il::DefKind, ElabError> {
    if !valid_tid(&def.id) {
        return Err(error::typ::type_extern_identifier_invalid(&def.id));
    }
    // An extern type also names a meta-variable
    ctx.add_typdef(def.id.clone(), TypeDef::Extern)?;
    let typ_kind_il = il::TypKind::Var(def.id.clone(), vec![]);
    let typ_il = phrase!(node: typ_kind_il, span: def.id.span.clone());
    ctx.add_metavar(def.id.clone(), typ_il)?;
    let extern_typ_il = il::ExternTyp { id: def.id, hints: def.hints };
    Ok(il::DefKind::Typ(il::TypDef::Extern(extern_typ_il)))
}

/// Forward-declares the types of a syntax block ahead of their bodies.
fn elab_syntax_def(ctx: &mut Context, def: &el::SyntaxDef) -> Result<(), ElabError> {
    for entry in &def.entries {
        if let Some((tparam, span_previous)) = find_repeated_tparam(&entry.tparams) {
            return Err(error::typ::type_parameter_repeated(tparam, span_previous));
        }
        if !valid_tid(&entry.id) {
            return Err(error::typ::type_syntax_identifier_invalid(&entry.id));
        }
        // A type without parameters also names a meta-variable
        ctx.add_typdef(entry.id.clone(), TypeDef::Defining(entry.tparams.clone()))?;
        if entry.tparams.is_empty() {
            let typ_kind_il = il::TypKind::Var(entry.id.clone(), vec![]);
            let typ_il = phrase!(node: typ_kind_il, span: entry.id.span.clone());
            ctx.add_metavar(entry.id.clone(), typ_il)?;
        }
    }
    Ok(())
}

// - Type definitions

/// Elaborates a type body, completing a forward declaration or a new type.
fn elab_typ_def(ctx: &mut Context, def: el::TypDef) -> Result<il::DefKind, ElabError> {
    let typdef_previous = ctx
        .tdenv
        .get_key_value(&def.id)
        .map(|(id, typdef)| (id.clone(), typdef.clone()));
    let is_new = match typdef_previous {
        // A forward-declared type must repeat its parameters
        Some((id_previous, TypeDef::Defining(tparams))) => {
            let matches = tparams.len() == def.tparams.len()
                && tparams
                    .iter()
                    .zip(&def.tparams)
                    .all(|(id_l, id_r)| id_l.node == id_r.node);
            if !matches {
                let span_tparams = match (def.tparams.first(), def.tparams.last()) {
                    (Some(tparam_first), Some(tparam_last)) => {
                        Span::new(tparam_first.span.left.clone(), tparam_last.span.right.clone())
                    }
                    _ => def.id.span.clone(),
                };
                let span_tparams_previous = match (tparams.first(), tparams.last()) {
                    (Some(tparam_first), Some(tparam_last)) => {
                        Span::new(tparam_first.span.left.clone(), tparam_last.span.right.clone())
                    }
                    _ => id_previous.span.clone(),
                };
                return Err(error::typ::type_parameter_mismatch(
                    &def.id,
                    &tparams,
                    &def.tparams,
                    &span_tparams,
                    &span_tparams_previous,
                ));
            }
            false
        }
        Some((id_previous, TypeDef::Defined(_, _))) => {
            return Err(error::decl::type_definition_repeated(&def.id, &id_previous.span));
        }
        Some((id_previous, TypeDef::Extern)) => {
            return Err(error::typ::type_definition_extern_unsupported(&def.id, &id_previous.span));
        }
        Some((_, TypeDef::Parameter)) => unreachable!("top-level type parameter"),
        // A new type is declared on the spot
        None => {
            if !valid_tid(&def.id) {
                return Err(error::typ::type_identifier_invalid(&def.id));
            }
            true
        }
    };
    // Parameter validation follows declaration admission and mismatch checks
    if let Some(tparam) = def.tparams.iter().find(|tparam| !valid_tid(tparam)) {
        return Err(error::typ::type_parameter_identifier_invalid(tparam));
    }
    if let Some((tparam, span_previous)) = find_repeated_tparam(&def.tparams) {
        return Err(error::typ::type_parameter_repeated(tparam, span_previous));
    }
    if is_new {
        ctx.add_typdef(def.id.clone(), TypeDef::Defining(def.tparams.clone()))?;
        if def.tparams.is_empty() {
            let typ_kind_il = il::TypKind::Var(def.id.clone(), vec![]);
            let typ_il = phrase!(node: typ_kind_il, span: def.id.span.clone());
            ctx.add_metavar(def.id.clone(), typ_il)?;
        }
    }
    // Elaborate the body with the type parameters in scope
    let (typdef, def_typ_il) = {
        let mut ctx_local = ctx.clone();
        ctx_local.add_tparams(&def.tparams)?;
        elab_def_typ(&ctx_local, &def.id, &def.tparams, &def.def_typ)?
    };
    ctx.update_typdef(&def.id, typdef);
    let defined_typ_il =
        il::DefinedTyp { id: def.id, tparams: def.tparams, def_typ: def_typ_il, hints: def.hints };
    Ok(il::DefKind::Typ(il::TypDef::Defined(Box::new(defined_typ_il))))
}

// - Variable definitions

/// Declares a global meta-variable.
fn elab_var_def(ctx: &mut Context, def: el::VarDef) -> Result<il::DefKind, ElabError> {
    if !valid_tid(&def.id) {
        return Err(error::decl::meta_variable_identifier_invalid(&def.id));
    }
    // A meta-variable name must not clash with a type
    if let Some((id_previous, _)) = ctx.tdenv.get_key_value(&def.id) {
        return Err(error::decl::meta_variable_type_repeated(&def.id, &id_previous.span));
    }
    let typ_il = elab_plain_typ(ctx, &def.plain_typ)?;
    ctx.add_metavar(def.id.clone(), typ_il.clone())?;
    let var_def_il = il::VarDef { id: def.id, typ: typ_il, hints: def.hints };
    Ok(il::DefKind::Var(var_def_il))
}

// - Input hints

/// Reads a relation's `input` hint; by default every position is an input.
fn fetch_input_hint(
    span: &Span,
    warnings: &mut Vec<Report>,
    id: &Id,
    not_typ_il: &il::NotTyp,
    hints: &[el::Hint],
) -> Result<input::InputHint, ElabError> {
    let arity = not_typ_il.node.arity();
    // Without a hint every position is an input
    let Some(el::Hint { exp: exp_hint, .. }) = hints.iter().find(|hint| hint.id.node == "input")
    else {
        warnings.push(error::prem::relation_input_hint_missing(id, span));
        return Ok(input::InputHint::new(
            (0..arity)
                .map(|idx| phrase!(node: idx, span: Span::default()))
                .collect(),
        ));
    };
    let Some(input_hint) = input::init(exp_hint) else {
        return Err(error::prem::relation_input_hint_invalid(
            &exp_hint.span,
            &Print::to_string(exp_hint),
        ));
    };
    // The hint must stay within the notation arity
    if let Err(error_input) = input::validate(&input_hint, arity) {
        return Err(match error_input {
            input::InputError::InputEmpty => error::prem::relation_input_hint_empty(&exp_hint.span),
            input::InputError::IndexDuplicate { idx, idx_previous } => {
                error::prem::relation_input_hint_index_repeated(
                    idx.node,
                    &idx.span,
                    &idx_previous.span,
                )
            }
            input::InputError::IndexOutOfBounds { idx, arity } => {
                error::prem::relation_input_hint_index_out_of_bounds(
                    idx.node,
                    arity,
                    &idx.span,
                    &not_typ_il.node.at(),
                )
            }
            input::InputError::InputCountMismatch {
                expected: inputs_len_expect,
                actual: inputs_len_actual,
            } => error::prem::relation_input_hint_mismatch(
                &exp_hint.span,
                format!(
                    "input hint expects {inputs_len_expect} input items, \
                        but got {inputs_len_actual}"
                ),
            ),
            input::InputError::OutputCountMismatch {
                expected: outputs_len_expect,
                actual: outputs_len_actual,
            } => error::prem::relation_input_hint_mismatch(
                &exp_hint.span,
                format!(
                    "input hint expects {outputs_len_expect} output items, \
                        but got {outputs_len_actual}"
                ),
            ),
        });
    }
    Ok(input_hint)
}

// - Relation definitions

/// Declares an extern relation.
fn elab_extern_rel_def(
    ctx: &mut Context,
    span: &Span,
    warnings: &mut Vec<Report>,
    def: el::ExternRelDef,
) -> Result<il::DefKind, ElabError> {
    let typ = el::Typ::Notation(def.not_typ.clone());
    let not_typ_il = elab_not_typ(ctx, &typ)?;
    let input_hint = fetch_input_hint(span, warnings, &def.id, &not_typ_il, &def.hints)?;
    let extern_rel_il =
        il::ExternRel { id: def.id, not_typ: not_typ_il, input_hint, hints: def.hints };
    ctx.add_extern_rel(extern_rel_il.clone())?;
    Ok(il::DefKind::Rel(il::RelDef::Extern(Box::new(extern_rel_il))))
}

/// Declares a relation whose rule groups arrive later.
fn elab_rel_def(
    ctx: &mut Context,
    span: &Span,
    warnings: &mut Vec<Report>,
    def: el::RelDef,
) -> Result<il::DefKind, ElabError> {
    let typ = el::Typ::Notation(def.not_typ.clone());
    let not_typ_il = elab_not_typ(ctx, &typ)?;
    let input_hint = fetch_input_hint(span, warnings, &def.id, &not_typ_il, &def.hints)?;
    let defined_rel_il = il::DefinedRel {
        id: def.id,
        not_typ: not_typ_il,
        input_hint,
        rule_groups: vec![],
        else_group: None,
        hints: def.hints,
    };
    ctx.add_defined_rel(defined_rel_il.clone())?;
    Ok(il::DefKind::Rel(il::RelDef::Defined(Box::new(defined_rel_il))))
}

// - Rule group definitions

/// Elaborates a rule group and attaches it to its relation.
fn elab_rule_group_def(
    ctx: &mut Context,
    def: &Phrase<&el::RuleGroupDef>,
) -> Result<(), ElabError> {
    let (rule_group_il, else_group_il) = elab_rule_group(ctx, def)?;
    let def = def.node;
    if let Some(rule_group_il) = rule_group_il {
        ctx.add_defined_rule_group(&def.relid, rule_group_il)?;
    }
    if let Some(else_group_il) = else_group_il {
        ctx.add_defined_else_group(&def.relid, else_group_il)?;
    }
    Ok(())
}

// - Function declarations

/// Declares an extern function.
fn elab_extern_dec_def(ctx: &mut Context, def: el::ExternDecDef) -> Result<il::DefKind, ElabError> {
    // Label both occurrences of the first repeated type parameter
    if let Some((tparam, span_previous)) = find_repeated_tparam(&def.tparams) {
        return Err(error::decl::function_extern_type_parameter_repeated(tparam, span_previous));
    }
    // Parameters and return type see the type parameters
    let (params_il, typ_il) = {
        let mut ctx_local = ctx.clone();
        ctx_local.add_tparams(&def.tparams)?;
        let params_il = def
            .params
            .iter()
            .map(|param| elab_param(&mut ctx_local, param))
            .collect::<Result<Vec<_>, _>>()?;
        let typ_il = elab_plain_typ(&ctx_local, &def.plain_typ)?;
        (params_il, typ_il)
    };
    let extern_func_il = il::ExternFunc {
        id: def.id,
        tparams: def.tparams,
        params: params_il,
        typ: typ_il,
        hints: def.hints,
    };
    ctx.add_extern_func(extern_func_il.clone())?;
    Ok(il::DefKind::MetaFunc(il::MetaFuncDef::Extern(extern_func_il)))
}

/// Declares a builtin function.
fn elab_builtin_dec_def(
    ctx: &mut Context,
    def: el::BuiltinDecDef,
) -> Result<il::DefKind, ElabError> {
    // Label both occurrences of the first repeated type parameter
    if let Some((tparam, span_previous)) = find_repeated_tparam(&def.tparams) {
        return Err(error::decl::function_builtin_type_parameter_repeated(tparam, span_previous));
    }
    // Parameters and return type see the type parameters
    let (params_il, typ_il) = {
        let mut ctx_local = ctx.clone();
        ctx_local.add_tparams(&def.tparams)?;
        let params_il = def
            .params
            .iter()
            .map(|param| elab_param(&mut ctx_local, param))
            .collect::<Result<Vec<_>, _>>()?;
        let typ_il = elab_plain_typ(&ctx_local, &def.plain_typ)?;
        (params_il, typ_il)
    };
    let builtin_func_il = il::BuiltinFunc {
        id: def.id,
        tparams: def.tparams,
        params: params_il,
        typ: typ_il,
        hints: def.hints,
    };
    ctx.add_builtin_func(builtin_func_il.clone())?;
    Ok(il::DefKind::MetaFunc(il::MetaFuncDef::Builtin(builtin_func_il)))
}

/// Declares a table function, which takes plain parameters and returns `bool`.
fn elab_table_dec_def(ctx: &mut Context, def: el::TableDecDef) -> Result<il::DefKind, ElabError> {
    let params_il = def
        .params
        .iter()
        .map(|param| elab_param(ctx, param))
        .collect::<Result<Vec<_>, _>>()?;
    // Locate the offending function parameter rather than the whole declaration
    for param_il in &params_il {
        if let il::ParamKind::Def(id, _, _, _) = &param_il.node {
            return Err(error::decl::table_parameter_unsupported(id, &param_il.span));
        }
    }
    // Accept boolean aliases under the same equivalence relation as other types
    let typ_il = elab_plain_typ(ctx, &def.plain_typ)?;
    let typ_bool_il = phrase!(node: il::TypKind::Bool, span: typ_il.span.clone());
    if !equiv_typ(&ctx.tdenv, &typ_il, &typ_bool_il).map_err(|error| {
        error::typ::type_operation_invalid("cannot check table return type", error)
    })? {
        return Err(error::decl::table_return_type_invalid(
            &def.id,
            &typ_il.span,
            &Print::to_string(&typ_il),
        ));
    }
    let table_func_il = il::TableFunc {
        id: def.id,
        params: params_il,
        typ: typ_il,
        rows: vec![],
        hints: def.hints,
    };
    ctx.add_table_func(table_func_il.clone())?;
    Ok(il::DefKind::MetaFunc(il::MetaFuncDef::Table(table_func_il)))
}

/// Declares a function whose clauses arrive later.
fn elab_func_dec_def(ctx: &mut Context, def: el::FuncDecDef) -> Result<il::DefKind, ElabError> {
    // Label both occurrences of the first repeated type parameter
    if let Some((tparam, span_previous)) = find_repeated_tparam(&def.tparams) {
        return Err(error::decl::function_type_parameter_repeated(tparam, span_previous));
    }
    // Parameters and return type see the type parameters
    let (params_il, typ_il) = {
        let mut ctx_local = ctx.clone();
        ctx_local.add_tparams(&def.tparams)?;
        let params_il = def
            .params
            .iter()
            .map(|param| elab_param(&mut ctx_local, param))
            .collect::<Result<Vec<_>, _>>()?;
        let typ_il = elab_plain_typ(&ctx_local, &def.plain_typ)?;
        (params_il, typ_il)
    };
    let defined_func_il = il::DefinedFunc {
        id: def.id,
        tparams: def.tparams,
        params: params_il,
        typ: typ_il,
        clauses: vec![],
        else_clause: None,
        hints: def.hints,
    };
    ctx.add_defined_func(defined_func_il.clone())?;
    Ok(il::DefKind::MetaFunc(il::MetaFuncDef::Defined(Box::new(defined_func_il))))
}

// - Table function definitions

/// Elaborates table rows against the declared parameters and result type.
fn elab_table_def(ctx: &mut Context, def: &el::TableDef) -> Result<(), ElabError> {
    let table_func_il = ctx.find_table_func(&def.id)?;
    let span_declaration = table_func_il.id.span.clone();
    let params_il = table_func_il.params.clone();
    let typ_il = table_func_il.typ.clone();
    let mut rows_il = Vec::with_capacity(def.rows.len());
    for row in &def.rows {
        let el::TableRowKind { exp_pattern, exp_body } = &row.node;
        // A row pattern is a tuple of arguments or a single argument
        let exps = match &exp_pattern.node {
            el::ExpKind::Tuple(exps) => exps.clone(),
            _ => vec![exp_pattern.clone()],
        };
        let args = exps
            .into_iter()
            .map(|exp| {
                let span = exp.span.clone();
                let arg = el::ArgKind::Exp(Box::new(exp));
                phrase!(node: arg, span: span)
            })
            .collect::<Vec<_>>();
        // Each row elaborates under its own free identifiers
        let (args_il, exp_body_il) = {
            let mut ctx_local = ctx.clone();
            ctx_local.reset_frees();
            let frees = row.free_ids();
            ctx_local.add_frees(&frees);
            let args_il = finish(elab_args(
                &mut ctx_local,
                &params_il,
                &args,
                ArgMode::Def,
                &row.span,
                &def.id,
                &span_declaration,
            ))?;
            let expect = ExpExpect::func_return(&def.id, &typ_il);
            let exp_body_il = finish(elab_exp(&mut ctx_local, &expect, exp_body))?;
            (args_il, exp_body_il)
        };
        let row_il = phrase!(node: il::TableRowKind { args: args_il, exp: exp_body_il }, span: row.span.clone());
        rows_il.push(row_il);
    }
    ctx.add_table_func_rows(&def.id, rows_il)?;
    Ok(())
}

// - Function definitions

/// Elaborates a clause and attaches it to its declared function.
fn elab_func_def(ctx: &mut Context, def: &Phrase<&el::FuncDef>) -> Result<(), ElabError> {
    let (clause_il, is_else) = elab_clause(ctx, def)?;
    let def = def.node;
    if is_else {
        ctx.add_defined_func_else_clause(&def.id, clause_il)?;
    } else {
        ctx.add_defined_func_clause(&def.id, clause_il);
    }
    Ok(())
}

// == Specification

// - Definition population

/// Moves the collected rule groups of a relation into its IL definition.
fn populate_rel(ctx: &mut Context, rel_def_il: il::RelDef) -> il::RelDef {
    match rel_def_il {
        il::RelDef::Extern(_) => rel_def_il,
        il::RelDef::Defined(mut defined_rel_il) => {
            // [elab_rel_def] constructs an empty declaration
            assert!(defined_rel_il.rule_groups.is_empty() && defined_rel_il.else_group.is_none());

            // The collected rule groups replace the empty declaration
            let defined_rel_stored_il = ctx.take_defined_rel(&defined_rel_il.id);
            defined_rel_il.rule_groups = defined_rel_stored_il.rule_groups;
            defined_rel_il.else_group = defined_rel_stored_il.else_group;
            il::RelDef::Defined(defined_rel_il)
        }
    }
}

/// Moves the collected rows or clauses of a function into its IL definition.
fn populate_meta_func(ctx: &mut Context, meta_func_def_il: il::MetaFuncDef) -> il::MetaFuncDef {
    match meta_func_def_il {
        // Extern and builtin functions have no body to fill
        il::MetaFuncDef::Extern(_) => meta_func_def_il,
        il::MetaFuncDef::Builtin(_) => meta_func_def_il,
        il::MetaFuncDef::Table(mut table_func_il) => {
            // [elab_table_dec_def] constructs an empty declaration
            assert!(table_func_il.rows.is_empty());

            // The collected rows replace the empty declaration
            let table_func_stored_il = ctx.take_table_func(&table_func_il.id);
            table_func_il.rows = table_func_stored_il.rows;
            il::MetaFuncDef::Table(table_func_il)
        }
        il::MetaFuncDef::Defined(mut defined_func_il) => {
            // [elab_func_dec_def] constructs an empty declaration
            assert!(defined_func_il.clauses.is_empty() && defined_func_il.else_clause.is_none());

            // The collected clauses replace the empty declaration
            let defined_func_stored_il = ctx.take_defined_func(&defined_func_il.id);
            defined_func_il.clauses = defined_func_stored_il.clauses;
            defined_func_il.else_clause = defined_func_stored_il.else_clause;
            il::MetaFuncDef::Defined(defined_func_il)
        }
    }
}

/// Fills every declaration with the bodies collected in the context.
fn populate_defs(ctx: &mut Context, defs_il: il::Spec) -> il::Spec {
    defs_il
        .into_iter()
        .map(|def_il| {
            // Only relations and functions collect bodies
            let def_kind_il = match def_il.node {
                il::DefKind::Rel(rel_def_il) => il::DefKind::Rel(populate_rel(ctx, rel_def_il)),
                il::DefKind::MetaFunc(meta_func_def_il) => {
                    il::DefKind::MetaFunc(populate_meta_func(ctx, meta_func_def_il))
                }
                def_kind_il => def_kind_il,
            };
            phrase!(node: def_kind_il, span: def_il.span)
        })
        .collect()
}

// - Missing definition warnings

/// Collects missing definitions in type, relation, then function order.
fn warn_undef_defs(ctx: &Context, warnings: &mut Vec<Report>, defs_il: &[il::Def]) {
    // Report incomplete types in identifier order
    for (id, typdef) in ctx.tdenv.iter() {
        if let TypeDef::Defining(tparams) = typdef {
            warnings.push(error::typ::type_definition_missing(id, tparams));
        }
    }
    // Report missing relation bodies before missing function bodies
    for def_il in defs_il {
        if let il::DefKind::Rel(il::RelDef::Defined(rel_il)) = &def_il.node
            && rel_il.rule_groups.is_empty()
            && rel_il.else_group.is_none()
        {
            warnings.push(error::decl::relation_rule_missing(&rel_il.id, &def_il.span));
        }
    }
    // Report missing table rows and function clauses in declaration order
    for def_il in defs_il {
        match &def_il.node {
            // Empty tables remain valid declarations
            il::DefKind::MetaFunc(il::MetaFuncDef::Table(func_il)) if func_il.rows.is_empty() => {
                warnings.push(error::decl::table_row_missing(&func_il.id, &def_il.span));
            }
            // An otherwise clause is a body even without regular clauses
            il::DefKind::MetaFunc(il::MetaFuncDef::Defined(func_il))
                if func_il.clauses.is_empty() && func_il.else_clause.is_none() =>
            {
                warnings.push(error::decl::function_clause_missing(&func_il.id, &def_il.span));
            }
            // Other definitions have no missing body warning
            _ => {}
        }
    }
}

// - Entry point

/// Elaborates a specification: definitions, population, dimension analysis.
pub(super) fn elab_spec(
    warnings: &mut Vec<Report>,
    spec_el: el::Spec,
) -> Result<il::Spec, ElabError> {
    let mut ctx = Context::new();
    let mut defs_il = Vec::new();
    // Declarations become IL definitions, bodies are collected in the context
    for def_el in spec_el {
        if let Some(def_il) = elab_def(&mut ctx, warnings, def_el)? {
            defs_il.push(def_il);
        }
    }
    // Attach the collected bodies to their declarations
    let mut defs_il = populate_defs(&mut ctx, defs_il);
    // Retain missing-body warnings even if dimension analysis fails
    warn_undef_defs(&ctx, warnings, &defs_il);
    // Annotate iterations with their source variables
    dimension::analyze_spec(&mut defs_il)?;
    Ok(defs_il)
}
