//! Direct SL-to-PL conversion with fused continuation linearization
//!
//! Each SL instruction becomes a PL block:
//! a let or rule call is followed by the instructions of its body,
//! so nested continuations flatten into one sequence;
//! a block of several instructions becomes a route or backtrack
//! with one arm each.
//! Relation bodies convert at the dispatch tier, group bodies at the group
//! tier, and each node receives the `prose*` hints of the definition it
//! refers to, validated against the number of items they describe.
//!
//! For example, an SL `let x = e` whose body applies `R(x)` becomes the PL
//! sequence `let x be e` then `R(x)` as sibling steps rather than a nested
//! block, so the prose reads as consecutive numbered steps.

use crate::lang::{
    common::{ds::set::IdSet, source::Span},
    hints::{alter, fields, input},
    traits::at::At,
};

use crate::lang::il::ast as il;

use crate::lang::al;

use crate::lang::sl::ast as sl;

use crate::lang::pl::{annot, ast as pl};

use super::{Context, ProseError, error};

// == Hint validation

/// Validates a template using its source and the owning declaration.
fn validate_hint_alter(
    span_decl: &Span,
    name_hint: &str,
    hint: &alter::AlterHint,
    num_items: usize,
) -> Result<(), ProseError> {
    alter::validate(hint, num_items)
        .map_err(|error| error::alteration_hint_index_out_of_bounds(span_decl, name_hint, error))
}

/// Checks the `prose` template against the items it describes.
fn validate_hint_prose(hints: &annot::Hints, num_items: usize) -> Result<(), ProseError> {
    if let Some(hint) = &hints.node.prose {
        validate_hint_alter(&hints.span, "prose", hint, num_items)?;
    }
    Ok(())
}

/// Checks the `prose_in` template against the items it describes.
fn validate_hint_prose_in(hints: &annot::Hints, num_items: usize) -> Result<(), ProseError> {
    if let Some(hint) = &hints.node.prose_in {
        validate_hint_alter(&hints.span, "prose_in", hint, num_items)?;
    }
    Ok(())
}

/// Checks the `prose_out` template against the items it describes.
fn validate_hint_prose_out(hints: &annot::Hints, num_items: usize) -> Result<(), ProseError> {
    if let Some(hint) = &hints.node.prose_out {
        validate_hint_alter(&hints.span, "prose_out", hint, num_items)?;
    }
    Ok(())
}

/// Checks the `prose_true` template against the items it describes.
fn validate_hint_prose_true(hints: &annot::Hints, num_items: usize) -> Result<(), ProseError> {
    if let Some(hint) = &hints.node.prose_true {
        validate_hint_alter(&hints.span, "prose_true", hint, num_items)?;
    }
    Ok(())
}

/// Checks the `prose_false` template against the items it describes.
fn validate_hint_prose_false(hints: &annot::Hints, num_items: usize) -> Result<(), ProseError> {
    if let Some(hint) = &hints.node.prose_false {
        validate_hint_alter(&hints.span, "prose_false", hint, num_items)?;
    }
    Ok(())
}

/// Checks the field hint against the number of fields.
fn validate_hint_fields(hints: &annot::Hints, num_fields: usize) -> Result<(), ProseError> {
    if let Some(hint) = &hints.node.prose_fields {
        fields::validate(hint, num_fields).map_err(
            |fields::FieldError::FieldCountMismatch { expected, actual }| {
                error::field_hint_arity_mismatch(&hints.span, hint, expected, actual)
            },
        )?;
    }
    Ok(())
}

// == Expressions

// - Expression

/// Converts an expression; only cases and calls pick up hints.
fn prosify_exp(ctx: &Context, exp_sl: &sl::Exp) -> Result<pl::Exp, ProseError> {
    match &exp_sl.node {
        il::ExpKind::Bool(value) => Ok(crate::annotated_note_phrase! {
            node: pl::ExpKind::Bool(*value),
            note: exp_sl.note.as_ref().clone(),
            span: exp_sl.span.clone(),
        }),
        il::ExpKind::Num(num) => Ok(crate::annotated_note_phrase! {
            node: pl::ExpKind::Num(num.clone()),
            note: exp_sl.note.as_ref().clone(),
            span: exp_sl.span.clone(),
        }),
        il::ExpKind::Text(text) => Ok(crate::annotated_note_phrase! {
            node: pl::ExpKind::Text(text.clone()),
            note: exp_sl.note.as_ref().clone(),
            span: exp_sl.span.clone(),
        }),
        il::ExpKind::Id(id) => Ok(crate::annotated_note_phrase! {
            node: pl::ExpKind::Id(id.clone()),
            note: exp_sl.note.as_ref().clone(),
            span: exp_sl.span.clone(),
        }),
        il::ExpKind::Un(op, op_typ, exp_inner_sl) => {
            prosify_un_exp(ctx, exp_sl, *op, *op_typ, exp_inner_sl)
        }
        il::ExpKind::Bin(op, op_typ, exp_l_sl, exp_r_sl) => {
            prosify_bin_exp(ctx, exp_sl, *op, *op_typ, exp_l_sl, exp_r_sl)
        }
        il::ExpKind::Cmp(op, op_typ, exp_l_sl, exp_r_sl) => {
            prosify_cmp_exp(ctx, exp_sl, *op, *op_typ, exp_l_sl, exp_r_sl)
        }
        il::ExpKind::UpCast(typ, exp_inner_sl) => {
            prosify_upcast_exp(ctx, exp_sl, typ, exp_inner_sl)
        }
        il::ExpKind::DownCast(typ, exp_inner_sl) => {
            prosify_downcast_exp(ctx, exp_sl, typ, exp_inner_sl)
        }
        il::ExpKind::Sub(exp_inner_sl, typ, subcheck) => {
            prosify_sub_exp(ctx, exp_sl, exp_inner_sl, typ, subcheck)
        }
        il::ExpKind::Match(exp_inner_sl, pattern) => {
            prosify_match_exp(ctx, exp_sl, exp_inner_sl, pattern)
        }
        il::ExpKind::Tuple(exps_sl) => prosify_tuple_exp(ctx, exp_sl, exps_sl),
        il::ExpKind::Case(not_exp_sl) => prosify_case_exp(ctx, exp_sl, not_exp_sl),
        il::ExpKind::Str(fields_sl) => prosify_struct_exp(ctx, exp_sl, fields_sl),
        il::ExpKind::Opt(exp_opt_sl) => prosify_option_exp(ctx, exp_sl, exp_opt_sl),
        il::ExpKind::List(exps_sl) => prosify_list_exp(ctx, exp_sl, exps_sl),
        il::ExpKind::Cons(exp_l_sl, exp_r_sl) => prosify_cons_exp(ctx, exp_sl, exp_l_sl, exp_r_sl),
        il::ExpKind::Cat(exp_l_sl, exp_r_sl) => prosify_cat_exp(ctx, exp_sl, exp_l_sl, exp_r_sl),
        il::ExpKind::Mem(exp_l_sl, exp_r_sl) => prosify_mem_exp(ctx, exp_sl, exp_l_sl, exp_r_sl),
        il::ExpKind::Len(exp_inner_sl) => prosify_len_exp(ctx, exp_sl, exp_inner_sl),
        il::ExpKind::Dot(exp_inner_sl, atom) => prosify_dot_exp(ctx, exp_sl, exp_inner_sl, atom),
        il::ExpKind::Idx(exp_l_sl, exp_r_sl) => prosify_idx_exp(ctx, exp_sl, exp_l_sl, exp_r_sl),
        il::ExpKind::Slice(exp_base_sl, exp_idx_sl, exp_len_sl) => {
            prosify_slice_exp(ctx, exp_sl, exp_base_sl, exp_idx_sl, exp_len_sl)
        }
        il::ExpKind::Upd(exp_base_sl, path_sl, exp_field_sl) => {
            prosify_update_exp(ctx, exp_sl, exp_base_sl, path_sl, exp_field_sl)
        }
        il::ExpKind::Call(id, targs, args_sl) => prosify_call_exp(ctx, exp_sl, id, targs, args_sl),
        il::ExpKind::Iter(exp_inner_sl, iter_exp) => {
            prosify_iter_exp(ctx, exp_sl, exp_inner_sl, iter_exp)
        }
    }
}

// - Unary expression

/// Converts a unary expression.
fn prosify_un_exp(
    ctx: &Context,
    exp_sl: &sl::Exp,
    op: il::UnOp,
    op_typ: il::OpTyp,
    exp_inner_sl: &sl::Exp,
) -> Result<pl::Exp, ProseError> {
    let exp_inner_pl = prosify_exp(ctx, exp_inner_sl)?;
    let exp_kind_pl = pl::ExpKind::Un(op, op_typ, Box::new(exp_inner_pl));
    Ok(crate::annotated_note_phrase! {
        node: exp_kind_pl,
        note: exp_sl.note.as_ref().clone(),
        span: exp_sl.span.clone(),
    })
}

// - Binary expression

/// Converts a binary expression.
fn prosify_bin_exp(
    ctx: &Context,
    exp_sl: &sl::Exp,
    op: il::BinOp,
    op_typ: il::OpTyp,
    exp_l_sl: &sl::Exp,
    exp_r_sl: &sl::Exp,
) -> Result<pl::Exp, ProseError> {
    let exp_l_pl = prosify_exp(ctx, exp_l_sl)?;
    let exp_r_pl = prosify_exp(ctx, exp_r_sl)?;
    let exp_kind_pl = pl::ExpKind::Bin(op, op_typ, Box::new(exp_l_pl), Box::new(exp_r_pl));
    Ok(crate::annotated_note_phrase! {
        node: exp_kind_pl,
        note: exp_sl.note.as_ref().clone(),
        span: exp_sl.span.clone(),
    })
}

// - Comparison expression

/// Converts a comparison.
fn prosify_cmp_exp(
    ctx: &Context,
    exp_sl: &sl::Exp,
    op: il::CmpOp,
    op_typ: il::OpTyp,
    exp_l_sl: &sl::Exp,
    exp_r_sl: &sl::Exp,
) -> Result<pl::Exp, ProseError> {
    let exp_l_pl = prosify_exp(ctx, exp_l_sl)?;
    let exp_r_pl = prosify_exp(ctx, exp_r_sl)?;
    let exp_kind_pl = pl::ExpKind::Cmp(op, op_typ, Box::new(exp_l_pl), Box::new(exp_r_pl));
    Ok(crate::annotated_note_phrase! {
        node: exp_kind_pl,
        note: exp_sl.note.as_ref().clone(),
        span: exp_sl.span.clone(),
    })
}

// - Upcast expression

/// Converts an upcast.
fn prosify_upcast_exp(
    ctx: &Context,
    exp_sl: &sl::Exp,
    typ: &sl::Typ,
    exp_inner_sl: &sl::Exp,
) -> Result<pl::Exp, ProseError> {
    let exp_inner_pl = prosify_exp(ctx, exp_inner_sl)?;
    let exp_kind_pl = pl::ExpKind::UpCast(typ.clone(), Box::new(exp_inner_pl));
    Ok(crate::annotated_note_phrase! {
        node: exp_kind_pl,
        note: exp_sl.note.as_ref().clone(),
        span: exp_sl.span.clone(),
    })
}

// - Downcast expression

/// Converts a downcast.
fn prosify_downcast_exp(
    ctx: &Context,
    exp_sl: &sl::Exp,
    typ: &sl::Typ,
    exp_inner_sl: &sl::Exp,
) -> Result<pl::Exp, ProseError> {
    let exp_inner_pl = prosify_exp(ctx, exp_inner_sl)?;
    let exp_kind_pl = pl::ExpKind::DownCast(typ.clone(), Box::new(exp_inner_pl));
    Ok(crate::annotated_note_phrase! {
        node: exp_kind_pl,
        note: exp_sl.note.as_ref().clone(),
        span: exp_sl.span.clone(),
    })
}

// - Subtype expression

/// Converts a subtype test.
fn prosify_sub_exp(
    ctx: &Context,
    exp_sl: &sl::Exp,
    exp_inner_sl: &sl::Exp,
    typ: &sl::Typ,
    subcheck: &sl::Subcheck,
) -> Result<pl::Exp, ProseError> {
    let exp_inner_pl = prosify_exp(ctx, exp_inner_sl)?;
    let exp_kind_pl =
        pl::ExpKind::Sub(Box::new(exp_inner_pl), typ.clone(), Box::new(subcheck.clone()));
    Ok(crate::annotated_note_phrase! {
        node: exp_kind_pl,
        note: exp_sl.note.as_ref().clone(),
        span: exp_sl.span.clone(),
    })
}

// - Match expression

/// Converts a pattern match test.
fn prosify_match_exp(
    ctx: &Context,
    exp_sl: &sl::Exp,
    exp_inner_sl: &sl::Exp,
    pattern: &sl::Pattern,
) -> Result<pl::Exp, ProseError> {
    let exp_inner_pl = prosify_exp(ctx, exp_inner_sl)?;
    let exp_kind_pl = pl::ExpKind::Match(Box::new(exp_inner_pl), pattern.clone());
    Ok(crate::annotated_note_phrase! {
        node: exp_kind_pl,
        note: exp_sl.note.as_ref().clone(),
        span: exp_sl.span.clone(),
    })
}

// - Tuple expression

/// Converts a tuple.
fn prosify_tuple_exp(
    ctx: &Context,
    exp_sl: &sl::Exp,
    exps_sl: &[sl::Exp],
) -> Result<pl::Exp, ProseError> {
    let exps_pl = prosify_exps(ctx, exps_sl)?;
    let exp_kind_pl = pl::ExpKind::Tuple(exps_pl);
    Ok(crate::annotated_note_phrase! {
        node: exp_kind_pl,
        note: exp_sl.note.as_ref().clone(),
        span: exp_sl.span.clone(),
    })
}

// - Case expression

/// Converts a case notation with its variant's `prose`/`prose_fields` hints.
fn prosify_case_exp(
    ctx: &Context,
    exp_sl: &sl::Exp,
    not_exp_sl: &sl::NotExp,
) -> Result<pl::Exp, ProseError> {
    let not_exp_pl = prosify_not_exp(ctx, not_exp_sl)?;
    // The variant is looked up by the expression's type and its mixfix operator
    let mut hints = annot::Hints::default();
    if let il::TypKind::Var(id_typ, _) = exp_sl.note.as_ref()
        && let Some(hints_case) = ctx.hints_case(id_typ, not_exp_sl.mixop().as_ref())
    {
        hints.span = hints_case.span.clone();
        hints.node.prose = hints_case.node.prose.clone();
        hints.node.prose_fields = hints_case.node.prose_fields.clone();
        // Holes and field names count the notation's arguments
        let num_args = not_exp_sl.args().len();
        validate_hint_prose(&hints, num_args)?;
        validate_hint_fields(&hints, num_args)?;
    }
    let exp_kind_pl = pl::ExpKind::Case(Box::new(not_exp_pl));
    Ok(crate::annotated_note_phrase! {
        node: exp_kind_pl,
        note: exp_sl.note.as_ref().clone(),
        span: exp_sl.span.clone(),
        hints: hints,
    })
}

// - Struct expression

/// Converts a struct literal.
fn prosify_struct_exp(
    ctx: &Context,
    exp_sl: &sl::Exp,
    fields_sl: &[il::ExpField],
) -> Result<pl::Exp, ProseError> {
    let mut fields_pl = Vec::with_capacity(fields_sl.len());
    for il::ExpField { atom, exp: exp_sl } in fields_sl {
        let exp_pl = prosify_exp(ctx, exp_sl)?;
        fields_pl.push((atom.clone(), exp_pl));
    }
    let exp_kind_pl = pl::ExpKind::Str(fields_pl);
    Ok(crate::annotated_note_phrase! {
        node: exp_kind_pl,
        note: exp_sl.note.as_ref().clone(),
        span: exp_sl.span.clone(),
    })
}

// - Optional expression

/// Converts an optional.
fn prosify_option_exp(
    ctx: &Context,
    exp_sl: &sl::Exp,
    exp_opt_sl: &Option<Box<sl::Exp>>,
) -> Result<pl::Exp, ProseError> {
    let exp_opt_pl = match exp_opt_sl.as_deref() {
        Some(exp_inner_sl) => {
            let exp_inner_pl = prosify_exp(ctx, exp_inner_sl)?;
            Some(Box::new(exp_inner_pl))
        }
        None => None,
    };
    let exp_kind_pl = pl::ExpKind::Opt(exp_opt_pl);
    Ok(crate::annotated_note_phrase! {
        node: exp_kind_pl,
        note: exp_sl.note.as_ref().clone(),
        span: exp_sl.span.clone(),
    })
}

// - List expression

/// Converts a list.
fn prosify_list_exp(
    ctx: &Context,
    exp_sl: &sl::Exp,
    exps_sl: &[sl::Exp],
) -> Result<pl::Exp, ProseError> {
    let exps_pl = prosify_exps(ctx, exps_sl)?;
    let exp_kind_pl = pl::ExpKind::List(exps_pl);
    Ok(crate::annotated_note_phrase! {
        node: exp_kind_pl,
        note: exp_sl.note.as_ref().clone(),
        span: exp_sl.span.clone(),
    })
}

// - Cons expression

/// Converts a cons.
fn prosify_cons_exp(
    ctx: &Context,
    exp_sl: &sl::Exp,
    exp_l_sl: &sl::Exp,
    exp_r_sl: &sl::Exp,
) -> Result<pl::Exp, ProseError> {
    let exp_l_pl = prosify_exp(ctx, exp_l_sl)?;
    let exp_r_pl = prosify_exp(ctx, exp_r_sl)?;
    let exp_kind_pl = pl::ExpKind::Cons(Box::new(exp_l_pl), Box::new(exp_r_pl));
    Ok(crate::annotated_note_phrase! {
        node: exp_kind_pl,
        note: exp_sl.note.as_ref().clone(),
        span: exp_sl.span.clone(),
    })
}

// - Concatenation expression

/// Converts a concatenation.
fn prosify_cat_exp(
    ctx: &Context,
    exp_sl: &sl::Exp,
    exp_l_sl: &sl::Exp,
    exp_r_sl: &sl::Exp,
) -> Result<pl::Exp, ProseError> {
    let exp_l_pl = prosify_exp(ctx, exp_l_sl)?;
    let exp_r_pl = prosify_exp(ctx, exp_r_sl)?;
    let exp_kind_pl = pl::ExpKind::Cat(Box::new(exp_l_pl), Box::new(exp_r_pl));
    Ok(crate::annotated_note_phrase! {
        node: exp_kind_pl,
        note: exp_sl.note.as_ref().clone(),
        span: exp_sl.span.clone(),
    })
}

// - Membership expression

/// Converts a membership test.
fn prosify_mem_exp(
    ctx: &Context,
    exp_sl: &sl::Exp,
    exp_l_sl: &sl::Exp,
    exp_r_sl: &sl::Exp,
) -> Result<pl::Exp, ProseError> {
    let exp_l_pl = prosify_exp(ctx, exp_l_sl)?;
    let exp_r_pl = prosify_exp(ctx, exp_r_sl)?;
    let exp_kind_pl = pl::ExpKind::Mem(Box::new(exp_l_pl), Box::new(exp_r_pl));
    Ok(crate::annotated_note_phrase! {
        node: exp_kind_pl,
        note: exp_sl.note.as_ref().clone(),
        span: exp_sl.span.clone(),
    })
}

// - Length expression

/// Converts a length.
fn prosify_len_exp(
    ctx: &Context,
    exp_sl: &sl::Exp,
    exp_inner_sl: &sl::Exp,
) -> Result<pl::Exp, ProseError> {
    let exp_inner_pl = prosify_exp(ctx, exp_inner_sl)?;
    let exp_kind_pl = pl::ExpKind::Len(Box::new(exp_inner_pl));
    Ok(crate::annotated_note_phrase! {
        node: exp_kind_pl,
        note: exp_sl.note.as_ref().clone(),
        span: exp_sl.span.clone(),
    })
}

// - Dot expression

/// Converts a field access.
fn prosify_dot_exp(
    ctx: &Context,
    exp_sl: &sl::Exp,
    exp_inner_sl: &sl::Exp,
    atom: &il::Atom,
) -> Result<pl::Exp, ProseError> {
    let exp_inner_pl = prosify_exp(ctx, exp_inner_sl)?;
    let exp_kind_pl = pl::ExpKind::Dot(Box::new(exp_inner_pl), atom.clone());
    Ok(crate::annotated_note_phrase! {
        node: exp_kind_pl,
        note: exp_sl.note.as_ref().clone(),
        span: exp_sl.span.clone(),
    })
}

// - Index expression

/// Converts an index.
fn prosify_idx_exp(
    ctx: &Context,
    exp_sl: &sl::Exp,
    exp_l_sl: &sl::Exp,
    exp_r_sl: &sl::Exp,
) -> Result<pl::Exp, ProseError> {
    let exp_l_pl = prosify_exp(ctx, exp_l_sl)?;
    let exp_r_pl = prosify_exp(ctx, exp_r_sl)?;
    let exp_kind_pl = pl::ExpKind::Idx(Box::new(exp_l_pl), Box::new(exp_r_pl));
    Ok(crate::annotated_note_phrase! {
        node: exp_kind_pl,
        note: exp_sl.note.as_ref().clone(),
        span: exp_sl.span.clone(),
    })
}

// - Slice expression

/// Converts a slice.
fn prosify_slice_exp(
    ctx: &Context,
    exp_sl: &sl::Exp,
    exp_base_sl: &sl::Exp,
    exp_idx_sl: &sl::Exp,
    exp_len_sl: &sl::Exp,
) -> Result<pl::Exp, ProseError> {
    let exp_base_pl = prosify_exp(ctx, exp_base_sl)?;
    let exp_idx_pl = prosify_exp(ctx, exp_idx_sl)?;
    let exp_len_pl = prosify_exp(ctx, exp_len_sl)?;
    let exp_kind_pl =
        pl::ExpKind::Slice(Box::new(exp_base_pl), Box::new(exp_idx_pl), Box::new(exp_len_pl));
    Ok(crate::annotated_note_phrase! {
        node: exp_kind_pl,
        note: exp_sl.note.as_ref().clone(),
        span: exp_sl.span.clone(),
    })
}

// - Update expression

/// Converts an update of a path within a value.
fn prosify_update_exp(
    ctx: &Context,
    exp_sl: &sl::Exp,
    exp_base_sl: &sl::Exp,
    path_sl: &sl::Path,
    exp_field_sl: &sl::Exp,
) -> Result<pl::Exp, ProseError> {
    let exp_base_pl = prosify_exp(ctx, exp_base_sl)?;
    let path_pl = prosify_path(ctx, path_sl)?;
    let exp_field_pl = prosify_exp(ctx, exp_field_sl)?;
    let exp_kind_pl =
        pl::ExpKind::Upd(Box::new(exp_base_pl), Box::new(path_pl), Box::new(exp_field_pl));
    Ok(crate::annotated_note_phrase! {
        node: exp_kind_pl,
        note: exp_sl.note.as_ref().clone(),
        span: exp_sl.span.clone(),
    })
}

// - Call expression

/// Converts a call with the function's hints, holes counting its arguments.
fn prosify_call_exp(
    ctx: &Context,
    exp_sl: &sl::Exp,
    id_func: &sl::Id,
    targs: &[sl::Targ],
    args_sl: &[sl::Arg],
) -> Result<pl::Exp, ProseError> {
    let args_pl = prosify_args(ctx, args_sl)?;
    let hints = build_func_hints(ctx, id_func, args_sl.len())?;
    let exp_kind_pl = pl::ExpKind::Call(id_func.clone(), targs.to_vec(), args_pl);
    Ok(crate::annotated_note_phrase! {
        node: exp_kind_pl,
        note: exp_sl.note.as_ref().clone(),
        span: exp_sl.span.clone(),
        hints: hints,
    })
}

// - Iterated expression

/// Converts an iterated expression.
fn prosify_iter_exp(
    ctx: &Context,
    exp_sl: &sl::Exp,
    exp_inner_sl: &sl::Exp,
    iter_exp: &sl::ExpIter,
) -> Result<pl::Exp, ProseError> {
    let exp_inner_pl = prosify_exp(ctx, exp_inner_sl)?;
    let exp_kind_pl = pl::ExpKind::Iter(Box::new(exp_inner_pl), iter_exp.clone());
    Ok(crate::annotated_note_phrase! {
        node: exp_kind_pl,
        note: exp_sl.note.as_ref().clone(),
        span: exp_sl.span.clone(),
    })
}

// - Expression list

/// Converts expressions in order.
fn prosify_exps(ctx: &Context, exps_sl: &[sl::Exp]) -> Result<Vec<pl::Exp>, ProseError> {
    let mut exps_pl = Vec::with_capacity(exps_sl.len());
    for exp_sl in exps_sl {
        let exp_pl = prosify_exp(ctx, exp_sl)?;
        exps_pl.push(exp_pl);
    }
    Ok(exps_pl)
}

// - Notation expression

/// Converts the arguments of a notation, keeping its shape.
fn prosify_not_exp(ctx: &Context, not_exp_sl: &sl::NotExp) -> Result<pl::NotExp, ProseError> {
    not_exp_sl.try_map(|exp_sl| prosify_exp(ctx, exp_sl))
}

// == Paths

/// Converts a path.
fn prosify_path(ctx: &Context, path_sl: &sl::Path) -> Result<pl::Path, ProseError> {
    match &path_sl.node {
        il::PathKind::Root => Ok(crate::note_phrase! {
            node: pl::PathKind::Root,
            note: path_sl.note.as_ref().clone(),
            span: path_sl.span.clone(),
        }),
        il::PathKind::Idx(path_inner_sl, exp_idx_sl) => {
            prosify_idx_path(ctx, path_sl, path_inner_sl, exp_idx_sl)
        }
        il::PathKind::Slice(path_inner_sl, exp_idx_sl, exp_len_sl) => {
            prosify_slice_path(ctx, path_sl, path_inner_sl, exp_idx_sl, exp_len_sl)
        }
        il::PathKind::Dot(path_inner_sl, atom) => {
            prosify_dot_path(ctx, path_sl, path_inner_sl, atom)
        }
    }
}

/// Converts an index step.
fn prosify_idx_path(
    ctx: &Context,
    path_sl: &sl::Path,
    path_inner_sl: &sl::Path,
    exp_idx_sl: &sl::Exp,
) -> Result<pl::Path, ProseError> {
    let path_inner_pl = prosify_path(ctx, path_inner_sl)?;
    let exp_idx_pl = prosify_exp(ctx, exp_idx_sl)?;
    Ok(crate::note_phrase! {
        node: pl::PathKind::Idx(Box::new(path_inner_pl), Box::new(exp_idx_pl)),
        note: path_sl.note.as_ref().clone(),
        span: path_sl.span.clone(),
    })
}

/// Converts a slice step.
fn prosify_slice_path(
    ctx: &Context,
    path_sl: &sl::Path,
    path_inner_sl: &sl::Path,
    exp_idx_sl: &sl::Exp,
    exp_len_sl: &sl::Exp,
) -> Result<pl::Path, ProseError> {
    let path_inner_pl = prosify_path(ctx, path_inner_sl)?;
    let exp_idx_pl = prosify_exp(ctx, exp_idx_sl)?;
    let exp_len_pl = prosify_exp(ctx, exp_len_sl)?;
    Ok(crate::note_phrase! {
        node: pl::PathKind::Slice(
            Box::new(path_inner_pl),
            Box::new(exp_idx_pl),
            Box::new(exp_len_pl),
        ),
        note: path_sl.note.as_ref().clone(),
        span: path_sl.span.clone(),
    })
}

/// Converts a field step.
fn prosify_dot_path(
    ctx: &Context,
    path_sl: &sl::Path,
    path_inner_sl: &sl::Path,
    atom: &sl::Atom,
) -> Result<pl::Path, ProseError> {
    let path_inner_pl = prosify_path(ctx, path_inner_sl)?;
    Ok(crate::note_phrase! {
        node: pl::PathKind::Dot(Box::new(path_inner_pl), atom.clone()),
        note: path_sl.note.as_ref().clone(),
        span: path_sl.span.clone(),
    })
}

// == Arguments

/// Converts an argument; function arguments are names only.
fn prosify_arg(ctx: &Context, arg_sl: &sl::Arg) -> Result<pl::Arg, ProseError> {
    match &arg_sl.node {
        il::ArgKind::Exp(exp_sl) => prosify_exp_arg(ctx, arg_sl, exp_sl),
        il::ArgKind::Def(id) => Ok(crate::phrase! {
            node: pl::ArgKind::Def(id.clone()),
            span: arg_sl.span.clone(),
        }),
    }
}

/// Converts an expression argument.
fn prosify_exp_arg(
    ctx: &Context,
    arg_sl: &sl::Arg,
    exp_sl: &sl::Exp,
) -> Result<pl::Arg, ProseError> {
    let exp_pl = prosify_exp(ctx, exp_sl)?;
    Ok(crate::phrase! {
        node: pl::ArgKind::Exp(Box::new(exp_pl)),
        span: arg_sl.span.clone(),
    })
}

/// Converts arguments in order.
fn prosify_args(ctx: &Context, args_sl: &[sl::Arg]) -> Result<Vec<pl::Arg>, ProseError> {
    let mut args_pl = Vec::with_capacity(args_sl.len());
    for arg_sl in args_sl {
        let arg_pl = prosify_arg(ctx, arg_sl)?;
        args_pl.push(arg_pl);
    }
    Ok(args_pl)
}

// == Parameters

/// Converts a parameter.
fn prosify_param(ctx: &Context, param_sl: &sl::Param) -> Result<pl::Param, ProseError> {
    match &param_sl.node {
        sl::ParamKind::Exp(typ, exp_sl) => prosify_exp_param(ctx, param_sl, typ, exp_sl),
        sl::ParamKind::Def(id, tparams, params_sl, typ) => {
            prosify_def_param(ctx, param_sl, id, tparams, params_sl, typ)
        }
    }
}

/// Converts a typed pattern parameter.
fn prosify_exp_param(
    ctx: &Context,
    param_sl: &sl::Param,
    typ: &sl::Typ,
    exp_sl: &sl::Exp,
) -> Result<pl::Param, ProseError> {
    let exp_pl = prosify_exp(ctx, exp_sl)?;
    Ok(crate::phrase! {
        node: pl::ParamKind::Exp(typ.clone(), Box::new(exp_pl)),
        span: param_sl.span.clone(),
    })
}

/// Converts a function parameter with its own parameters.
fn prosify_def_param(
    ctx: &Context,
    param_sl: &sl::Param,
    id: &sl::Id,
    tparams: &[sl::TParam],
    params_sl: &[sl::Param],
    typ: &sl::Typ,
) -> Result<pl::Param, ProseError> {
    let params_pl = prosify_params(ctx, params_sl)?;
    Ok(crate::phrase! {
        node: pl::ParamKind::Def(id.clone(), tparams.to_vec(), params_pl, typ.clone()),
        span: param_sl.span.clone(),
    })
}

/// Converts parameters in order.
fn prosify_params(ctx: &Context, params_sl: &[sl::Param]) -> Result<Vec<pl::Param>, ProseError> {
    let mut params_pl = Vec::with_capacity(params_sl.len());
    for param_sl in params_sl {
        let param_pl = prosify_param(ctx, param_sl)?;
        params_pl.push(param_pl);
    }
    Ok(params_pl)
}

// == Guards

/// Converts a guard; the shorthand guards arise later.
fn prosify_guard(ctx: &Context, guard_sl: &sl::Guard) -> Result<pl::Guard, ProseError> {
    let guard_pl = match guard_sl {
        sl::Guard::Bool(value) => pl::Guard::Bool(*value),
        sl::Guard::Cmp(op, op_typ, exp_sl) => {
            let exp_pl = prosify_exp(ctx, exp_sl)?;
            pl::Guard::Cmp(*op, *op_typ, exp_pl)
        }
        sl::Guard::Sub(typ, subcheck) => pl::Guard::Sub(typ.clone(), subcheck.clone()),
        sl::Guard::Match(pattern) => pl::Guard::Match(pattern.clone()),
        sl::Guard::Mem(exp_sl) => {
            let exp_pl = prosify_exp(ctx, exp_sl)?;
            pl::Guard::Mem(exp_pl)
        }
    };
    Ok(guard_pl)
}

// == Dispatch instructions

// - Instruction

/// Converts a dispatch-tier instruction into the block it expands to.
fn prosify_dispatch_instr(
    ctx: &Context,
    instr_sl: sl::Instr,
) -> Result<pl::DispatchBlock, ProseError> {
    let span = instr_sl.span;
    match instr_sl.node {
        sl::InstrKind::If(instr_sl) => prosify_dispatch_if_instr(ctx, instr_sl, span),
        sl::InstrKind::Hold(instr_sl) => prosify_dispatch_hold_instr(ctx, instr_sl, span),
        sl::InstrKind::Case(instr_sl) => prosify_dispatch_case_instr(ctx, instr_sl, span),
        sl::InstrKind::Let(instr_sl) => prosify_dispatch_let_instr(ctx, instr_sl, span),
        sl::InstrKind::Debug(instr_sl) => prosify_dispatch_debug_instr(ctx, instr_sl, span),
        sl::InstrKind::Group(instr_sl) => prosify_dispatch_rulegroup_instr(ctx, instr_sl, span),
        // Group-body instructions cannot appear before a group is chosen
        sl::InstrKind::Rule(_) | sl::InstrKind::Result(_) | sl::InstrKind::Return(_) => {
            // structure(false) keeps rule results inside groups; expansion only adds lets
            unreachable!("structure(false) preserves dispatch tiers")
        }
    }
}

// - If instruction

/// Converts a condition and its then-block.
fn prosify_dispatch_if_instr(
    ctx: &Context,
    instr_sl: sl::IfInstr,
    span: Span,
) -> Result<pl::DispatchBlock, ProseError> {
    let exp_pl = prosify_exp(ctx, &instr_sl.exp)?;
    let block_pl = prosify_dispatch_block(ctx, instr_sl.block)?;
    let instr_pl = pl::IfInstr {
        exp: exp_pl,
        iter_exps: instr_sl.iter_exps,
        block: block_pl,
        dangle: instr_sl.dangle,
    };
    let instr_kind_pl = pl::InstrKind::If(instr_pl);
    let instr_pl = crate::annotated_note_phrase! {
        node: instr_kind_pl,
        note: None,
        span: span,
    };
    Ok(vec![instr_pl])
}

// - Hold instruction

/// Converts a hold with the relation's `prose_true` and `prose_false` hints.
fn prosify_dispatch_hold_instr(
    ctx: &Context,
    instr_sl: sl::HoldInstr,
    span: Span,
) -> Result<pl::DispatchBlock, ProseError> {
    let mut hints = annot::Hints::default();
    if let Some(hints_rel) = ctx.hints_rel(&instr_sl.id) {
        hints.span = hints_rel.span.clone();
        hints.node.prose_true = hints_rel.node.prose_true.clone();
        hints.node.prose_false = hints_rel.node.prose_false.clone();
        // Holes count the notation's arguments
        let num_args = instr_sl.not_exp.args().len();
        validate_hint_prose_true(&hints, num_args)?;
        validate_hint_prose_false(&hints, num_args)?;
    }
    let not_exp_pl = prosify_not_exp(ctx, &instr_sl.not_exp)?;
    let hold_case_pl = prosify_dispatch_hold_case(ctx, instr_sl.hold_case)?;
    let instr_pl = pl::HoldInstr {
        id: instr_sl.id,
        not_exp: not_exp_pl,
        iter_exps: instr_sl.iter_exps,
        hold_case: hold_case_pl,
    };
    let instr_kind_pl = pl::InstrKind::Hold(instr_pl);
    let instr_pl = crate::annotated_note_phrase! {
        node: instr_kind_pl,
        note: None,
        span: span,
        hints: hints,
    };
    Ok(vec![instr_pl])
}

/// Converts whichever branches a hold has.
fn prosify_dispatch_hold_case(
    ctx: &Context,
    hold_case_sl: sl::HoldCase,
) -> Result<pl::HoldCase<pl::DispatchInstr>, ProseError> {
    let hold_case_pl = match hold_case_sl {
        sl::HoldCase::Both(block_hold_sl, block_not_hold_sl) => {
            let block_hold_pl = prosify_dispatch_block(ctx, block_hold_sl)?;
            let block_not_hold_pl = prosify_dispatch_block(ctx, block_not_hold_sl)?;
            pl::HoldCase::Both(block_hold_pl, block_not_hold_pl)
        }
        sl::HoldCase::Hold(block_sl, dangle) => {
            let block_pl = prosify_dispatch_block(ctx, block_sl)?;
            pl::HoldCase::Hold(block_pl, dangle)
        }
        sl::HoldCase::NotHold(block_sl, dangle) => {
            let block_pl = prosify_dispatch_block(ctx, block_sl)?;
            pl::HoldCase::NotHold(block_pl, dangle)
        }
    };
    Ok(hold_case_pl)
}

// - Case instruction

/// Converts one arm.
fn prosify_dispatch_case(
    ctx: &Context,
    case_sl: sl::Case,
) -> Result<pl::Case<pl::DispatchInstr>, ProseError> {
    let guard_pl = prosify_guard(ctx, &case_sl.guard)?;
    let block_pl = prosify_dispatch_block(ctx, case_sl.block)?;
    Ok(pl::Case { guard: guard_pl, block: block_pl })
}

/// Converts a case analysis.
fn prosify_dispatch_case_instr(
    ctx: &Context,
    instr_sl: sl::CaseInstr,
    span: Span,
) -> Result<pl::DispatchBlock, ProseError> {
    let exp_pl = prosify_exp(ctx, &instr_sl.exp)?;
    let mut cases_pl = Vec::with_capacity(instr_sl.cases.len());
    for case_sl in instr_sl.cases {
        let case_pl = prosify_dispatch_case(ctx, case_sl)?;
        cases_pl.push(case_pl);
    }
    let instr_pl = pl::CaseInstr { exp: exp_pl, cases: cases_pl, dangle: instr_sl.dangle };
    let instr_kind_pl = pl::InstrKind::Case(instr_pl);
    let instr_pl = crate::annotated_note_phrase! {
        node: instr_kind_pl,
        note: None,
        span: span,
    };
    Ok(vec![instr_pl])
}

// - Let instruction

/// Converts a let, then appends its body: the continuation flattens.
fn prosify_dispatch_let_instr(
    ctx: &Context,
    instr_sl: sl::LetInstr,
    span: Span,
) -> Result<pl::DispatchBlock, ProseError> {
    let exp_l_pl = prosify_exp(ctx, &instr_sl.exp_l)?;
    // A case pattern keeps its variant's field names for destructuring
    let hints = if matches!(exp_l_pl.node.node, pl::ExpKind::Case(_)) {
        crate::phrase! {
            node: annot::HintsKind {
                prose_fields: exp_l_pl.hints.node.prose_fields.clone(),
                ..annot::HintsKind::default()
            },
            span: exp_l_pl.hints.span.clone(),
        }
    } else {
        annot::Hints::default()
    };
    let exp_r_pl = prosify_exp(ctx, &instr_sl.exp_r)?;
    let instr_pl =
        pl::LetInstr { exp_l: exp_l_pl, exp_r: exp_r_pl, iter_instrs: instr_sl.iter_instrs };
    let instr_kind_pl = pl::InstrKind::Let(instr_pl);
    let instr_pl = crate::annotated_note_phrase! {
        node: instr_kind_pl,
        note: None,
        span: span,
        hints: hints,
    };
    // The body follows the let in the same block
    let block_pl = prosify_dispatch_block(ctx, instr_sl.block)?;
    let mut instrs_pl = vec![instr_pl];
    instrs_pl.extend(block_pl);
    Ok(instrs_pl)
}

// - Debug instruction

/// Converts a debug, then appends the instruction it wraps.
fn prosify_dispatch_debug_instr(
    ctx: &Context,
    instr_sl: sl::DebugInstr,
    span: Span,
) -> Result<pl::DispatchBlock, ProseError> {
    let exp_pl = prosify_exp(ctx, &instr_sl.exp)?;
    let instr_pl = pl::DebugInstr { exp: exp_pl };
    let instr_kind_pl = pl::InstrKind::Debug(instr_pl);
    let instr_pl = crate::annotated_note_phrase! {
        node: instr_kind_pl,
        note: None,
        span: span,
    };
    let instrs_follow_pl = prosify_dispatch_instr(ctx, *instr_sl.instr)?;
    let mut instrs_pl = vec![instr_pl];
    instrs_pl.extend(instrs_follow_pl);
    Ok(instrs_pl)
}

// - Group instruction

/// Converts a rule group with the relation's `prose_in` and `prose_true` hints.
fn prosify_dispatch_rulegroup_instr(
    ctx: &Context,
    instr_sl: sl::GroupInstr,
    span: Span,
) -> Result<pl::DispatchBlock, ProseError> {
    let mut hints = annot::Hints::default();
    if let Some(hints_rel) = ctx.hints_rel(ctx.namespace()) {
        hints.span = hints_rel.span.clone();
        hints.node.prose_in = hints_rel.node.prose_in.clone();
        hints.node.prose_true = hints_rel.node.prose_true.clone();
        // Group headings describe the relation inputs
        let num_inputs = instr_sl.rel_signature.input_hint.indices().len();
        validate_hint_prose_in(&hints, num_inputs)?;
        validate_hint_prose_true(&hints, num_inputs)?;
    }
    let exps_input_pl = prosify_exps(ctx, &instr_sl.exps)?;
    let block_pl = prosify_group_block(ctx, instr_sl.block)?;
    let instr_pl = pl::RuleGroupInstr {
        id_rel: ctx.namespace().clone(),
        id_group: instr_sl.id,
        rel_signature: instr_sl.rel_signature,
        exps_input: exps_input_pl,
        block: block_pl,
    };
    let tier_pl = pl::DispatchInstr::Group(instr_pl);
    let instr_pl = pl::TierInstr { tier: tier_pl };
    let instr_kind_pl = pl::InstrKind::Tier(instr_pl);
    let instr_pl = crate::annotated_note_phrase! {
        node: instr_kind_pl,
        note: None,
        span: span,
        hints: hints,
    };
    Ok(vec![instr_pl])
}

// - Block

/// Converts a block; several instructions become a route with one arm each.
fn prosify_dispatch_block(
    ctx: &Context,
    block_sl: sl::Block,
) -> Result<pl::DispatchBlock, ProseError> {
    match block_sl.len() {
        0 => Ok(Vec::new()),
        // One instruction expands in place
        1 => {
            let instr_sl = block_sl.into_iter().next().unwrap();
            prosify_dispatch_instr(ctx, instr_sl)
        }
        _ => {
            let span = block_sl.at();
            let mut blocks_pl = Vec::with_capacity(block_sl.len());
            for instr_sl in block_sl {
                let block_pl = prosify_dispatch_instr(ctx, instr_sl)?;
                blocks_pl.push(block_pl);
            }
            // Alternatives become the arms of a route spanning them all
            let instr_pl = pl::RouteInstr { blocks: blocks_pl };
            let tier_pl = pl::DispatchInstr::Route(instr_pl);
            let instr_pl = pl::TierInstr { tier: tier_pl };
            let instr_kind_pl = pl::InstrKind::Tier(instr_pl);
            let instr_pl = crate::annotated_note_phrase! {
                node: instr_kind_pl,
                note: None,
                span: span,
            };
            Ok(vec![instr_pl])
        }
    }
}

// == Group instructions

// - Instruction

/// Converts a group-tier instruction into the block it expands to.
fn prosify_group_instr(ctx: &Context, instr_sl: sl::Instr) -> Result<pl::GroupBlock, ProseError> {
    let span = instr_sl.span;
    match instr_sl.node {
        sl::InstrKind::If(instr_sl) => prosify_group_if_instr(ctx, instr_sl, span),
        sl::InstrKind::Hold(instr_sl) => prosify_group_hold_instr(ctx, instr_sl, span),
        sl::InstrKind::Case(instr_sl) => prosify_group_case_instr(ctx, instr_sl, span),
        sl::InstrKind::Let(instr_sl) => prosify_group_let_instr(ctx, instr_sl, span),
        sl::InstrKind::Debug(instr_sl) => prosify_group_debug_instr(ctx, instr_sl, span),
        sl::InstrKind::Rule(instr_sl) => prosify_group_rule_instr(ctx, instr_sl, span),
        sl::InstrKind::Result(instr_sl) => prosify_group_result_instr(ctx, instr_sl, span),
        sl::InstrKind::Return(instr_sl) => prosify_group_return_instr(ctx, instr_sl, span),
        // A rule group cannot nest inside a group body
        // struct_rule_path never creates groups; optimization preserves group boundaries
        sl::InstrKind::Group(_) => unreachable!("structure(false) preserves group tiers"),
    }
}

// - If instruction

/// Converts a condition and its then-block.
fn prosify_group_if_instr(
    ctx: &Context,
    instr_sl: sl::IfInstr,
    span: Span,
) -> Result<pl::GroupBlock, ProseError> {
    let exp_pl = prosify_exp(ctx, &instr_sl.exp)?;
    let block_pl = prosify_group_block(ctx, instr_sl.block)?;
    let instr_pl = pl::IfInstr {
        exp: exp_pl,
        iter_exps: instr_sl.iter_exps,
        block: block_pl,
        dangle: instr_sl.dangle,
    };
    let instr_kind_pl = pl::InstrKind::If(instr_pl);
    let instr_pl = crate::annotated_note_phrase! {
        node: instr_kind_pl,
        note: None,
        span: span,
    };
    Ok(vec![instr_pl])
}

// - Hold instruction

/// Converts a hold with the relation's `prose_true` and `prose_false` hints.
fn prosify_group_hold_instr(
    ctx: &Context,
    instr_sl: sl::HoldInstr,
    span: Span,
) -> Result<pl::GroupBlock, ProseError> {
    let mut hints = annot::Hints::default();
    if let Some(hints_rel) = ctx.hints_rel(&instr_sl.id) {
        hints.span = hints_rel.span.clone();
        hints.node.prose_true = hints_rel.node.prose_true.clone();
        hints.node.prose_false = hints_rel.node.prose_false.clone();
        // Holes count the notation's arguments
        let num_args = instr_sl.not_exp.args().len();
        validate_hint_prose_true(&hints, num_args)?;
        validate_hint_prose_false(&hints, num_args)?;
    }
    let not_exp_pl = prosify_not_exp(ctx, &instr_sl.not_exp)?;
    let hold_case_pl = prosify_group_hold_case(ctx, instr_sl.hold_case)?;
    let instr_pl = pl::HoldInstr {
        id: instr_sl.id,
        not_exp: not_exp_pl,
        iter_exps: instr_sl.iter_exps,
        hold_case: hold_case_pl,
    };
    let instr_kind_pl = pl::InstrKind::Hold(instr_pl);
    let instr_pl = crate::annotated_note_phrase! {
        node: instr_kind_pl,
        note: None,
        span: span,
        hints: hints,
    };
    Ok(vec![instr_pl])
}

/// Converts whichever branches a hold has.
fn prosify_group_hold_case(
    ctx: &Context,
    hold_case_sl: sl::HoldCase,
) -> Result<pl::HoldCase<pl::GroupInstr>, ProseError> {
    let hold_case_pl = match hold_case_sl {
        sl::HoldCase::Both(block_hold_sl, block_not_hold_sl) => {
            let block_hold_pl = prosify_group_block(ctx, block_hold_sl)?;
            let block_not_hold_pl = prosify_group_block(ctx, block_not_hold_sl)?;
            pl::HoldCase::Both(block_hold_pl, block_not_hold_pl)
        }
        sl::HoldCase::Hold(block_sl, dangle) => {
            let block_pl = prosify_group_block(ctx, block_sl)?;
            pl::HoldCase::Hold(block_pl, dangle)
        }
        sl::HoldCase::NotHold(block_sl, dangle) => {
            let block_pl = prosify_group_block(ctx, block_sl)?;
            pl::HoldCase::NotHold(block_pl, dangle)
        }
    };
    Ok(hold_case_pl)
}

// - Case instruction

/// Converts one arm.
fn prosify_group_case(
    ctx: &Context,
    case_sl: sl::Case,
) -> Result<pl::Case<pl::GroupInstr>, ProseError> {
    let guard_pl = prosify_guard(ctx, &case_sl.guard)?;
    let block_pl = prosify_group_block(ctx, case_sl.block)?;
    Ok(pl::Case { guard: guard_pl, block: block_pl })
}

/// Converts a case analysis.
fn prosify_group_case_instr(
    ctx: &Context,
    instr_sl: sl::CaseInstr,
    span: Span,
) -> Result<pl::GroupBlock, ProseError> {
    let exp_pl = prosify_exp(ctx, &instr_sl.exp)?;
    let mut cases_pl = Vec::with_capacity(instr_sl.cases.len());
    for case_sl in instr_sl.cases {
        let case_pl = prosify_group_case(ctx, case_sl)?;
        cases_pl.push(case_pl);
    }
    let instr_pl = pl::CaseInstr { exp: exp_pl, cases: cases_pl, dangle: instr_sl.dangle };
    let instr_kind_pl = pl::InstrKind::Case(instr_pl);
    let instr_pl = crate::annotated_note_phrase! {
        node: instr_kind_pl,
        note: None,
        span: span,
    };
    Ok(vec![instr_pl])
}

// - Let instruction

/// Converts a let, then appends its body: the continuation flattens.
fn prosify_group_let_instr(
    ctx: &Context,
    instr_sl: sl::LetInstr,
    span: Span,
) -> Result<pl::GroupBlock, ProseError> {
    let exp_l_pl = prosify_exp(ctx, &instr_sl.exp_l)?;
    // A case pattern keeps its variant's field names for destructuring
    let hints = if matches!(exp_l_pl.node.node, pl::ExpKind::Case(_)) {
        crate::phrase! {
            node: annot::HintsKind {
                prose_fields: exp_l_pl.hints.node.prose_fields.clone(),
                ..annot::HintsKind::default()
            },
            span: exp_l_pl.hints.span.clone(),
        }
    } else {
        annot::Hints::default()
    };
    let exp_r_pl = prosify_exp(ctx, &instr_sl.exp_r)?;
    let instr_pl =
        pl::LetInstr { exp_l: exp_l_pl, exp_r: exp_r_pl, iter_instrs: instr_sl.iter_instrs };
    let instr_kind_pl = pl::InstrKind::Let(instr_pl);
    let instr_pl = crate::annotated_note_phrase! {
        node: instr_kind_pl,
        note: None,
        span: span,
        hints: hints,
    };
    // The body follows the let in the same block
    let block_pl = prosify_group_block(ctx, instr_sl.block)?;
    let mut instrs_pl = vec![instr_pl];
    instrs_pl.extend(block_pl);
    Ok(instrs_pl)
}

// - Debug instruction

/// Converts a debug, then appends the instruction it wraps.
fn prosify_group_debug_instr(
    ctx: &Context,
    instr_sl: sl::DebugInstr,
    span: Span,
) -> Result<pl::GroupBlock, ProseError> {
    let exp_pl = prosify_exp(ctx, &instr_sl.exp)?;
    let instr_pl = pl::DebugInstr { exp: exp_pl };
    let instr_kind_pl = pl::InstrKind::Debug(instr_pl);
    let instr_pl = crate::annotated_note_phrase! {
        node: instr_kind_pl,
        note: None,
        span: span,
    };
    let instrs_follow_pl = prosify_group_instr(ctx, *instr_sl.instr)?;
    let mut instrs_pl = vec![instr_pl];
    instrs_pl.extend(instrs_follow_pl);
    Ok(instrs_pl)
}

// - Rule instruction

/// Converts a rule call with the callee's `prose_in` and realigned `prose_out`.
fn prosify_group_rule_instr(
    ctx: &Context,
    instr_sl: sl::RuleInstr,
    span: Span,
) -> Result<pl::GroupBlock, ProseError> {
    let mut hints = annot::Hints::default();
    if let Some(hints_rel) = ctx.hints_rel(&instr_sl.id) {
        hints.span = hints_rel.span.clone();
        hints.node.prose_in = hints_rel.node.prose_in.clone();
        // Output holes are numbered after the inputs are removed
        hints.node.prose_out = hints_rel
            .node
            .prose_out
            .as_ref()
            .map(|hint| alter::realign(hint, &instr_sl.input_hint));
        // Elaboration validates indices; structure and expansion preserve arity
        let num_args = instr_sl.not_exp.args().len();
        let num_inputs = instr_sl.input_hint.indices().len();
        let num_outputs = num_args - num_inputs;
        validate_hint_prose_in(&hints, num_inputs)?;
        validate_hint_prose_out(&hints, num_outputs)?;
    }
    let not_exp_pl = prosify_not_exp(ctx, &instr_sl.not_exp)?;
    let instr_pl = pl::RuleInstr {
        id: instr_sl.id,
        not_exp: not_exp_pl,
        input_hint: instr_sl.input_hint,
        iter_instrs: instr_sl.iter_instrs,
    };
    let tier_pl = pl::GroupInstr::Rule(instr_pl);
    let instr_pl = pl::TierInstr { tier: tier_pl };
    let instr_kind_pl = pl::InstrKind::Tier(instr_pl);
    let instr_pl = crate::annotated_note_phrase! {
        node: instr_kind_pl,
        note: None,
        span: span,
        hints: hints,
    };
    // The bound outputs are used by the instructions that follow
    let block_pl = prosify_group_block(ctx, instr_sl.block)?;
    let mut instrs_pl = vec![instr_pl];
    instrs_pl.extend(block_pl);
    Ok(instrs_pl)
}

// - Result instruction

/// Converts a result with the relation's realigned `prose_out` hint.
fn prosify_group_result_instr(
    ctx: &Context,
    instr_sl: sl::ResultInstr,
    span: Span,
) -> Result<pl::GroupBlock, ProseError> {
    let mut hints = annot::Hints::default();
    if let Some(hints_rel) = ctx.hints_rel(ctx.namespace()) {
        hints.span = hints_rel.span.clone();
        // The output template is the enclosing relation's, realigned
        hints.node.prose_out = hints_rel
            .node
            .prose_out
            .as_ref()
            .map(|hint| alter::realign(hint, &instr_sl.rel_signature.input_hint));
        validate_hint_prose_out(&hints, instr_sl.exps.len())?;
    }
    let exps_output_pl = prosify_exps(ctx, &instr_sl.exps)?;
    let instr_pl =
        pl::ResultInstr { rel_signature: instr_sl.rel_signature, exps_output: exps_output_pl };
    let tier_pl = pl::GroupInstr::Result(instr_pl);
    let instr_pl = pl::TierInstr { tier: tier_pl };
    let instr_kind_pl = pl::InstrKind::Tier(instr_pl);
    let instr_pl = crate::annotated_note_phrase! {
        node: instr_kind_pl,
        note: None,
        span: span,
        hints: hints,
    };
    Ok(vec![instr_pl])
}

// - Return instruction

/// Converts a return.
fn prosify_group_return_instr(
    ctx: &Context,
    instr_sl: sl::ReturnInstr,
    span: Span,
) -> Result<pl::GroupBlock, ProseError> {
    let exp_pl = prosify_exp(ctx, &instr_sl.exp)?;
    let instr_pl = pl::ReturnInstr { exp: exp_pl };
    let tier_pl = pl::GroupInstr::Return(instr_pl);
    let instr_pl = pl::TierInstr { tier: tier_pl };
    let instr_kind_pl = pl::InstrKind::Tier(instr_pl);
    let instr_pl = crate::annotated_note_phrase! {
        node: instr_kind_pl,
        note: None,
        span: span,
    };
    Ok(vec![instr_pl])
}

// - Block

/// Converts a block; several instructions become a backtrack with one arm each.
fn prosify_group_block(ctx: &Context, block_sl: sl::Block) -> Result<pl::GroupBlock, ProseError> {
    match block_sl.len() {
        0 => Ok(Vec::new()),
        // One instruction expands in place
        1 => {
            let instr_sl = block_sl.into_iter().next().unwrap();
            prosify_group_instr(ctx, instr_sl)
        }
        _ => {
            let span = block_sl.at();
            let mut blocks_pl = Vec::with_capacity(block_sl.len());
            for instr_sl in block_sl {
                let block_pl = prosify_group_instr(ctx, instr_sl)?;
                blocks_pl.push(block_pl);
            }
            // Alternatives become the arms of a backtrack spanning them all
            let instr_pl = pl::BacktrackInstr { blocks: blocks_pl };
            let tier_pl = pl::GroupInstr::Backtrack(instr_pl);
            let instr_pl = pl::TierInstr { tier: tier_pl };
            let instr_kind_pl = pl::InstrKind::Tier(instr_pl);
            let instr_pl = crate::annotated_note_phrase! {
                node: instr_kind_pl,
                note: None,
                span: span,
            };
            Ok(vec![instr_pl])
        }
    }
}

// == Table rows

/// Converts a table row.
fn prosify_table_row(ctx: &Context, row_sl: sl::TableRow) -> Result<pl::TableRow, ProseError> {
    let exps_input_pl = prosify_exps(ctx, &row_sl.exps_input)?;
    let exp_pl = prosify_exp(ctx, &row_sl.exp)?;
    let block_pl = prosify_group_block(ctx, row_sl.block)?;
    Ok(pl::TableRow { exps_input: exps_input_pl, exp: exp_pl, block: block_pl })
}

// == Type definitions

// - Type definition

/// Converts a type definition; types carry no hints.
fn prosify_typ_def(typdef_sl: sl::TypDef, span: Span) -> pl::Def {
    match typdef_sl {
        sl::TypDef::Extern(def_typ_sl) => prosify_extern_typ_def(def_typ_sl, span),
        sl::TypDef::Defined(def_typ_sl) => prosify_defined_typ_def(*def_typ_sl, span),
    }
}

// - External type definition

/// Converts an extern type.
fn prosify_extern_typ_def(def_typ_sl: sl::ExternTyp, span: Span) -> pl::Def {
    let def_typ_pl = pl::ExternTyp { id: def_typ_sl.id };
    let def_typ_pl = pl::TypDef::Extern(def_typ_pl);
    crate::annotated_note_phrase! {
        node: pl::DefKind::Typ(def_typ_pl),
        note: (),
        span: span,
    }
}

// - Defined type definition

/// Converts a defined type.
fn prosify_defined_typ_def(def_typ_sl: sl::DefinedTyp, span: Span) -> pl::Def {
    let def_typ_pl = pl::DefinedTyp {
        id: def_typ_sl.id,
        tparams: def_typ_sl.tparams,
        def_typ: def_typ_sl.def_typ,
    };
    let def_typ_pl = pl::TypDef::Defined(Box::new(def_typ_pl));
    crate::annotated_note_phrase! {
        node: pl::DefKind::Typ(def_typ_pl),
        note: (),
        span: span,
    }
}

// == Meta-variable definitions

/// Converts a meta-variable definition.
fn prosify_var_def(def_var_sl: sl::VarDef, span: Span) -> pl::Def {
    let def_var_pl = pl::VarDef { id: def_var_sl.id, typ: def_var_sl.typ };
    crate::annotated_note_phrase! {
        node: pl::DefKind::Var(def_var_pl),
        note: (),
        span: span,
    }
}

// == Relation definitions

// - Relation definition

/// Converts a relation definition.
fn prosify_rel_def(
    ctx: &mut Context,
    def_rel_sl: sl::RelDef,
    span: Span,
) -> Result<pl::Def, ProseError> {
    match def_rel_sl {
        sl::RelDef::Extern(def_rel_sl) => prosify_extern_rel_def(ctx, def_rel_sl, span),
        sl::RelDef::Defined(def_rel_sl) => prosify_defined_rel_def(ctx, def_rel_sl, span),
    }
}

/// Collects a relation's hints; with `prose_in`, fresh input and output
/// expressions are invented from the signature types for the summary line.
fn build_rel_hints(
    ctx: &Context,
    id_rel: &sl::Id,
    rel_signature: &sl::RelSignature,
) -> Result<annot::Hints, ProseError> {
    // Resolve the declaration before selecting templates for this signature
    let Some(hints_rel) = ctx.hints_rel(id_rel) else {
        return Ok(annot::Hints::default());
    };
    let prose_out = hints_rel
        .node
        .prose_out
        .as_ref()
        .map(|hint| alter::realign(hint, &rel_signature.input_hint));
    // Fresh expressions only when the relation has an input template
    let (prose_input_exps, prose_output_exps) = if hints_rel.node.prose_in.is_some() {
        let typs = rel_signature.not_typ.node.args().to_vec();
        let (typs_input, typs_output) = input::split(&rel_signature.input_hint, typs)
            .expect("elaboration validates relation inputs; structure preserves signature arity");
        let fresh_exps_from_typs = |typs: Vec<sl::Typ>| {
            let mut ids_used = IdSet::new();
            typs.into_iter()
                .map(|typ| {
                    let (ids_fresh, exp_sl) =
                        al::fresh::exp_from_typ(ctx.menv(), true, &ids_used, &typ);
                    ids_used = ids_fresh;
                    exp_sl
                })
                .collect::<Vec<_>>()
        };
        let exps_input_sl = Some(fresh_exps_from_typs(typs_input));
        // Output expressions only when an output template exists too
        let exps_output_sl = prose_out
            .is_some()
            .then(|| fresh_exps_from_typs(typs_output));
        (exps_input_sl, exps_output_sl)
    } else {
        (None, None)
    };
    let hints = crate::phrase! {
        node: annot::HintsKind {
            prose: hints_rel.node.prose.clone(),
            prose_in: hints_rel.node.prose_in.clone(),
            prose_out,
            prose_true: hints_rel.node.prose_true.clone(),
            prose_false: hints_rel.node.prose_false.clone(),
            prose_input_exps,
            prose_output_exps,
            prose_fields: None,
        },
        span: hints_rel.span.clone(),
    };
    // Definition titles use full notation, input, and realigned output domains
    let num_args = rel_signature.not_typ.node.args().len();
    let num_inputs = rel_signature.input_hint.indices().len();
    let num_outputs = num_args - num_inputs;
    validate_hint_prose(&hints, num_args)?;
    validate_hint_prose_in(&hints, num_inputs)?;
    validate_hint_prose_out(&hints, num_outputs)?;
    validate_hint_prose_true(&hints, num_inputs)?;
    validate_hint_prose_false(&hints, num_inputs)?;
    Ok(hints)
}

// - External relation definition

/// Converts an extern relation with its hints.
fn prosify_extern_rel_def(
    ctx: &Context,
    def_rel_sl: sl::ExternRel,
    span: Span,
) -> Result<pl::Def, ProseError> {
    let hints = build_rel_hints(ctx, &def_rel_sl.id, &def_rel_sl.rel_signature)?;
    let exps_input_pl = prosify_exps(ctx, &def_rel_sl.exps_input)?;
    let def_rel_pl = pl::ExternRel {
        id: def_rel_sl.id,
        rel_signature: def_rel_sl.rel_signature,
        exps_input: exps_input_pl,
    };
    let def_rel_pl = pl::RelDef::Extern(def_rel_pl);
    Ok(crate::annotated_note_phrase! {
        node: pl::DefKind::Rel(def_rel_pl),
        note: (),
        span: span,
        hints: hints,
    })
}

// - Defined relation definition

/// Converts a defined relation; its name becomes the namespace for its body.
fn prosify_defined_rel_def(
    ctx: &mut Context,
    def_rel_sl: sl::DefinedRel,
    span: Span,
) -> Result<pl::Def, ProseError> {
    let hints = build_rel_hints(ctx, &def_rel_sl.id, &def_rel_sl.rel_signature)?;
    // The body looks up its definition's hints by this name
    ctx.set_namespace(def_rel_sl.id.clone());
    let exps_input_pl = prosify_exps(ctx, &def_rel_sl.exps_input)?;
    let block_pl = prosify_dispatch_block(ctx, def_rel_sl.block)?;
    let block_else_opt_pl = match def_rel_sl.block_else {
        Some(block_else_sl) => {
            let block_else_pl = prosify_dispatch_block(ctx, block_else_sl)?;
            Some(block_else_pl)
        }
        None => None,
    };
    let def_rel_pl = pl::DefinedRel {
        id: def_rel_sl.id,
        rel_signature: def_rel_sl.rel_signature,
        exps_input: exps_input_pl,
        block: block_pl,
        block_else_opt: block_else_opt_pl,
    };
    let def_rel_pl = pl::RelDef::Defined(def_rel_pl);
    Ok(crate::annotated_note_phrase! {
        node: pl::DefKind::Rel(def_rel_pl),
        note: (),
        span: span,
        hints: hints,
    })
}

// == Meta-function definitions

// - Meta-function definition

/// Converts a function definition.
fn prosify_func_def(
    ctx: &mut Context,
    def_func_sl: sl::MetaFuncDef,
    span: Span,
) -> Result<pl::Def, ProseError> {
    match def_func_sl {
        sl::MetaFuncDef::Extern(def_func_sl) => prosify_extern_func_def(ctx, def_func_sl, span),
        sl::MetaFuncDef::Builtin(def_func_sl) => prosify_builtin_func_def(ctx, def_func_sl, span),
        sl::MetaFuncDef::Table(def_func_sl) => prosify_table_func_def(ctx, def_func_sl, span),
        sl::MetaFuncDef::Defined(def_func_sl) => prosify_defined_func_def(ctx, def_func_sl, span),
    }
}

/// Selects and validates a function's input and condition templates.
fn build_func_hints(
    ctx: &Context,
    id_func: &sl::Id,
    num_args: usize,
) -> Result<annot::Hints, ProseError> {
    // Resolve the declaration that owns the function templates
    let Some(hints_func) = ctx.hints_func(id_func) else {
        return Ok(annot::Hints::default());
    };
    // Function rendering consumes input and condition templates
    let hints = crate::phrase! {
        node: annot::HintsKind {
            prose_in: hints_func.node.prose_in.clone(),
            prose_true: hints_func.node.prose_true.clone(),
            prose_false: hints_func.node.prose_false.clone(),
            ..annot::HintsKind::default()
        },
        span: hints_func.span.clone(),
    };
    // Calls and definitions supply the argument count for their own use
    validate_hint_prose_in(&hints, num_args)?;
    validate_hint_prose_true(&hints, num_args)?;
    validate_hint_prose_false(&hints, num_args)?;
    Ok(hints)
}

// - External function definition

/// Converts an extern function with its hints.
fn prosify_extern_func_def(
    ctx: &Context,
    def_func_sl: sl::ExternFunc,
    span: Span,
) -> Result<pl::Def, ProseError> {
    let hints = build_func_hints(ctx, &def_func_sl.id, def_func_sl.params.len())?;
    let params_pl = prosify_params(ctx, &def_func_sl.params)?;
    let def_func_pl = pl::ExternFunc {
        id: def_func_sl.id,
        tparams: def_func_sl.tparams,
        params: params_pl,
        typ: def_func_sl.typ,
    };
    let def_func_pl = pl::MetaFuncDef::Extern(def_func_pl);
    Ok(crate::annotated_note_phrase! {
        node: pl::DefKind::MetaFunc(def_func_pl),
        note: (),
        span: span,
        hints: hints,
    })
}

// - Builtin function definition

/// Converts a builtin function with its hints.
fn prosify_builtin_func_def(
    ctx: &Context,
    def_func_sl: sl::BuiltinFunc,
    span: Span,
) -> Result<pl::Def, ProseError> {
    let hints = build_func_hints(ctx, &def_func_sl.id, def_func_sl.params.len())?;
    let params_pl = prosify_params(ctx, &def_func_sl.params)?;
    let def_func_pl = pl::BuiltinFunc {
        id: def_func_sl.id,
        tparams: def_func_sl.tparams,
        params: params_pl,
        typ: def_func_sl.typ,
    };
    let def_func_pl = pl::MetaFuncDef::Builtin(def_func_pl);
    Ok(crate::annotated_note_phrase! {
        node: pl::DefKind::MetaFunc(def_func_pl),
        note: (),
        span: span,
        hints: hints,
    })
}

// - Table function definition

/// Converts a table function; its name becomes the namespace for the rows.
fn prosify_table_func_def(
    ctx: &mut Context,
    def_func_sl: sl::TableFunc,
    span: Span,
) -> Result<pl::Def, ProseError> {
    let hints = build_func_hints(ctx, &def_func_sl.id, def_func_sl.params.len())?;
    // The body looks up its definition's hints by this name
    ctx.set_namespace(def_func_sl.id.clone());
    let params_pl = prosify_params(ctx, &def_func_sl.params)?;
    let mut rows_pl = Vec::with_capacity(def_func_sl.table_rows.len());
    for row_sl in def_func_sl.table_rows {
        let row_pl = prosify_table_row(ctx, row_sl)?;
        rows_pl.push(row_pl);
    }
    let def_func_pl = pl::TableFunc {
        id: def_func_sl.id,
        params: params_pl,
        typ: def_func_sl.typ,
        rows: rows_pl,
    };
    let def_func_pl = pl::MetaFuncDef::Table(def_func_pl);
    Ok(crate::annotated_note_phrase! {
        node: pl::DefKind::MetaFunc(def_func_pl),
        note: (),
        span: span,
        hints: hints,
    })
}

// - Defined function definition

/// Converts a defined function; its name becomes the namespace for its body.
fn prosify_defined_func_def(
    ctx: &mut Context,
    def_func_sl: sl::DefinedFunc,
    span: Span,
) -> Result<pl::Def, ProseError> {
    let hints = build_func_hints(ctx, &def_func_sl.id, def_func_sl.params.len())?;
    // The body looks up its definition's hints by this name
    ctx.set_namespace(def_func_sl.id.clone());
    let params_pl = prosify_params(ctx, &def_func_sl.params)?;
    let block_pl = prosify_group_block(ctx, def_func_sl.block)?;
    let block_else_opt_pl = match def_func_sl.block_else {
        Some(block_else_sl) => {
            let block_else_pl = prosify_group_block(ctx, block_else_sl)?;
            Some(block_else_pl)
        }
        None => None,
    };
    let def_func_pl = pl::DefinedFunc {
        id: def_func_sl.id,
        tparams: def_func_sl.tparams,
        params: params_pl,
        typ: def_func_sl.typ,
        block: block_pl,
        block_else_opt: block_else_opt_pl,
    };
    let def_func_pl = pl::MetaFuncDef::Defined(def_func_pl);
    Ok(crate::annotated_note_phrase! {
        node: pl::DefKind::MetaFunc(def_func_pl),
        note: (),
        span: span,
        hints: hints,
    })
}

// == Definitions

// - Definition

/// Converts one definition.
fn prosify_def(ctx: &mut Context, def_sl: sl::Def) -> Result<pl::Def, ProseError> {
    match def_sl.node {
        sl::DefKind::Typ(def_typ_sl) => Ok(prosify_typ_def(def_typ_sl, def_sl.span)),
        sl::DefKind::Var(def_var_sl) => Ok(prosify_var_def(def_var_sl, def_sl.span)),
        sl::DefKind::Rel(def_rel_sl) => prosify_rel_def(ctx, def_rel_sl, def_sl.span),
        sl::DefKind::MetaFunc(def_func_sl) => prosify_func_def(ctx, def_func_sl, def_sl.span),
    }
}

// == Entry point

/// The whole conversion: load hints, expand calls, convert, shorten, stamp.
pub(super) fn prosify_spec(spec_sl: sl::Spec) -> Result<pl::Spec, ProseError> {
    let mut ctx = Context::load(&spec_sl)?;
    let spec_sl = super::expand::expand_spec(spec_sl);
    let mut spec_pl = Vec::with_capacity(spec_sl.len());
    for def_sl in spec_sl {
        let def_pl = prosify_def(&mut ctx, def_sl)?;
        spec_pl.push(def_pl);
    }
    let spec_pl = super::shorthand::shorten_spec(spec_pl);
    let spec_pl = super::stamp::stamp_spec(spec_pl);
    Ok(spec_pl)
}
