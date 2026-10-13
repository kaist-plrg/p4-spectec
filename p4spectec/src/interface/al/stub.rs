//! Placeholders for information omitted from the AL meta representation

use std::rc::Rc;

use super::Error;
use crate::{
    lang::{
        common::{Id, notation::atom::Atom, source::Span},
        data::{
            notation::tree::{Mixfix, Mixop},
            typ::make as make_typ,
        },
        hints::input::InputHint,
        il::ast as il,
    },
    phrase,
};

// == Helpers

fn relation_notation<T>(
    args_input: Vec<T>,
    args_output: Vec<T>,
) -> Result<(Mixfix<T>, InputHint), Error> {
    let span = Span::default();
    let input_hint = InputHint::new(
        (0..args_input.len())
            .map(|pos| phrase!(node: pos, span: span))
            .collect(),
    );
    let mixop_input = Mixop::Seq(vec![Mixop::Arg; args_input.len()]);
    let mixop_output = Mixop::Seq(vec![Mixop::Arg; args_output.len()]);
    let atom_arrow = phrase!(node: Atom::Operator("->".into()), span: span);
    let mixop = Rc::new(Mixop::Infix(Box::new(mixop_input), atom_arrow, Box::new(mixop_output)));
    let args_notation = crate::lang::hints::input::combine(&input_hint, args_input, args_output)?;
    Ok((Mixfix::new(mixop, args_notation)?, input_hint))
}

// == Stubs

/// The origin omitted from a variant case.
pub(super) fn typ_origin() -> il::TypOrigin {
    let span = Span::default();
    phrase!(node: il::TypOriginKind {
        id: phrase!(node: "".into(), span: span),
        targs: vec![],
    }, span: span)
}

/// The signature omitted from a runtime function value.
pub(super) fn func_typ_params() -> (Vec<Id>, Vec<il::Typ>, il::Typ) {
    (vec![], vec![], make_typ::bool())
}

/// A relation type notation and its canonical input positions.
pub(super) fn not_typ(
    typs_input: Vec<il::Typ>,
    typs_output: Vec<il::Typ>,
) -> Result<(il::NotTyp, InputHint), Error> {
    let (notation, input_hint) = relation_notation(typs_input, typs_output)?;
    Ok((phrase!(node: notation, span: Span::default()), input_hint))
}

/// A relation expression notation and its canonical input positions.
pub(super) fn not_exp(
    exps_input: Vec<il::Exp>,
    exps_output: Vec<il::Exp>,
) -> Result<(il::NotExp, InputHint), Error> {
    relation_notation(exps_input, exps_output)
}

/// The type note omitted from expressions and paths.
pub(super) fn exp_note() -> Rc<il::TypKind> {
    Rc::new(il::TypKind::Bool)
}

/// The subtype plan omitted from a subtype expression.
pub(super) fn subcheck(typ: il::Typ) -> il::Subcheck {
    il::Subcheck::Recurse(typ)
}

/// The general hints omitted from meta definitions.
pub(super) fn hints() -> Vec<il::Hint> {
    vec![]
}

/// The written signature omitted from a rule match.
pub(super) fn rule_match_signature(exps_input: &[il::Exp]) -> Vec<il::Exp> {
    exps_input.to_vec()
}

/// The written signature omitted from a table row.
pub(super) fn table_row_signature() -> Vec<il::Exp> {
    vec![]
}

/// The recursive value type omitted from runtime payloads.
pub(super) fn value_typ() -> il::Typ {
    make_typ::var(phrase!(node: "val".into(), span: Span::default()), vec![])
}

/// The external JSON type omitted from runtime payloads.
pub(super) fn json_typ() -> il::Typ {
    make_typ::var(phrase!(node: "json".into(), span: Span::default()), vec![])
}
