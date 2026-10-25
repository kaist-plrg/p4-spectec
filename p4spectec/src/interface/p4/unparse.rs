//! Rendering of runtime values back to P4 surface syntax
//!
//! The unparser collects each variant case of one specification,
//! with its declared argument types and `print` hint,
//! and renders a value against the type `print_` was instantiated at.
//! A case renders through its hint when it has one, else by its mixfix shape,
//! and its arguments render against the case's declared argument types.
//! For example, `"c"` rendered as an `annotationToken` finds the
//! `stringLiteral` case `'"' text '"'` and its fused hint, giving `"c"`.
//! Value type notes are never read, so a value without one renders the same.

use std::collections::HashMap;

use crate::util::text::escape_text;

use crate::lang::{
    common::{iter::Iter, notation::atom::Atom, prim::num::Number},
    data::{
        arena::Arena,
        notation::{items::Item, tree::Mixop},
        value::flat::{Value, ValueCase, ValueKind},
    },
    hints::alter::{self, AlterHint, Renderer},
    traits::print::Print,
};

use crate::lang::il::{
    self,
    ast::{DefTypKind, Hint, TypCase, TypKind},
};

use crate::lang::al;

use crate::lang::sl;

use crate::lang::pl;

use crate::runtime::{
    envs::elab::TDEnv,
    ops::typ::{Theta, TypeArityMismatch, TypeError, TypeErrorKind, expand_typ, subst_typs},
    typdef::TypeDef,
};

use super::error::P4UnparseError;

// == Unparser and errors

/// Renders values using the specification's case types and print hints.
#[derive(Clone, Debug, Default)]
pub struct P4Unparser {
    /// Type definitions, for aliases and type parameters.
    tdenv: TDEnv,
    /// Variant cases by type name and case shape.
    cases: HashMap<(String, Mixop), CaseTyp>,
}

/// A variant case: its declared argument types and optional print hint.
#[derive(Clone, Debug)]
struct CaseTyp {
    typs_arg: Vec<il::ast::Typ>,
    hint: Option<AlterHint>,
}

impl P4Unparser {
    // - Construction

    /// Collects case types and print hints from an IL specification.
    pub fn from_il_spec(spec_il: &[il::ast::Def]) -> Self {
        let mut unparser = Self::default();
        for def_il in spec_il {
            let il::ast::DefKind::Typ(typdef_il) = &def_il.node else {
                continue;
            };
            match typdef_il {
                // Extern types are opaque
                il::ast::TypDef::Extern(extern_typ_il) => {
                    unparser.insert_extern_typ(&extern_typ_il.id)
                }
                // Defined types record their parameters, body, and cases
                il::ast::TypDef::Defined(defined_typ_il) => unparser.insert_defined_typ(
                    &defined_typ_il.id,
                    &defined_typ_il.tparams,
                    &defined_typ_il.def_typ,
                ),
            }
        }
        unparser
    }

    /// Collects case types and print hints from an AL specification.
    pub fn from_al_spec(spec_al: &[al::ast::Def]) -> Self {
        let mut unparser = Self::default();
        for def_al in spec_al {
            let al::ast::DefKind::Typ(typdef_al) = &def_al.node else {
                continue;
            };
            match typdef_al {
                // Extern types are opaque
                al::ast::TypDef::Extern(extern_typ_al) => {
                    unparser.insert_extern_typ(&extern_typ_al.id)
                }
                // Defined types record their parameters, body, and cases
                al::ast::TypDef::Defined(defined_typ_al) => unparser.insert_defined_typ(
                    &defined_typ_al.id,
                    &defined_typ_al.tparams,
                    &defined_typ_al.def_typ,
                ),
            }
        }
        unparser
    }

    /// Collects case types and print hints from an SL specification.
    pub fn from_sl_spec(spec_sl: &[sl::ast::Def]) -> Self {
        let mut unparser = Self::default();
        for def_sl in spec_sl {
            let sl::ast::DefKind::Typ(typdef_sl) = &def_sl.node else {
                continue;
            };
            match typdef_sl {
                // Extern types are opaque
                sl::ast::TypDef::Extern(extern_typ_sl) => {
                    unparser.insert_extern_typ(&extern_typ_sl.id)
                }
                // Defined types record their parameters, body, and cases
                sl::ast::TypDef::Defined(defined_typ_sl) => unparser.insert_defined_typ(
                    &defined_typ_sl.id,
                    &defined_typ_sl.tparams,
                    &defined_typ_sl.def_typ,
                ),
            }
        }
        unparser
    }

    /// Collects case types and print hints from a PL specification.
    pub fn from_pl_spec(spec_pl: &[pl::ast::Def]) -> Self {
        let mut unparser = Self::default();
        for def_pl in spec_pl {
            let pl::ast::DefKind::Typ(typdef_pl) = &def_pl.node.node else {
                continue;
            };
            match typdef_pl {
                // Extern types are opaque
                pl::ast::TypDef::Extern(extern_typ_pl) => {
                    unparser.insert_extern_typ(&extern_typ_pl.id)
                }
                // Defined types record their parameters, body, and cases
                pl::ast::TypDef::Defined(defined_typ_pl) => unparser.insert_defined_typ(
                    &defined_typ_pl.id,
                    &defined_typ_pl.tparams,
                    &defined_typ_pl.def_typ,
                ),
            }
        }
        unparser
    }

    /// Records an extern type, which has no cases to render.
    fn insert_extern_typ(&mut self, id: &il::ast::Id) {
        self.tdenv.insert(id.clone(), TypeDef::Extern);
    }

    /// Records a defined type and, for a variant, each case with its hint.
    fn insert_defined_typ(
        &mut self,
        id: &il::ast::Id,
        tparams: &[il::ast::TParam],
        def_typ: &il::ast::DefTyp,
    ) {
        let typdef = TypeDef::Defined(tparams.to_vec(), Box::new(def_typ.clone()));
        self.tdenv.insert(id.clone(), typdef);
        // Only variant cases have shapes to render
        let DefTypKind::Variant(cases) = &def_typ.node else {
            return;
        };
        for TypCase { not_typ, hints, .. } in cases {
            // Cases without a print hint render by their shape
            let hint = hints
                .iter()
                .find(|hint| hint.id.node.as_ref() == "print")
                .map(|Hint { exp, .. }| alter::init(exp));
            let case_typ = CaseTyp { typs_arg: not_typ.node.args().to_vec(), hint };
            let mixop = not_typ.node.mixop().as_ref().clone();
            self.cases.insert((id.node.to_string(), mixop), case_typ);
        }
    }

    // - Rendering

    /// Renders a value of the declared type `typ` as P4 text.
    pub fn render(
        &self,
        arena: &Arena,
        typ: &il::ast::Typ,
        value: &Value,
    ) -> Result<String, P4UnparseError> {
        // Aliases unfold before the type is matched against the value
        let typ = expand_typ(&self.tdenv, typ)?;
        match (arena.value.kind(value.node), &typ.node) {
            // Primitives print as themselves
            (ValueKind::Bool(value), _) => Ok(value.to_string()),
            (ValueKind::Num(Number::Nat(value)), _) => Ok(value.to_string()),
            (ValueKind::Num(Number::Int(value)), _) => Ok(value.to_string()),
            (ValueKind::Text(value), _) => Ok(escape_text(value)),
            // Structs have no P4 spelling
            (ValueKind::Struct(_), _) => Err(P4UnparseError::ValueUnsupported("Struct")),
            // Cases go through their declared case
            (ValueKind::Case(value_case), TypKind::Var(id, targs)) => {
                self.render_case(arena, id, targs, value_case)
            }
            // Tuples in parentheses, comma separated, each against its type
            (ValueKind::Tuple(values), TypKind::Tuple(typs)) if values.len() == typs.len() => {
                let rendered = self.render_values(arena, typs, values, ", ")?;
                Ok(format!("({rendered})"))
            }
            // An option is its content or nothing
            (ValueKind::Opt(Some(value)), TypKind::Iter(typ_elem, Iter::Opt)) => {
                self.render(arena, typ_elem, value)
            }
            (ValueKind::Opt(None), TypKind::Iter(_, Iter::Opt)) => Ok(String::new()),
            // Lists are space separated, each against the element type
            (ValueKind::List(values), TypKind::Iter(typ_elem, Iter::List)) => {
                let rendered = values
                    .iter()
                    .map(|value| self.render(arena, typ_elem, value))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(rendered.join(" "))
            }
            // Functions and externs have no P4 spelling
            (ValueKind::Func(_), _) => Err(P4UnparseError::ValueUnsupported("Func")),
            (ValueKind::Extern(_), _) => Err(P4UnparseError::ValueUnsupported("Extern")),
            // Any other pairing is a value outside its declared type
            _ => Err(P4UnparseError::TypeMismatch(Print::to_string(typ.as_ref()))),
        }
    }

    /// Renders a case of `id<targs>` by its print hint, else by its shape.
    fn render_case(
        &self,
        arena: &Arena,
        id: &il::ast::Id,
        targs: &[il::ast::Typ],
        value_case: &ValueCase,
    ) -> Result<String, P4UnparseError> {
        // Find the declared case of this shape
        let mixop = value_case.mixop().into_tree(&arena.mixop);
        let Some(case_typ) = self.cases.get(&(id.node.to_string(), mixop.clone())) else {
            return Err(P4UnparseError::CaseUndefined {
                typ: id.node.to_string(),
                mixop: Print::to_string(&mixop),
            });
        };
        // Instantiate the argument types at the type arguments
        let tparams = self.tdenv.get(id).map(TypeDef::tparams).unwrap_or_default();
        let theta = Theta::from_lists(tparams, targs).map_err(|arity_mismatch| {
            let arity_mismatch = TypeArityMismatch::TypeArgument(arity_mismatch);
            TypeError::new(TypeErrorKind::ArityMismatch(arity_mismatch), id.span)
        })?;
        let typs_arg = subst_typs(&|id| theta.get(id), &case_typ.typs_arg)?;
        // Render through the hint, or by shape without one
        match &case_typ.hint {
            Some(hint) => self.render_hint(arena, hint, &typs_arg, value_case.args()),
            None => {
                let mut rendered = Vec::new();
                self.render_mixfix(arena, &typs_arg, value_case, &mut rendered)?;
                Ok(rendered.join(" "))
            }
        }
    }

    /// Renders the case arguments through a print-hint template.
    fn render_hint(
        &self,
        arena: &Arena,
        hint: &AlterHint,
        typs: &[il::ast::Typ],
        values: &[Value],
    ) -> Result<String, P4UnparseError> {
        let items = typs.iter().zip(values).collect::<Vec<_>>();
        let rendered = alter::alternate(hint, &items, &ValueRenderer(self, arena));
        match rendered {
            Ok(rendered) => rendered,
            Err(error) => Err(error.into()),
        }
    }

    /// Renders values against their types, joined by a separator.
    fn render_values(
        &self,
        arena: &Arena,
        typs: &[il::ast::Typ],
        values: &[Value],
        separator: &str,
    ) -> Result<String, P4UnparseError> {
        let rendered = typs
            .iter()
            .zip(values)
            .map(|(typ, value)| self.render(arena, typ, value))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(rendered.join(separator))
    }

    /// The P4 spelling of an atom: tags vanish, the rest lowercase or bare.
    fn render_atom(atom: &Atom) -> String {
        match atom {
            // Tags are silent; keywords lowercase; brackets print bare
            Atom::Tag(_) => String::new(),
            Atom::Operator(op) => op.to_ascii_lowercase(),
            Atom::LAngle => "<".to_owned(),
            Atom::RAngle => ">".to_owned(),
            Atom::LParen => "(".to_owned(),
            Atom::RParen => ")".to_owned(),
            Atom::LBrack => "[".to_owned(),
            Atom::RBrack => "]".to_owned(),
            Atom::LBrace => "{".to_owned(),
            Atom::RBrace => "}".to_owned(),
            atom => Print::to_string(atom).to_ascii_lowercase(),
        }
    }

    /// Renders a case by its shape, skipping atoms that print as nothing.
    fn render_mixfix(
        &self,
        arena: &Arena,
        typs_arg: &[il::ast::Typ],
        value_case: &ValueCase,
        rendered: &mut Vec<String>,
    ) -> Result<(), P4UnparseError> {
        // Traversal items in reading order: atoms, and arguments by position
        let arena_mixop = &arena.mixop;
        let mut visit_items = Vec::new();
        value_case.mixop().visit(arena_mixop, |visit_item| {
            visit_items.push(visit_item);
        });
        for visit_item in visit_items {
            match visit_item {
                // Arguments render recursively against their declared types
                Item::Arg(pos) => {
                    rendered.push(self.render(arena, &typs_arg[pos], &value_case.args()[pos])?)
                }
                // Silent atoms are dropped rather than left as empty pieces
                Item::Atom(atom) => {
                    let rendered_atom = Self::render_atom(&atom.node);
                    if !rendered_atom.is_empty() {
                        rendered.push(rendered_atom);
                    }
                }
            }
        }
        Ok(())
    }
}

// == Print-hint rendering

/// The print-hint renderer producing P4 text.
struct ValueRenderer<'a>(&'a P4Unparser, &'a Arena);

impl Renderer<(&il::ast::Typ, &Value)> for ValueRenderer<'_> {
    type Output = Result<String, P4UnparseError>;

    fn empty(&self) -> Self::Output {
        Ok(String::new())
    }

    fn text(&self, text: &str) -> Option<Self::Output> {
        (!text.is_empty()).then(|| Ok(text.to_owned()))
    }

    fn atom(&self, atom: &crate::lang::el::ast::Atom) -> Self::Output {
        Ok(P4Unparser::render_atom(&atom.node))
    }

    // Empty pieces leave no double spaces
    fn join(&self, items: Vec<Self::Output>) -> Self::Output {
        let items = items.into_iter().collect::<Result<Vec<_>, _>>()?;
        Ok(items
            .into_iter()
            .filter(|item| !item.is_empty())
            .collect::<Vec<_>>()
            .join(" "))
    }

    fn fuse(&self, output_l: Self::Output, output_r: Self::Output) -> Self::Output {
        let output_l = output_l?;
        let output_r = output_r?;
        Ok(output_l + &output_r)
    }

    fn other(&self, exp: &crate::lang::el::ast::Exp) -> Self::Output {
        Ok(Print::to_string(exp))
    }

    fn item(&self, (typ, value): &(&il::ast::Typ, &Value)) -> Self::Output {
        self.0.render(self.1, typ, value)
    }
}
