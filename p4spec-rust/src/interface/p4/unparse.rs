//! Rendering of runtime values back to P4 surface syntax
//!
//! The unparser collects `print` hints from one specification,
//! recursively renders each runtime value,
//! and applies a matching hint before falling back to its mixfix shape.
//! For example, a case carrying an infix `+` hint renders its two arguments
//! as `left + right`.

use std::collections::HashMap;

use crate::{
    lang::data::value::{Value, ValueArena, ValueCase, ValueKind},
    lang::{
        al,
        common::notation::{atom::Atom, mixfix::Mixfix, mixop::Mixop},
        common::prim::num::Number,
        hints::alter::{self, AlterHint, Renderer},
        il::ast::{DefTypKind, Hint, TypCase, TypKind},
        pl, sl,
        traits::print::Print,
    },
    util::text::escape_text,
};

use super::error::P4UnparseError;

// == Unparser and errors

/// Renders values using the specification's print hints.
#[derive(Clone, Debug, Default)]
pub struct P4Unparser {
    /// Print hints by type name and case shape.
    hints: HashMap<(String, Mixop), AlterHint>,
}

/// Records the print hint of each case of a variant type.
fn insert_case_hints(
    hints: &mut HashMap<(String, Mixop), AlterHint>,
    type_id: &str,
    def_typ: &crate::lang::il::ast::DefTyp,
) {
    // Only variant cases carry print hints
    let DefTypKind::Variant(cases) = &def_typ.node else {
        return;
    };
    for TypCase { not_typ, hints: hints_case, .. } in cases {
        // Cases without a print hint fall back to their shape
        let Some(Hint { exp, .. }) = hints_case.iter().find(|hint| hint.id.node == "print") else {
            continue;
        };
        let hint = alter::init(exp);
        hints.insert((type_id.to_owned(), not_typ.node.to_mixop()), hint);
    }
}

impl P4Unparser {
    // - Construction

    /// Collects print hints from an AL specification.
    pub fn from_al_spec(spec_al: &[al::ast::Def]) -> Self {
        let mut hints = HashMap::new();
        // Only defined types carry cases with print hints
        for definition_al in spec_al {
            let al::ast::DefKind::Typ(typ_def_al) = &definition_al.node else {
                continue;
            };
            let al::ast::TypDef::Defined(defined_typ_al) = typ_def_al else {
                continue;
            };
            insert_case_hints(&mut hints, &defined_typ_al.id.node, &defined_typ_al.def_typ);
        }
        Self { hints }
    }

    /// Collects print hints from an SL specification.
    pub fn from_sl_spec(spec_sl: &[sl::ast::Def]) -> Self {
        let mut hints = HashMap::new();
        // Only defined types carry cases with print hints
        for definition_sl in spec_sl {
            let sl::ast::DefKind::Typ(typ_def_sl) = &definition_sl.node else {
                continue;
            };
            let sl::ast::TypDef::Defined(defined_typ_sl) = typ_def_sl else {
                continue;
            };
            insert_case_hints(&mut hints, &defined_typ_sl.id.node, &defined_typ_sl.def_typ);
        }
        Self { hints }
    }

    /// Collects print hints from a PL specification.
    pub fn from_pl_spec(spec_pl: &[pl::ast::Def]) -> Self {
        let mut hints = HashMap::new();
        for def_pl in spec_pl {
            let pl::ast::DefKind::Typ(typdef_pl) = &def_pl.node.node else {
                continue;
            };
            let pl::ast::TypDef::Defined(typdef_pl) = typdef_pl else {
                continue;
            };
            insert_case_hints(&mut hints, &typdef_pl.id.node, &typdef_pl.def_typ);
        }
        Self { hints }
    }

    // - Rendering

    /// Renders a value as P4 text.
    pub fn render(&self, arena: &ValueArena, value: &Value) -> Result<String, P4UnparseError> {
        match arena.kind(value) {
            // Primitives print as themselves
            ValueKind::Bool(value) => Ok(value.to_string()),
            ValueKind::Num(Number::Nat(value)) => Ok(value.to_string()),
            ValueKind::Num(Number::Int(value)) => Ok(value.to_string()),
            ValueKind::Text(value) => Ok(escape_text(value)),
            // Structs have no P4 spelling
            ValueKind::Struct(_) => Err(P4UnparseError::UnsupportedValue("Struct")),
            // Cases go through their hint or shape
            ValueKind::Case(value_case) => self.render_case(arena, arena.typ(value), value_case),
            // Tuples in parentheses, comma separated
            ValueKind::Tuple(values) => {
                let rendered = self.render_values(arena, values, ", ")?;
                Ok(format!("({rendered})"))
            }
            // An option is its content or nothing
            ValueKind::Opt(Some(value)) => self.render(arena, value),
            ValueKind::Opt(None) => Ok(String::new()),
            // Lists are space separated
            ValueKind::List(values) => self.render_values(arena, values, " "),
            // Functions and externs have no P4 spelling
            ValueKind::Func(_) => Err(P4UnparseError::UnsupportedValue("Func")),
            ValueKind::Extern(_) => Err(P4UnparseError::UnsupportedValue("Extern")),
        }
    }

    /// Renders a case by its print hint when the type has one, else by shape.
    fn render_case(
        &self,
        arena: &ValueArena,
        typ: &TypKind,
        value_case: &ValueCase,
    ) -> Result<String, P4UnparseError> {
        let (mixop, values) = value_case.split();
        if let TypKind::Var(type_id, _) = typ
            && let Some(hint) = self.hints.get(&(type_id.node.clone(), mixop))
        {
            return self.render_hint(arena, hint, &values);
        }
        let mut rendered = Vec::new();
        self.render_mixfix(arena, value_case, &mut rendered)?;
        Ok(rendered.join(" "))
    }

    /// Renders the case arguments through a print-hint template.
    fn render_hint(
        &self,
        arena: &ValueArena,
        hint: &AlterHint,
        values: &[&Value],
    ) -> Result<String, P4UnparseError> {
        let rendered = alter::alternate(hint, values, &ValueRenderer(self, arena));
        match rendered {
            Ok(rendered) => rendered,
            Err(error) => Err(error.into()),
        }
    }

    /// Renders values joined by a separator.
    fn render_values(
        &self,
        arena: &ValueArena,
        values: &[Value],
        separator: &str,
    ) -> Result<String, P4UnparseError> {
        let rendered = values
            .iter()
            .map(|value| self.render(arena, value))
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
        arena: &ValueArena,
        mixfix: &ValueCase,
        rendered: &mut Vec<String>,
    ) -> Result<(), P4UnparseError> {
        match mixfix {
            // Arguments render recursively
            Mixfix::Arg(value) => {
                let value = self.render(arena, value)?;
                rendered.push(value);
            }
            // Silent atoms are dropped rather than left as empty pieces
            Mixfix::Atom(atom) => {
                let rendered_atom = Self::render_atom(&atom.node);
                if !rendered_atom.is_empty() {
                    rendered.push(rendered_atom);
                }
            }
            // Brackets around the inner form
            Mixfix::Brack(atom_l, mixfix, atom_r) => {
                let rendered_atom_l = Self::render_atom(&atom_l.node);
                if !rendered_atom_l.is_empty() {
                    rendered.push(rendered_atom_l);
                }
                self.render_mixfix(arena, mixfix, rendered)?;
                let rendered_atom_r = Self::render_atom(&atom_r.node);
                if !rendered_atom_r.is_empty() {
                    rendered.push(rendered_atom_r);
                }
            }
            // Left, operator, right
            Mixfix::Infix(mixfix_l, atom, mixfix_r) => {
                self.render_mixfix(arena, mixfix_l, rendered)?;
                let rendered_atom = Self::render_atom(&atom.node);
                if !rendered_atom.is_empty() {
                    rendered.push(rendered_atom);
                }
                self.render_mixfix(arena, mixfix_r, rendered)?;
            }
            // Pieces in order
            Mixfix::Seq(mixfixes) => {
                for mixfix in mixfixes {
                    self.render_mixfix(arena, mixfix, rendered)?;
                }
            }
        }
        Ok(())
    }
}

// == Print-hint rendering

/// The print-hint renderer producing P4 text.
struct ValueRenderer<'a>(&'a P4Unparser, &'a ValueArena);

impl Renderer<&Value> for ValueRenderer<'_> {
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

    fn item(&self, item: &&Value) -> Self::Output {
        self.0.render(self.1, item)
    }
}
