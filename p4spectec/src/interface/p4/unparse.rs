//! Rendering of runtime values back to P4 surface syntax
//!
//! The unparser collects `print` hints from one specification,
//! recursively renders each runtime value,
//! and applies a matching hint before falling back to its mixfix shape.
//! For example, a case carrying an infix `+` hint renders its two arguments
//! as `left + right`.

use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use crate::util::text::escape_text;
use foldhash::fast::RandomState;
use hashbrown::HashMap;

use crate::lang::{
    common::{notation::atom::Atom, prim::num::Number},
    data::{
        arena::Arena,
        intern::Interned,
        notation::{items::Item, tree::Mixop},
        value::flat::{Value, ValueCase, ValueKind},
    },
    hints::alter::{self, AlterHint, Renderer},
    traits::print::Print,
};

use crate::lang::il::ast::{DefTypKind, Hint, TypCase, TypKind};

use crate::lang::al;

use crate::lang::sl;

use crate::lang::pl;

use super::error::P4UnparseError;

// == Unparser and errors

/// Renders values using the specification's print hints.
#[derive(Clone, Debug, Default)]
pub struct P4Unparser {
    /// Print hints by type name and case shape.
    hints: HashMap<(String, Mixop), Rc<AlterHint>, RandomState>,
    rendered: Rc<RefCell<RenderCache>>,
}

/// Longest rendering kept, so prefix-shaped values do not retain every subtree.
const RENDER_TEXT_LEN_MAX: usize = 4096;
/// Total rendered text kept for one value generation.
const RENDER_BYTES_MAX: usize = 4 * 1024 * 1024;
/// Renderings, and separately case hints, kept for one value generation.
const RENDER_ENTRIES_MAX: usize = 16384;

/// A case shape under its type annotation.
type HintKey = (Interned<Rc<TypKind>>, crate::lang::data::notation::flat::Mixop);
/// A body under its type annotation.
type RenderKey = (Interned<ValueKind>, Interned<Rc<TypKind>>);

/// Rendering depends on the complete body and its type, but not its root span.
#[derive(Debug, Default)]
struct RenderCache {
    generation: Weak<()>,
    bytes: usize,
    hints: HashMap<HintKey, Option<Rc<AlterHint>>, RandomState>,
    values: HashMap<RenderKey, String, RandomState>,
}

/// Records the print hint of each case of a variant type.
fn insert_case_hints(
    hints: &mut HashMap<(String, Mixop), Rc<AlterHint>, RandomState>,
    type_id: &str,
    def_typ: &crate::lang::il::ast::DefTyp,
) {
    // Only variant cases carry print hints
    let DefTypKind::Variant(cases) = &def_typ.node else {
        return;
    };
    for TypCase { not_typ, hints: hints_case, .. } in cases {
        // Cases without a print hint fall back to their shape
        let Some(Hint { exp, .. }) = hints_case
            .iter()
            .find(|hint| hint.id.node.as_ref() == "print")
        else {
            continue;
        };
        let hint = alter::init(exp);
        hints.insert((type_id.to_owned(), not_typ.node.mixop().as_ref().clone()), Rc::new(hint));
    }
}

impl P4Unparser {
    // - Construction

    /// Collects print hints from an AL specification.
    pub fn from_al_spec(spec_al: &[al::ast::Def]) -> Self {
        let mut hints = HashMap::default();
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
        Self { hints, ..Self::default() }
    }

    /// Collects print hints from an SL specification.
    pub fn from_sl_spec(spec_sl: &[sl::ast::Def]) -> Self {
        let mut hints = HashMap::default();
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
        Self { hints, ..Self::default() }
    }

    /// Collects print hints from a PL specification.
    pub fn from_pl_spec(spec_pl: &[pl::ast::Def]) -> Self {
        let mut hints = HashMap::default();
        for def_pl in spec_pl {
            let pl::ast::DefKind::Typ(typdef_pl) = &def_pl.node.node else {
                continue;
            };
            let pl::ast::TypDef::Defined(typdef_pl) = typdef_pl else {
                continue;
            };
            insert_case_hints(&mut hints, &typdef_pl.id.node, &typdef_pl.def_typ);
        }
        Self { hints, ..Self::default() }
    }

    // - Rendering

    /// Renders a value as P4 text.
    pub fn render(&self, arena: &Arena, value: &Value) -> Result<String, P4UnparseError> {
        if !matches!(
            arena.value.kind(value.node),
            ValueKind::Case(_) | ValueKind::Tuple(_) | ValueKind::Opt(_) | ValueKind::List(_)
        ) {
            return self.render_uncached(arena, value);
        }
        let key = (value.node, value.note);
        {
            let mut rendered = self.rendered.borrow_mut();
            if rendered.generation.as_ptr() != Rc::as_ptr(arena.value.generation()) {
                rendered.values.clear();
                rendered.hints.clear();
                rendered.bytes = 0;
                rendered.generation = Rc::downgrade(arena.value.generation());
            }
            if let Some(text) = rendered.values.get(&key) {
                return Ok(text.clone());
            }
        }
        let text = self.render_uncached(arena, value)?;
        let mut rendered = self.rendered.borrow_mut();
        // Prefix-shaped values must not retain every increasingly large subtree.
        if text.len() <= RENDER_TEXT_LEN_MAX
            && rendered.bytes + text.len() <= RENDER_BYTES_MAX
            && rendered.values.len() < RENDER_ENTRIES_MAX
        {
            rendered.bytes += text.len();
            rendered.values.insert(key, text.clone());
        }
        Ok(text)
    }

    fn render_uncached(&self, arena: &Arena, value: &Value) -> Result<String, P4UnparseError> {
        match arena.value.kind(value.node) {
            // Primitives print as themselves
            ValueKind::Bool(value) => Ok(value.to_string()),
            ValueKind::Num(Number::Nat(value)) => Ok(value.to_string()),
            ValueKind::Num(Number::Int(value)) => Ok(value.to_string()),
            ValueKind::Text(value) => Ok(escape_text(value)),
            // Structs have no P4 spelling
            ValueKind::Struct(_) => Err(P4UnparseError::ValueUnsupported("Struct")),
            // Cases go through their hint or shape
            ValueKind::Case(value_case) => self.render_case(arena, value.note, value_case),
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
            ValueKind::Func(_) => Err(P4UnparseError::ValueUnsupported("Func")),
            ValueKind::Extern(_) => Err(P4UnparseError::ValueUnsupported("Extern")),
        }
    }

    /// Renders a case by its print hint when the type has one, else by shape.
    fn render_case(
        &self,
        arena: &Arena,
        note: Interned<Rc<TypKind>>,
        value_case: &ValueCase,
    ) -> Result<String, P4UnparseError> {
        let key = (note, *value_case.mixop());
        let cached = self.rendered.borrow().hints.get(&key).cloned();
        let hint = cached.unwrap_or_else(|| {
            let hint = if let TypKind::Var(type_id, _) = arena.value.typ(note).as_ref() {
                let mixop = value_case.mixop().into_tree(&arena.mixop);
                self.hints.get(&(type_id.node.to_string(), mixop)).cloned()
            } else {
                None
            };
            let mut rendered = self.rendered.borrow_mut();
            if rendered.hints.len() < RENDER_ENTRIES_MAX {
                rendered.hints.insert(key, hint.clone());
            }
            hint
        });
        if let Some(hint) = hint {
            return self.render_hint(arena, &hint, value_case.args());
        }
        let mut rendered = Vec::new();
        self.render_mixfix(arena, value_case, &mut rendered)?;
        Ok(rendered.join(" "))
    }

    /// Renders the case arguments through a print-hint template.
    fn render_hint(
        &self,
        arena: &Arena,
        hint: &AlterHint,
        values: &[Value],
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
        arena: &Arena,
        values: &[Value],
        separator: &str,
    ) -> Result<String, P4UnparseError> {
        let Some((first, rest)) = values.split_first() else {
            return Ok(String::new());
        };
        let mut rendered = self.render(arena, first)?;
        for value in rest {
            let value = self.render(arena, value)?;
            rendered.push_str(separator);
            rendered.push_str(&value);
        }
        Ok(rendered)
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
                // Arguments render recursively
                Item::Arg(pos) => rendered.push(self.render(arena, &value_case.args()[pos])?),
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

impl Renderer<Value> for ValueRenderer<'_> {
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
        let mut rendered = String::new();
        for item in items {
            let item = item?;
            if item.is_empty() {
                continue;
            }
            if rendered.is_empty() {
                rendered = item;
            } else {
                rendered.push(' ');
                rendered.push_str(&item);
            }
        }
        Ok(rendered)
    }

    fn fuse(&self, output_l: Self::Output, output_r: Self::Output) -> Self::Output {
        let output_l = output_l?;
        let output_r = output_r?;
        Ok(output_l + &output_r)
    }

    fn other(&self, exp: &crate::lang::el::ast::Exp) -> Self::Output {
        Ok(Print::to_string(exp))
    }

    fn item(&self, item: &Value) -> Self::Output {
        self.0.render(self.1, item)
    }
}
