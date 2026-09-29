//! Definition and hint state for prose conversion
//!
//! `Context` holds the hint environment keyed by definition,
//! the meta-variable environment used to invent fresh expressions,
//! and the relation or function being converted as the namespace.

use crate::lang::{
    common::{notation::mixop::Mixop, source::Span},
    data::typ,
    hints::{alter, fields},
    il,
    pl::annot::{Hint, Hints},
    sl::ast::{self as sl, Id},
};
use crate::runtime::envs::{algo::MEnv, prosify::HEnv};

use super::{ProseError, error};

// == Context

#[derive(Debug)]
/// Hints and meta-variables collected from the whole specification.
pub(super) struct Context {
    /// The relation or function whose body is being converted.
    id_namespace: Option<Id>,
    /// Prose hints by case, function, and relation.
    henv: HEnv,
    /// Meta-variable types, seeded with the builtin types.
    menv: MEnv,
}

impl Context {
    // - Constructor

    /// Seeds the meta-variables with `bool`, `nat`, `int`, and `text`.
    fn init() -> Self {
        let mut menv = MEnv::new();
        for (text_name, typ) in [
            ("bool", typ::make::bool()),
            ("nat", typ::make::nat()),
            ("int", typ::make::int()),
            ("text", typ::make::text()),
        ] {
            let id = crate::phrase! { node: text_name.to_owned(), span: Span::default() };
            menv.insert(id, typ);
        }
        Self { id_namespace: None, henv: HEnv::default(), menv }
    }

    // - Namespace

    /// Enters the definition whose body is converted next.
    pub(super) fn set_namespace(&mut self, id_namespace: Id) {
        self.id_namespace = Some(id_namespace);
    }

    /// The current definition; set before any body is converted.
    pub(super) fn namespace(&self) -> &Id {
        self.id_namespace
            .as_ref()
            .expect("relation conversion establishes its namespace")
    }

    // - Hint lookup

    /// The hints of a meta-function.
    pub(super) fn hints_func(&self, id_func: &Id) -> Option<&Hints> {
        self.henv.get_func(id_func)
    }

    /// The hints of a relation.
    pub(super) fn hints_rel(&self, id_rel: &Id) -> Option<&Hints> {
        self.henv.get_rel(id_rel)
    }

    /// The hints of a variant case.
    pub(super) fn hints_case(&self, id_typ: &Id, mixop: &Mixop) -> Option<&Hints> {
        self.henv.get_case(id_typ, mixop)
    }

    // - Metavariables

    /// The meta-variable environment.
    pub(super) fn menv(&self) -> &MEnv {
        &self.menv
    }

    // - Adders

    /// Records a meta-variable from validated SL.
    fn add_metavar(&mut self, id_metavar: Id, typ: il::ast::Typ) {
        // Elaboration rejects duplicates; algo and structure preserve declarations
        assert!(
            !self.menv.contains_key(&id_metavar),
            "elaboration rejects duplicate meta-variables"
        );
        self.menv.insert(id_metavar, typ);
    }

    // - Hint loading

    /// Reads the `prose*` hints of one definition; other hints are ignored.
    fn load_hints(
        hints_sl: &[sl::Hint],
        span_decl: &Span,
        num_fields: Option<usize>,
    ) -> Result<Hints, ProseError> {
        let mut hints = Hints::default();
        for sl::Hint { id: id_hint, exp: exp_hint } in hints_sl {
            let text_hint = id_hint.node.as_str();
            match text_hint {
                // Alteration hints share one parser
                "prose" | "prose_in" | "prose_out" | "prose_true" | "prose_false" => {
                    let hint = Hint {
                        id: id_hint.clone(),
                        value: alter::init(exp_hint),
                        span_decl: span_decl.clone(),
                    };
                    match text_hint {
                        "prose" => hints.prose = Some(hint),
                        "prose_in" => hints.prose_in = Some(hint),
                        "prose_out" => hints.prose_out = Some(hint),
                        "prose_true" => hints.prose_true = Some(hint),
                        "prose_false" => hints.prose_false = Some(hint),
                        _ => unreachable!(),
                    }
                }
                // Field hints list strings
                "prose_fields" => {
                    let value = fields::init(exp_hint).map_err(|exp| {
                        error::field_hint_element_invalid(id_hint, exp, span_decl)
                    })?;
                    let hint = Hint { id: id_hint.clone(), value, span_decl: span_decl.clone() };
                    // Validate each field hint before a later hint can replace it
                    if let Some(num_fields) = num_fields {
                        fields::validate(&hint.value, num_fields).map_err(
                            |fields::FieldError::ArityMismatch { expected, actual }| {
                                error::field_hint_arity_mismatch(&hint, expected, actual)
                            },
                        )?;
                    }
                    hints.prose_fields = Some(hint);
                }
                _ => {}
            }
        }
        Ok(hints)
    }

    // - Definition loading

    /// Records what one definition contributes: meta-variables and hints.
    fn load_def(&mut self, def_sl: &sl::Def) -> Result<(), ProseError> {
        match &def_sl.node {
            sl::DefKind::Typ(def_typ_sl) => self.load_typ_def(def_typ_sl),
            sl::DefKind::Var(def_var_sl) => self.load_var_def(def_var_sl),
            sl::DefKind::Rel(def_rel_sl) => self.load_rel_def(def_rel_sl),
            sl::DefKind::MetaFunc(def_func_sl) => self.load_func_def(def_func_sl),
        }
    }

    /// Records a type definition.
    fn load_typ_def(&mut self, def_typ_sl: &sl::TypDef) -> Result<(), ProseError> {
        match def_typ_sl {
            sl::TypDef::Extern(def_typ_sl) => self.load_extern_typ_def(def_typ_sl),
            sl::TypDef::Defined(def_typ_sl) => self.load_defined_typ_def(def_typ_sl),
        }
    }

    /// An extern type names itself as a meta-variable.
    fn load_extern_typ_def(&mut self, def_typ_sl: &sl::ExternTyp) -> Result<(), ProseError> {
        let typ = crate::phrase! {
            node: il::ast::TypKind::Var(def_typ_sl.id.clone(), Vec::new()),
            span: def_typ_sl.id.span.clone(),
        };
        self.add_metavar(def_typ_sl.id.clone(), typ);
        Ok(())
    }

    /// A monomorphic type names itself; each variant case adds its hints.
    fn load_defined_typ_def(&mut self, def_typ_sl: &sl::DefinedTyp) -> Result<(), ProseError> {
        // Only monomorphic types can be meta-variables
        if def_typ_sl.tparams.is_empty() {
            let typ = crate::phrase! {
                node: il::ast::TypKind::Var(def_typ_sl.id.clone(), Vec::new()),
                span: def_typ_sl.id.span.clone(),
            };
            self.add_metavar(def_typ_sl.id.clone(), typ);
        }
        // Only variant cases carry prose hints
        let il::ast::DefTypKind::Variant(cases) = &def_typ_sl.def_typ.node else {
            return Ok(());
        };
        for il::ast::TypCase { not_typ, hints: hints_sl, .. } in cases {
            let hints = Self::load_hints(hints_sl, &not_typ.span, Some(not_typ.node.args().len()))?;
            self.henv
                .insert_case(&def_typ_sl.id, &not_typ.node.to_mixop(), hints);
        }
        Ok(())
    }

    /// A meta-variable declaration.
    fn load_var_def(&mut self, def_var_sl: &sl::VarDef) -> Result<(), ProseError> {
        self.add_metavar(def_var_sl.id.clone(), def_var_sl.typ.clone());
        Ok(())
    }

    /// A relation's hints, extern or defined.
    fn load_rel_def(&mut self, def_rel_sl: &sl::RelDef) -> Result<(), ProseError> {
        let (id_rel, hints_sl) = match def_rel_sl {
            sl::RelDef::Extern(def_rel_sl) => (&def_rel_sl.id, &def_rel_sl.hints),
            sl::RelDef::Defined(def_rel_sl) => (&def_rel_sl.id, &def_rel_sl.hints),
        };
        let hints = Self::load_hints(hints_sl, &id_rel.span, None)?;
        self.henv.insert_rel(id_rel, hints);
        Ok(())
    }

    /// A function's hints, whatever its kind.
    fn load_func_def(&mut self, def_func_sl: &sl::MetaFuncDef) -> Result<(), ProseError> {
        let (id_func, hints_sl) = match def_func_sl {
            sl::MetaFuncDef::Extern(def_func_sl) => (&def_func_sl.id, &def_func_sl.hints),
            sl::MetaFuncDef::Builtin(def_func_sl) => (&def_func_sl.id, &def_func_sl.hints),
            sl::MetaFuncDef::Table(def_func_sl) => (&def_func_sl.id, &def_func_sl.hints),
            sl::MetaFuncDef::Defined(def_func_sl) => (&def_func_sl.id, &def_func_sl.hints),
        };
        let hints = Self::load_hints(hints_sl, &id_func.span, None)?;
        self.henv.insert_func(id_func, hints);
        Ok(())
    }

    /// Builds the context from every definition in source order.
    pub(super) fn load(spec_sl: &sl::Spec) -> Result<Self, ProseError> {
        let mut ctx = Self::init();
        for def_sl in spec_sl {
            ctx.load_def(def_sl)?;
        }
        Ok(ctx)
    }
}
