//! Interface presets assemble language-specific runner interfaces
//!
//! `p4` is the preset for the P4 specification:
//! the P4 parser and standard builtins plus `print_`,
//! which renders a value back to P4 syntax
//! using the specification's print hints
//! along the type `print_` is instantiated at.
//!
//! `meta` is the preset for spec-meta: standard builtins,
//! with specification inputs parsed as AL scripts;
//! `meta` also converts AL to and from spec-meta's values.
//! `meta_ext` is the preset for spec-meta-ext, with inputs parsed as AL or IL
//! scripts and converted to and from spec-meta-ext's values.
//!
//! `kore` is the preset for spec-k, the K semantics:
//! standard builtins plus the K hooks spec-k delegates,
//! with a K definition and initial term parsed from KORE

use crate::lang::{common::source::Span, data::value};

use crate::runner::Spec;

use self::{
    builtin::{call::Builtins, extract},
    kore::KoreInterface,
    meta::MetaInterface,
    meta_ext::MetaExtInterface,
    p4::{P4Interface, unparse::P4Unparser},
};

pub mod builtin;
pub mod kore;
pub mod meta;
pub mod meta_ext;
pub mod p4;

/// The language of the input a specification accepts,
/// in the meta syntax of the specification accepting it.
#[derive(Clone, Copy, Debug, PartialEq, Eq, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Lang {
    /// An AL script for spec-meta, converted by the native `algo` pass.
    Al,
    /// An AL script for spec-meta-ext, converted by the native `algo` pass.
    AlExt,
    /// An IL script for spec-meta-ext, converted to AL by spec-meta-ext.
    IlExt,
    /// A P4 program, which only the P4 specification accepts.
    P4,
    /// A K definition and initial term in KORE, which only spec-k accepts.
    Kore,
}

impl Lang {
    /// Returns whether the language is a script the level above can be passed as,
    /// so that a level accepting it may sit below another level.
    pub fn is_script(self) -> bool {
        matches!(self, Self::Al | Self::AlExt | Self::IlExt)
    }
}

// == Meta

/// Builds the interface of spec-meta, accepting AL scripts,
/// with the standard specification builtins.
pub fn meta() -> MetaInterface {
    MetaInterface::new(Builtins::new())
}

/// Builds the interface of spec-meta-ext accepting `lang` scripts,
/// with the standard specification builtins.
pub fn meta_ext(lang: Lang) -> MetaExtInterface {
    MetaExtInterface::new(lang, Builtins::new())
}

// == KORE

/// Builds the interface of spec-k, with the standard builtins and the K hooks.
pub fn kore() -> KoreInterface {
    KoreInterface::new()
}

// == P4

/// Builds the P4 interface, with `print_` reading hints from `spec`.
pub fn p4(spec: &Spec) -> P4Interface {
    let unparser = match spec {
        Spec::Al(spec) => P4Unparser::from_al_spec(spec),
        Spec::Sl(spec) => P4Unparser::from_sl_spec(spec),
        Spec::Pl(spec) => P4Unparser::from_pl_spec(spec),
    };
    p4_with_unparser(unparser)
}

/// Builds the P4 interface, with `print_` rendering through `unparser`.
pub fn p4_with_unparser(unparser: P4Unparser) -> P4Interface {
    let builtins = Builtins::with_extensions([(
        "print_",
        // `print_<T>(T) : text`: one type argument, one value
        Box::new(move |arena, targs, values| {
            let typ = extract::one(targs)?;
            let value = extract::one(values)?;
            let text = unparser.render(arena, typ, value)?;
            Ok(value::flat::make::text(arena, text, Span::default())?)
        }),
    )]);
    P4Interface::new(builtins)
}
