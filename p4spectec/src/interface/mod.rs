//! Interface presets assemble language-specific runner interfaces
//!
//! `p4` is the preset for the P4 specification:
//! the P4 parser and standard builtins plus `print_`,
//! which renders a value back to P4 syntax
//! using the specification's print hints
//! along the type `print_` is instantiated at.
//!
//! `meta` is the preset for meta specifications: standard builtins,
//! with specification inputs parsed as AL or IL scripts;
//! `meta` also converts IL and AL to and from meta-language values

use crate::lang::{common::source::Span, data::value};

use crate::runner::Spec;

use self::{
    builtin::{call::Builtins, extract},
    meta::MetaInterface,
    p4::{P4Interface, unparse::P4Unparser},
};

pub mod builtin;
pub mod meta;
pub mod p4;

/// The language of the input a specification accepts.
#[derive(Clone, Copy, Debug, PartialEq, Eq, serde::Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Lang {
    /// An AL script, converted by the native `algo` pass.
    Al,
    /// An IL script, converted to AL by the accepting specification.
    Il,
    /// An EL script, elaborated and converted to AL by the accepting specification.
    El,
    /// A P4 program, which only the P4 specification accepts.
    P4,
}

impl Lang {
    /// Returns whether the language is a script the level above can be passed as,
    /// so that a level accepting it may sit below another level.
    pub fn is_script(self) -> bool {
        matches!(self, Self::Al | Self::Il | Self::El)
    }
}

// == Meta

/// Builds the interface of a meta specification accepting `lang` scripts,
/// with the standard specification builtins.
pub fn meta(lang: Lang) -> MetaInterface {
    MetaInterface::new(lang, Builtins::new())
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
