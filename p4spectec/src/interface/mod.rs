//! Interface presets assemble specification builtins
//!
//! `p4` is the preset for the P4 specification:
//! the standard builtins plus `print_`,
//! which renders a value back to P4 syntax
//! using the specification's print hints.

use crate::lang::{common::source::Span, data::value};

use crate::runner::{BuiltinInterface, Spec};

use self::{
    builtin::{call::Builtins, extract},
    p4::unparse::P4Unparser,
};

pub mod builtin;
pub mod p4;

// == P4

/// The P4 builtin interface, with `print_` reading hints from `spec`.
pub fn p4(spec: &Spec) -> BuiltinInterface {
    let unparser = match spec {
        Spec::Al(spec) => P4Unparser::from_al_spec(spec),
        Spec::Sl(spec) => P4Unparser::from_sl_spec(spec),
        Spec::Pl(spec) => P4Unparser::from_pl_spec(spec),
    };
    p4_with_unparser(unparser)
}

/// Installs `print_` over the standard builtins.
fn p4_with_unparser(unparser: P4Unparser) -> BuiltinInterface {
    let builtins = Builtins::with_extensions([(
        "print_",
        // `print_<T>(T) : text`: one type argument, one value
        Box::new(move |arena, targs, values| {
            let _typ = extract::one(targs)?;
            let value = extract::one(values)?;
            let text = unparser.render(arena, value)?;
            Ok(value::flat::make::text(arena, text, Span::default())?)
        }),
    )]);
    BuiltinInterface::new(builtins)
}
