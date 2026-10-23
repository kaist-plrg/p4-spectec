//! Interface presets assemble language-specific runner interfaces
//!
//! `p4` is the preset for the P4 specification:
//! the P4 parser and standard builtins plus `print_`,
//! which renders a value back to P4 syntax
//! using the specification's print hints
//! `al` lowers specification inputs to meta scripts with standard builtins

use crate::lang::{common::source::Span, data::value};

use crate::runner::Spec;

use self::{
    al::AlInterface,
    builtin::{call::Builtins, extract},
    p4::{P4Interface, unparse::P4Unparser},
};

pub mod al;
pub mod builtin;
pub mod p4;

// == AL

/// Builds the AL interface with the standard specification builtins.
pub fn al() -> AlInterface {
    AlInterface::new(Builtins::new())
}

// == P4

/// Builds the P4 interface, with `print_` reading hints from `spec`.
pub fn p4(spec: &Spec) -> P4Interface {
    let unparser = match spec {
        Spec::Al(spec) => P4Unparser::from_al_spec(spec),
        Spec::Sl(spec) => P4Unparser::from_sl_spec(spec),
        Spec::Pl(spec) => P4Unparser::from_pl_spec(spec),
        Spec::Meta { target, .. } => P4Unparser::from_al_spec(target),
    };
    p4_with_unparser(unparser)
}

/// Installs `print_` over the standard builtins.
fn p4_with_unparser(unparser: P4Unparser) -> P4Interface {
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
    P4Interface::new(builtins)
}
