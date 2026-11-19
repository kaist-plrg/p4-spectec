//! Host type definitions of extern types
//!
//! A host registers its `HostTypDef`s through `Extern::host_typdefs`;
//! `HTDEnv` indexes them by the extern type each defines.

use std::{collections::BTreeMap, fmt};

use crate::lang::data::value::{ValueError, host::HostTyp};

/// Extern type names to the host's definitions of them.
pub struct HTDEnv(BTreeMap<&'static str, HostTyp>);

impl HTDEnv {
    /// Indexes the host's type definitions, rejecting a repeated name.
    pub fn new(typdefs: Vec<HostTyp>) -> Result<Self, ValueError> {
        let mut entries = BTreeMap::new();
        for typdef in typdefs {
            let name = typdef.name();
            if entries.insert(name, typdef).is_some() {
                return Err(ValueError::HostTypDefRepeated { name: name.to_owned() });
            }
        }
        Ok(Self(entries))
    }

    /// Whether the host defines the extern type `name`.
    pub fn contains(&self, name: &str) -> bool {
        self.0.contains_key(name)
    }

    /// Finds the host's definition of the extern type `name`, if any.
    pub fn find(&self, name: &str) -> Option<HostTyp> {
        self.0.get(name).copied()
    }
}

impl fmt::Debug for HTDEnv {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_set().entries(self.0.keys()).finish()
    }
}
