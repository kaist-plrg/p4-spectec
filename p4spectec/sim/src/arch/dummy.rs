//! The extern for running a program without an architecture
//!
//! `p4spectec run` and the run suites evaluate a program's relations directly,
//! with no packet pipeline.
//! Its archState and every objectState are the unit payload;
//! compile-time assertions such as `static_assert` are supported,
//! and runtime extern function and method calls are not.
//! `spec::entry` holds its `Extern` impl.

use crate::p4::HostTypDef;

/// The dummy architecture, with its definitions of the extern types.
pub struct Dummy {
    /// The definition of `archState`, which the dummy architecture leaves empty.
    pub(crate) typdef_state: HostTypDef<()>,
    /// The definition of `objectState`, which every dummy object leaves empty.
    pub(crate) typdef_object: HostTypDef<()>,
}

// Every instance makes definitions of its own
impl Default for Dummy {
    fn default() -> Self {
        Self {
            typdef_state: HostTypDef::new("archState"),
            typdef_object: HostTypDef::new("objectState"),
        }
    }
}
