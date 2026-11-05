//! Placeholders for information omitted from the shared meta representation

use crate::lang::il::ast as il;

/// The general hints omitted from meta definitions.
pub(in crate::interface::meta) fn hints() -> Vec<il::Hint> {
    vec![]
}
