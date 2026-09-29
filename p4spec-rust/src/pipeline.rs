//! File-based entry points for specification transformations
//!
//! [`parse`], [`elab`], [`algo`], [`structure`], and [`prosify`] run the passes
//! from ordered source paths to the requested language.
//! Errors preserve the failing stage and its source diagnostics;
//! the `*_with_warnings` variants also return ordered elaboration warnings,
//! including when a later stage fails. Callers choose how to report them.

use std::path::Path;

use crate::{
    diagnostic::Report,
    frontend::{error::FrontendError, parse::parse_files},
    lang::{al, el, il, pl, sl},
    pass::{
        self, algo::AlgoError, elaborate::ElabError, prosify::ProseError, structure::StructureError,
    },
};

// = Errors

/// A failure while reading or transforming a specification.
#[derive(Debug, thiserror::Error)]
pub enum Error {
    /// Reading or parsing source failed.
    #[error(transparent)]
    Frontend(#[from] FrontendError),
    /// Elaborating EL into IL failed.
    #[error(transparent)]
    Elab(ElabError),
    /// Converting IL into AL failed.
    #[error(transparent)]
    Algo(AlgoError),
    /// Structuring AL into SL failed.
    #[error(transparent)]
    Structure(StructureError),
    /// Converting SL into PL failed.
    #[error(transparent)]
    Prose(#[from] ProseError),
}

// = Transformations

/// Parses specification paths in processing order into EL.
pub fn parse<I, P>(paths: I) -> Result<el::ast::Spec, Error>
where
    I: IntoIterator<Item = P>,
    P: AsRef<Path>,
{
    Ok(parse_files(paths)?)
}

/// Parses and elaborates specification paths into typed IL.
pub fn elab<I, P>(paths: I) -> Result<il::ast::Spec, Error>
where
    I: IntoIterator<Item = P>,
    P: AsRef<Path>,
{
    elab_with_warnings(paths).0
}

/// Elaborates paths into typed IL and returns accumulated warnings.
///
/// Preserves warnings from [`pass::elaborate::convert_with_warnings`]
/// on success or elaboration failure; frontend failures return no warnings.
pub fn elab_with_warnings<I, P>(paths: I) -> (Result<il::ast::Spec, Error>, Vec<Report>)
where
    I: IntoIterator<Item = P>,
    P: AsRef<Path>,
{
    let spec_el = match parse(paths) {
        // Elaborate only after every source file has parsed
        Ok(spec_el) => spec_el,
        // Parsing fails before elaboration can accumulate warnings
        Err(error) => return (Err(error), Vec::new()),
    };
    let (result, warnings) = pass::elaborate::convert_with_warnings(spec_el);
    (result.map_err(Error::Elab), warnings)
}

/// Parses, elaborates, and converts specification paths into AL.
pub fn algo<I, P>(paths: I) -> Result<al::ast::Spec, Error>
where
    I: IntoIterator<Item = P>,
    P: AsRef<Path>,
{
    algo_with_warnings(paths).0
}

/// Converts paths into AL and preserves warnings across stage failures.
pub fn algo_with_warnings<I, P>(paths: I) -> (Result<al::ast::Spec, Error>, Vec<Report>)
where
    I: IntoIterator<Item = P>,
    P: AsRef<Path>,
{
    let (result, warnings) = elab_with_warnings(paths);
    let result = result.and_then(|spec_il| pass::algo::convert(spec_il).map_err(Error::Algo));
    (result, warnings)
}

/// Converts specification paths into SL, optionally removing rule groups.
///
/// Set `without_rule_groups` to true for SL execution;
/// PL conversion requires the groups to remain.
pub fn structure<I, P>(paths: I, without_rule_groups: bool) -> Result<sl::ast::Spec, Error>
where
    I: IntoIterator<Item = P>,
    P: AsRef<Path>,
{
    structure_with_warnings(paths, without_rule_groups).0
}

/// Converts paths into SL and preserves warnings across stage failures.
///
/// Uses the same rule-group setting as [`structure`].
pub fn structure_with_warnings<I, P>(
    paths: I,
    without_rule_groups: bool,
) -> (Result<sl::ast::Spec, Error>, Vec<Report>)
where
    I: IntoIterator<Item = P>,
    P: AsRef<Path>,
{
    let (result, warnings) = algo_with_warnings(paths);
    let result = result.and_then(|spec_al| {
        pass::structure::convert(spec_al, without_rule_groups).map_err(Error::Structure)
    });
    (result, warnings)
}

/// Converts specification paths through SL with rule groups into annotated PL.
pub fn prosify<I, P>(paths: I) -> Result<pl::ast::Spec, Error>
where
    I: IntoIterator<Item = P>,
    P: AsRef<Path>,
{
    prosify_with_warnings(paths).0
}

/// Converts paths into annotated PL and preserves warnings across stage failures.
///
/// Retains SL rule groups for prose conversion, as in [`prosify`].
pub fn prosify_with_warnings<I, P>(paths: I) -> (Result<pl::ast::Spec, Error>, Vec<Report>)
where
    I: IntoIterator<Item = P>,
    P: AsRef<Path>,
{
    let (result, warnings) = structure_with_warnings(paths, false);
    let result = result.and_then(|spec_sl| pass::prosify::convert(spec_sl).map_err(Error::Prose));
    (result, warnings)
}
