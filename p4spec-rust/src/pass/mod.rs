//! File-based entry points for specification transformations
//!
//! [`elab`], [`algo`], [`structure`], and [`prosify`] run the passes
//! from ordered source paths to the requested language.
//! Errors preserve the reports produced by the failing stage;
//! the `*_with_warnings` variants also return ordered elaboration warnings,
//! including when a later stage fails. Callers choose how to report them.

use std::path::{Path, PathBuf};

use crate::lang::il;

use crate::lang::al;

use crate::lang::sl;

use crate::lang::pl;

use crate::diagnostic::Report;

use crate::frontend::parse::parse_files;

use crate::{lang::el, runner};

pub mod algo;
pub mod elaborate;
pub mod prosify;
pub mod structure;

// == Errors

/// A report produced while reading or transforming a specification.
pub type Error = Box<Report>;

// == Transformations

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
/// Preserves warnings from [`elaborate::convert_with_warnings`]
/// on success or elaboration failure; frontend failures return no warnings.
pub fn elab_with_warnings<I, P>(paths: I) -> (Result<il::ast::Spec, Error>, Vec<Report>)
where
    I: IntoIterator<Item = P>,
    P: AsRef<Path>,
{
    let spec_el = match parse_files(paths) {
        // Elaborate only after every source file has parsed
        Ok(spec_el) => spec_el,
        // Parsing fails before elaboration can accumulate warnings
        Err(error) => return (Err(error), Vec::new()),
    };
    elaborate::convert_with_warnings(spec_el)
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
    let result = result.and_then(algo::convert);
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
    let result = result.and_then(|spec_al| structure::convert(spec_al, without_rule_groups));
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
    let result = result.and_then(prosify::convert);
    (result, warnings)
}

// == Runner specifications

/// Selects the specification language used for execution.
#[derive(Clone, Copy, Debug)]
pub enum SpecLang {
    /// The algorithmic language.
    Al,
    /// The structured language.
    Sl,
    /// The prose language.
    Pl,
}

/// Converts paths into the selected runner language with accumulated warnings.
pub fn runner_spec_with_warnings(
    lang: SpecLang,
    paths: &[PathBuf],
) -> (Result<runner::Spec, Error>, Vec<Report>) {
    match lang {
        SpecLang::Al => {
            let (result, warnings) = algo_with_warnings(paths);
            (result.map(runner::Spec::Al), warnings)
        }
        SpecLang::Sl => {
            let (result, warnings) = structure_with_warnings(paths, true);
            (result.map(runner::Spec::Sl), warnings)
        }
        SpecLang::Pl => {
            let (result, warnings) = prosify_with_warnings(paths);
            (result.map(runner::Spec::Pl), warnings)
        }
    }
}

// == Document specifications

/// Prepares source and prose specifications for document splicing.
pub fn specdoc_spec_with_warnings(
    paths: &[PathBuf],
) -> (Result<(el::ast::Spec, pl::ast::Spec), Error>, Vec<Report>) {
    // Retain source definitions before starting prose conversion
    let spec_el = match parse_files(paths) {
        // Keep the source representation for document fragments
        Ok(spec_el) => spec_el,
        // Stop before prose conversion can accumulate warnings
        Err(error) => return (Err(error), Vec::new()),
    };

    // Preserve prose warnings even when conversion fails
    let (result, warnings) = prosify_with_warnings(paths);
    let result = result.map(|spec_pl| (spec_el, spec_pl));
    (result, warnings)
}
