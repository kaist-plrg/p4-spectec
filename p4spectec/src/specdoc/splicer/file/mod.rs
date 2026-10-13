//! Staged skeleton-file replacement
//!
//! `prepare_splice_files_with_warnings` reads and renders the input batch,
//! then stages output beside each destination.
//! Callers render warnings before `PreparedSplice::commit` replaces inputs.
//! For `--inplace`, diagnostics therefore still read the original skeleton.
//! Dropping a prepared batch removes its temporary outputs.
//! `splice_files` prepares and commits directly, discarding warnings.

mod pending;

use std::{fs, path::PathBuf};

use crate::lang::{el::ast as el, pl::ast as pl};

use crate::diagnostic::Report;

use super::{
    driver,
    error::{self, Error},
};

use pending::PendingFile;

// == Prepared output

/// Owns staged splice outputs until commit or cleanup on drop.
pub struct PreparedSplice {
    files: Vec<PendingFile>,
}

impl PreparedSplice {
    /// Replaces destinations in input order, consuming the staged outputs.
    ///
    /// Replacements are atomic per file;
    /// a later failure retains earlier commits and removes uncommitted outputs.
    pub fn commit(self) -> Result<(), Error> {
        for file in self.files {
            file.commit()?;
        }
        Ok(())
    }
}

// == Preparation

/// Renders and stages all outputs without replacing destinations.
fn prepare_splice_files_impl(
    warnings: &mut Vec<Report>,
    spec_el: &el::Spec,
    spec_pl: &pl::Spec,
    path_pairs: &[(PathBuf, PathBuf)],
) -> Result<PreparedSplice, Error> {
    // Read all inputs before any path can be replaced by another output
    let mut sources = Vec::with_capacity(path_pairs.len());
    for (path_input, _) in path_pairs {
        let text =
            fs::read_to_string(path_input).map_err(|source| error::io(path_input, source))?;
        sources.push((path_input.to_string_lossy().into_owned(), text));
    }
    // Complete string splicing before staging any output
    let sources: Vec<_> = sources
        .iter()
        .map(|(file, text)| (file.as_str(), text.as_str()))
        .collect();
    let (result, warnings_splice) =
        driver::splice_strings_with_warnings(spec_el, spec_pl, &sources);
    // Retain warnings even when rendering or staging fails
    warnings.extend(warnings_splice);
    let texts = result?;
    // Own every staged file until the caller commits or drops the batch
    let mut files = Vec::with_capacity(path_pairs.len());
    for ((_, path_output), text) in path_pairs.iter().zip(texts) {
        files.push(PendingFile::new(path_output, &text)?);
    }
    Ok(PreparedSplice { files })
}

// == Entry points

/// Renders and replaces output files, discarding nonfatal warnings.
///
/// Preparation failures leave every destination untouched.
/// Replacement follows the per-file commit order of [`PreparedSplice::commit`].
pub fn splice_files(
    spec_el: &el::Spec,
    spec_pl: &pl::Spec,
    path_pairs: &[(PathBuf, PathBuf)],
) -> Result<(), Error> {
    let (result, _) = prepare_splice_files_with_warnings(spec_el, spec_pl, path_pairs);
    result?.commit()
}

/// Stages output files and retains warnings even when preparation fails.
///
/// Callers can render warnings before committing the prepared outputs.
pub fn prepare_splice_files_with_warnings(
    spec_el: &el::Spec,
    spec_pl: &pl::Spec,
    path_pairs: &[(PathBuf, PathBuf)],
) -> (Result<PreparedSplice, Error>, Vec<Report>) {
    let mut warnings = Vec::new();
    let result = prepare_splice_files_impl(&mut warnings, spec_el, spec_pl, path_pairs);
    (result, warnings)
}
