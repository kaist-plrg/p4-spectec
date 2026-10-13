//! Staged skeleton-file replacement
//!
//! Both entry points read the complete input batch before staging replacements.
//! `splice_files_with_warnings` registers original skeletons with its renderer
//! before committing, so `--inplace` diagnostics retain their source text.
//! Preparation and commit stay internal; uncommitted temporary files are removed.
//! `splice_files` uses the same preparation, discarding warnings.

mod pending;

use std::{fs, path::PathBuf};

use crate::lang::{el::ast as el, pl::ast as pl};

use crate::diagnostic::{Renderer, Report};

use super::{
    driver,
    error::{self, Error},
};

use pending::PendingFile;

// == Prepared output

/// Owns staged splice outputs until commit or cleanup on drop.
struct PreparedSplice {
    files: Vec<PendingFile>,
}

impl PreparedSplice {
    /// Replaces destinations in input order, consuming the staged outputs.
    ///
    /// Replacements are atomic per file;
    /// a later failure retains earlier commits and removes uncommitted outputs.
    fn commit(self) -> Result<(), Error> {
        for file in self.files {
            file.commit()?;
        }
        Ok(())
    }
}

// == Preparation

/// Reads every skeleton before any destination can replace an input.
fn read_sources(path_pairs: &[(PathBuf, PathBuf)]) -> Result<Vec<(String, String)>, Error> {
    let mut sources = Vec::with_capacity(path_pairs.len());
    for (path_input, _) in path_pairs {
        let text =
            fs::read_to_string(path_input).map_err(|source| error::io(path_input, source))?;
        sources.push((path_input.to_string_lossy().into_owned(), text));
    }
    Ok(sources)
}

/// Renders and stages all outputs without replacing destinations.
fn prepare_splice_files_impl(
    warnings: &mut Vec<Report>,
    spec_el: &el::Spec,
    spec_pl: &pl::Spec,
    path_pairs: &[(PathBuf, PathBuf)],
    sources: &[(String, String)],
) -> Result<PreparedSplice, Error> {
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
    // Own every staged file until the batch is committed or dropped
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
/// A later replacement failure retains earlier commits.
pub fn splice_files(
    spec_el: &el::Spec,
    spec_pl: &pl::Spec,
    path_pairs: &[(PathBuf, PathBuf)],
) -> Result<(), Error> {
    let sources = read_sources(path_pairs)?;
    prepare_splice_files_impl(&mut Vec::new(), spec_el, spec_pl, path_pairs, &sources)?.commit()
}

/// Replaces output files and retains warnings even when splicing fails.
///
/// Registers original skeletons with the renderer before replacing any file.
/// Returned reports must be rendered with that same renderer.
pub fn splice_files_with_warnings(
    renderer: &mut Renderer,
    spec_el: &el::Spec,
    spec_pl: &pl::Spec,
    path_pairs: &[(PathBuf, PathBuf)],
) -> (Result<(), Error>, Vec<Report>) {
    // Read the complete batch before preparing replacements
    let sources = match read_sources(path_pairs) {
        Ok(sources) => sources,
        Err(error) => return (Err(error), Vec::new()),
    };
    let mut warnings = Vec::new();
    let result = prepare_splice_files_impl(&mut warnings, spec_el, spec_pl, path_pairs, &sources);
    // Preserve sources even when rendering or staging has failed
    for (file, text) in sources {
        renderer.insert_source(file, text);
    }
    // Commit only fully prepared batches, retaining warnings on failure
    (result.and_then(PreparedSplice::commit), warnings)
}
