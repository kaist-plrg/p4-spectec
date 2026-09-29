//! Ordered marker dispatch and document-batch rendering
//!
//! A fresh registry and rendering context are built for each invocation.
//! The target prepass covers every input before any replacement is rendered.

use std::{fs, path::PathBuf};

use super::super::anchor::AnchorContext;
use super::{
    anchor,
    error::{self, Error},
    file::PendingFile,
    parser,
    source::Source,
    splicer::{Splice, Splicer},
    splicers::*,
};
use crate::{
    diagnostic::Report,
    lang::{el::ast as el, pl::ast as pl},
};

// == Splicers

/// Registers marker kinds in their matching and unused-warning order.
fn init<'spec>(spec_el: &'spec el::Spec, spec_pl: &'spec pl::Spec) -> Vec<Box<dyn Splice + 'spec>> {
    vec![
        Box::new(Splicer::<syntax::Source>::new(spec_el, spec_pl)),
        Box::new(Splicer::<rel_title::Source>::new(spec_el, spec_pl)),
        Box::new(Splicer::<rel_title::Latex>::new(spec_el, spec_pl)),
        Box::new(Splicer::<rel_title::Prose>::new(spec_el, spec_pl)),
        Box::new(Splicer::<rule_group_else::Prose>::new(spec_el, spec_pl)),
        Box::new(Splicer::<rule_group::Source>::new(spec_el, spec_pl)),
        Box::new(Splicer::<rule_group::Latex>::new(spec_el, spec_pl)),
        Box::new(Splicer::<rule_group::Prose>::new(spec_el, spec_pl)),
        Box::new(Splicer::<rule_group_dispatch::Prose>::new(spec_el, spec_pl)),
        Box::new(Splicer::<func_title::Source>::new(spec_el, spec_pl)),
        Box::new(Splicer::<func_title::Latex>::new(spec_el, spec_pl)),
        Box::new(Splicer::<func_title::Prose>::new(spec_el, spec_pl)),
        Box::new(Splicer::<func::Source>::new(spec_el, spec_pl)),
        Box::new(Splicer::<func::Latex>::new(spec_el, spec_pl)),
        Box::new(Splicer::<func::Prose>::new(spec_el, spec_pl)),
        Box::new(Splicer::<table::Source>::new(spec_el, spec_pl)),
        Box::new(Splicer::<table::Latex>::new(spec_el, spec_pl)),
        Box::new(Splicer::<table::Prose>::new(spec_el, spec_pl)),
    ]
}

// == Splicing
//
// A skeleton keeps literal slices and references to parsed marker requests:
//   "before ${func-prose: f} after"
//   -> [Text("before "), Marker { idx_splicer: 14, idx_request: 0 }, Text(" after")]
// `idx_splicer` selects the func-prose entry in the registry above.
// `idx_request` selects its stored keys (`f` here), including source positions.
// Parsing stores those keys once; rendering replaces only Marker segments.

struct Marker {
    idx_splicer: usize,
    idx_request: usize,
}

enum Segment<'src> {
    Text(&'src str),
    Marker(Marker),
}

type Skeleton<'src> = Vec<Segment<'src>>;

/// Parses selections once and retains untouched text as borrowed slices.
fn parse_skeleton<'src>(
    splicers: &mut [Box<dyn Splice + '_>],
    file: &str,
    text: &'src str,
) -> Result<Skeleton<'src>, Error> {
    let mut source = Source::new(file, text);
    let mut skeleton = Vec::new();
    let mut pos_text = 0;
    // Dispatch only at possible marker openings
    while let Some(offset) = source.remaining().find("${") {
        source.advn_bytes(offset);
        let pos_marker = source.offset();
        let mut parsed = false;
        for (idx_splicer, splicer) in splicers.iter_mut().enumerate() {
            if parser::parse_splice_start(&mut source, splicer.name()) {
                // Store the typed request and preceding literal slice
                let idx_request = splicer.parse(&mut source)?;
                if pos_text < pos_marker {
                    skeleton.push(Segment::Text(&text[pos_text..pos_marker]));
                }
                skeleton.push(Segment::Marker(Marker { idx_splicer, idx_request }));
                pos_text = source.offset();
                parsed = true;
                break;
            }
        }
        // Unknown openings stay literal and may contain recognized markers
        if !parsed {
            source.advn_bytes(2);
        }
    }
    if pos_text < text.len() {
        skeleton.push(Segment::Text(&text[pos_text..]));
    }
    Ok(skeleton)
}

/// Renders stored requests without rescanning generated text.
fn render_skeleton(
    anchor_ctx: &mut AnchorContext<'_>,
    warnings: &mut Vec<Report>,
    splicers: &mut [Box<dyn Splice + '_>],
    skeleton: &Skeleton<'_>,
) -> Result<String, Error> {
    let mut text = String::new();
    for segment in skeleton {
        match segment {
            Segment::Text(literal) => text.push_str(literal),
            Segment::Marker(marker) => text.push_str(&splicers[marker.idx_splicer].render(
                anchor_ctx,
                warnings,
                marker.idx_request,
            )?),
        }
    }
    Ok(text)
}

/// Renders skeletons with batch-wide anchors.
fn splice_strings_impl(
    warnings: &mut Vec<Report>,
    spec_el: &el::Spec,
    spec_pl: &pl::Spec,
    sources: &[(&str, &str)],
) -> Result<Vec<String>, Error> {
    // Parse every request before collecting its link targets
    let mut splicers = init(spec_el, spec_pl);
    let skeletons = sources
        .iter()
        .map(|(file, text)| parse_skeleton(&mut splicers, file, text))
        .collect::<Result<Vec<_>, _>>()?;
    let decls = anchor::Decls::collect_from_el(spec_el);
    let mut targets = anchor::Targets::default();
    for skeleton in &skeletons {
        for segment in skeleton {
            if let Segment::Marker(marker) = segment {
                splicers[marker.idx_splicer].collect_link_targets(
                    &mut targets,
                    warnings,
                    &decls,
                    marker.idx_request,
                );
            }
        }
    }
    let func = |presentation, name: &str| targets.func(presentation, name);
    let rel = |presentation, name: &str| targets.rel(presentation, name);
    let mut anchor_ctx = AnchorContext::new(&func, &rel);
    let mut texts = Vec::with_capacity(sources.len());
    // Share usage flags and counters through every input
    for skeleton in &skeletons {
        texts.push(render_skeleton(&mut anchor_ctx, warnings, &mut splicers, skeleton)?);
    }
    // Report unused keys only after all sources rendered successfully
    for splicer in &splicers {
        splicer.warn_unused(warnings);
    }
    Ok(texts)
}

/// Renders all inputs before staging and replacing output files.
///
/// Parsing and rendering failures leave every destination untouched.
/// Replacements are atomic per file; an I/O failure during the final rename
/// sequence can leave earlier files committed.
fn splice_files_impl(
    warnings: &mut Vec<Report>,
    spec_el: &el::Spec,
    spec_pl: &pl::Spec,
    path_pairs: &[(PathBuf, PathBuf)],
) -> Result<(), Error> {
    // Read all inputs before any path can be replaced by another output
    let mut sources = Vec::with_capacity(path_pairs.len());
    for (path_input, _) in path_pairs {
        let text =
            fs::read_to_string(path_input).map_err(|source| error::io(path_input, source))?;
        sources.push((path_input.to_string_lossy().into_owned(), text));
    }
    // Complete the target prepass and rendering before filesystem mutations
    let sources: Vec<_> = sources
        .iter()
        .map(|(file, text)| (file.as_str(), text.as_str()))
        .collect();
    let texts = splice_strings_impl(warnings, spec_el, spec_pl, &sources)?;
    // Stage every output before starting the per-file commit sequence
    let mut pending = Vec::with_capacity(path_pairs.len());
    for ((_, path_output), text) in path_pairs.iter().zip(texts) {
        pending.push(PendingFile::new(path_output, &text)?);
    }
    // Rename in the caller's order, retaining earlier commits on I/O failure
    for file in pending {
        file.commit()?;
    }
    Ok(())
}

// == Entry points

/// Renders skeletons with batch-wide anchors, discarding nonfatal warnings.
pub fn splice_strings(
    spec_el: &el::Spec,
    spec_pl: &pl::Spec,
    sources: &[(&str, &str)],
) -> Result<Vec<String>, Error> {
    splice_strings_with_warnings(spec_el, spec_pl, sources).0
}

/// Renders skeletons and retains warnings even when a later rendering fails.
pub fn splice_strings_with_warnings(
    spec_el: &el::Spec,
    spec_pl: &pl::Spec,
    sources: &[(&str, &str)],
) -> (Result<Vec<String>, Error>, Vec<Report>) {
    let mut warnings = Vec::new();
    let result = splice_strings_impl(&mut warnings, spec_el, spec_pl, sources);
    (result, warnings)
}

/// Renders and replaces output files, discarding nonfatal warnings.
///
/// Parsing and rendering failures leave every destination untouched.
/// Replacements are atomic per file; a later rename failure can leave
/// earlier files committed.
pub fn splice_files(
    spec_el: &el::Spec,
    spec_pl: &pl::Spec,
    path_pairs: &[(PathBuf, PathBuf)],
) -> Result<(), Error> {
    splice_files_with_warnings(spec_el, spec_pl, path_pairs).0
}

/// Replaces output files and retains warnings even when rendering or I/O fails.
///
/// File replacement follows the same staging and commit order as [`splice_files`].
pub fn splice_files_with_warnings(
    spec_el: &el::Spec,
    spec_pl: &pl::Spec,
    path_pairs: &[(PathBuf, PathBuf)],
) -> (Result<(), Error>, Vec<Report>) {
    let mut warnings = Vec::new();
    let result = splice_files_impl(&mut warnings, spec_el, spec_pl, path_pairs);
    (result, warnings)
}
