//! Ordered marker dispatch and document-batch rendering
//!
//! A fresh registry and rendering context are built for each invocation.
//! The target prepass covers every input before any replacement is rendered.

use std::{
    fs::{self, OpenOptions},
    io::Write,
    path::{Path, PathBuf},
    sync::atomic::{AtomicU64, Ordering},
};

use super::{
    anchor,
    context::{self, Context},
    error::Error,
    parser,
    source::Source,
    splicer::{Run, Splicer},
    splicers::*,
};
use crate::{
    diagnostic::Report,
    lang::{el::ast as el, pl::ast as pl},
};

// == Splicers

/// Registers all marker kinds in the OCaml dispatch order.
fn init<'spec>(spec_el: &'spec el::Spec, spec_pl: &'spec pl::Spec) -> Vec<Box<dyn Run + 'spec>> {
    vec![
        Box::new(Splicer::<syntax::Source>::new(spec_el, spec_pl)),
        Box::new(Splicer::<rel_title::Source>::new(spec_el, spec_pl)),
        Box::new(Splicer::<rel_title::Latex>::new(spec_el, spec_pl)),
        Box::new(Splicer::<rel_title::Prose>::new(spec_el, spec_pl)),
        Box::new(Splicer::<rulegroup_else::Prose>::new(spec_el, spec_pl)),
        Box::new(Splicer::<rulegroup::Source>::new(spec_el, spec_pl)),
        Box::new(Splicer::<rulegroup::Latex>::new(spec_el, spec_pl)),
        Box::new(Splicer::<rulegroup::Prose>::new(spec_el, spec_pl)),
        Box::new(Splicer::<rulegroup_dispatch::Prose>::new(spec_el, spec_pl)),
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

/// Tries each registered marker without consuming mismatched prefixes.
fn try_splice_anchors(
    splicers: &mut [Box<dyn Run + '_>],
    source: &mut Source<'_>,
    ctx: &mut Context<'_>,
) -> Result<Option<String>, Error> {
    // Dispatch in the OCaml registry order
    for splicer in splicers {
        if parser::parse_splice_start(source, splicer.name()) {
            parser::parse_space(source);
            return splicer.splice(source, ctx).map(Some);
        }
    }
    Ok(None)
}

// == File system helpers

/// Resolves output links without requiring their final target to exist.
fn resolve_output(path: &Path) -> std::io::Result<PathBuf> {
    let mut path = path.to_owned();
    let mut paths_seen = std::collections::BTreeSet::new();
    // Follow only the final component; the filesystem resolves parent directories
    loop {
        match fs::symlink_metadata(&path) {
            // Resolve relative link targets against the link's actual directory
            Ok(metadata) if metadata.file_type().is_symlink() => {
                let parent = path
                    .parent()
                    .filter(|path| !path.as_os_str().is_empty())
                    .unwrap_or(Path::new("."));
                let parent = fs::canonicalize(parent)?;
                let path_link = parent.join(path.file_name().expect("symlink has a name"));
                // Canonical parents also detect cycles spelled with dot components
                if !paths_seen.insert(path_link.clone()) {
                    return Err(std::io::Error::other("symbolic link cycle in splice output"));
                }
                path = parent.join(fs::read_link(path_link)?);
            }
            // Existing regular paths are ready for staging
            Ok(_) => return Ok(path),
            // A missing target is created by the subsequent output commit
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(path),
            // Propagate failures that would also prevent opening the destination
            Err(error) => return Err(error),
        }
    }
}

struct PendingFile {
    path: PathBuf,
    path_output: PathBuf,
}

impl PendingFile {
    /// Stages a complete output beside its destination for an atomic rename.
    fn new(path_output: &Path, text: &str) -> Result<Self, Error> {
        // Follow output links while permitting a new final destination
        let path_target = resolve_output(path_output)
            .map_err(|source| Error::Io { path: path_output.to_owned(), source })?;
        let path_output = path_target.as_path();
        // Create the parent only after every input has rendered successfully
        let parent = path_output
            .parent()
            .filter(|path| !path.as_os_str().is_empty())
            .unwrap_or(Path::new("."));
        fs::create_dir_all(parent)
            .map_err(|source| Error::Io { path: parent.to_owned(), source })?;
        // Exclusive creation prevents concurrent runs from sharing a temporary file
        static NEXT: AtomicU64 = AtomicU64::new(0);
        let (pending, mut file) = loop {
            let num = NEXT.fetch_add(1, Ordering::Relaxed);
            let path = parent.join(format!(".p4spec-splice-{}-{num}.tmp", std::process::id()));
            match OpenOptions::new().write(true).create_new(true).open(&path) {
                // Own cleanup immediately, including subsequent write failures
                Ok(file) => break (Self { path, path_output: path_output.to_owned() }, file),
                // Skip temporary names already reserved by another process
                Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => continue,
                // Preserve the destination path in the diagnostic
                Err(source) => return Err(Error::Io { path: path_output.to_owned(), source }),
            }
        };
        // Preserve existing destination permissions when replacing a file
        match fs::metadata(path_output) {
            // Keep the destination's permission bits
            Ok(metadata) => file
                .set_permissions(metadata.permissions())
                .map_err(|source| Error::Io { path: path_output.to_owned(), source })?,
            // New destinations inherit the process's normal creation permissions
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            // Other metadata errors must not silently change permissions
            Err(source) => return Err(Error::Io { path: path_output.to_owned(), source }),
        }
        // Finish writing before allowing any destination to be replaced
        file.write_all(text.as_bytes())
            .and_then(|()| file.sync_all())
            .map_err(|source| Error::Io { path: path_output.to_owned(), source })?;
        Ok(pending)
    }

    fn commit(self) -> Result<(), Error> {
        fs::rename(&self.path, &self.path_output)
            .map_err(|source| Error::Io { path: self.path_output.clone(), source })
    }
}

impl Drop for PendingFile {
    fn drop(&mut self) {
        let _ = fs::remove_file(&self.path);
    }
}

// == Entry points

/// Copies untouched UTF-8 text and replaces recognized markers in source order.
fn splice_string(
    splicers: &mut [Box<dyn Run + '_>],
    source: &mut Source<'_>,
    ctx: &mut Context<'_>,
) -> Result<String, Error> {
    let mut text = String::with_capacity(source.remaining().len());
    // Consume markers once; generated text is never rescanned
    while !source.eos() {
        if let Some(text_spliced) = try_splice_anchors(splicers, source, ctx)? {
            text.push_str(&text_spliced);
        } else {
            // Copy whole characters so byte offsets remain valid UTF-8 boundaries
            let ch = source.remaining().chars().next().expect("nonempty source");
            text.push(ch);
            source.adv();
        }
    }
    Ok(text)
}

/// Renders skeletons with batch-wide anchors and returns warnings on failure too.
pub fn splice_strings(
    spec_el: &el::Spec,
    spec_pl: &pl::Spec,
    sources: &[(&str, &str)],
) -> (Result<Vec<String>, Error>, Vec<Report>) {
    let mut warnings = Vec::new();
    let result = (|| {
        // Collect forward targets before constructing the renderers
        let targets = anchor::collect(spec_el, sources, &mut warnings)?;
        let anchor = |subject: &super::super::adoc::pl::doc::doc::Subject| {
            context::resolve_prose(&targets, subject)
        };
        let mut ctx = Context::new(&targets, &anchor, &mut warnings);
        let mut splicers = init(spec_el, spec_pl);
        let mut texts = Vec::with_capacity(sources.len());
        // Share usage flags and counters through every input
        for (file, text) in sources {
            texts.push(splice_string(&mut splicers, &mut Source::new(file, text), &mut ctx)?);
        }
        // Report unused keys only after all sources rendered successfully
        for splicer in &splicers {
            splicer.warn_unused(ctx.warnings);
        }
        Ok(texts)
    })();
    (result, warnings)
}

/// Renders all inputs before staging and replacing output files.
///
/// Parsing and rendering failures leave every destination untouched.
/// Replacements are atomic per file; an I/O failure during the final rename
/// sequence can leave earlier files committed. Warnings survive every failure.
pub fn splice_files(
    spec_el: &el::Spec,
    spec_pl: &pl::Spec,
    path_pairs: &[(PathBuf, PathBuf)],
) -> (Result<(), Error>, Vec<Report>) {
    let mut warnings = Vec::new();
    let result = (|| {
        // Read all inputs before any path can be replaced by another output
        let mut sources = Vec::with_capacity(path_pairs.len());
        for (path_input, _) in path_pairs {
            let text = fs::read_to_string(path_input)
                .map_err(|source| Error::Io { path: path_input.clone(), source })?;
            sources.push((path_input.to_string_lossy().into_owned(), text));
        }
        // Complete the target prepass and rendering before filesystem mutations
        let sources: Vec<_> = sources
            .iter()
            .map(|(file, text)| (file.as_str(), text.as_str()))
            .collect();
        let (result, warnings_render) = splice_strings(spec_el, spec_pl, &sources);
        warnings = warnings_render;
        let texts = result?;
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
    })();
    (result, warnings)
}
