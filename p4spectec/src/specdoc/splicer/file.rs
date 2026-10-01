//! Staged output files and symlink destination resolution
//!
//! With `out.adoc -> generated/chapter.adoc`, resolve the final target first,
//! write `.p4spec-splice-<pid>-<counter>.tmp` beside it, then rename that file
//! over `generated/chapter.adoc`, preserving the `out.adoc` symlink.
//! Dropping an uncommitted `PendingFile` removes its temporary file.
//! Each rename is atomic; a later commit failure leaves earlier commits in place.

use std::{
    fs::{self, OpenOptions},
    io::Write,
    path::{Path, PathBuf},
    sync::atomic::{AtomicU64, Ordering},
};

use super::error::{self, Error};

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

/// Owns a staged output until commit or cleanup on drop.
pub(super) struct PendingFile {
    path: PathBuf,
    path_output: PathBuf,
}

impl PendingFile {
    /// Stages a complete output beside its destination for an atomic rename.
    pub(super) fn new(path_output: &Path, text: &str) -> Result<Self, Error> {
        // Follow output links while permitting a new final destination
        let path_target =
            resolve_output(path_output).map_err(|source| error::io(path_output, source))?;
        let path_output = path_target.as_path();
        // Create the parent only after every input has rendered successfully
        let parent = path_output
            .parent()
            .filter(|path| !path.as_os_str().is_empty())
            .unwrap_or(Path::new("."));
        fs::create_dir_all(parent).map_err(|source| error::io(parent, source))?;
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
                Err(source) => return Err(error::io(path_output, source)),
            }
        };
        // Preserve existing destination permissions when replacing a file
        match fs::metadata(path_output) {
            // Keep the destination's permission bits
            Ok(metadata) => file
                .set_permissions(metadata.permissions())
                .map_err(|source| error::io(path_output, source))?,
            // New destinations inherit the process's normal creation permissions
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            // Other metadata errors must not silently change permissions
            Err(source) => return Err(error::io(path_output, source)),
        }
        // Finish writing before allowing any destination to be replaced
        file.write_all(text.as_bytes())
            .and_then(|()| file.sync_all())
            .map_err(|source| error::io(path_output, source))?;
        Ok(pending)
    }

    /// Replaces one destination with its fully written temporary file.
    pub(super) fn commit(self) -> Result<(), Error> {
        fs::rename(&self.path, &self.path_output)
            .map_err(|source| error::io(&self.path_output, source))
    }
}

impl Drop for PendingFile {
    fn drop(&mut self) {
        let _ = fs::remove_file(&self.path);
    }
}
