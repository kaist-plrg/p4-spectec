//! Scoped stdout suppression during corpus execution
//!
//! Guards own process-wide redirection and must be used in lexical order.
//! Finish suppression before checking expectations, whose diffs use stdout.

use std::{
    fs::File,
    io::{self, Write},
    os::fd::{AsFd, OwnedFd},
};

#[cfg(test)]
mod tests;

/// Discards process stdout until explicitly restored or dropped.
pub(crate) struct SilenceStdout {
    fd_stdout: Option<OwnedFd>,
}

impl SilenceStdout {
    /// Saves stdout and redirects it to the null device.
    pub(crate) fn new() -> io::Result<Self> {
        io::stdout().flush()?;
        let fd_stdout = io::stdout().as_fd().try_clone_to_owned()?;
        let file_null = File::options().write(true).open("/dev/null")?;
        rustix::stdio::dup2_stdout(&file_null)?;
        Ok(Self { fd_stdout: Some(fd_stdout) })
    }

    /// Restores stdout and reports flush or redirection failures.
    pub(crate) fn finish(mut self) -> io::Result<()> {
        self.restore()
    }

    /// Flushes suppressed bytes and restores the saved descriptor.
    fn restore(&mut self) -> io::Result<()> {
        // Skip guards already restored by finish
        let Some(fd_stdout) = &self.fd_stdout else {
            return Ok(());
        };
        // Restore stdout even when flushing fails
        let result = io::stdout().flush();
        rustix::stdio::dup2_stdout(fd_stdout)?;
        // Disarm cleanup only after redirection succeeds
        self.fd_stdout = None;
        result
    }
}

impl Drop for SilenceStdout {
    fn drop(&mut self) {
        if let Err(error) = self.restore() {
            eprintln!("cannot restore test-driver stdout: {error}");
        }
    }
}
