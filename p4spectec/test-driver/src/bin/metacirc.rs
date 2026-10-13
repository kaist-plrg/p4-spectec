//! Acceptance entry point for AL meta representations
//!
//! The default checks include guarded semantics
//! `--parse-only` selects compiler and codec checks without interpretation

use clap::Parser;
use p4spectec_test::metacirc::{self, Result};

/// Selects AL representation acceptance checks.
#[derive(Parser)]
#[command(name = "metacirc-test")]
struct Cli {
    /// Check parsing and codecs without evaluating a specification
    #[arg(long)]
    parse_only: bool,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    if cli.parse_only { metacirc::parse::run() } else { metacirc::run() }
}
