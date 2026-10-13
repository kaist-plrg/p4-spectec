//! Execution entry point for the AL metacircular specification

use clap::Parser;
use p4spectec_test::metacirc::{self, Result};
use std::path::PathBuf;

#[derive(Parser)]
struct Cli {
    /// The standalone metacirc executable to exercise.
    #[arg(long)]
    cli: PathBuf,
}

fn main() -> Result<()> {
    metacirc::run(&Cli::parse().cli)
}
