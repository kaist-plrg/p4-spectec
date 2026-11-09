//! Specification statistics
//!
//! Parses specification files into EL and prints counts of files, lines, and definitions.

use std::{
    fs, io,
    path::{Path, PathBuf},
    process::ExitCode,
};

use clap::Parser;
use p4spectec::{
    diagnostic::{RenderConfig, Renderer, Report},
    lang::el::ast::{DefKind, Spec},
};

/// Print statistics of P4-SpecTec specifications.
#[derive(Parser)]
#[command(version)]
struct Cli {
    /// Specification files or directories in processing order.
    #[arg(required = true, value_name = "PATH")]
    paths: Vec<PathBuf>,
}

/// Definition counts of a parsed specification.
#[derive(Default)]
struct Counts {
    syntax: usize,
    relation: usize,
    rulegroup: usize,
    rule: usize,
    dec: usize,
    def: usize,
}

impl Counts {
    fn of(spec: &Spec) -> Self {
        let mut counts = Self::default();
        for def in spec {
            match &def.node {
                DefKind::ExternSyntax(_) | DefKind::Syntax(_) | DefKind::Typ(_) => {
                    counts.syntax += 1
                }
                DefKind::ExternRel(_) | DefKind::Rel(_) => counts.relation += 1,
                DefKind::RuleGroup(group) => {
                    counts.rulegroup += 1;
                    counts.rule += group.rules.len();
                }
                DefKind::ExternDec(_)
                | DefKind::BuiltinDec(_)
                | DefKind::TableDec(_)
                | DefKind::FuncDec(_) => counts.dec += 1,
                DefKind::TableDef(_) | DefKind::FuncDef(_) => counts.def += 1,
                DefKind::Var(_) | DefKind::Sep => {}
            }
        }
        counts
    }
}

/// Adds a file, or the `.watsup` files under a directory, as `parse_files` reads them.
fn expand_path(path: &Path, files: &mut Vec<PathBuf>) -> io::Result<()> {
    if !fs::metadata(path)?.is_dir() {
        files.push(path.to_owned());
        return Ok(());
    }
    let mut entries = fs::read_dir(path)?.collect::<io::Result<Vec<_>>>()?;
    entries.sort_by_key(fs::DirEntry::file_name);
    for entry in entries {
        let entry_path = entry.path();
        if fs::metadata(&entry_path)?.is_dir() {
            if entry.file_name() != "include" {
                expand_path(&entry_path, files)?;
            }
        } else if entry_path.to_string_lossy().ends_with(".watsup") {
            files.push(entry_path);
        }
    }
    Ok(())
}

/// Counts every line, including comments and blank lines.
fn count_lines(path: &Path) -> io::Result<usize> {
    let text = fs::read(path)?;
    let lines = text.iter().filter(|byte| **byte == b'\n').count();
    Ok(lines + usize::from(!text.is_empty() && !text.ends_with(b"\n")))
}

/// Renders a diagnostic to stderr.
fn render_report(report: &Report) {
    let mut renderer = Renderer::new(RenderConfig::default());
    if let Err(error) = renderer.render_to_stderr(report) {
        eprintln!("{report}\ndiagnostic rendering failed: {error}");
    }
}

fn run(cli: Cli) -> Result<(), ()> {
    // Parsing first reports missing paths and syntax errors as diagnostics
    let spec = p4spectec::parse(&cli.paths).map_err(|report| render_report(&report))?;

    let mut files = Vec::new();
    let mut lines = 0;
    for path in &cli.paths {
        expand_path(path, &mut files)
            .map_err(|error| eprintln!("cannot read {}: {error}", path.display()))?;
    }
    for path in &files {
        lines += count_lines(path)
            .map_err(|error| eprintln!("cannot read {}: {error}", path.display()))?;
    }

    let counts = Counts::of(&spec);
    let rows = [
        ("files", files.len()),
        ("LoC", lines),
        ("syntax", counts.syntax),
        ("relation", counts.relation),
        ("rulegroup", counts.rulegroup),
        ("rule", counts.rule),
        ("dec", counts.dec),
        ("def", counts.def),
    ];
    for (name, count) in rows {
        println!("{name:<10}{count:>8}");
    }
    Ok(())
}

fn main() -> ExitCode {
    match run(Cli::parse()) {
        Ok(()) => ExitCode::SUCCESS,
        Err(()) => ExitCode::FAILURE,
    }
}
