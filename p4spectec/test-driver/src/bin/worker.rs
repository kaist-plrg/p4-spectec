//! Worker executable that runs corpus cases for the integration tests under `tests/`
//!
//! `p4spectec-test-worker <options>... <command> <args>...` prepares the specification once,
//! then answers one request per stdin line (see `harness::worker`):
//! `run <language> <relation>` evaluates a P4 program through the relation,
//! `sim <language> <arch>` runs a `<p4>\t<stf>` pair on the native simulator,
//! and `p4parse` parses a P4 program, prints it, and parses the text again.
//! Options name the specification sources (`--spec PATH`), P4 include directories
//! (`--include PATH`), and the runner options enabled (`--cache`, `--det`, `--guard`, `--tco`);
//! none has a default.
//! It runs from the repository root, against which every path is spelled.

use std::{
    path::{Path, PathBuf},
    process::ExitCode,
};

use p4spectec::{
    interface::p4::{
        P4Interface,
        error::{P4Error, P4ErrorKind},
        parse::{parse_file, parse_string},
        preprocessor::preprocess,
        unparse::P4Unparser,
    },
    lang::{
        common::source::Span,
        data::{arena::Arena, typ},
    },
    runner::{Config, Runner},
};

use p4spectec_sim::Dummy;

use p4spectec_test::harness::{
    Error, Result,
    corpus::Outcome,
    interpreter::{self, Language},
    worker::Channel,
};

/// Worker options given before the command.
struct Options {
    /// Specification sources.
    paths_spec: Vec<PathBuf>,
    /// Include directories of P4 programs.
    paths_include: Vec<PathBuf>,
    /// Runner options.
    config: Config,
}

/// Splits `args` into options and the command with its arguments.
fn parse_args(args: &[String]) -> Result<(Options, Vec<&str>)> {
    let mut paths_spec = Vec::new();
    let mut paths_include = Vec::new();
    let mut config = Config { cache: false, det: false, guard: false, tco: false };
    let mut args = args.iter().map(String::as_str);
    let mut command = Vec::new();
    while let Some(arg) = args.next() {
        match arg {
            // Take the path after a path option
            "--spec" | "--include" => {
                let path = args
                    .next()
                    .ok_or_else(|| Error::from(format!("{arg} needs a path")))?;
                let paths = if arg == "--spec" { &mut paths_spec } else { &mut paths_include };
                paths.push(PathBuf::from(path));
            }
            // Enable a runner option
            "--cache" => config.cache = true,
            "--det" => config.det = true,
            "--guard" => config.guard = true,
            "--tco" => config.tco = true,
            // Keep the command and everything after it
            _ => {
                command.push(arg);
                command.extend(args);
                break;
            }
        }
    }
    Ok((Options { paths_spec, paths_include, config }, command))
}

// = Run

/// Serves P4 programs through `relation` under `language`.
fn serve_run(options: &Options, language: Language, relation: &str) -> Result<()> {
    let channel = Channel::open()?;
    let mut arena = Arena::new();
    let spec = language.spec(&options.paths_spec)?;
    let mut runner = interpreter::build(&mut arena, spec, options.config, Dummy::default())?;
    channel.serve(|request| {
        let outcome =
            run(&mut arena, &mut runner, &options.paths_include, relation, Path::new(request))?;
        Ok((outcome.name().to_owned(), String::new()))
    })
}

/// Evaluates the program at `path` through `relation`.
fn run(
    arena: &mut Arena,
    runner: &mut Runner<P4Interface, Dummy>,
    paths_include: &[PathBuf],
    relation: &str,
    path: &Path,
) -> Result<Outcome> {
    let source = preprocess(paths_include, path).map_err(|error| failure(path, error))?;
    // Reset before parsing: no value may cross this program boundary
    runner.reset();
    arena.reset_values();
    match parse_string(arena, path, &source) {
        // Evaluate a parsed program
        Ok(program) => match runner.eval_program(arena, relation, program).result {
            Ok(_) => Ok(Outcome::Pass),
            Err(_) => Ok(Outcome::Fail),
        },
        // Reject a program with a syntax error
        Err(P4Error { kind: P4ErrorKind::Syntax(_), .. }) => Ok(Outcome::Fail),
        // Report any other parser failure as an execution error
        Err(error) => Err(failure(path, error)),
    }
}

// = Sim

/// Serves STF tests on the native simulator of `arch` under `language`.
fn serve_sim(options: &Options, language: Language, arch: &str) -> Result<()> {
    let channel = Channel::open()?;
    let spec = language.spec(&options.paths_spec)?;
    let mut simulator = p4spectec_sim::build(spec, arch, options.config)?;
    let paths_include = &options.paths_include;
    channel.serve(|request| {
        let (path_p4, path_stf) = request
            .split_once('\t')
            .ok_or_else(|| Error::from(format!("invalid sim request: {request}")))?;
        let result =
            simulator.run_stf_test(paths_include, Path::new(path_p4), Path::new(path_stf), |_| {});
        // Answer a failing test with its simulation error
        match result {
            Ok(()) => Ok((Outcome::Pass.name().to_owned(), String::new())),
            Err(error) => Ok((Outcome::Fail.name().to_owned(), error.to_string())),
        }
    })
}

// = P4parse

/// Serves P4 parse/print/parse roundtrips.
fn serve_p4parse(options: &Options) -> Result<()> {
    let channel = Channel::open()?;
    let spec_al = p4spectec::algo(&options.paths_spec)?;
    let unparser = P4Unparser::from_al_spec(&spec_al);
    channel.serve(|request| {
        let outcome = roundtrip(&unparser, &options.paths_include, Path::new(request))?;
        Ok((outcome.name().to_owned(), String::new()))
    })
}

/// Parses, prints, and reparses the program at `path`.
fn roundtrip(unparser: &P4Unparser, paths_include: &[PathBuf], path: &Path) -> Result<Outcome> {
    let mut arena = Arena::new();
    let program = match parse_file(&mut arena, paths_include, path) {
        Ok(program) => program,
        Err(P4Error { kind: P4ErrorKind::Syntax(_), .. }) => return Ok(Outcome::ParseFail),
        Err(error) => return Err(failure(path, error)),
    };
    // The parser's root nonterminal is the program's declared type
    let typ_program = typ::make::var(
        p4spectec::phrase!(node: "p4program".into(), span: Span::default()),
        Vec::new(),
    );
    let text = unparser
        .render(&arena, &typ_program, &program)
        .map_err(|error| Error::from(format!("{}: {error}", path.display())))?;
    let program_roundtrip = match parse_string(&mut arena, path, &text) {
        Ok(program) => program,
        Err(P4Error { kind: P4ErrorKind::Syntax(_), .. }) => return Ok(Outcome::ReparseFail),
        Err(error) => return Err(failure(path, error)),
    };
    // Canonical bodies ignore source spans and type notes, like IL value equality
    if arena.value.canon_id(program.node) == arena.value.canon_id(program_roundtrip.node) {
        Ok(Outcome::Pass)
    } else {
        Ok(Outcome::RoundtripFail)
    }
}

// = Entry

/// Names the program in an error that is not a test outcome.
fn failure(path: &Path, error: P4Error) -> Error {
    Error::from(format!("{}: {}", path.display(), error.into_report()))
}

fn execute(args: &[String]) -> Result<()> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()?;
    std::env::set_current_dir(&root)?;
    let (options, command) = parse_args(args)?;
    match command.as_slice() {
        ["run", language, relation] => {
            serve_run(&options, Language::from_name(language)?, relation)
        }
        ["sim", language, arch] => serve_sim(&options, Language::from_name(language)?, arch),
        ["p4parse"] => serve_p4parse(&options),
        _ => Err(Error::from(format!("unknown worker command: {}", command.join(" ")))),
    }
}

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();
    // Run on a large stack: specification evaluation recurses deeply
    let result = std::thread::Builder::new()
        .name("test-worker".to_owned())
        .stack_size(64 * 1024 * 1024)
        .spawn(move || execute(&args).map_err(|error| error.to_string()));
    match result {
        Ok(thread) => match thread.join() {
            Ok(Ok(())) => ExitCode::SUCCESS,
            Ok(Err(error)) => {
                eprintln!("{error}");
                ExitCode::FAILURE
            }
            Err(_) => {
                eprintln!("test worker panicked");
                ExitCode::FAILURE
            }
        },
        Err(error) => {
            eprintln!("cannot start test worker: {error}");
            ExitCode::FAILURE
        }
    }
}
