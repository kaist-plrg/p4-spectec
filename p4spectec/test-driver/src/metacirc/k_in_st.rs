//! K definitions run through a K-in-ST tower and compared with `krun`,
//! registered in `suites/metacirc/k-in-st.toml`
//!
//! A suite names a tower whose single level is spec-k over KORE, and its
//! definitions and programs, in one of two ways:
//!
//! * `root`, a directory of cases written for spec-k. A case is a directory under
//!   `root` with a definition, its programs, and what krun gives for them.
//! * `src`, `dir`, and `def`, a definition in a checkout of another repository,
//!   such as a language of the K tutorial: `src` names the environment variable
//!   that gives the checkout (`K_SRC`), and a suite whose variable is not set is
//!   skipped. Its `programs` are paths under `dir`, and what krun gives for them
//!   is in the directory `expected`.
//!
//! ```text
//! <name>.k               the definition of a case, one file of K, kompiled with its main module
//! <program>.<ext>        a program of a case, as krun takes it (`ext`, `prog` by default)
//! <program>.out          what krun's run gives, as `KoreInterface::print_runres` prints
//! <program>.depth        the step bound, if the run has one (or `depth` of the suite)
//! <program>.search       the final configurations K allows, for a program where spec-k
//!                        ends elsewhere than krun
//! ```
//!
//! A program reads `<program>.in` beside it, or in `inputs` of the suite, as its standard input.
//! A suite with `prepare` passes each program through that command first (its standard
//! output is what krun takes), as KWasm preprocesses its tests. A suite with `kore`
//! has a definition already in KORE and programs that are initial terms, as the tests
//! of the LLVM backend: it is built with `llvm-kompile` alone, and krun is not run.
//! A suite with `init` makes the initial term of each program with that command instead
//! of krun, as KEVM and KMIR make theirs with their own tools: in it, `{program}` is the
//! program, `{name}` its name, `{kompiled}` the kompiled directory, and `{output}` the
//! file to write. A program `<path>#<name>` is named `<name>` rather than by its file,
//! as one file of KEVM tests holds several.
//! In `kompile`, `krun`, `runtime`, `prepare`, `init`, and `programs`, `{src}` is the checkout.
//! Running a suite needs K: `run` kompiles the definition with the LLVM backend
//! (`kompile --enable-search`, and `kompile` of the suite), builds its K library
//! (`llvm-kompile … c`, `runtime.so`, with `runtime` of the suite as clang flags),
//! and makes the initial term of each program (`krun --dry-run`, and `krun` of the
//! suite), into `$CARGO_TARGET_DIR/k-in-st/`, again only when a source is newer.
//! It runs each program with `p4spectec::metacirc::krun` and compares its output
//! with `<program>.out`: the steps taken and how the run ends must be the same,
//! and the configurations the same in normal form (`interface::kore::normal`),
//! which covers how krun writes a configuration, not what K computes.
//! K leaves the choice among some rules open, as rules of the same priority that
//! both apply, or the alternatives of an or-pattern, and krun's run takes the
//! LLVM backend's choice. So a run that ends elsewhere still passes "by search"
//! if `<program>.search` is there and its final configuration is one of those.
//!
//! With `UPDATE_EXPECT=1`, `run` writes `<program>.out` from krun's own run
//! instead (the interpreter krun runs, with `--statistics`): the steps and the
//! final configuration, or for a run that fails, `error after n steps` and the
//! configuration before the failing step, or `error on the initial term`.
//! It writes `<program>.search` (the kompiled `search` binary from the initial
//! term) for a program whose run through spec-k still differs, and removes it otherwise.
//! The interpreter runs with `FASTTHROW` preloaded, so that a failing hook ends it at once.

use std::{
    fs,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    time::{Instant, SystemTime},
};

use serde::Deserialize;

use p4spectec::{
    interface::kore::{ast::Pattern, info::Info, normal, parse},
    metacirc::{self, TowerConfig},
};

use super::config::{ByBase, Configuration};
use crate::harness::{self, Error, Result};

/// Supplies configurations and suites of K-in-ST runs.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Registration {
    pub configs: Vec<Configuration>,
    pub suites: Vec<KInStSuite>,
}

/// Registers definitions and their programs, run through one tower per base:
/// the cases under `root`, or the definition `def` in `dir` of the checkout `src`.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct KInStSuite {
    pub name: String,
    pub towers: ByBase<PathBuf>,
    /// The directory of cases written for spec-k.
    pub root: Option<PathBuf>,
    /// The extension of the programs of its cases.
    #[serde(default = "ext_default")]
    pub ext: String,
    /// The environment variable that gives the checkout of the definition.
    pub src: Option<String>,
    /// The directory of the definition in the checkout.
    pub dir: Option<PathBuf>,
    /// The definition, in `dir`.
    pub def: Option<PathBuf>,
    /// The programs, in `dir` (or in the checkout, from `{src}`).
    #[serde(default)]
    pub programs: Vec<PathBuf>,
    /// The directory of the programs' `<program>.in`, in `dir`, if not beside them.
    pub inputs: Option<PathBuf>,
    /// The directory of what krun gives for the programs.
    pub expected: Option<PathBuf>,
    /// Extra options of kompile.
    #[serde(default)]
    pub kompile: Vec<String>,
    /// Extra options of krun.
    #[serde(default)]
    pub krun: Vec<String>,
    /// Extra clang flags of the K library, as for a plugin.
    #[serde(default)]
    pub runtime: Vec<String>,
    /// The step bound of every run.
    pub depth: Option<u64>,
    /// A command the programs pass through before krun takes them.
    #[serde(default)]
    pub prepare: Vec<String>,
    /// Whether the definition is KORE already, and the programs initial terms.
    #[serde(default)]
    pub kore: bool,
    /// A command that makes the initial term of a program, in place of krun.
    #[serde(default)]
    pub init: Vec<String>,
}

/// The options of a suite with `{src}` replaced by its checkout.
struct Options {
    kompile: Vec<String>,
    krun: Vec<String>,
    runtime: Vec<String>,
    prepare: Vec<String>,
    init: Vec<String>,
    kore: bool,
}

impl Options {
    /// The options of `suite`, in the checkout `src` (none for cases of its own).
    fn new(suite: &KInStSuite, src: &str) -> Self {
        let resolve = |words: &[String]| {
            words
                .iter()
                .map(|word| word.replace("{src}", src))
                .collect()
        };
        Self {
            kompile: resolve(&suite.kompile),
            krun: resolve(&suite.krun),
            runtime: resolve(&suite.runtime),
            prepare: resolve(&suite.prepare),
            init: resolve(&suite.init),
            kore: suite.kore,
        }
    }
}

/// The extension of programs when a suite names none.
fn ext_default() -> String {
    "prog".to_owned()
}

/// Exit codes of the interpreter for a run that fails: a stuck configuration,
/// and an uncaught exception, which aborts.
const EXIT_FAILED: [i32; 2] = [113, 134];

/// A library preloaded into the interpreter: a hook given an invalid argument
/// throws a C++ exception that nothing catches, and unwinding the interpreter's
/// stack to find that out takes up to a minute before it aborts. Exiting with
/// the status of abort at the throw gives the same outcome at once.
const FASTTHROW: &str = r#"#include <stdlib.h>
#include <unistd.h>

__attribute__((constructor)) static void only_this_process(void) {
  unsetenv("LD_PRELOAD");
}

void __cxa_throw(void *object, void *type, void (*destructor)(void *)) {
  static const char message[] = "interpreter: uncaught exception\n";
  (void)object, (void)type, (void)destructor;
  if (write(2, message, sizeof message - 1) < 0) {
  }
  _exit(134);
}
"#;

/// The directory of what is built for the cases.
fn dir_build() -> PathBuf {
    std::env::var_os("CARGO_TARGET_DIR")
        .map_or_else(|| PathBuf::from("p4spectec/target"), PathBuf::from)
        .join("k-in-st")
}

/// `FASTTHROW` compiled once with `cc`; none if it cannot be, and the interpreter
/// then takes its time to abort.
fn fastthrow() -> Option<PathBuf> {
    let dir = dir_build();
    let path_library = dir.join("fastthrow.so");
    if path_library.exists() {
        return Some(path_library);
    }
    let path_source = dir.join("fastthrow.c");
    fs::create_dir_all(&dir).ok()?;
    fs::write(&path_source, FASTTHROW).ok()?;
    let status = Command::new("cc")
        .args(["-shared", "-fPIC", "-O2", "-o"])
        .arg(&path_library)
        .arg(&path_source)
        .status()
        .ok()?;
    status.success().then_some(path_library)
}

/// Runs the suites under the named configuration, comparing each case with `krun`.
///
/// `name_suite` restricts the run to the registered suite of that name.
pub fn run(name_config: &str, name_suite: Option<&str>) -> Result<()> {
    let registration: Registration = harness::load("metacirc/k-in-st")?;
    let config = Configuration::find(&registration.configs, name_config)?;
    if config.skip_pending("k-in-st") {
        return Ok(());
    }
    if let Some(name) = name_suite
        && !registration.suites.iter().any(|suite| suite.name == name)
    {
        return Err(Error::from(format!("unknown K-in-ST suite {name}")));
    }
    let update = std::env::var_os("UPDATE_EXPECT").is_some();
    let text_mode = format!("metacirc k-in-st {}", config.describe());
    let start = Instant::now();
    let mut executed = 0;
    let mut mismatches = Vec::new();
    for suite in &registration.suites {
        // Skip suites of other names and suites without a tower for the base
        if name_suite.is_some_and(|name| suite.name != name) {
            continue;
        }
        let Some(path_tower) = suite.towers.get(config.base) else {
            continue;
        };
        let config_tower = TowerConfig::from_file(path_tower)
            .map_err(|error| Error::from(format!("{}: {error}", suite.name)))?;
        let Some(groups) = groups(suite)? else {
            continue;
        };
        for (case, programs) in groups {
            for program in programs {
                let start_program = Instant::now();
                let name = program.expected.display().to_string();
                let path_init = case.init(&program)?;
                let depth = match suite.depth {
                    Some(depth) => Some(depth),
                    None => read_depth(Path::new(&format!("{name}.depth")))?,
                };
                let input = program.input.as_deref();
                let path_expected = PathBuf::from(format!("{name}.out"));
                let path_search = PathBuf::from(format!("{name}.search"));
                if update {
                    let output = case.krun(&path_init, depth, input)?;
                    fs::create_dir_all(program.expected.parent().unwrap_or(Path::new(".")))?;
                    fs::write(&path_expected, output + "\n")?;
                }
                let expected = fs::read_to_string(&path_expected).map_err(|error| {
                    Error::from(format!(
                        "{}: {error} (UPDATE_EXPECT=1 writes it)",
                        path_expected.display()
                    ))
                })?;
                let paths_program = [case.path_definition(), path_init.clone()];
                let output = metacirc::krun(&config_tower, &paths_program, config.runner(), depth)
                    .unwrap_or_else(|error| format!("failed: {error}"));
                let differs = compare(&case.info, &output, &expected);
                if update {
                    // Only a run that ends elsewhere than krun's needs the finals K allows
                    if differs.is_err() {
                        case.search(&path_init, depth, input, &path_search)?;
                    } else if path_search.exists() {
                        fs::remove_file(&path_search)?;
                    }
                }
                let verdict = differs.map(|()| "pass").or_else(|difference| {
                    if !path_search.exists() {
                        return Err(difference);
                    }
                    let finals = fs::read(&path_search).map_err(|error| error.to_string())?;
                    reached(&case.info, &output, &finals)
                        .map(|()| "pass by search")
                        .map_err(|error| format!("{difference}, and {error}"))
                });
                let secs = start_program.elapsed().as_secs_f64();
                match &verdict {
                    Ok(pass) => eprintln!("{name}: {pass} ({secs:.1}s)"),
                    Err(difference) => {
                        eprintln!("{name}: FAIL, {difference} ({secs:.1}s)");
                        mismatches.push(format!("{name} ({}): {difference}", suite.name));
                    }
                }
                executed += 1;
            }
        }
    }
    let text_summary = format!(
        "{text_mode}: executed={executed} mismatched={} elapsed={:.3}s",
        mismatches.len(),
        start.elapsed().as_secs_f64()
    );
    eprintln!("{text_summary}");
    // Report every mismatched program before failing
    if !mismatches.is_empty() {
        return Err(Error::from(format!(
            "{text_mode}: runs differ from krun's:\n{}",
            mismatches.join("\n")
        )));
    }
    eprintln!("{text_mode}: all {executed} runs matched krun's");
    Ok(())
}

/// A program of a definition.
struct Program {
    /// The program, as krun takes it.
    path: PathBuf,
    /// Its name, which names its initial term and its expectations.
    name: String,
    /// Its standard input, if any.
    input: Option<PathBuf>,
    /// What krun gives for it, without the extension (`.out`, `.search`, `.depth`).
    expected: PathBuf,
}

impl Program {
    /// A program, with its input beside it or in `dir_inputs`, and its expectations
    /// in `dir_expected`.
    /// `path` may end in `#<name>`, which names the program.
    fn new(path: PathBuf, dir_inputs: Option<&Path>, dir_expected: &Path) -> Self {
        let text = path.to_string_lossy().into_owned();
        let (path, name) = match text.rsplit_once('#') {
            Some((path, name)) => (PathBuf::from(path), name.to_owned()),
            None => {
                let stem = path
                    .file_stem()
                    .unwrap_or_default()
                    .to_string_lossy()
                    .into_owned();
                (path, stem)
            }
        };
        let file = path
            .file_name()
            .unwrap_or_default()
            .to_string_lossy()
            .into_owned();
        let input = dir_inputs
            .unwrap_or_else(|| path.parent().unwrap_or(Path::new(".")))
            .join(format!("{file}.in"));
        let input = input.is_file().then_some(input);
        let expected = dir_expected.join(&name);
        Self { path, name, input, expected }
    }
}

/// A definition, built, with its programs.
type Group = (Case, Vec<Program>);

/// The definitions of a suite, built, with their programs;
/// none if the checkout they are in is not given.
fn groups(suite: &KInStSuite) -> Result<Option<Vec<Group>>> {
    let missing =
        |field: &str| Error::from(format!("{}: a suite needs `root` or `{field}`", suite.name));
    if let Some(root) = &suite.root {
        let mut groups = Vec::new();
        for dir_case in cases(root)? {
            let mut sources: Vec<_> = files(&dir_case)?
                .into_iter()
                .filter(|path| has_extension(path, "k"))
                .collect();
            let (Some(source), None) = (sources.pop(), sources.pop()) else {
                return Err(Error::from(format!("{}: a case has one .k file", dir_case.display())));
            };
            let case = Case::build(&source, Options::new(suite, ""), dir_build().join(&dir_case))?;
            let mut programs: Vec<_> = files(&dir_case)?
                .into_iter()
                .filter(|path| has_extension(path, &suite.ext))
                .map(|path| Program::new(path, None, &dir_case))
                .collect();
            programs.sort_by(|a, b| a.path.cmp(&b.path));
            groups.push((case, programs));
        }
        return Ok(Some(groups));
    }
    let src = suite.src.as_deref().ok_or_else(|| missing("src"))?;
    let Some(checkout) = std::env::var_os(src) else {
        eprintln!("metacirc k-in-st {}: skipped, {src} is not set", suite.name);
        return Ok(None);
    };
    let options = Options::new(suite, &checkout.to_string_lossy());
    let dir = PathBuf::from(&checkout).join(suite.dir.as_deref().ok_or_else(|| missing("dir"))?);
    let source = dir.join(suite.def.as_deref().ok_or_else(|| missing("def"))?);
    let dir_expected = suite
        .expected
        .as_deref()
        .ok_or_else(|| missing("expected"))?;
    let dir_inputs = suite.inputs.as_ref().map(|inputs| dir.join(inputs));
    let case = Case::build(&source, options, dir_build().join(&suite.name))?;
    let programs = suite
        .programs
        .iter()
        .map(|path| {
            // A path may start at the checkout, `{src}`, rather than in `dir`
            let path = PathBuf::from(
                path.to_string_lossy()
                    .replace("{src}", &checkout.to_string_lossy()),
            );
            Program::new(dir.join(path), dir_inputs.as_deref(), dir_expected)
        })
        .collect();
    Ok(Some(vec![(case, programs)]))
}

/// The cases under `root`: the directories that hold a `.k` file, in order.
fn cases(root: &Path) -> Result<Vec<PathBuf>> {
    let mut cases = Vec::new();
    let mut entries = files(root)?;
    entries.sort();
    if entries.iter().any(|path| has_extension(path, "k")) {
        cases.push(root.to_owned());
    }
    for entry in entries {
        if entry.is_dir() {
            cases.extend(self::cases(&entry)?);
        }
    }
    Ok(cases)
}

/// The entries of a directory.
fn files(dir: &Path) -> Result<Vec<PathBuf>> {
    Ok(fs::read_dir(dir)?
        .map(|entry| entry.map(|entry| entry.path()))
        .collect::<std::io::Result<Vec<_>>>()?)
}

/// Whether `path` is a file with extension `ext`.
fn has_extension(path: &Path, ext: &str) -> bool {
    path.is_file() && path.extension().is_some_and(|extension| extension == ext)
}

/// The time a file was last written; none if it is missing.
fn modified(path: &Path) -> Option<SystemTime> {
    fs::metadata(path)
        .and_then(|metadata| metadata.modified())
        .ok()
}

/// Whether `target` is missing or older than any of `sources`.
fn stale(target: &Path, sources: &[&Path]) -> bool {
    let Some(time_target) = modified(target) else {
        return true;
    };
    sources
        .iter()
        .any(|source| modified(source).is_none_or(|time_source| time_source > time_target))
}

/// Runs a command with `input` as its standard input, failing with the end of its output.
fn run_command(
    command: &mut Command,
    input: Option<&Path>,
    what: &str,
) -> Result<std::process::Output> {
    let stdin = match input {
        Some(input) => Stdio::from(fs::File::open(input)?),
        None => Stdio::null(),
    };
    let output = command
        .stdin(stdin)
        .output()
        .map_err(|error| Error::from(format!("{what}: {error}")))?;
    if !output.status.success() {
        let text = String::from_utf8_lossy(&output.stderr).into_owned()
            + &String::from_utf8_lossy(&output.stdout);
        let tail = &text[text.len().saturating_sub(2000)..];
        return Err(Error::from(format!("{what} failed: {tail}")));
    }
    Ok(output)
}

/// A definition, kompiled.
struct Case {
    /// The directory of what is built for it.
    dir_build: PathBuf,
    /// The options of its suite.
    options: Options,
    /// The sorts, collections, and binders of the kompiled definition.
    info: Info,
}

impl Case {
    /// Kompiles a definition with the options of its suite into `dir_build`,
    /// and builds its K library, if not up to date.
    fn build(source: &Path, options: Options, dir_build: PathBuf) -> Result<Self> {
        let kompiled = dir_build.join("kompiled");
        if stale(&kompiled.join("timestamp"), &[source]) {
            eprintln!("{}: kompiling", source.display());
            fs::create_dir_all(&dir_build)?;
            if options.kore {
                build_kore(source, &kompiled)?;
            } else {
                run_command(
                    Command::new("kompile")
                        .args(["--backend", "llvm", "--enable-search", "--output-definition"])
                        .arg(&kompiled)
                        .args(&options.kompile)
                        .arg(source),
                    None,
                    &format!("kompile {}", source.display()),
                )?;
            }
        }
        let path_definition = kompiled.join("definition.kore");
        // The library Offload opens, next to definition.kore
        if stale(&kompiled.join("runtime.so"), &[&path_definition]) {
            build_library(&kompiled, &options.runtime)?;
        }
        let definition =
            parse::parse_definition(&fs::read(&path_definition)?).map_err(|error| {
                Error::from(format!("{}: {}", path_definition.display(), error.message))
            })?;
        Ok(Self { dir_build, options, info: Info::new(&definition) })
    }

    /// The kompiled directory.
    fn kompiled(&self) -> PathBuf {
        self.dir_build.join("kompiled")
    }

    /// The kompiled definition, as krun reads it.
    fn path_definition(&self) -> PathBuf {
        self.kompiled().join("definition.kore")
    }

    /// The initial term krun makes of a program, made again when the program
    /// or the definition is newer: the input that `krun --dry-run` gives the interpreter.
    fn init(&self, program: &Program) -> Result<PathBuf> {
        // An initial term already
        if self.options.kore {
            return Ok(program.path.clone());
        }
        let path_init = self.dir_build.join(format!("{}.init.kore", program.name));
        let stamp = self.kompiled().join("timestamp");
        let mut sources = vec![program.path.as_path(), stamp.as_path()];
        sources.extend(program.input.as_deref());
        if !stale(&path_init, &sources) {
            return Ok(path_init);
        }
        // A tool of the language makes it
        if let Some((command, args)) = self.options.init.split_first() {
            let kompiled = self.kompiled();
            let args: Vec<String> = args
                .iter()
                .map(|arg| {
                    arg.replace("{program}", &program.path.to_string_lossy())
                        .replace("{name}", &program.name)
                        .replace("{kompiled}", &kompiled.to_string_lossy())
                        .replace("{output}", &path_init.to_string_lossy())
                })
                .collect();
            run_command(
                Command::new(command).args(&args),
                None,
                &format!("{command} {}", program.path.display()),
            )?;
            return Ok(path_init);
        }
        // What krun takes: the program, or what `prepare` makes of it
        let path_program = match self.options.prepare.split_first() {
            Some((command, args)) => {
                let name = program.path.file_name().unwrap_or_default();
                let path_prepared = self.dir_build.join(name);
                let output = run_command(
                    Command::new(command).args(args).arg(&program.path),
                    None,
                    &format!("{command} {}", program.path.display()),
                )?;
                fs::write(&path_prepared, output.stdout)?;
                path_prepared
            }
            None => program.path.clone(),
        };
        let dir_temp = tempfile::tempdir()?;
        let output = run_command(
            Command::new("krun")
                .arg("-d")
                .arg(self.kompiled())
                .arg(&path_program)
                .args(&self.options.krun)
                .args(["--save-temps", "--temp-dir"])
                .arg(dir_temp.path())
                .arg("--dry-run"),
            program.input.as_deref(),
            &format!("krun --dry-run {}", program.path.display()),
        )?;
        // krun prints the interpreter command: interpreter <initial term> <depth> <output>
        let text = String::from_utf8_lossy(&output.stdout).into_owned()
            + &String::from_utf8_lossy(&output.stderr);
        let path_term = text
            .lines()
            .filter_map(|line| {
                let words: Vec<_> = line.split_whitespace().collect();
                let index = words
                    .iter()
                    .position(|word| word.ends_with("interpreter"))?;
                words.get(index + 1).map(|word| PathBuf::from(*word))
            })
            .next()
            .ok_or_else(|| {
                Error::from(format!(
                    "krun --dry-run {}: no interpreter command",
                    program.path.display()
                ))
            })?;
        fs::copy(path_term, &path_init)?;
        Ok(path_init)
    }

    /// The interpreter's output after at most `depth` steps (all if none):
    /// the steps and the final configuration; none if the run fails.
    fn interpret(
        &self,
        path_init: &Path,
        depth: Option<u64>,
        input: Option<&Path>,
    ) -> Result<Option<String>> {
        let dir_temp = tempfile::tempdir()?;
        let path_out = dir_temp.path().join("result.kore");
        let mut command = Command::new(self.kompiled().join("interpreter"));
        if let Some(path_fastthrow) = fastthrow() {
            command.env("LD_PRELOAD", path_fastthrow);
        }
        let stdin = match input {
            Some(input) => Stdio::from(fs::File::open(input)?),
            None => Stdio::null(),
        };
        let status = command
            .arg(path_init)
            .arg(depth.map_or_else(|| "-1".to_owned(), |depth| depth.to_string()))
            .arg(&path_out)
            .arg("--statistics")
            .stdin(stdin)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()?;
        // A signal (no code) or a failing code is a failed run; any other code is the exit cell's
        let failed = status.code().is_none_or(|code| EXIT_FAILED.contains(&code));
        if failed || !path_out.exists() {
            return Ok(None);
        }
        Ok(Some(fs::read_to_string(path_out)?.trim_end().to_owned()))
    }

    /// What krun's run of an initial term gives, as `print_runres` prints a run:
    /// for a run that fails, the configuration before the failing step,
    /// found by running again with fewer steps.
    fn krun(&self, path_init: &Path, depth: Option<u64>, input: Option<&Path>) -> Result<String> {
        if let Some(output) = self.interpret(path_init, depth, input)? {
            return Ok(output);
        }
        if self.interpret(path_init, Some(0), input)?.is_none() {
            return Ok("error on the initial term".to_owned());
        }
        // Step ok + 1 succeeds before step bad fails; bisect between them
        let (mut ok, mut bad) = (0, 1);
        while self.interpret(path_init, Some(bad), input)?.is_some() {
            (ok, bad) = (bad, bad * 2);
        }
        while bad - ok > 1 {
            let mid = (ok + bad) / 2;
            if self.interpret(path_init, Some(mid), input)?.is_some() {
                ok = mid;
            } else {
                bad = mid;
            }
        }
        let output = self
            .interpret(path_init, Some(ok), input)?
            .ok_or_else(|| Error::from("the interpreter failed where it succeeded before"))?;
        let config = output.split_once('\n').map_or("", |(_, config)| config);
        Ok(format!("error after {ok} steps\n{config}"))
    }

    /// Writes the final configurations K allows from an initial term to `path_search`,
    /// as the kompiled `search` binary finds them.
    fn search(
        &self,
        path_init: &Path,
        depth: Option<u64>,
        input: Option<&Path>,
        path_search: &Path,
    ) -> Result<()> {
        if path_search.exists() {
            // The search binary appends to its output file
            fs::remove_file(path_search)?;
        }
        run_command(
            Command::new(self.kompiled().join("search"))
                .arg(path_init)
                .arg(depth.map_or_else(|| "-1".to_owned(), |depth| depth.to_string()))
                .arg(path_search),
            input,
            &format!("search {}", path_init.display()),
        )?;
        Ok(())
    }
}

/// A kompiled directory for a definition in KORE already, as `kompile --enable-search`
/// makes one: the definition, its decision trees, the interpreter, and the search binary.
fn build_kore(source: &Path, kompiled: &Path) -> Result<()> {
    fs::create_dir_all(kompiled.join("dt"))?;
    fs::copy(source, kompiled.join("definition.kore"))?;
    let commands: [&[&str]; 3] = [
        &["llvm-kompile-matching", "definition.kore", "qbaL", "dt", "0"],
        &["llvm-kompile", "definition.kore", "dt", "main", "-o", "interpreter"],
        &["llvm-kompile", "definition.kore", "dt", "search", "-o", "search"],
    ];
    for words in commands {
        run_command(
            Command::new(words[0])
                .args(&words[1..])
                .current_dir(kompiled),
            None,
            &format!("{} {}", words[0], source.display()),
        )?;
    }
    fs::write(kompiled.join("timestamp"), "")?;
    Ok(())
}

/// Builds the K library of a kompiled definition, `runtime.so`, from the
/// definition and its decision trees (`llvm-kompile definition.kore dt c`).
///
/// A rule label with `@` in it gives a symbol whose name the linker reads as a
/// version ("undefined version"). Such symbols are defined and used only in the
/// object of the definition, so they are renamed there, `@` to `_AT_`, and the
/// library is linked again with the command `llvm-kompile -v` printed.
fn build_library(kompiled: &Path, flags: &[String]) -> Result<()> {
    let path_library = kompiled.join("runtime.so");
    let dir_temp = tempfile::tempdir()?;
    let llvm_kompile = |extra: &[&str]| {
        Command::new("llvm-kompile")
            .arg(kompiled.join("definition.kore"))
            .arg(kompiled.join("dt"))
            .arg("c")
            .args(extra)
            .arg("--")
            .args(flags)
            .arg("-o")
            .arg(&path_library)
            .current_dir(dir_temp.path())
            .stdin(Stdio::null())
            .output()
    };
    let output = llvm_kompile(&[])?;
    if output.status.success() {
        return Ok(());
    }
    let stderr = String::from_utf8_lossy(&output.stderr);
    if !stderr.contains("undefined version") {
        let tail = &stderr[stderr.len().saturating_sub(2000)..];
        return Err(Error::from(format!("llvm-kompile {} failed: {tail}", kompiled.display())));
    }
    let output = llvm_kompile(&["--save-temps", "-v"])?;
    let text = String::from_utf8_lossy(&output.stdout).into_owned()
        + &String::from_utf8_lossy(&output.stderr);
    let link = text
        .lines()
        .rfind(|line| line.contains("clang++-") && line.contains(" -shared"))
        .map(|line| {
            line.trim_start()
                .trim_start_matches(['|', '+', ' '])
                .to_owned()
        })
        .ok_or_else(|| Error::from("llvm-kompile -v printed no link command"))?;
    let object = link
        .split_whitespace()
        .find(|word| word.contains("tmp.") && word.ends_with(".o"))
        .ok_or_else(|| Error::from("the link command has no object of the definition"))?;
    let symbols = run_command(
        Command::new("llvm-nm-17")
            .arg(object)
            .current_dir(dir_temp.path()),
        None,
        "llvm-nm-17",
    )?;
    let renames: String = String::from_utf8_lossy(&symbols.stdout)
        .lines()
        .filter_map(|line| line.split_whitespace().last())
        .filter(|name| name.contains('@'))
        .map(|name| format!("{name} {}\n", name.replace('@', "_AT_")))
        .collect();
    fs::write(dir_temp.path().join("rename.txt"), renames)?;
    run_command(
        Command::new("llvm-objcopy-17")
            .args(["--redefine-syms=rename.txt", object])
            .current_dir(dir_temp.path()),
        None,
        "llvm-objcopy-17",
    )?;
    run_command(
        Command::new("sh")
            .args(["-c", &link])
            .current_dir(dir_temp.path()),
        None,
        "relinking the K library",
    )?;
    Ok(())
}

/// The step bound in `path`, if the file exists.
fn read_depth(path: &Path) -> Result<Option<u64>> {
    if !path.exists() {
        return Ok(None);
    }
    let text = fs::read_to_string(path)?;
    let depth = text
        .trim()
        .parse()
        .map_err(|error| Error::from(format!("{}: {error}", path.display())))?;
    Ok(Some(depth))
}

/// Whether a run's output agrees with krun's: the same first line, which
/// gives the steps and how the run ended, and the same configuration in normal form.
fn compare(info: &Info, output: &str, expected: &str) -> std::result::Result<(), String> {
    let (head_output, config_output) = split(output);
    let (head_expected, config_expected) = split(expected);
    if head_output != head_expected {
        return Err(format!("{head_output}, krun: {head_expected}"));
    }
    let normal = |config: &str| {
        parse::parse_pattern(config.as_bytes())
            .map(|pattern| normal::normal_text(info, &pattern))
            .map_err(|error| error.message)
    };
    match (config_output, config_expected) {
        (None, None) => Ok(()),
        (Some(config_output), Some(config_expected)) => {
            if normal(config_output)? == normal(config_expected)? {
                Ok(())
            } else {
                Err("the configuration differs".to_owned())
            }
        }
        _ => Err("one run has a configuration, the other none".to_owned()),
    }
}

/// Whether a run ended in one of the final configurations K allows, `finals`,
/// as the `search` binary writes them: one configuration, or several under `\or`.
fn reached(info: &Info, output: &str, finals: &[u8]) -> std::result::Result<(), String> {
    let (head, config) = split(output);
    let config = match config {
        Some(config) if head.bytes().all(|byte| byte.is_ascii_digit()) => config,
        _ => return Err("the run did not end in a final configuration".to_owned()),
    };
    let parse = |src: &[u8]| parse::parse_pattern(src).map_err(|error| error.message);
    let normal = normal::normal_text(info, &parse(config.as_bytes())?);
    let finals = match parse(finals)? {
        Pattern::App(name, _, args) if name == "\\or" => args,
        Pattern::App(name, _, _) if name == "\\bottom" => vec![],
        pattern => vec![pattern],
    };
    if finals
        .iter()
        .any(|pattern| normal::normal_text(info, pattern) == normal)
    {
        Ok(())
    } else {
        Err(format!("is none of the {} final configurations K allows", finals.len()))
    }
}

/// The first line of an output and the configuration after it, if any.
fn split(output: &str) -> (&str, Option<&str>) {
    let output = output.trim_end();
    match output.split_once('\n') {
        Some((head, config)) => (head, Some(config)),
        None => (output, None),
    }
}
