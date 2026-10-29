//! Building a definition with K, as `krun -d` and K-in-ST read it
//!
//! `Case::build` kompiles a definition (`kompile --backend llvm --enable-search`,
//! or `llvm-kompile` for KORE) and builds its K library, `runtime.so`, which
//! `interface::kore::offload` opens; `Case::init` makes the initial term of a
//! program. Each is made again only when what it is made from is newer.

use std::{
    fs,
    path::{Path, PathBuf},
    process::{Command, Output, Stdio},
    time::SystemTime,
};

use p4spectec::interface::kore::{info::Info, parse};

use super::suite::{KInStSuite, Program};
use crate::harness::{Error, Result};

/// A definition, kompiled.
pub struct Case<'a> {
    /// The directory of what is built for it.
    dir_build: PathBuf,
    /// Its suite.
    suite: &'a KInStSuite,
    /// The checkout of the definition, which `{src}` in the suite's commands
    /// names; empty for cases of its own.
    checkout: String,
    /// The sorts, collections, and binders of the kompiled definition.
    pub info: Info,
}

impl<'a> Case<'a> {
    /// Kompiles a definition with the options of its suite, in `checkout`, into
    /// `dir_build`, and builds its K library, if not up to date.
    pub fn build(
        source: &Path,
        suite: &'a KInStSuite,
        checkout: &str,
        dir_build: PathBuf,
    ) -> Result<Self> {
        let words = |words: &[String]| resolve(words, checkout);
        let kompiled = dir_build.join("kompiled");
        if stale(&kompiled.join("timestamp"), &[source]) {
            eprintln!("{}: kompiling", source.display());
            fs::create_dir_all(&dir_build)?;
            if suite.kore {
                build_kore(source, &kompiled)?;
            } else {
                run(
                    Command::new("kompile")
                        .args(["--backend", "llvm", "--enable-search", "--output-definition"])
                        .arg(&kompiled)
                        .args(words(&suite.kompile))
                        .arg(source),
                    None,
                    &format!("kompile {}", source.display()),
                )?;
            }
        }
        let path_definition = kompiled.join("definition.kore");
        // The library Offload opens, next to definition.kore
        if stale(&kompiled.join("runtime.so"), &[&path_definition]) {
            build_library(&kompiled, &words(&suite.runtime))?;
        }
        let definition =
            parse::parse_definition(&fs::read(&path_definition)?).map_err(|error| {
                Error::from(format!("{}: {}", path_definition.display(), error.message))
            })?;
        let checkout = checkout.to_owned();
        Ok(Self { dir_build, suite, checkout, info: Info::new(&definition) })
    }

    /// A command or options of the suite, in the definition's checkout.
    fn words(&self, words: &[String]) -> Vec<String> {
        resolve(words, &self.checkout)
    }

    /// The kompiled directory.
    pub fn kompiled(&self) -> PathBuf {
        self.dir_build.join("kompiled")
    }

    /// The kompiled definition, as krun reads it.
    pub fn path_definition(&self) -> PathBuf {
        self.kompiled().join("definition.kore")
    }

    /// The initial term krun makes of a program, made again when the program
    /// or the definition is newer: the program itself for KORE, what the suite's
    /// `init` writes, or the input that `krun --dry-run` gives the interpreter.
    pub fn init(&self, program: &Program) -> Result<PathBuf> {
        if self.suite.kore {
            return Ok(program.path.clone());
        }
        let path_init = self.dir_build.join(format!("{}.init.kore", program.name));
        let stamp = self.kompiled().join("timestamp");
        let mut sources = vec![program.path.as_path(), stamp.as_path()];
        sources.extend(program.input.as_deref());
        if !stale(&path_init, &sources) {
            return Ok(path_init);
        }
        if let Some((command, args)) = self.words(&self.suite.init).split_first() {
            let kompiled = self.kompiled();
            let args = args.iter().map(|arg| {
                arg.replace("{program}", &program.path.to_string_lossy())
                    .replace("{name}", &program.name)
                    .replace("{kompiled}", &kompiled.to_string_lossy())
                    .replace("{output}", &path_init.to_string_lossy())
            });
            let what = format!("{command} {}", program.path.display());
            run(Command::new(command).args(args), None, &what)?;
        } else {
            self.dry_run(program, &path_init)?;
        }
        Ok(path_init)
    }

    /// Writes to `path_init` the initial term `krun --dry-run` makes of a program,
    /// or of what the suite's `prepare` makes of it.
    fn dry_run(&self, program: &Program, path_init: &Path) -> Result<()> {
        let path_program = match self.words(&self.suite.prepare).split_first() {
            Some((command, args)) => {
                let path_prepared = self
                    .dir_build
                    .join(program.path.file_name().unwrap_or_default());
                let what = format!("{command} {}", program.path.display());
                let output = run(Command::new(command).args(args).arg(&program.path), None, &what)?;
                fs::write(&path_prepared, output.stdout)?;
                path_prepared
            }
            None => program.path.clone(),
        };
        let dir_temp = tempfile::tempdir()?;
        let output = run(
            Command::new("krun")
                .arg("-d")
                .arg(self.kompiled())
                .arg(&path_program)
                .args(self.words(&self.suite.krun))
                .args(["--save-temps", "--temp-dir"])
                .arg(dir_temp.path())
                .arg("--dry-run"),
            program.input.as_deref(),
            &format!("krun --dry-run {}", program.path.display()),
        )?;
        // krun prints the interpreter command: interpreter <initial term> <depth> <output>
        let path_term = text(&output)
            .lines()
            .find_map(|line| {
                let words: Vec<_> = line.split_whitespace().collect();
                let index = words
                    .iter()
                    .position(|word| word.ends_with("interpreter"))?;
                words.get(index + 1).map(PathBuf::from)
            })
            .ok_or_else(|| {
                Error::from(format!(
                    "krun --dry-run {}: no interpreter command",
                    program.path.display()
                ))
            })?;
        fs::copy(path_term, path_init)?;
        Ok(())
    }
}

/// `words` with `{src}` replaced by `checkout`.
fn resolve(words: &[String], checkout: &str) -> Vec<String> {
    words
        .iter()
        .map(|word| word.replace("{src}", checkout))
        .collect()
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
        let what = format!("{} {}", words[0], source.display());
        run(
            Command::new(words[0])
                .args(&words[1..])
                .current_dir(kompiled),
            None,
            &what,
        )?;
    }
    fs::write(kompiled.join("timestamp"), "")?;
    Ok(())
}

/// Builds the K library of a kompiled definition, `runtime.so`, from the
/// definition and its decision trees (`llvm-kompile definition.kore dt c`).
fn build_library(kompiled: &Path, flags: &[String]) -> Result<()> {
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
            .arg(kompiled.join("runtime.so"))
            .current_dir(dir_temp.path())
            .stdin(Stdio::null())
            .output()
    };
    let output = llvm_kompile(&[])?;
    if output.status.success() {
        return Ok(());
    }
    if !String::from_utf8_lossy(&output.stderr).contains("undefined version") {
        return Err(failed(&format!("llvm-kompile {}", kompiled.display()), &output));
    }
    let output = llvm_kompile(&["--save-temps", "-v"])?;
    relink(&text(&output), dir_temp.path())
}

/// Links the K library again, as `llvm-kompile -v` printed its link command,
/// with `@` renamed to `_AT_` in the symbols of the definition's object.
///
/// A rule label with `@`, as FUN's `@` operator gives, names a symbol
/// (`str_lit_…@…`) that the linker reads as `name@version`, and the library
/// does not link: `symbol … has undefined version …`.
fn relink(text_verbose: &str, dir_temp: &Path) -> Result<()> {
    let link = text_verbose
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
    let symbols =
        run(Command::new("llvm-nm-17").arg(object).current_dir(dir_temp), None, "llvm-nm-17")?;
    let renames: String = String::from_utf8_lossy(&symbols.stdout)
        .lines()
        .filter_map(|line| line.split_whitespace().last())
        .filter(|name| name.contains('@'))
        .map(|name| format!("{name} {}\n", name.replace('@', "_AT_")))
        .collect();
    fs::write(dir_temp.join("rename.txt"), renames)?;
    run(
        Command::new("llvm-objcopy-17")
            .args(["--redefine-syms=rename.txt", object])
            .current_dir(dir_temp),
        None,
        "llvm-objcopy-17",
    )?;
    run(
        Command::new("sh").args(["-c", &link]).current_dir(dir_temp),
        None,
        "relinking the K library",
    )?;
    Ok(())
}

/// Whether `target` is missing or older than any of `sources`.
fn stale(target: &Path, sources: &[&Path]) -> bool {
    let modified = |path: &Path| {
        fs::metadata(path)
            .and_then(|metadata| metadata.modified())
            .ok()
    };
    let Some(time_target): Option<SystemTime> = modified(target) else {
        return true;
    };
    sources
        .iter()
        .any(|source| modified(source).is_none_or(|time_source| time_source > time_target))
}

/// A standard input from `input`, or none.
pub fn stdin(input: Option<&Path>) -> Result<Stdio> {
    Ok(match input {
        Some(input) => Stdio::from(fs::File::open(input)?),
        None => Stdio::null(),
    })
}

/// Runs a command, `what`, with `input` as its standard input, failing with the
/// end of its output.
pub fn run(command: &mut Command, input: Option<&Path>, what: &str) -> Result<Output> {
    let output = command
        .stdin(stdin(input)?)
        .output()
        .map_err(|error| Error::from(format!("{what}: {error}")))?;
    if !output.status.success() {
        return Err(failed(what, &output));
    }
    Ok(output)
}

/// The failure of a command, `what`, with the end of its output.
fn failed(what: &str, output: &Output) -> Error {
    let text = String::from_utf8_lossy(&output.stderr).into_owned()
        + &String::from_utf8_lossy(&output.stdout);
    let tail = &text[text.floor_char_boundary(text.len().saturating_sub(2000))..];
    Error::from(format!("{what} failed: {tail}"))
}

/// The standard output and then the standard error of a command.
fn text(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).into_owned() + &String::from_utf8_lossy(&output.stderr)
}
