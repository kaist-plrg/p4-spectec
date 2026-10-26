//! Running a kompiled K definition with the K specification
//!
//! As the LLVM backend's interpreter does with krun's initial term:
//! `Krun: kdefinition |- kore nat? : runres` evaluates the term
//! and rewrites it until no rule applies, or up to a number of steps.
//! A checked run (`check`) takes the specification's `Run` one step at a
//! time instead, from the configuration `Krun` gives with no step.

use std::{path::Path, rc::Rc, time::Instant};

use crate::lang::{
    common::{
        prim::num::{self, Natural},
        source::Span,
    },
    data::{
        arena::Arena,
        typ,
        value::flat::{self as value, Value},
    },
};

use crate::runner::{Config, RunError, Runner, Spec};

use num_bigint::BigInt;

use super::{KoreInterface, ast::Pattern, check::Checker, externs::KoreExtern, parse};

/// A definition and an initial term read for a run.
pub struct Krun {
    pub arena: Arena,
    runner: Runner<KoreInterface, KoreExtern>,
    /// `Krun`'s inputs: the definition, the initial term, the step limit.
    inputs: [Value; 3],
    /// The number of axioms in the definition.
    pub axioms: usize,
    /// Seconds to read the definition and the initial term.
    pub secs_load: f64,
}

impl Krun {
    /// Prepares the specification and reads a definition and an initial term.
    pub fn load(
        spec: Spec,
        config: Config,
        path_definition: &Path,
        path_library: Option<&Path>,
        path_term: &Path,
        depth: Option<u64>,
    ) -> Result<Self, RunError> {
        let mut arena = Arena::new();
        let interface = super::super::kore();
        let mut runner = Runner::new(&mut arena, spec, config, interface, KoreExtern::default())
            .map_err(RunError::Build)?;

        // Open the runtime, read the definition, then the initial term against its symbols
        let time = Instant::now();
        if let Some(path_library) = path_library {
            runner
                .interface
                .open_runtime(path_library)
                .map_err(|message| {
                    RunError::Parse(super::diagnostic(super::KORE_RUNTIME, message))
                })?;
        }
        let (value_definition, axioms) = runner
            .interface
            .load_definition(&mut arena, path_definition)
            .map_err(RunError::Parse)?;
        runner.external.attach(runner.interface.loaded());
        let value_term = runner
            .parse_program(&mut arena, &[], &[path_term.to_path_buf()])
            .map_err(RunError::Parse)?;
        let value_depth = depth_value(&mut arena, depth).map_err(RunError::Parse)?;
        let secs_load = time.elapsed().as_secs_f64();

        // Krun's inputs, kept for the run
        let inputs = [value_definition, value_term, value_depth];
        Ok(Self { arena, runner, inputs, axioms, secs_load })
    }

    /// Runs `Krun`; its `runres`.
    pub fn run(&mut self) -> Result<Value, RunError> {
        let values = self
            .runner
            .eval_rel(&mut self.arena, "Krun", &self.inputs)
            .result
            .map_err(RunError::Eval)?;
        let [result] = values[..] else { unreachable!("Krun has one output") };
        Ok(result)
    }
}

/// `nat?`: the step limit, if any.
fn depth_value(
    arena: &mut Arena,
    depth: Option<u64>,
) -> Result<Value, crate::runner::InterfaceError> {
    let span = Span::default();
    let to_error = |error: crate::lang::data::value::ValueError| {
        crate::runner::InterfaceError::from(crate::interface::builtin::BuiltinError::from(error))
    };
    let value_depth = depth
        .map(|n| {
            let n = Natural::try_from(num_bigint::BigInt::from(n)).expect("a u64 is natural");
            value::make::nat(arena, n, span)
        })
        .transpose()
        .map_err(to_error)?;
    let typ = Rc::new(typ::make::opt(typ::make::nat()).node);
    value::make::opt(arena, typ, value_depth, span).map_err(to_error)
}

/// The outcome of `Krun`, its `runres`.
pub enum Outcome {
    /// The configuration after the steps taken, at the end or the step limit.
    Final(String, Pattern),
    /// The configuration where a step failed, after the steps taken.
    Error(String, Pattern),
    /// The initial term cannot be evaluated.
    InitFail,
}

impl Krun {
    /// A `runres`: whether it is `FINAL` (or `ERROR`), the steps, and the
    /// configuration; none for `INITFAIL`.
    fn runres(&mut self, result: &Value) -> Result<Option<(bool, u64, Value)>, String> {
        let arena = &mut self.arena;
        let selected = value::get::matches_string! { arena, result,
            "FINAL nat term" => |values| Some((true, values.to_vec())),
            "ERROR nat term" => |values| Some((false, values.to_vec())),
            _ => None,
        };
        let Some((is_final, values)) = selected else {
            return Ok(None);
        };
        let [steps, term] = values[..] else {
            return Err("a runres has two arguments".to_owned());
        };
        let steps = value::get::num(&self.arena, &steps).map_err(|error| error.to_string())?;
        let steps = u64::try_from(num::to_int(steps)).map_err(|error| error.to_string())?;
        Ok(Some((is_final, steps, term)))
    }

    /// A `runres` with its configuration in KORE.
    pub fn outcome(&mut self, result: &Value) -> Result<Outcome, String> {
        let Some((is_final, steps, term)) = self.runres(result)? else {
            return Ok(Outcome::InitFail);
        };
        let kore = self.runner.interface.kore_of_term(&self.arena, &term)?;
        let steps = steps.to_string();
        Ok(if is_final { Outcome::Final(steps, kore) } else { Outcome::Error(steps, kore) })
    }

    /// Runs step by step, checking each step against K; the outcome, and the
    /// check's report and verdict.
    pub fn run_checked(
        &mut self,
        kompiled: &Path,
        depth: Option<u64>,
    ) -> Result<(Outcome, String, bool), String> {
        let eval_error = |error: RunError| error.to_string();
        let run = |krun: &mut Self, name: &str, inputs: &[Value]| {
            krun.runner
                .eval_rel(&mut krun.arena, name, inputs)
                .result
                .map_err(|error| eval_error(RunError::Eval(error)))
        };
        // The loaded definition, and the initial configuration after no step
        let [value_kdefinition, value_term, _] = self.inputs;
        let [value_definition] = run(self, "Load", &[value_kdefinition])?[..] else {
            return Err("Load has one output".to_owned());
        };
        let value_none = depth_value(&mut self.arena, Some(0)).map_err(|e| e.to_string())?;
        let [result] = run(self, "Krun", &[value_kdefinition, value_term, value_none])?[..] else {
            return Err("Krun has one output".to_owned());
        };
        let Some((_, _, mut term)) = self.runres(&result)? else {
            let checker = Checker::new(kompiled);
            let (report, ok) = checker.finish(false)?;
            return Ok((Outcome::InitFail, report, ok));
        };
        let value_zero = value::make::nat(
            &mut self.arena,
            Natural::try_from(BigInt::from(0)).expect("natural"),
            Span::default(),
        )
        .map_err(|e| e.to_string())?;
        let value_one = depth_value(&mut self.arena, Some(1)).map_err(|e| e.to_string())?;
        // One step at a time: Run with a limit of one step, checked by K
        let mut checker = Checker::new(kompiled);
        let mut steps = 0;
        let (is_final, ended) = loop {
            let pattern = self.runner.interface.kore_of_term(&self.arena, &term)?;
            let text = self.runner.interface.normal_text(&pattern)?;
            let interface = &self.runner.interface;
            checker.step(pattern, text, |pattern| {
                interface.normal_text(pattern).unwrap_or_default()
            })?;
            if depth == Some(steps) {
                break (true, false);
            }
            let [result] = run(self, "Run", &[value_definition, term, value_zero, value_one])?[..]
            else {
                return Err("Run has one output".to_owned());
            };
            // A step taken, the end of the run, or a failing step
            match self.runres(&result)? {
                Some((true, 1, next)) => {
                    term = next;
                    steps += 1;
                }
                Some((true, _, _)) => break (true, true),
                _ => break (false, false),
            }
        };
        let (report, ok) = checker.finish(ended)?;
        let pattern = self.runner.interface.kore_of_term(&self.arena, &term)?;
        let outcome = if is_final {
            Outcome::Final(steps.to_string(), pattern)
        } else {
            Outcome::Error(steps.to_string(), pattern)
        };
        Ok((outcome, report, ok))
    }
}

impl Krun {
    /// A configuration and krun's, read from its output file, in normal form
    /// (`normal`). The file may start with the step count, as the interpreter
    /// writes it with `--statistics`.
    pub fn normal_texts(
        &self,
        pattern: &Pattern,
        path_expected: &Path,
    ) -> Result<(String, String), String> {
        let src = std::fs::read(path_expected)
            .map_err(|error| format!("{}: {error}", path_expected.display()))?;
        let src = match src.iter().position(|&byte| byte == b'\n') {
            Some(end) if src[..end].iter().all(u8::is_ascii_digit) && end > 0 => &src[end + 1..],
            _ => &src[..],
        };
        let expected = parse::parse_pattern(src).map_err(|error| {
            format!("{}:{}: {}", path_expected.display(), error.line, error.message)
        })?;
        let interface = &self.runner.interface;
        Ok((interface.normal_text(pattern)?, interface.normal_text(&expected)?))
    }
}
