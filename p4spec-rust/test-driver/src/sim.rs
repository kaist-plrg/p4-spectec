use std::{
    collections::BTreeMap,
    fs,
    path::{Path, PathBuf},
    time::Instant,
};

use expect_test::{ExpectFile, expect_file};
use indicatif::{ProgressBar, ProgressStyle};

use p4spec_rust::lang::data::value::external::Encoding;

use p4spec_rust::runner::{Config, Spec};

use p4spec_rust::sim_plugin::{self, io::Tx};

use crate::{Error, Result, corpus};

struct Suite {
    arch: &'static str,
    name: &'static str,
    dir_p4: &'static str,
    dir_stf: &'static str,
    dir_patch: Option<&'static str>,
}

// Suites mirror p4spec/test/sim/dune. The six shared OCaml SL outcomes and
// ordered transmissions are byte-identical to the existing AL expectations
const SUITES: [Suite; 6] = [
    Suite {
        arch: "v1model",
        name: "v1model-p4c",
        dir_p4: "p4c/testdata/p4_16_samples",
        dir_stf: "p4c/testdata/p4_16_samples",
        dir_patch: Some("patches/v1model"),
    },
    Suite {
        arch: "v1model",
        name: "v1model-p4testgen",
        dir_p4: "p4c/testdata/p4_16_samples",
        dir_stf: "testdata/p4testgen",
        dir_patch: Some("patches/v1model"),
    },
    Suite {
        arch: "v1model",
        name: "v1model-custom",
        dir_p4: "testdata/custom",
        dir_stf: "testdata/custom",
        dir_patch: Some("patches/v1model"),
    },
    Suite {
        arch: "ebpf",
        name: "ebpf-p4c",
        dir_p4: "p4c/testdata/p4_16_samples",
        dir_stf: "p4c/testdata/p4_16_samples",
        dir_patch: None,
    },
    Suite {
        arch: "ebpf",
        name: "ebpf-p4testgen",
        dir_p4: "p4c/testdata/p4_16_samples",
        dir_stf: "testdata/p4testgen",
        dir_patch: None,
    },
    Suite {
        arch: "psa",
        name: "psa-p4c",
        dir_p4: "p4c/testdata/p4_16_samples",
        dir_stf: "p4c/testdata/p4_16_samples",
        dir_patch: None,
    },
];

struct Input {
    dir: PathBuf,
    path: PathBuf,
    patched: bool,
}

fn collect(dir: &str, suffix: &str) -> Result<Vec<Input>> {
    let dir = Path::new(dir);
    corpus::collect(dir, suffix)?
        .into_iter()
        .map(|path| {
            let path = path
                .strip_prefix(dir)
                .map_err(|error| Error::Invalid(error.to_string()))?
                .to_owned();
            Ok(Input { dir: dir.to_owned(), path, patched: false })
        })
        .collect()
}

fn patch(inputs: &mut [Input], patches: &[Input]) {
    for input in inputs {
        if let Some(patch) = patches
            .iter()
            .find(|patch| patch.path.file_stem() == input.path.file_stem())
        {
            input.dir.clone_from(&patch.dir);
            input.path.clone_from(&patch.path);
            input.patched = true;
        }
    }
}

struct Pair {
    path_p4: PathBuf,
    path_stf: PathBuf,
    patched: bool,
}

impl Suite {
    fn collect(&self) -> Result<Vec<Pair>> {
        let mut inputs_p4 = collect(self.dir_p4, ".p4")?;
        let include = match self.arch {
            "v1model" => "v1model.p4",
            "ebpf" => "ebpf_model.p4",
            "psa" => "bmv2/psa.p4",
            _ => {
                return Err(Error::Invalid(format!("unknown architecture {}", self.arch)));
            }
        };
        let mut inputs_arch = Vec::new();
        for input in inputs_p4.drain(..) {
            let text = fs::read_to_string(input.dir.join(&input.path))?;
            if text.contains(&format!("#include <{include}>"))
                || text.contains(&format!("#include \"{include}\""))
            {
                inputs_arch.push(input);
            }
        }
        let mut inputs_stf = collect(self.dir_stf, ".stf")?;
        if let Some(dir) = self.dir_patch {
            patch(&mut inputs_arch, &collect(dir, ".p4")?);
            patch(&mut inputs_stf, &collect(dir, ".stf")?);
        }
        let mut pairs = Vec::new();
        for input_p4 in inputs_arch {
            for input_stf in &inputs_stf {
                let stem_p4 = input_p4.path.file_stem();
                let dir_stf = input_stf.path.parent().unwrap_or(Path::new(""));
                if stem_p4 == Some(dir_stf.as_os_str())
                    || (input_p4.path.parent() == input_stf.path.parent()
                        && stem_p4 == input_stf.path.file_stem())
                {
                    pairs.push(Pair {
                        path_p4: input_p4.dir.join(&input_p4.path),
                        path_stf: input_stf.dir.join(&input_stf.path),
                        patched: input_p4.patched || input_stf.patched,
                    });
                }
            }
        }
        Ok(pairs)
    }
}

struct Results {
    expected: ExpectFile,
    records: BTreeMap<(PathBuf, PathBuf), String>,
}

impl Results {
    fn new(name: &str) -> Self {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("expected/sim")
            .join(format!("{name}.expected"));
        Self { expected: expect_file![path], records: BTreeMap::new() }
    }

    fn record(&mut self, pair: &Pair, status: &str, txs: &[Tx]) -> Result<()> {
        for path in [&pair.path_p4, &pair.path_stf] {
            if !path
                .to_str()
                .is_some_and(|path| !path.contains(['\t', '\r', '\n']))
            {
                return Err(Error::Invalid(format!(
                    "invalid simulation result path: {}",
                    path.display()
                )));
            }
        }
        let mut text =
            format!("{status}\t{}\t{}\n", pair.path_p4.display(), pair.path_stf.display());
        for tx in txs {
            text.push_str(&format!("tx\t{}\t{}\n", tx.port, tx.packet));
        }
        if self
            .records
            .insert((pair.path_p4.clone(), pair.path_stf.clone()), text)
            .is_some()
        {
            return Err(Error::Invalid(format!(
                "duplicate simulation result: {} / {}",
                pair.path_p4.display(),
                pair.path_stf.display()
            )));
        }
        Ok(())
    }

    fn check(self) {
        self.expected
            .assert_eq(&self.records.into_values().collect::<String>());
    }
}

pub fn run(det: bool) -> Result<()> {
    run_with(det, || {
        p4spec_rust::algo(["spec"])
            .map(Spec::Al)
            .map_err(|error| Error::Invalid(error.to_string()))
    })
}

pub fn run_sl(det: bool) -> Result<()> {
    run_with(det, || {
        p4spec_rust::structure(["spec"], true)
            .map(Spec::Sl)
            .map_err(|error| Error::Invalid(error.to_string()))
    })
}

pub fn run_pl(det: bool) -> Result<()> {
    run_with(det, || {
        p4spec_rust::prosify(["spec"])
            .map(Spec::Pl)
            .map_err(|error| Error::Invalid(error.to_string()))
    })
}

fn run_with<BuildSpec>(det: bool, build_spec: BuildSpec) -> Result<()>
where
    BuildSpec: Fn() -> Result<Spec>,
{
    let start = Instant::now();
    let mut excludes = corpus::collect_excludes(Path::new("excludes/static"))?;
    excludes.extend(corpus::collect_excludes(Path::new("excludes/dynamic"))?);
    let suites = SUITES
        .iter()
        .map(|suite| Ok((suite, suite.collect()?)))
        .collect::<Result<Vec<_>>>()?;
    let collected: usize = suites.iter().map(|(_, pairs)| pairs.len()).sum();
    let excluded = suites
        .iter()
        .flat_map(|(_, pairs)| pairs)
        .filter(|pair| {
            excludes.contains(&pair.path_p4.to_string_lossy().into_owned())
                || excludes.contains(&pair.path_stf.to_string_lossy().into_owned())
        })
        .count();
    eprintln!(
        "Simulation cache=on det={det}: collected={collected} excluded={excluded}; preparing specification"
    );
    let includes = vec![PathBuf::from("p4c/p4include")];
    let progress = ProgressBar::new(collected as u64).with_style(
        ProgressStyle::with_template("[{bar:24}] {pos}/{len} {elapsed_precise} {msg}")
            .map_err(|error| Error::Invalid(error.to_string()))?,
    );
    let mut executed = 0;
    let mut matched = 0;
    let mut patched = 0;
    for arch in ["v1model", "ebpf", "psa"] {
        let pairs_arch = suites
            .iter()
            .filter(|(suite, _)| suite.arch == arch)
            .flat_map(|(_, pairs)| pairs);
        let collected_arch = pairs_arch.clone().count();
        let excluded_arch = suites
            .iter()
            .filter(|(suite, _)| suite.arch == arch)
            .flat_map(|(_, pairs)| pairs)
            .filter(|pair| {
                excludes.contains(&pair.path_p4.to_string_lossy().into_owned())
                    || excludes.contains(&pair.path_stf.to_string_lossy().into_owned())
            })
            .count();
        let patched_arch = pairs_arch.filter(|pair| pair.patched).count();
        let mut simulator = sim_plugin::build(
            build_spec()?,
            arch,
            Config::new(true, det, false),
            Encoding::default(),
        )
        .map_err(|error| Error::Invalid(error.to_string()))?;
        for (suite, pairs) in suites.iter().filter(|(suite, _)| suite.arch == arch) {
            let mut results = Results::new(suite.name);
            for pair in pairs {
                let id = format!(
                    "{}:{}:{}",
                    suite.name,
                    pair.path_p4.display(),
                    pair.path_stf.display()
                );
                progress.set_message(pair.path_stf.display().to_string());
                if pair.patched {
                    patched += 1;
                }
                if excludes.contains(&pair.path_p4.to_string_lossy().into_owned())
                    || excludes.contains(&pair.path_stf.to_string_lossy().into_owned())
                {
                    results.record(pair, "exclude", &[])?;
                    progress.inc(1);
                    continue;
                }
                // File access failures are execution errors, never expected exclusions
                fs::File::open(&pair.path_p4)?;
                fs::File::open(&pair.path_stf)?;
                let mut txs = Vec::new();
                simulator
                    .run_stf_test(&includes, &pair.path_p4, &pair.path_stf, |tx| {
                        txs.push(tx.clone());
                    })
                    .map_err(|error| Error::Invalid(format!("{id}: {error}")))?;
                matched += txs.len();
                results.record(pair, "pass", &txs)?;
                executed += 1;
                progress.inc(1);
            }
            results.check();
        }
        eprintln!(
            "Simulation {arch} cache=on det={det}: collected={collected_arch} excluded={excluded_arch} executed={} patched={patched_arch}",
            collected_arch - excluded_arch
        );
    }
    progress.finish_with_message("complete");
    eprintln!(
        "Simulation cache=on det={det}: collected={collected} excluded={excluded} executed={executed} patched={patched} matches={matched} elapsed={:.3}s; all expected records matched",
        start.elapsed().as_secs_f64()
    );
    Ok(())
}
