use p4spec_rust::lang::data::value::ValueArena;
use p4spec_rust::{diagnostic::ReportKind, interp::shared::backtrack::Failure};
use std::path::Path;

use p4spec_rust::{
    frontend::parse::parse_files,
    interface::{self, p4::parse::parse_file},
    interp::al::{AlInterp, Config, context::Global},
    lang::data::value::Value,
    pass::{algo, elaborate},
    runner::{BuiltinInterface, Extern, Runner, Spec},
};

#[path = "core/mod.rs"]
mod core;
#[path = "dummy.rs"]
mod dummy;
#[path = "io.rs"]
mod io;

fn repo() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap()
}

fn runner<Ext: Extern>(external: Ext) -> Runner<AlInterp, BuiltinInterface, Ext> {
    runner_from_spec(&repo().join("spec"), external)
}

fn runner_from_spec<Ext: Extern>(
    spec: &Path,
    external: Ext,
) -> Runner<AlInterp, BuiltinInterface, Ext> {
    let spec_el = parse_files([spec]).expect("native specification parsing");
    let spec_il = elaborate::convert(spec_el).expect("native elaboration");
    let spec_al = algo::convert(spec_il).expect("native algorithmic conversion");
    let spec = Spec::Al(spec_al);
    let interface = interface::p4(&spec);
    let Spec::Al(spec_al) = spec else { unreachable!() };
    Runner::new(
        Global::load(spec_al).unwrap(),
        AlInterp::new(Config::new(true, false, false)),
        interface,
        external,
    )
}

fn has_extern_failure(failure: &Failure, expected: &str) -> bool {
    let Failure::Fatal(report) = failure else { return false };
    let mut pending = vec![report.as_ref()];
    while let Some(report) = pending.pop() {
        if let ReportKind::Cause(diagnostic) = &report.kind
            && diagnostic.code.as_deref() == Some("runtime/extern-failed")
            && diagnostic.message == expected
        {
            return true;
        }
        pending.extend(&report.children);
    }
    false
}

fn parse_program(arena: &mut ValueArena, path: &Path) -> Value {
    parse_file(arena, &[repo().join("p4c/p4include")], path).expect("native P4 parsing")
}

#[path = "hash.rs"]
mod hash;

#[path = "table.rs"]
mod table;

#[path = "ebpf/mod.rs"]
mod ebpf;

#[path = "psa/mod.rs"]
mod psa;

#[path = "v1model/mod.rs"]
mod v1model;

#[path = "build.rs"]
mod build;
#[path = "runner.rs"]
mod runner;
