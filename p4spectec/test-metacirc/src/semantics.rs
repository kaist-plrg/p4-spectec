//! Executable DSL regressions under guarded native runners
//!
//! The meta specification and semantic fixture pass through AL, SL, and PL.
//! Calls check signatures, capture avoidance, membership, and evaluation.

use p4spectec::{
    SpecLang,
    lang::data::arena::Arena,
    runner::{self, Config, NullExtern, Spec},
};

use crate::{Result, check_guard, fixture, source};

/// Runs semantic fixtures through every native preparation path.
pub fn run() -> Result<()> {
    let paths = [source("spec-meta"), fixture("semantics.watsup")];

    // Rebuild each stage with guards and candidate determinism enabled
    for (name, lang) in [("AL", SpecLang::Al), ("SL", SpecLang::Sl), ("PL", SpecLang::Pl)] {
        let mut arena = Arena::new();
        let spec = p4spectec::runner_spec_with_warnings(lang, &paths).0?;
        let config = Config::new(false, true, true);
        let mut runner = match spec {
            Spec::Al(spec_al) => runner::build_al(&mut arena, spec_al, config, NullExtern)?,
            Spec::Sl(spec_sl) => runner::build_sl(&mut arena, spec_sl, config, NullExtern)?,
            Spec::Pl(spec_pl) => runner::build_pl(&mut arena, spec_pl, config, NullExtern)?,
        };

        // Evaluate separate entries to identify each regression
        for name_func in
            ["check_signatures", "check_capture", "check_membership", "check_call", "check_slice"]
        {
            check_guard(&mut runner, &mut arena, name_func, &[])?;
        }
        eprintln!("metacirc semantics: {name} signatures, capture, membership, calls, slices");
    }
    Ok(())
}
