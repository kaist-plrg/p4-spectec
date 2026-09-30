use p4spec_rust::{
    lang::data::value::external::Encoding,
    runner::{Config, Spec},
    sim_plugin::build,
};

#[test]
fn test_unsupported_architecture_precedes_spec_loading() {
    let report = match build(
        Spec::Al(vec![]),
        "unknown",
        Config::new(true, false, false),
        Encoding::default(),
    ) {
        Ok(_) => panic!("unsupported architecture accepted"),
        Err(report) => report,
    };
    let p4spec_rust::diagnostic::ReportKind::Cause(diagnostic) = &report.kind else {
        panic!("expected architecture diagnostic")
    };
    assert_eq!(diagnostic.code.as_deref(), Some("sim/architecture-unsupported"));
    assert_eq!(diagnostic.source, "sim");
    assert!(diagnostic.labels.is_empty());
}

#[test]
fn test_selected_architecture_uses_its_program_entry() {
    let path = std::env::temp_dir().join(format!("p4spec-stf-build-{}.p4", std::process::id()));
    let path_stf = path.with_extension("stf");
    std::fs::write(&path, "").unwrap();
    std::fs::write(&path_stf, "").unwrap();
    for (arch, relation) in
        [("ebpf", "EBPF_init"), ("psa", "PSA_init"), ("v1model", "V1Model_init")]
    {
        let mut simulator =
            build(Spec::Al(vec![]), arch, Config::new(true, false, false), Encoding::default())
                .unwrap();
        let error = match simulator.run_stf_test(&[], &path, &path_stf, |_| {}) {
            Ok(_) => panic!("an empty specification cannot initialize {arch}"),
            Err(error) => error,
        };
        let p4spec_rust::diagnostic::ReportKind::Cause(diagnostic) = &error.kind else {
            panic!("expected missing relation diagnostic");
        };
        assert_eq!(diagnostic.code.as_deref(), Some("runtime/binding-undefined"));
        assert!(error.to_string().contains(relation), "{error}");
    }
    std::fs::remove_file(path).unwrap();
    std::fs::remove_file(path_stf).unwrap();
}
