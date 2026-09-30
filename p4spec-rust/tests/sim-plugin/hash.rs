use num_bigint::BigInt;
use p4spec_rust::{
    frontend::parse::parse_mixop,
    lang::{
        common::{notation::mixop::Mixop, source::Span},
        data::{
            typ,
            value::{Value, ValueArena, make},
        },
    },
    sim_plugin::{hash, spec::pack},
    util::bigint,
};

#[test]
fn test_hashes_match_ocaml_vectors() {
    // Captured from the pinned backend-sim/hash.ml functions
    for (width, int, int_init, ints_expect) in [
        (0, 0_u64, 0, [0_u64, 0, 65535, 65535, 0]),
        (16, 10, 0, [1920, 2701982689, 65525, 10, 10]),
        (16, 4660, 0, [30477, 412718745, 60875, 4660, 4660]),
        (32, 305419896, 0, [13435, 1242107544, 38739, 26796, 305419896]),
        (32, 4294967295, 0, [37889, 4294967295, 0, 65535, 4294967295]),
        (16, 4660, 65280, [30477, 412718745, 61130, 4915, 4660]),
    ] {
        let bits = (width.into(), int.into());
        for (algo, int_expect) in ["crc16", "crc32", "csum16", "csum16_sub", "identity"]
            .into_iter()
            .zip(ints_expect)
        {
            assert_eq!(
                hash::compute_hash(algo, Some(&int_init.into()), &bits).unwrap(),
                int_expect.into(),
                "{algo}: {bits:?}"
            );
        }
    }
}

fn precision(arena: &mut ValueArena, shape: &str, ints: &[i64]) -> Value {
    let values: Vec<_> = ints
        .iter()
        .enumerate()
        .map(|(idx, int)| {
            if idx + 1 < ints.len() {
                make::nat(arena, BigInt::from(*int).try_into().unwrap(), Span::default()).unwrap()
            } else {
                make::int(arena, (*int).into(), Span::default()).unwrap()
            }
        })
        .collect();
    let mixop = parse_mixop(shape).unwrap();
    let value_case = Mixop::fill(&mixop, values).unwrap();
    let typ = typ::make::var(
        p4spec_rust::phrase!(node: "value".to_owned(), span: Span::default()),
        Vec::new(),
    );
    make::case(arena, typ.node.into(), value_case, Span::default()).unwrap()
}

#[test]
fn test_package_normalizes_signed_fields_and_pads_without_shifting() {
    let mut arena = ValueArena::new();
    let value_nibble = pack::p4_fixed_bit(&mut arena, 4.into(), 10.into()).unwrap();
    let bits = hash::package(&arena, &[value_nibble]).unwrap();
    assert_eq!(bits, (16.into(), 10.into()));
    assert_eq!(
        hash::compute_checksum("crc16", None, &arena, &[value_nibble]).unwrap(),
        1920.into()
    );
    let value_signed = precision(&mut arena, "nat S int", &[8, -1]);
    let value_var = precision(&mut arena, "nat '.' nat V int", &[32, 4, 5]);
    let bits = hash::package(&arena, &[value_nibble, value_signed, value_var]).unwrap();
    assert_eq!(bits, (16.into(), 0xAFF5.into()));
    let bits = hash::package(&arena, &[value_var, value_signed, value_nibble]).unwrap();
    assert_eq!(bits.1, 0x5FFA.into());
    let value_over = pack::p4_fixed_bit(&mut arena, 4.into(), 26.into()).unwrap();
    assert_eq!(hash::package(&arena, &[value_over]).unwrap().1, 10.into());
    let bits = hash::package(&arena, &[]).unwrap();
    assert_eq!(bits, (0.into(), 0.into()));
}

#[test]
fn test_hash_width_and_complement_boundaries() {
    assert_eq!(bigint::bitwise_neg(&74565.into(), &8.into()).unwrap(), 74682.into());
    assert_eq!(bigint::bitwise_neg(&74565.into(), &0.into()).unwrap(), 74565.into());
    assert_eq!(bigint::bitwise_neg(&74565.into(), &(-1).into()).unwrap(), 74565.into());
    for (int_init, int_sum, int_sub) in [(-65537, 60876, 4661), (131072, 60874, 4659)] {
        let bits = (16.into(), 4660.into());
        assert_eq!(
            hash::compute_hash("csum16", Some(&int_init.into()), &bits).unwrap(),
            int_sum.into()
        );
        assert_eq!(
            hash::compute_hash("csum16_sub", Some(&int_init.into()), &bits).unwrap(),
            int_sub.into()
        );
    }
    for algo in ["crc16", "crc32", "csum16", "csum16_sub"] {
        let report = hash::compute_hash(algo, None, &(4.into(), 10.into()))
            .unwrap_err()
            .into_report();
        let p4spec_rust::diagnostic::ReportKind::Cause(diagnostic) = &report.kind else {
            panic!("expected cause")
        };
        assert_eq!(diagnostic.code.as_deref(), Some("sim/hash-width-invalid"));
        let alignment = if algo.starts_with("crc") { 8 } else { 16 };
        assert_eq!(
            diagnostic.message,
            format!("hash width 4 must be a multiple of {alignment} bits")
        );
    }
    let bits = ((-1).into(), BigInt::from(123));
    assert_eq!(hash::compute_hash("identity", None, &bits).unwrap(), bits.1);
    assert!(hash::compute_hash("unsupported", None, &bits).is_err());
    let mut arena = ValueArena::new();
    let value_bad = precision(&mut arena, "nat W int", &[4, 10]);
    let value_bad = make::list(
        &mut arena,
        typ::make::list(typ::make::text()).node.into(),
        vec![value_bad],
        Span::default(),
    )
    .unwrap();
    assert!(hash::package(&arena, &[value_bad]).is_err());
}
