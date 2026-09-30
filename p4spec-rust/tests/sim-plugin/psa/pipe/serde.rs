use p4spec_rust::{
    lang::{
        common::source::Span,
        data::value::{
            ValueArena,
            external::{Encoding, decode_with, encode, encode_with},
            get, make,
        },
    },
    sim_plugin::{
        core::object::PacketIn,
        psa::{object::Register, pipe::ObjectState},
    },
};
use serde_json::json;

#[test]
fn test_derived_register_object_preserves_payload_and_annotations() {
    let mut arena = ValueArena::new();
    let span = Span::default();
    let value_typ = make::text(&mut arena, "register type".to_owned(), span.clone()).unwrap();
    let value = make::int(&mut arena, (-123).into(), span.clone()).unwrap();
    let object = ObjectState::Register(Register { value_typ, values: vec![value] });
    let mut json = encode(&arena, &object).unwrap();
    assert_eq!(json["Register"]["value_typ"]["node"], json!({"Text": "register type"}));
    assert_eq!(
        json["Register"]["values"][0]["node"],
        json!({"Num": {"Int": serde_json::to_value(num_bigint::BigInt::from(-123)).unwrap()}})
    );
    let mut arena_decoded = ValueArena::new();
    make::bool(&mut arena_decoded, false, span.clone()).unwrap();
    let object: ObjectState =
        decode_with(&mut arena_decoded, Encoding::ArenaIndependent, &json).unwrap();
    let ObjectState::Register(reg) = &object else {
        panic!("expected register");
    };
    assert_eq!(get::text(&arena_decoded, &reg.value_typ).unwrap(), "register type");
    assert_eq!(get::num(&arena_decoded, &reg.values[0]), get::num(&arena, &value));
    assert_eq!(arena_decoded.typ(&reg.values[0]), arena.typ(&value));
    assert_eq!(arena_decoded.span(&reg.values[0]), &span);
    let value_object = object
        .to_value(&mut arena_decoded, Encoding::ArenaIndependent)
        .unwrap();
    let object_decoded =
        ObjectState::from_value(&mut arena_decoded, Encoding::ArenaIndependent, &value_object)
            .unwrap();
    assert_eq!(encode(&arena_decoded, &object_decoded).unwrap(), json);
    json["Register"]["extra"] = json!(true);
    assert!(
        decode_with::<ObjectState>(&mut arena_decoded, Encoding::ArenaIndependent, &json).is_err()
    );
}

#[test]
fn test_object_packet_state_preserves_cursor_without_validation() {
    let mut arena = ValueArena::new();
    let mut pkt = PacketIn::init("AB").unwrap();
    pkt.idx = 9;
    let object = ObjectState::PacketIn(pkt);
    let json = encode(&arena, &object).unwrap();
    assert_eq!(
        decode_with::<ObjectState>(&mut arena, Encoding::ArenaIndependent, &json).unwrap(),
        object
    );
    for encoding in [Encoding::ArenaRelative, Encoding::ArenaIndependent] {
        let value_object = object.to_value(&mut arena, encoding).unwrap();
        assert_eq!(ObjectState::from_value(&mut arena, encoding, &value_object).unwrap(), object);
    }
}

#[test]
fn test_object_codec_rejects_malformed_variant_and_register_records() {
    let mut arena = ValueArena::new();
    for json in [
        json!({"PacketOut": {"bits": []}, "Meter": {}}),
        json!({"Register": {"values": []}}),
        json!({"Unknown": {}}),
    ] {
        assert!(decode_with::<ObjectState>(&mut arena, Encoding::ArenaIndependent, &json).is_err());
    }
}

#[test]
fn test_object_restores_nested_register_values_from_its_arena() {
    const DEPTH: usize = 8;
    let mut arena = ValueArena::new();
    let value_typ = make::bool(&mut arena, false, Span::default()).unwrap();
    let mut value = make::bool(&mut arena, true, Span::default()).unwrap();
    for _ in 0..DEPTH {
        value = make::opt(
            &mut arena,
            p4spec_rust::lang::data::typ::TypKind::Bool.into(),
            Some(value),
            Span::default(),
        )
        .unwrap();
    }
    let object = ObjectState::Register(Register { value_typ, values: vec![value] });
    let value_object = object
        .to_value(&mut arena, Encoding::ArenaIndependent)
        .unwrap();
    let object =
        ObjectState::from_value(&mut arena, Encoding::ArenaIndependent, &value_object).unwrap();
    let ObjectState::Register(reg) = object else {
        panic!("expected register");
    };
    let mut value = reg.values[0];
    for _ in 0..DEPTH {
        value = get::opt(&arena, &value).unwrap().unwrap();
    }
    assert!(get::bool(&arena, &value).unwrap());
    crate::diagnostic_fixture::assert_diagnostic(
        ObjectState::from_value(&mut arena, Encoding::ArenaIndependent, &value_typ).unwrap_err(),
        Some("runtime/extern-value-invalid"),
        "expected Extern value, got Bool",
    );
    stacker::grow(32 * 1024 * 1024, || drop(arena));
}

#[test]
fn test_architecture_codec_preserves_nested_registers_in_each_mode() {
    use p4spec_rust::sim_plugin::psa::{
        arch::Arch,
        packet::{Entrypoint, Packet},
    };

    for encoding in [Encoding::ArenaRelative, Encoding::ArenaIndependent] {
        let mut arena = ValueArena::new();
        let value_typ =
            make::text(&mut arena, "register type".to_owned(), Span::default()).unwrap();
        let value = make::int(&mut arena, 42.into(), Span::default()).unwrap();
        let object = ObjectState::Register(Register { value_typ, values: vec![value] });
        let value_object = object.to_value(&mut arena, encoding).unwrap();
        let arch = Arch {
            queue: [Packet {
                value_ctx: value_object,
                packet_in: PacketIn::init("AB").unwrap(),
                entrypoint: Entrypoint::Ingress,
            }]
            .into(),
            ..Arch::default()
        };
        let value_arch = arch.to_value(&mut arena, encoding).unwrap();
        let arch_decoded = Arch::from_value(&mut arena, encoding, &value_arch).unwrap();
        if encoding == Encoding::ArenaRelative {
            assert_eq!(arch_decoded, arch);
        }
        assert_eq!(
            &encode_with(&arena, encoding, &arch_decoded).unwrap(),
            get::external(&arena, &value_arch).unwrap().as_ref()
        );
        let object_decoded =
            ObjectState::from_value(&mut arena, encoding, &arch_decoded.queue[0].value_ctx)
                .unwrap();
        assert_eq!(encode(&arena, &object_decoded).unwrap(), encode(&arena, &object).unwrap());
        if encoding == Encoding::ArenaRelative {
            continue;
        }
        let json = encode(&arena, &value_arch).unwrap();
        let mut arena_decoded = ValueArena::new();
        let value_arch =
            decode_with(&mut arena_decoded, Encoding::ArenaIndependent, &json).unwrap();
        let arch_decoded =
            Arch::from_value(&mut arena_decoded, Encoding::ArenaIndependent, &value_arch).unwrap();
        let object_decoded = ObjectState::from_value(
            &mut arena_decoded,
            Encoding::ArenaIndependent,
            &arch_decoded.queue[0].value_ctx,
        )
        .unwrap();
        assert_eq!(
            encode(&arena_decoded, &object_decoded).unwrap(),
            encode(&arena, &object).unwrap()
        );
    }
}
