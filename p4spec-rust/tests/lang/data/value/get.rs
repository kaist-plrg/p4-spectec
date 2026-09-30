//! Value projection error tests

use p4spec_rust::lang::{
    common::source::Span,
    data::value::{ValueArena, ValueError, ValueTag, get, make},
};

#[test]
fn test_getters_report_expected_and_actual_kinds() {
    let mut arena = ValueArena::new();
    let value = make::text(&mut arena, "payload".to_owned(), Span::default()).unwrap();

    assert_eq!(
        get::bool(&arena, &value),
        Err(ValueError::KindMismatch { expected: ValueTag::Bool, actual: ValueTag::Text })
    );
    assert_eq!(get::one(&[]), Err(ValueError::CountMismatch { expected: 1, actual: 0 }));
}
