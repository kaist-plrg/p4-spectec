use super::*;

#[test]
fn test_numbers_preserve_ocaml_variant_order_and_subtyping() {
    let large_nat = natural(100);
    let small_int = Number::Int(BigInt::from(-100));

    assert_eq!(num_impl::compare(&large_nat, &small_int), Ordering::Less);
    assert_eq!(num_impl::compare_typ(Typ::Nat, Typ::Int), Ordering::Less);
    assert!(num_impl::sub(Typ::Nat, Typ::Int));
    assert!(!num_impl::sub(Typ::Int, Typ::Nat));
}
#[test]
fn test_numeric_operations_preserve_kinds_and_signed_rendering() {
    let two = natural(2);
    let three = natural(3);
    let negative_three = Number::Int(BigInt::from(-3));

    assert_eq!(num_impl::bin(BinOp::Add, &two, &three), Ok(natural(5)));
    assert_eq!(num_impl::bin(BinOp::Sub, &two, &three), Ok(Number::Int((-1).into())));
    assert_eq!(num_impl::un(UnOp::Minus, &two), Number::Int((-2).into()));
    assert_eq!(num_impl::cmp(CmpOp::Lt, &two, &three), Ok(true));
    assert_eq!(Print::to_string(&Number::Int(3.into())), "+3");
    assert_eq!(Print::to_string(&negative_three), "-3");
}
#[test]
fn test_natural_numbers_reject_negative_payloads() {
    assert_eq!(
        Natural::try_from(BigInt::from(-1)),
        Err(NumericError::NaturalNegative(BigInt::from(-1)))
    );
}
#[test]
fn test_binary_operations_report_zero_divisors() {
    let operands = [(natural(5), natural(0)), (Number::Int(5.into()), Number::Int(0.into()))];

    for (number_l, number_r) in operands {
        for operation in [BinOp::Div, BinOp::Mod] {
            assert_eq!(
                num_impl::bin(operation, &number_l, &number_r),
                Err(NumericError::DivisorZero(operation))
            );
        }
    }
}
#[test]
fn test_numeric_operations_report_mismatched_kinds() {
    let nat = natural(1);
    let int = Number::Int(1.into());
    let error = NumericError::KindMismatch { typ_l: Typ::Nat, typ_r: Typ::Int };

    assert_eq!(num_impl::bin(BinOp::Add, &nat, &int), Err(error.clone()));
    assert_eq!(num_impl::cmp(CmpOp::Lt, &nat, &int), Err(error));
}
#[test]
fn test_unsupported_binary_operations_return_errors() {
    assert_eq!(
        num_impl::bin(BinOp::Pow, &natural(2), &natural(3)),
        Err(NumericError::BinaryOperationUnsupported(BinOp::Pow))
    );
}
