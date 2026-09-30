use super::*;

#[test]
fn test_counter_init_uses_first_named_argument_and_exact_size() {
    let mut arena = ValueArena::new();
    for sparse in [false, true] {
        let value_ids = names(&mut arena, &["max_index", "sparse", "max_index", "unused"]);
        let value_max = pack::p4_fixed_bit(&mut arena, 32.into(), 2.into()).unwrap();
        let value_sparse = boolean(&mut arena, sparse);
        let value_later = pack::p4_fixed_bit(&mut arena, 32.into(), 4.into()).unwrap();
        let value_args = list(&mut arena, vec![value_max, value_sparse, value_later, value_ids]);
        let counter = CounterArray::init(&arena, value_ids, value_args).unwrap();
        assert_eq!(counter.counts, vec![0, 0]);
    }
    let value_ids = names(&mut arena, &["max_index", "sparse"]);
    let value_sparse = boolean(&mut arena, false);
    for max in [0, -1, 1_i64 << 62] {
        let value_max = pack::p4_fixed_bit(&mut arena, 64.into(), max.into()).unwrap();
        let value_args = list(&mut arena, vec![value_max, value_sparse]);
        let result = CounterArray::init(&arena, value_ids, value_args);
        if max == 0 {
            assert!(result.unwrap().counts.is_empty());
        } else {
            assert!(result.is_err());
        }
    }
    let value_args = list(&mut arena, vec![value_sparse]);
    assert!(CounterArray::init(&arena, value_ids, value_args).is_err());
    let value_ids = names(&mut arena, &["missing"]);
    assert!(CounterArray::init(&arena, value_ids, value_args).is_err());
    let value_ids = names(&mut arena, &["max_index", "sparse"]);
    let value_max = pack::p4_fixed_bit(&mut arena, 32.into(), 2.into()).unwrap();
    let value_bool = make::bool(&mut arena, false, Span::default()).unwrap();
    let value_args = list(&mut arena, vec![value_max, value_bool]);
    assert!(CounterArray::init(&arena, value_ids, value_args).is_err());
}

#[test]
fn test_counter_operations_preserve_state_and_wrap_at_32_bits() {
    let mut runner = Runner::new((), CounterInterp::default(), NullInterface, Dummy);
    let value_ctx = make::text(runner.arena_mut(), "ctx".to_owned(), Span::default()).unwrap();
    let value_arch = make::text(runner.arena_mut(), "arch".to_owned(), Span::default()).unwrap();
    let mut counter = CounterArray { counts: vec![u32::MAX, (1 << 16) - 1] };
    local(&mut runner, "index", 0);
    let result = counter
        .increment(&mut runner.context(), value_ctx, value_arch)
        .unwrap();
    assert_eq!(result.0.counts, vec![0, (1 << 16) - 1]);
    assert_eq!(result.1, value_ctx);
    assert_eq!(result.2, value_arch);
    let typ = typ::make::opt(typ::make::var(
        p4spec_rust::phrase!(node: "value".to_owned(), span: Span::default()),
        Vec::new(),
    ));
    let value_opt = make::opt(runner.arena_mut(), typ.node.into(), None, Span::default()).unwrap();
    let mixop = p4spec_rust::frontend::parse::parse_mixop("RETURN value?").unwrap();
    let value_case =
        p4spec_rust::lang::common::notation::mixop::Mixop::fill(&mixop, vec![value_opt]).unwrap();
    let typ = typ::make::var(
        p4spec_rust::phrase!(node: "returnResult".to_owned(), span: Span::default()),
        Vec::new(),
    );
    let value_return =
        make::case(runner.arena_mut(), typ.node.into(), value_case, Span::default()).unwrap();
    assert_eq!(runner.arena().canon_id(&result.3), runner.arena().canon_id(&value_return));
    assert_eq!(runner.arena().typ(&result.3), runner.arena().typ(&value_return));
    counter = result.0;
    local(&mut runner, "value", 0xffff_ffff);
    counter = counter
        .add(&mut runner.context(), value_ctx, value_arch)
        .unwrap()
        .0;
    assert_eq!(counter.counts[0], u32::MAX);
    local(&mut runner, "index", 1);
    counter = counter
        .increment(&mut runner.context(), value_ctx, value_arch)
        .unwrap()
        .0;
    assert_eq!(counter.counts[1], 1 << 16);
    for idx in [2, 3] {
        local(&mut runner, "index", idx);
        let counts = counter.counts.clone();
        counter = counter
            .add(&mut runner.context(), value_ctx, value_arch)
            .unwrap()
            .0;
        assert_eq!(counter.counts, counts);
    }
    assert_eq!(
        runner.context().interp().calls,
        ["index", "index", "value", "index", "index", "value", "index", "value"]
    );
}

#[test]
fn test_counter_reentry_failure_and_oversized_operands() {
    let mut runner = Runner::new((), CounterInterp::default(), NullInterface, Dummy);
    let value_ctx = make::text(runner.arena_mut(), "ctx".to_owned(), Span::default()).unwrap();
    let counter = CounterArray { counts: vec![0] };
    local(&mut runner, "index", 0);
    let error = counter
        .clone()
        .add(&mut runner.context(), value_ctx, value_ctx)
        .expect_err("missing local must propagate");
    assert!(
        matches!(error, TestError::Extern(ExternError::Message(msg)) if msg == "missing local value")
    );
    assert_eq!(runner.context().interp().calls, ["index", "value"]);
    local(&mut runner, "index", 1_i64 << 62);
    assert_eq!(
        counter
            .clone()
            .increment(&mut runner.context(), value_ctx, value_ctx)
            .unwrap()
            .0
            .counts,
        [0]
    );
    local(&mut runner, "index", 0);
    local(&mut runner, "value", 0xffff_ffff);
    assert_eq!(
        counter
            .clone()
            .add(&mut runner.context(), value_ctx, value_ctx)
            .unwrap()
            .0
            .counts,
        [u32::MAX]
    );
    local(&mut runner, "value", 1_i64 << 32);
    assert!(
        counter
            .clone()
            .add(&mut runner.context(), value_ctx, value_ctx)
            .is_err()
    );
    let value =
        pack::p4_fixed_bit(runner.arena_mut(), 128.into(), num_bigint::BigInt::from(u64::MAX) + 1)
            .unwrap();
    runner
        .context()
        .interp_mut()
        .values_var
        .insert("index".to_owned(), value);
    local(&mut runner, "value", 1);
    assert!(
        counter
            .clone()
            .add(&mut runner.context(), value_ctx, value_ctx)
            .is_err()
    );
}
