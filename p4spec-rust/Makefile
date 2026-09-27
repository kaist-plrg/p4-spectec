# Test targets for the Rust port.
#
# The default targets run the native Rust unit and integration tests.

CARGO := cargo

.PHONY: test test-frontend test-p4parse-unit test-elab-unit test-algo-unit test-structure-unit test-prose-unit test-lang \
        test-runtime test-run-al-unit test-run-sl-unit test-run-pl-unit

# Fast unit tests (no OCaml toolchain required)

test:
	$(CARGO) test

test-frontend:
	$(CARGO) test --test frontend

# P4 parser regressions; full roundtrip corpus lives in test-p4parse
test-p4parse-unit:
	$(CARGO) test --test interface p4_parse:: -- --nocapture

test-elab-unit:
	$(CARGO) test --test pass elaborate::

# algo tests live in two places: the binding/side-condition unit tests run
# inside the library (`--lib`), the conversion tests in the `pass` binary.
test-algo-unit:
	$(CARGO) test --lib pass::algo
	$(CARGO) test --test pass algo::

test-structure-unit:
	$(CARGO) test --locked --lib pass::structure::
	$(CARGO) test --locked --test pass structure::
	$(CARGO) test --locked --test lang al::fresh::

test-prose-unit:
	$(CARGO) test --locked --test lang hints::
	$(CARGO) test --locked --test lang pl::
	$(CARGO) test --locked --test pass prosify::

.PHONY: test-adoc-unit test-adoc
test-adoc-unit:
	$(CARGO) test --locked --test backend-doc

test-lang:
	$(CARGO) test --test lang

test-runtime:
	$(CARGO) test --test runtime

# Native AL execution and command-line checks
test-run-al-unit:
	$(CARGO) test --locked --test runtime interp::al::
	$(CARGO) test --locked --test cli

test-run-sl-unit:
	$(CARGO) test --locked --test runtime interp::sl::
	$(CARGO) test --locked --test runtime sim_plugin::ebpf::test_sl_counter_state
	$(CARGO) test --locked --test cli

test-run-pl-unit:
	$(CARGO) test --locked --test runtime interp::pl::
	$(CARGO) test --locked --test cli

# Standalone expected driver; product unit tests remain in tests/
.PHONY: test-expected test-promote test-p4parse test-elab test-algo test-structure test-prose test-run-al test-run-sl test-run-pl
.PHONY: test-diagnostics test-diagnostics-promote
DRIVER_TARGET_DIR ?= $(if $(CARGO_TARGET_DIR),$(CARGO_TARGET_DIR),target)
DRIVER = $(CARGO) run --locked --release --manifest-path test-driver/Cargo.toml --target-dir $(DRIVER_TARGET_DIR) --

test-p4parse:
	env -u UPDATE_EXPECT $(DRIVER) p4parse

test-elab:
	env -u UPDATE_EXPECT $(DRIVER) elab

test-algo:
	env -u UPDATE_EXPECT $(DRIVER) algo

test-structure:
	env -u UPDATE_EXPECT $(DRIVER) structure

test-prose:
	env -u UPDATE_EXPECT $(DRIVER) prose

test-diagnostics:
	env -u UPDATE_EXPECT $(DRIVER) diagnostics

test-diagnostics-promote:
	UPDATE_EXPECT=1 $(DRIVER) diagnostics

# Export fragments with $(DRIVER) adoc --output DIR for Asciidoctor validation
test-adoc:
	env -u UPDATE_EXPECT $(DRIVER) adoc

test-run-al:
	env -u UPDATE_EXPECT $(DRIVER) run-al

test-run-sl:
	env -u UPDATE_EXPECT $(DRIVER) run-sl
	env -u UPDATE_EXPECT $(DRIVER) run-sl --det

test-run-pl:
	env -u UPDATE_EXPECT $(DRIVER) run-pl
	env -u UPDATE_EXPECT $(DRIVER) run-pl --det

test-expected: test-p4parse test-elab test-algo test-structure test-prose test-adoc test-run-al test-run-sl test-run-pl test-diagnostics

# Rerun elab/algo/prose/adoc and update snapshots; does not approve a previous run
# P4 parser/structure/AL expected and excludes are never promoted
test-promote:
	UPDATE_EXPECT=1 $(DRIVER) elab
	UPDATE_EXPECT=1 $(DRIVER) algo
	UPDATE_EXPECT=1 $(DRIVER) prose
	UPDATE_EXPECT=1 $(DRIVER) adoc

# Native simulation uses source-derived expectations in both cached modes
.PHONY: test-sim-al test-sim-sl test-sim-pl

test-sim-al:
	env -u UPDATE_EXPECT $(DRIVER) sim-al
	env -u UPDATE_EXPECT $(DRIVER) sim-al --det

test-sim-sl:
	env -u UPDATE_EXPECT $(DRIVER) sim-sl
	env -u UPDATE_EXPECT $(DRIVER) sim-sl --det

test-sim-pl:
	env -u UPDATE_EXPECT $(DRIVER) sim-pl
	env -u UPDATE_EXPECT $(DRIVER) sim-pl --det
