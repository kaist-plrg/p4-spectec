# End-to-end acceptance through the registered Rust driver

CARGO := cargo

.PHONY: test test-adoc

test: test-expected test-sim-al test-sim-sl test-sim-pl

# Standalone acceptance driver; registrations live in test-driver/suites.json
.PHONY: test-expected test-promote test-p4parse test-elab test-algo test-structure test-prose test-run-al test-run-sl test-run-pl
.PHONY: test-diagnostics test-diagnostics-promote diagnostic-cli
DRIVER_TARGET_DIR ?= $(if $(CARGO_TARGET_DIR),$(CARGO_TARGET_DIR),target)
DRIVER = $(CARGO) run --locked --release --manifest-path test-driver/Cargo.toml --target-dir $(DRIVER_TARGET_DIR) --
DIAGNOSTIC_CLI = $(abspath $(DRIVER_TARGET_DIR))/release/p4spec-rust

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

diagnostic-cli:
	$(CARGO) build --locked --release --bin p4spec-rust --target-dir "$(DRIVER_TARGET_DIR)"

test-diagnostics: diagnostic-cli
	env -u UPDATE_EXPECT $(DRIVER) diagnostics --cli "$(DIAGNOSTIC_CLI)"

test-diagnostics-promote: diagnostic-cli
	UPDATE_EXPECT=1 $(DRIVER) diagnostics --cli "$(DIAGNOSTIC_CLI)"

# Export fragments with $(DRIVER) adoc --output DIR for Asciidoctor validation
test-adoc:
	env -u UPDATE_EXPECT $(DRIVER) adoc

test-run-al:
	env -u UPDATE_EXPECT $(DRIVER) run al
	env -u UPDATE_EXPECT $(DRIVER) run al --det

test-run-sl:
	env -u UPDATE_EXPECT $(DRIVER) run sl
	env -u UPDATE_EXPECT $(DRIVER) run sl --det

test-run-pl:
	env -u UPDATE_EXPECT $(DRIVER) run pl
	env -u UPDATE_EXPECT $(DRIVER) run pl --det

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
	env -u UPDATE_EXPECT $(DRIVER) sim al
	env -u UPDATE_EXPECT $(DRIVER) sim al --det

test-sim-sl:
	env -u UPDATE_EXPECT $(DRIVER) sim sl
	env -u UPDATE_EXPECT $(DRIVER) sim sl --det

test-sim-pl:
	env -u UPDATE_EXPECT $(DRIVER) sim pl
	env -u UPDATE_EXPECT $(DRIVER) sim pl --det
