# Rust builds and end-to-end acceptance through the registered driver

CARGO := cargo
TARGET_DIR := $(if $(CARGO_TARGET_DIR),$(CARGO_TARGET_DIR),target)
ifeq ($(filter /%,$(TARGET_DIR)),)
TARGET_DIR := $(CURDIR)/$(TARGET_DIR)
endif

.PHONY: build release debug fmt fmt-check lint rustdoc clean

debug:
	$(CARGO) build --locked --bin p4spectec --target-dir "$(TARGET_DIR)"

build release:
	$(CARGO) build --locked --release --bin p4spectec --target-dir "$(TARGET_DIR)"

fmt:
	$(CARGO) fmt
	$(CARGO) fmt --manifest-path test-driver/Cargo.toml

fmt-check:
	$(CARGO) fmt --check
	$(CARGO) fmt --manifest-path test-driver/Cargo.toml --check

lint:
	$(CARGO) clippy --locked --all-targets --no-deps --target-dir "$(TARGET_DIR)"
	$(CARGO) clippy --locked --all-targets --no-deps --manifest-path test-driver/Cargo.toml --target-dir "$(TARGET_DIR)"

rustdoc:
	RUSTDOCFLAGS="-D warnings" $(CARGO) doc --locked --no-deps --target-dir "$(TARGET_DIR)"

clean:
	$(CARGO) clean --target-dir "$(TARGET_DIR)"

.DEFAULT_GOAL := test

.PHONY: test test-adoc

test: test-expected test-sim-al test-sim-sl test-sim-pl

# Standalone acceptance driver; registrations live in test-driver/suites.json
.PHONY: test-expected test-promote test-elab test-algo test-structure test-prose test-p4parse test-run-al test-run-sl test-run-pl
.PHONY: test-diagnostics test-diagnostics-promote diagnostic-cli
DRIVER = $(CARGO) run --locked --release --manifest-path test-driver/Cargo.toml --target-dir "$(TARGET_DIR)" --
DIAGNOSTIC_CLI = $(TARGET_DIR)/release/p4spectec

test-elab:
	env -u UPDATE_EXPECT $(DRIVER) elab

test-algo:
	env -u UPDATE_EXPECT $(DRIVER) algo

test-structure:
	env -u UPDATE_EXPECT $(DRIVER) structure

test-prose:
	env -u UPDATE_EXPECT $(DRIVER) prose

# Export fragments with $(DRIVER) adoc --output DIR for Asciidoctor validation
test-adoc:
	env -u UPDATE_EXPECT $(DRIVER) adoc

test-p4parse:
	env -u UPDATE_EXPECT $(DRIVER) p4parse

test-run-al:
	env -u UPDATE_EXPECT $(DRIVER) run al
	env -u UPDATE_EXPECT $(DRIVER) run al --det

test-run-sl:
	env -u UPDATE_EXPECT $(DRIVER) run sl
	env -u UPDATE_EXPECT $(DRIVER) run sl --det

test-run-pl:
	env -u UPDATE_EXPECT $(DRIVER) run pl
	env -u UPDATE_EXPECT $(DRIVER) run pl --det

test-expected: test-elab test-algo test-structure test-prose test-adoc test-p4parse test-run-al test-run-sl test-run-pl test-diagnostics

# Rerun elab/algo/prose/adoc and update snapshots; does not approve a previous run
# Parsing, structure, execution, simulation, and exclusions are never promoted
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

diagnostic-cli: release

test-diagnostics: diagnostic-cli
	env -u UPDATE_EXPECT $(DRIVER) diagnostics --cli "$(DIAGNOSTIC_CLI)"

test-diagnostics-promote: diagnostic-cli
	UPDATE_EXPECT=1 $(DRIVER) diagnostics --cli "$(DIAGNOSTIC_CLI)"
