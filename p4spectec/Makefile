# Rust builds and integration tests

CARGO := cargo
TARGET_DIR := $(if $(CARGO_TARGET_DIR),$(CARGO_TARGET_DIR),target)
ifeq ($(filter /%,$(TARGET_DIR)),)
TARGET_DIR := $(CURDIR)/$(TARGET_DIR)
endif

.PHONY: build release debug fmt fmt-check lint rustdoc clean

debug:
	$(CARGO) build --locked -p p4spectec-cli --target-dir "$(TARGET_DIR)"

build release:
	$(CARGO) build --locked --release -p p4spectec-cli --target-dir "$(TARGET_DIR)"

fmt:
	$(CARGO) fmt --all

fmt-check:
	$(CARGO) fmt --all --check

lint:
	$(CARGO) clippy --locked --workspace --all-targets --no-deps --target-dir "$(TARGET_DIR)"

rustdoc:
	RUSTDOCFLAGS="-D warnings" $(CARGO) doc --locked --no-deps -p p4spectec -p p4spectec-sim --target-dir "$(TARGET_DIR)"

clean:
	$(CARGO) clean --target-dir "$(TARGET_DIR)"

.DEFAULT_GOAL := test

# Integration tests under test-driver/tests and cli/tests, run by cargo-nextest
# FILTER=EXPR selects tests by a nextest filterset, e.g. FILTER='binary(run)'
# PROFILE=NAME selects a profile of .config/nextest.toml (CI uses `ci`)
# NO_FAIL_FAST=1 runs every test instead of stopping at the first failure
# TIMEOUT=SECONDS gives each corpus case a timeout (none by default)
# TEST_LOG=DIR writes DIR/<test>.log per corpus test: each case's outcome, paths, and duration
# INSTA_UPDATE=always rewrites snapshots instead of comparing with them
.PHONY: test

test:
	$(if $(TIMEOUT),P4SPECTEC_TEST_TIMEOUT=$(TIMEOUT)) $(if $(TEST_LOG),P4SPECTEC_TEST_LOG="$(abspath $(TEST_LOG))") $(CARGO) nextest run --locked --release --workspace --target-dir "$(TARGET_DIR)" $(if $(PROFILE),--profile $(PROFILE)) $(if $(NO_FAIL_FAST),--no-fail-fast) $(if $(FILTER),-E '$(value FILTER)')

# test-<alias> runs the tests of the filterset FILTER_<alias>, e.g. `make test-sim-v1model-p4c-sl`
FILTER_speclang := binary(speclang)
FILTER_diagnostic := binary(diagnostic)
FILTER_p4parse := binary(p4parse)
FILTER_run := binary(run)
FILTER_sim := binary(sim)

LANGUAGES := al sl pl
RUN_SUITES := pos neg regression-pos regression-neg
SIM_SUITES := v1model-p4c v1model-p4testgen v1model-custom v1model-regression \
  ebpf-p4c ebpf-p4testgen ebpf-regression psa-p4c

# Every language of run.rs or sim.rs, and each suite of them in one language
$(foreach language,$(LANGUAGES),\
  $(eval FILTER_run-$(language) := binary(run) & test(_$(language)))\
  $(eval FILTER_sim-$(language) := binary(sim) & test(_$(language)))\
  $(foreach suite,$(RUN_SUITES),\
    $(eval FILTER_run-$(suite)-$(language) := binary(run) & test(=$(subst -,_,$(suite))_$(language))))\
  $(foreach suite,$(SIM_SUITES),\
    $(eval FILTER_sim-$(suite)-$(language) := binary(sim) & test(=$(subst -,_,$(suite))_$(language)))))

test-%:
	$(if $(FILTER_$*),,$(error unknown test alias $*))
	$(MAKE) test FILTER='$(FILTER_$*)'
