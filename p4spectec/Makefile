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

# Integration tests under test-driver/tests, run by cargo-nextest
# FILTER=EXPR selects tests by a nextest filterset, e.g. FILTER='binary(run)'
# PROFILE=NAME selects a profile of .config/nextest.toml (CI uses `ci`)
# INSTA_UPDATE=always rewrites snapshots instead of comparing with them
.PHONY: test

test:
	$(CARGO) nextest run --locked --release --workspace --target-dir "$(TARGET_DIR)" $(if $(PROFILE),--profile $(PROFILE)) $(if $(FILTER),-E '$(value FILTER)')

# test-<alias> runs the tests of the filterset FILTER_<alias>, e.g. `make test-sim-p4c-v1model-sl`
FILTER_speclang := binary(speclang)
FILTER_parse := binary(parse)
FILTER_elab := binary(elab)
FILTER_algo := binary(algo)
FILTER_structure := binary(structure)
FILTER_prose := binary(prose)
FILTER_specdoc := binary(specdoc)
FILTER_splice := binary(splice)
FILTER_run := binary(run)
FILTER_sim := binary(sim)
FILTER_p4parse := binary(p4parse)
FILTER_cli := binary(cli)

LANGUAGES := al sl pl
RUN_SUITES := p4c-pos p4c-neg regression-pos regression-neg
SIM_SUITES := p4c-v1model p4c-ebpf p4c-psa p4testgen-v1model p4testgen-ebpf \
  custom-v1model regression-v1model regression-ebpf

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
