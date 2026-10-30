# Independent metacircular build and P4 execution corpus
# TEST_LOG=DIR writes per-test timing logs of the metacirc run, micro, and sim targets
# TIMEOUT=SECONDS records a source of those targets still running after it as a timeout
# SUITE=NAME runs only that registered suite in those targets

CARGO := cargo
RUST_DIR = p4spectec
METACIRC = bin/metacirc
override CARGO_TARGET_DIR := $(if $(CARGO_TARGET_DIR),$(CARGO_TARGET_DIR),$(RUST_DIR)/target)
ifeq ($(filter /%,$(CARGO_TARGET_DIR)),)
override CARGO_TARGET_DIR := $(CURDIR)/$(CARGO_TARGET_DIR)
endif
export CARGO_TARGET_DIR

.DEFAULT_GOAL := metacirc
METACIRC_TEST = $(CARGO) run --locked --release --manifest-path test-driver/Cargo.toml --target-dir "$(CARGO_TARGET_DIR)" --bin metacirc-test -- $(if $(TEST_LOG),--log "$(abspath $(TEST_LOG))")
CORPUS_ARGS = $(if $(TIMEOUT),--timeout $(TIMEOUT)) $(if $(SUITE),--suite $(SUITE))
METACIRC_SNAPSHOTS := test-metacirc-elab test-metacirc-algo test-metacirc-structure test-metacirc-prose

.PHONY: metacirc metacirc-cli test-metacirc-run-al test-metacirc-run-il test-metacirc-micro-al test-metacirc-micro-il test-metacirc-sim-al test-metacirc-sim-il $(METACIRC_SNAPSHOTS) test-metacirc-metaparse test-metacirc-promote clean

metacirc-cli:
	cd $(RUST_DIR) && $(CARGO) build --locked --release --bin metacirc --target-dir "$(CARGO_TARGET_DIR)"

metacirc: metacirc-cli
	mkdir -p $(dir $(METACIRC))
	rm -f ./$(METACIRC)
	cp "$(CARGO_TARGET_DIR)/release/metacirc" ./$(METACIRC)

# P4 corpus through the AL/P4 towers
test-metacirc-run-al:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) run al --input p4 $(CORPUS_ARGS)

# P4 corpus through the IL/P4 towers, converted by Algo_IL
test-metacirc-run-il:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) run il --input p4 $(CORPUS_ARGS)

# Spec-language micro test sources through the AL tower
test-metacirc-micro-al:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) run al --input al $(CORPUS_ARGS)

# Spec-language micro test sources through the IL tower, converted by Algo_IL
test-metacirc-micro-il:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) run il --input al $(CORPUS_ARGS)

# Registered simulation suites through the AL/P4 tower, against native expectations
test-metacirc-sim-al:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) sim al $(CORPUS_ARGS)

# Registered simulation suites through the IL/P4 tower, converted by Algo_IL
test-metacirc-sim-il:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) sim il $(CORPUS_ARGS)

# Snapshots of the meta specification in spec-meta
$(METACIRC_SNAPSHOTS):
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) $(@:test-metacirc-%=%)

# IL and AL meta codec round trips of the specifications
test-metacirc-metaparse:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) metaparse

# Rerun elab/algo/structure/prose and update their snapshots
test-metacirc-promote:
	cd $(RUST_DIR) && UPDATE_EXPECT=1 $(METACIRC_TEST) elab
	cd $(RUST_DIR) && UPDATE_EXPECT=1 $(METACIRC_TEST) algo
	cd $(RUST_DIR) && UPDATE_EXPECT=1 $(METACIRC_TEST) structure
	cd $(RUST_DIR) && UPDATE_EXPECT=1 $(METACIRC_TEST) prose

clean:
	rm -f ./$(METACIRC)
