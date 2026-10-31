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

.PHONY: metacirc metacirc-cli kompile redex test-metacirc-run-al test-metacirc-run-il test-metacirc-run-k test-metacirc-run-redex test-metacirc-micro-al test-metacirc-micro-il test-metacirc-micro-k test-metacirc-micro-redex test-metacirc-sim-al test-metacirc-sim-il test-metacirc-sim-k test-metacirc-sim-redex test-metacirc-k-in-st $(METACIRC_SNAPSHOTS) test-metacirc-metaparse test-metacirc-promote clean

metacirc-cli:
	cd $(RUST_DIR) && $(CARGO) build --locked --release --bin metacirc --target-dir "$(CARGO_TARGET_DIR)"

metacirc: metacirc-cli
	mkdir -p $(dir $(METACIRC))
	rm -f ./$(METACIRC)
	cp "$(CARGO_TARGET_DIR)/release/metacirc" ./$(METACIRC)

# The library a k tower loads, compiled from spec-meta-k (`kompiled` in towers/k-*.json),
# with the metacirc binary that loads it
KSPECDIR = spec-meta-k
KLIBRARY = $(KSPECDIR)/kompiled/libspec-meta-k.so

kompile: metacirc $(KLIBRARY)

$(KLIBRARY): $(wildcard $(KSPECDIR)/*/*.k)
	kompile $(KSPECDIR)/3-execution/3.14-entry.k --main-module AL --syntax-module SYNTAX \
	  -o $(dir $(KLIBRARY)) --llvm-kompile-type c --llvm-kompile-output $(notdir $(KLIBRARY))

# The entry module a redex tower runs, compiled from spec-meta-redex (`module` in towers/redex*.json),
# with the metacirc binary that runs it; raco make recompiles only what changed
REDEXSPECDIR = spec-meta-redex
REDEXMODULE = $(REDEXSPECDIR)/3-execution/3.16-entry.rkt

redex: metacirc
	raco make -v $(REDEXMODULE)

# P4 corpus through the AL/P4 towers
test-metacirc-run-al:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) run al --input p4 $(CORPUS_ARGS)

# P4 corpus through the IL/P4 towers, converted by Algo_IL
test-metacirc-run-il:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) run il --input p4 $(CORPUS_ARGS)

# P4 corpus through the K/P4 towers, spec-meta-k on its K library (ST-in-K)
test-metacirc-run-k:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) run k --input p4 $(CORPUS_ARGS)

# P4 corpus through the Redex/P4 towers, spec-meta-redex in racket (ST-in-Redex)
test-metacirc-run-redex:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) run redex --input p4 $(CORPUS_ARGS)

# Spec-language micro test sources through the AL tower
test-metacirc-micro-al:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) run al --input al $(CORPUS_ARGS)

# Spec-language micro test sources through the IL tower, converted by Algo_IL
test-metacirc-micro-il:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) run il --input al $(CORPUS_ARGS)

# Spec-language micro test sources through the K tower (ST-in-K)
test-metacirc-micro-k:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) run k --input al $(CORPUS_ARGS)

# Spec-language micro test sources through the Redex tower (ST-in-Redex)
test-metacirc-micro-redex:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) run redex --input al $(CORPUS_ARGS)

# Registered simulation suites through the AL/P4 tower, against native expectations
test-metacirc-sim-al:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) sim al $(CORPUS_ARGS)

# Registered simulation suites through the IL/P4 tower, converted by Algo_IL
test-metacirc-sim-il:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) sim il $(CORPUS_ARGS)

# Registered simulation suites through the K/P4 tower (ST-in-K), against native expectations
test-metacirc-sim-k:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) sim k $(CORPUS_ARGS)

# Registered simulation suites through the Redex/P4 tower (ST-in-Redex), against native expectations
test-metacirc-sim-redex:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) sim redex $(CORPUS_ARGS)

# K definitions run through spec-k over KORE (K-in-ST), against krun's output
test-metacirc-k-in-st:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) k-in-st sl

# Snapshots of the meta specification in spec-meta
$(METACIRC_SNAPSHOTS):
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) $(@:test-metacirc-%=%)

# IL and AL meta codec round trips of the specifications
test-metacirc-metaparse:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) metaparse

# Rerun elab/algo/prose and update their snapshots; structure is never promoted
test-metacirc-promote:
	cd $(RUST_DIR) && UPDATE_EXPECT=1 $(METACIRC_TEST) elab
	cd $(RUST_DIR) && UPDATE_EXPECT=1 $(METACIRC_TEST) algo
	cd $(RUST_DIR) && UPDATE_EXPECT=1 $(METACIRC_TEST) prose

clean:
	rm -f ./$(METACIRC)
