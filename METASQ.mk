# Independent metacircular build and P4 execution corpus
# TEST_LOG=DIR writes per-test timing logs of the metacirc run, micro, and sim targets
# TIMEOUT=SECONDS records a source of those targets still running after it as a timeout
# SUITE=NAME runs only that registered suite in those targets
# TIMEOUT and SUITE apply to test-metacirc-k-in-st as well
# SUITE=NAME and GROUP=NAME select the registered suite and group of the run-group and sim-group
# targets, which require both

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

.PHONY: metacirc metacirc-cli kompile redex test-metacirc-run-al test-metacirc-run-il test-metacirc-run-k test-metacirc-run-redex test-metacirc-micro-al test-metacirc-micro-il test-metacirc-micro-k test-metacirc-micro-redex test-metacirc-run-group-al test-metacirc-run-group-il test-metacirc-run-group-k test-metacirc-run-group-redex metacirc-run-group-split test-metacirc-sim-al test-metacirc-sim-il test-metacirc-sim-k test-metacirc-sim-redex test-metacirc-sim-group-al test-metacirc-sim-group-il test-metacirc-sim-group-k test-metacirc-sim-group-redex metacirc-sim-group-split test-metacirc-k-in-st $(METACIRC_SNAPSHOTS) test-metacirc-metaparse test-metacirc-promote clean

metacirc-cli:
	cd $(RUST_DIR) && $(CARGO) build --locked --release --bin metacirc --target-dir "$(CARGO_TARGET_DIR)"

metacirc: metacirc-cli
	mkdir -p $(dir $(METACIRC))
	rm -f ./$(METACIRC)
	cp "$(CARGO_TARGET_DIR)/release/metacirc" ./$(METACIRC)

# The libraries k towers load (`kompiled` in towers/k*.json), compiled from spec-meta-k
# and its determinism-checking copy spec-meta-k-detcheck, with the metacirc binary that loads them
KSPECDIR = spec-meta-k
KLIBRARY = $(KSPECDIR)/kompiled/libspec-meta-k.so
KSPECDIR_DETCHECK = spec-meta-k-detcheck
KLIBRARY_DETCHECK = $(KSPECDIR_DETCHECK)/kompiled/libspec-meta-k.so

kompile: metacirc $(KLIBRARY) $(KLIBRARY_DETCHECK)

$(KLIBRARY): $(wildcard $(KSPECDIR)/*/*.k)
	kompile $(KSPECDIR)/3-execution/3.14-entry.k --main-module AL --syntax-module SYNTAX \
	  -o $(dir $(KLIBRARY)) --llvm-kompile-type c --llvm-kompile-output $(notdir $(KLIBRARY)) -O2

$(KLIBRARY_DETCHECK): $(wildcard $(KSPECDIR_DETCHECK)/*/*.k)
	kompile $(KSPECDIR_DETCHECK)/3-execution/3.14-entry.k --main-module AL --syntax-module SYNTAX \
	  -o $(dir $(KLIBRARY_DETCHECK)) --llvm-kompile-type c --llvm-kompile-output $(notdir $(KLIBRARY_DETCHECK)) -O2

# The entry modules redex towers run (`module` in towers/redex*.json), compiled from
# spec-meta-redex and its determinism-checking copy spec-meta-redex-detcheck,
# with the metacirc binary that runs them; raco make recompiles only what changed
REDEXSPECDIR = spec-meta-redex
REDEXMODULE = $(REDEXSPECDIR)/3-execution/3.15-entry.rkt
REDEXSPECDIR_DETCHECK = spec-meta-redex-detcheck
REDEXMODULE_DETCHECK = $(REDEXSPECDIR_DETCHECK)/3-execution/3.15-entry.rkt

redex: metacirc
	raco make -v $(REDEXMODULE) $(REDEXMODULE_DETCHECK)

# P4 corpus through the AL/P4 towers
test-metacirc-run-al:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) run al --input p4 $(CORPUS_ARGS)

# P4 corpus through the IL/P4 towers, converted by Algo_IL of spec-meta-ext
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

# Spec-language micro test sources through the IL tower, converted by Algo_IL of spec-meta-ext
test-metacirc-micro-il:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) run il --input al $(CORPUS_ARGS)

# Spec-language micro test sources through the K tower (ST-in-K)
test-metacirc-micro-k:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) run k --input al $(CORPUS_ARGS)

# Spec-language micro test sources through the Redex tower (ST-in-Redex)
test-metacirc-micro-redex:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) run redex --input al $(CORPUS_ARGS)

# One group of a P4 corpus suite of suites/metacirc/run-group.toml through the AL/P4, IL/P4,
# K/P4, or Redex/P4 tower, against the group's expected file for the suite
GROUP_ARGS = --suite $(SUITE) --group $(GROUP) $(if $(TIMEOUT),--timeout $(TIMEOUT))
RUN_GROUP_TARGETS := test-metacirc-run-group-al test-metacirc-run-group-il test-metacirc-run-group-k test-metacirc-run-group-redex

$(RUN_GROUP_TARGETS):
	$(if $(and $(SUITE),$(GROUP)),,$(error SUITE and GROUP are required, e.g. make $@ SUITE=run-positive GROUP=1))
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) run-group $(@:test-metacirc-run-group-%=%) $(GROUP_ARGS)

# Rewrite the group folders of suites/metacirc/run-group.toml from its suite's corpus,
# with this many programs in each folder in order
RUN_GROUP_SIZES := 927 170 170

metacirc-run-group-split:
	cd $(RUST_DIR) && $(METACIRC_TEST) run-group-split $(RUN_GROUP_SIZES)

# Registered simulation suites through the AL/P4 tower, against native expectations
test-metacirc-sim-al:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) sim al $(CORPUS_ARGS)

# Registered simulation suites through the IL/P4 tower, converted by Algo_IL of spec-meta-ext
test-metacirc-sim-il:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) sim il $(CORPUS_ARGS)

# Registered simulation suites through the K/P4 tower (ST-in-K), against native expectations
test-metacirc-sim-k:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) sim k $(CORPUS_ARGS)

# Registered simulation suites through the Redex/P4 tower (ST-in-Redex), against native expectations
test-metacirc-sim-redex:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) sim redex $(CORPUS_ARGS)

# One group of a simulation suite of suites/metacirc/sim-group.toml through the AL/P4, IL/P4,
# K/P4, or Redex/P4 tower, against the group's expected file for the suite
SIM_GROUP_TARGETS := test-metacirc-sim-group-al test-metacirc-sim-group-il test-metacirc-sim-group-k test-metacirc-sim-group-redex

$(SIM_GROUP_TARGETS):
	$(if $(and $(SUITE),$(GROUP)),,$(error SUITE and GROUP are required, e.g. make $@ SUITE=v1model-p4c GROUP=1))
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) sim-group $(@:test-metacirc-sim-group-%=%) $(GROUP_ARGS)

# Rewrite the group folders of suites/metacirc/sim-group.toml from its suites' corpora
metacirc-sim-group-split:
	cd $(RUST_DIR) && $(METACIRC_TEST) sim-group-split

# K definitions run through spec-k over KORE (K-in-ST), against krun's output
test-metacirc-k-in-st:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) k-in-st sl $(CORPUS_ARGS)

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
	rm -rf ./$(dir $(KLIBRARY))
	find $(REDEXSPECDIR) -name compiled -type d -prune -exec rm -rf {} +
