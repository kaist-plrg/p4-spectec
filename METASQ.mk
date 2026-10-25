# Independent metacircular build and P4 execution corpus

CARGO := cargo
RUST_DIR = p4spectec
METACIRC = bin/metacirc
override CARGO_TARGET_DIR := $(if $(CARGO_TARGET_DIR),$(CARGO_TARGET_DIR),$(RUST_DIR)/target)
ifeq ($(filter /%,$(CARGO_TARGET_DIR)),)
override CARGO_TARGET_DIR := $(CURDIR)/$(CARGO_TARGET_DIR)
endif
export CARGO_TARGET_DIR

.DEFAULT_GOAL := metacirc
METACIRC_TEST = $(CARGO) run --locked --release --manifest-path test-driver/Cargo.toml --target-dir "$(CARGO_TARGET_DIR)" --bin metacirc-test --
METACIRC_SNAPSHOTS := test-metacirc-elab test-metacirc-algo test-metacirc-structure test-metacirc-prose

.PHONY: metacirc metacirc-cli test-metacirc test-metacirc-micro $(METACIRC_SNAPSHOTS) test-metacirc-metaparse test-metacirc-promote clean

metacirc-cli:
	cd $(RUST_DIR) && $(CARGO) build --locked --release --bin metacirc --target-dir "$(CARGO_TARGET_DIR)"

metacirc: metacirc-cli
	mkdir -p $(dir $(METACIRC))
	rm -f ./$(METACIRC)
	cp "$(CARGO_TARGET_DIR)/release/metacirc" ./$(METACIRC)

# P4 corpus through the AL/P4 towers
test-metacirc:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) --input p4

# Spec-language micro test sources through the AL tower
test-metacirc-micro:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(METACIRC_TEST) --input al

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
