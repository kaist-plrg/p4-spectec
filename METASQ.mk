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
.PHONY: metacirc metacirc-cli test-metacirc clean

metacirc-cli:
	cd $(RUST_DIR) && $(CARGO) build --locked --release --bin metacirc --target-dir "$(CARGO_TARGET_DIR)"

metacirc: metacirc-cli
	mkdir -p $(dir $(METACIRC))
	rm -f ./$(METACIRC)
	cp "$(CARGO_TARGET_DIR)/release/metacirc" ./$(METACIRC)

test-metacirc:
	cd $(RUST_DIR) && env -u UPDATE_EXPECT $(CARGO) run --locked --release --manifest-path test-driver/Cargo.toml --target-dir "$(CARGO_TARGET_DIR)" --bin metacirc-test

clean:
	rm -f ./$(METACIRC)
