# Cross-checking AL under K and PLT Redex: the metacross emitter

CARGO := cargo
RUST_DIR = p4spectec
METACROSS = bin/metacross
override CARGO_TARGET_DIR := $(if $(CARGO_TARGET_DIR),$(CARGO_TARGET_DIR),$(RUST_DIR)/target)
ifeq ($(filter /%,$(CARGO_TARGET_DIR)),)
override CARGO_TARGET_DIR := $(CURDIR)/$(CARGO_TARGET_DIR)
endif
export CARGO_TARGET_DIR

.DEFAULT_GOAL := metacross
.PHONY: metacross metacross-cli clean

metacross-cli:
	cd $(RUST_DIR) && $(CARGO) build --locked --release --bin metacross --target-dir "$(CARGO_TARGET_DIR)"

metacross: metacross-cli
	mkdir -p $(dir $(METACROSS))
	rm -f ./$(METACROSS)
	cp "$(CARGO_TARGET_DIR)/release/metacross" ./$(METACROSS)

clean:
	rm -f ./$(METACROSS)
