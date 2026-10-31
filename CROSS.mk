# Cross-checking AL under K and PLT Redex: the metacross emitter, the FFI
# library the specifications call back into, and the K and Redex builds

CARGO := cargo
RUST_DIR = p4spectec
METACROSS = bin/metacross
FFI_SO = bin/libp4spectec.so
override CARGO_TARGET_DIR := $(if $(CARGO_TARGET_DIR),$(CARGO_TARGET_DIR),$(RUST_DIR)/target)
ifeq ($(filter /%,$(CARGO_TARGET_DIR)),)
override CARGO_TARGET_DIR := $(CURDIR)/$(CARGO_TARGET_DIR)
endif
export CARGO_TARGET_DIR

.DEFAULT_GOAL := metacross
.PHONY: metacross metacross-cli ffi-lib k test-k redex test-redex clean

# Rust

metacross-cli:
	cd $(RUST_DIR) && $(CARGO) build --locked --release --bin metacross --target-dir "$(CARGO_TARGET_DIR)"

metacross: metacross-cli
	mkdir -p $(dir $(METACROSS))
	rm -f ./$(METACROSS)
	cp "$(CARGO_TARGET_DIR)/release/metacross" ./$(METACROSS)

ffi-lib:
	cd $(RUST_DIR) && $(CARGO) build --locked --release --lib --target-dir "$(CARGO_TARGET_DIR)"

$(FFI_SO): ffi-lib
	mkdir -p $(dir $(FFI_SO))
	rm -f ./$(FFI_SO)
	cp "$(CARGO_TARGET_DIR)/release/libp4spectec.so" ./$(FFI_SO)

# K specification

KDEFDIR = al-kompiled
KSPECDIR = spec-meta-k

k: metacross $(FFI_SO)
	kompile $(KSPECDIR)/3-execution/3.14-entry.k --main-module AL --syntax-module SYNTAX -o $(KDEFDIR) \
	  -ccopt -L$(CURDIR)/bin -ccopt -Wl,--no-as-needed -ccopt -lp4spectec \
	  -ccopt '-Wl,-rpath,$$ORIGIN/../bin'

test-k: k
	@status=0; \
	python3 $(KSPECDIR)/scripts/run-k-typecheck.py || status=1; \
	python3 $(KSPECDIR)/scripts/run-k-typecheck.py --neg || status=1; \
	exit $$status

# PLT Redex specification

RSPECDIR = spec-meta-redex
RENTRY = $(RSPECDIR)/3-execution/3.16-entry.rkt
RTYPECHECK = $(RSPECDIR)/test/p4-typecheck.rkt

redex: metacross $(FFI_SO)
	raco make $(RENTRY)

test-redex: redex
	raco make $(RTYPECHECK)
	@status=0; \
	racket $(RTYPECHECK) --p4-dir p4c/testdata/p4_16_samples -e excludes/static -i p4c/p4include spec || status=1; \
	racket $(RTYPECHECK) --p4-dir p4c/testdata/p4_16_errors -e excludes/static -i p4c/p4include --neg spec || status=1; \
	exit $$status

# Cleanup

clean:
	rm -rf $(KDEFDIR) $(KSPECDIR)/.kore-cache
	rm -f $(KSPECDIR)/spectec-k-* $(KSPECDIR)/run-k-typecheck-*.result
	rm -f $(RSPECDIR)/p4-typecheck-*.result
	rm -rf $(RSPECDIR)/*/compiled
	rm -f ./$(METACROSS) ./$(FFI_SO)
