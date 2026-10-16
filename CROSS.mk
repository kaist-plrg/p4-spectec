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
.PHONY: metacross metacross-cli ffi ffi-lib k-spec k-test redex redex-test clean

# Rust

metacross-cli:
	cd $(RUST_DIR) && $(CARGO) build --locked --release --bin metacross --target-dir "$(CARGO_TARGET_DIR)"

metacross: metacross-cli
	mkdir -p $(dir $(METACROSS))
	rm -f ./$(METACROSS)
	cp "$(CARGO_TARGET_DIR)/release/metacross" ./$(METACROSS)

ffi-lib:
	cd $(RUST_DIR) && $(CARGO) build --locked --release --lib --target-dir "$(CARGO_TARGET_DIR)"

ffi: ffi-lib
	mkdir -p $(dir $(FFI_SO))
	rm -f ./$(FFI_SO)
	cp "$(CARGO_TARGET_DIR)/release/libp4spectec.so" ./$(FFI_SO)

# K specification
#
# The interpreter links libp4spectec.so and resolves host_* by dlsym at run
# time, so --no-as-needed keeps the DT_NEEDED entry that nothing references at
# link time; the rpath finds bin/ from al-kompiled/interpreter.

KDEFDIR = al-kompiled
KSPECDIR = spec-meta-k

k-spec: metacross ffi
	kompile $(KSPECDIR)/al/6-entry.k --main-module AL --syntax-module AL-SYNTAX -o $(KDEFDIR) \
	  -ccopt -L$(CURDIR)/bin -ccopt -Wl,--no-as-needed -ccopt -lp4spectec \
	  -ccopt '-Wl,-rpath,$$ORIGIN/../bin'

k-test: k-spec
	@status=0; \
	python3 $(KSPECDIR)/scripts/run-k-typecheck.py || status=1; \
	python3 $(KSPECDIR)/scripts/run-k-typecheck.py --neg || status=1; \
	exit $$status

# PLT Redex specification
#
# ffi2 loads bin/libp4spectec.so by path; raco make keeps compiled/ in step
# with the sources.

RSPECDIR = spec-meta-redex
RENTRY = $(RSPECDIR)/al/7-entry.rkt
RTYPECHECK = $(RSPECDIR)/test/p4-typecheck.rkt

redex: metacross ffi
	raco make $(RENTRY)

# Type-checks the P4 programs of p4_16_samples (positive) and p4_16_errors
# (negative).
redex-test: redex
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
