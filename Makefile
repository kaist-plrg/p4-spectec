SPEC = bin/p4spectec
RUST_DIR = p4spectec
# Resolve an override before entering the Rust crate or documentation directories
override CARGO_TARGET_DIR := $(if $(CARGO_TARGET_DIR),$(CARGO_TARGET_DIR),$(RUST_DIR)/target)
ifeq ($(filter /%,$(CARGO_TARGET_DIR)),)
override CARGO_TARGET_DIR := $(CURDIR)/$(CARGO_TARGET_DIR)
endif
export CARGO_TARGET_DIR

.DEFAULT_GOAL := build
# Root workflows share the installed executable and generated document sections
# Recursive Rust tests still use the caller's parallel job count
.NOTPARALLEL:

.PHONY: build release debug fmt fmt-check lint rustdoc clean promote

build release debug:
	$(MAKE) -C $(RUST_DIR) CARGO_TARGET_DIR="$(CARGO_TARGET_DIR)" $@
	mkdir -p $(dir $(SPEC))
	rm -f ./$(SPEC)
	cp "$(CARGO_TARGET_DIR)/$(if $(filter debug,$@),debug,release)/p4spectec" ./$(SPEC)

fmt fmt-check lint rustdoc:
	$(MAKE) -C $(RUST_DIR) CARGO_TARGET_DIR="$(CARGO_TARGET_DIR)" $@

# End-to-end acceptance and explicit snapshot updates
TEST_TARGETS := test test-expected test-elab test-algo test-structure test-prose \
  test-adoc test-p4parse test-run-al test-run-sl test-run-pl \
  test-sim-al test-sim-sl test-sim-pl test-diagnostics \
  test-promote test-diagnostics-promote

.PHONY: $(TEST_TARGETS)
$(TEST_TARGETS):
	$(MAKE) -C $(RUST_DIR) CARGO_TARGET_DIR="$(CARGO_TARGET_DIR)" $@

promote: test-promote

# Specification documents
.PHONY: p4spec-draft p4spec-draft-html p4spec-release p4spec-release-html

p4spec-draft:
	$(MAKE) -C docs/p4 CARGO_TARGET_DIR="$(CARGO_TARGET_DIR)" draft
p4spec-draft-html:
	$(MAKE) -C docs/p4 CARGO_TARGET_DIR="$(CARGO_TARGET_DIR)" draft-html
p4spec-release:
	$(MAKE) -C docs/p4 CARGO_TARGET_DIR="$(CARGO_TARGET_DIR)" release
p4spec-release-html:
	$(MAKE) -C docs/p4 CARGO_TARGET_DIR="$(CARGO_TARGET_DIR)" release-html

clean:
	rm -f ./$(SPEC)
	$(MAKE) -C $(RUST_DIR) CARGO_TARGET_DIR="$(CARGO_TARGET_DIR)" clean
