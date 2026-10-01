SPEC = p4spectec
RUST_DIR = p4spec-rust
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

.PHONY: build release fmt fmt-check lint rustdoc clean promote

build release:
	$(MAKE) -C $(RUST_DIR) CARGO_TARGET_DIR="$(CARGO_TARGET_DIR)" $@
	rm -f ./$(SPEC)
	cp "$(CARGO_TARGET_DIR)/$(if $(filter release,$@),release,debug)/p4spec-rust" ./$(SPEC)

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
.PHONY: slspec slspec-html alspec alspec-html

p4spec-draft:
	$(MAKE) -C docs/p4 CARGO_TARGET_DIR="$(CARGO_TARGET_DIR)" draft
p4spec-draft-html:
	$(MAKE) -C docs/p4 CARGO_TARGET_DIR="$(CARGO_TARGET_DIR)" draft-html
p4spec-release:
	$(MAKE) -C docs/p4 CARGO_TARGET_DIR="$(CARGO_TARGET_DIR)" release
p4spec-release-html:
	$(MAKE) -C docs/p4 CARGO_TARGET_DIR="$(CARGO_TARGET_DIR)" release-html

slspec:
	$(MAKE) -C docs/sl CARGO_TARGET_DIR="$(CARGO_TARGET_DIR)" spec
slspec-html:
	$(MAKE) -C docs/sl CARGO_TARGET_DIR="$(CARGO_TARGET_DIR)" spec-html
alspec:
	$(MAKE) -C docs/al CARGO_TARGET_DIR="$(CARGO_TARGET_DIR)" spec
alspec-html:
	$(MAKE) -C docs/al CARGO_TARGET_DIR="$(CARGO_TARGET_DIR)" spec-html

clean:
	rm -f ./$(SPEC)
	$(MAKE) -C $(RUST_DIR) CARGO_TARGET_DIR="$(CARGO_TARGET_DIR)" clean
