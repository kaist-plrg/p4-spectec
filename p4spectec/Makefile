# Rust builds and integration tests

CARGO := cargo
TARGET_DIR := $(if $(CARGO_TARGET_DIR),$(CARGO_TARGET_DIR),target)
ifeq ($(filter /%,$(TARGET_DIR)),)
TARGET_DIR := $(CURDIR)/$(TARGET_DIR)
endif

.PHONY: build release debug fmt fmt-check lint rustdoc clean

debug:
	$(CARGO) build --locked -p p4spectec-cli --target-dir "$(TARGET_DIR)"

build release:
	$(CARGO) build --locked --release -p p4spectec-cli --target-dir "$(TARGET_DIR)"

fmt:
	$(CARGO) fmt --all

fmt-check:
	$(CARGO) fmt --all --check

lint:
	$(CARGO) clippy --locked --workspace --all-targets --no-deps --target-dir "$(TARGET_DIR)"

rustdoc:
	RUSTDOCFLAGS="-D warnings" $(CARGO) doc --locked --no-deps -p p4spectec -p p4spectec-sim --target-dir "$(TARGET_DIR)"

clean:
	$(CARGO) clean --target-dir "$(TARGET_DIR)"

.DEFAULT_GOAL := test

# Integration tests under test-driver/tests and cli/tests, run by cargo-nextest
# PROFILE=NAME selects a profile of .config/nextest.toml (CI uses `ci`)
# TIMEOUT=SECONDS gives each corpus case a timeout (none by default)
# UPDATE_EXPECT=1 rewrites expected files instead of comparing with them
.PHONY: test

test:
	$(if $(TIMEOUT),P4SPECTEC_TEST_TIMEOUT=$(TIMEOUT)) $(CARGO) nextest run --locked --release --workspace --target-dir "$(TARGET_DIR)" $(if $(PROFILE),--profile $(PROFILE))
