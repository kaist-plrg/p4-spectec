SPEC = p4spectec
BOOT = spectec-boot

# Compile

.PHONY: build stat perf spec-test

EXESPEC = p4spec/_build/default/bin/main.exe
EXEBOOT = p4spec/_build/default/bin/boot.exe

build:
	rm -f ./$(SPEC)
	rm -f ./p4spec/lib/parsing/parser.ml ./p4spec/lib/parsing/parser.mli
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build bin/main.exe && echo
	ln -f $(EXESPEC) ./$(SPEC)

boot:
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build bin/boot.exe && echo
	ln -f $(EXEBOOT) ./$(BOOT)

release:
	rm -f ./$(SPEC)
	rm -f ./p4spec/lib/parsing/parser.ml ./p4spec/lib/parsing/parser.mli
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build --profile=release bin/main.exe && echo
	ln -f $(EXESPEC) ./$(SPEC)
	cd p4spec && opam exec -- dune build --profile=release bin/boot.exe && echo
	ln -f $(EXEBOOT) ./$(BOOT)

# Spec

spec-draft:
	cd docs && make draft && cd ..
spec-draft-html:
	cd docs && make draft-html && cd ..

spec-release:
	cd docs && make release && cd ..
spec-release-html:
	cd docs && make release-html && cd ..

# Format

.PHONY: fmt

fmt:
	opam switch 5.1.0
	cd p4spec && opam exec dune fmt

# Tests

# Generate a test target: test-<alias> that runs `dune build @<alias>`
define dune-alias-test
.PHONY: test-$(1)
test-$(1):
	echo "#### Running (dune build @$(1))"
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build @$(1) --profile=release && echo OK || \
	  (echo "####>" Failure running dune build @$(1). && \
	   echo "####>" Run \`make promote\` to accept changes in test expectations. && false)
endef

# Fast tests (no -det)
TEST_ALIASES := \
  speclang \
  run run-il run-sl \
  sim-il sim-sl \
  sim-v1model-p4c-il sim-v1model-p4c-sl \
  sim-v1model-p4testgen-il sim-v1model-p4testgen-sl \
  sim-v1model-custom-il sim-v1model-custom-sl \
  sim-ebpf-p4c-il sim-ebpf-p4c-sl \
  sim-ebpf-p4testgen-il sim-ebpf-p4testgen-sl \
  sim-psa-p4c-il sim-psa-p4c-sl \
  p4parse \
	boot

$(foreach a,$(TEST_ALIASES),$(eval $(call dune-alias-test,$(a))))

# Det tests (slow, with -det)
DET_ALIASES := \
  run-det run-il-det run-sl-det \
  sim-il-det sim-sl-det \
  sim-v1model-p4c-il-det sim-v1model-p4c-sl-det \
  sim-v1model-p4testgen-il-det sim-v1model-p4testgen-sl-det \
  sim-v1model-custom-il-det sim-v1model-custom-sl-det \
  sim-ebpf-p4c-il-det sim-ebpf-p4c-sl-det \
  sim-ebpf-p4testgen-il-det sim-ebpf-p4testgen-sl-det \
  sim-psa-p4c-il-det sim-psa-p4c-sl-det

$(foreach a,$(DET_ALIASES),$(eval $(call dune-alias-test,$(a))))

.PHONY: test-fast
test-fast:
	echo "#### Running fast tests (speclang, p4parse, run-sl, sim-sl)"
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build @speclang @p4parse @run-sl @sim-sl --profile=release && echo OK || \
	  (echo "####>" Failure running fast tests. && \
	   echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

.PHONY: test-all
test-all:
	echo "#### Running all tests (without -det)"
	opam switch 5.1.0
	cd p4spec && opam exec -- dune runtest test --profile=release && echo OK || \
	  (echo "####>" Failure running dune test. && \
	   echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

.PHONY: test-all-det
test-all-det:
	echo "#### Running all tests (with -det)"
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build @speclang @p4parse @run-det @sim-il-det @sim-sl-det --profile=release && echo OK || \
	  (echo "####>" Failure running det tests. && \
	   echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

.PHONY: promote
promote:
	opam switch 5.1.0
	cd p4spec && opam exec -- dune promote

# Cleanup

.PHONY: clean

clean:
	rm -f ./$(SPEC)
	cd p4spec && dune clean
