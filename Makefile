SPEC = p4spectec
BOOT = spectec-boot
NANO = nano-p4spectec

# Compile

.PHONY: build stat perf spec-test

EXESPEC = _build/default/p4spec/bin/main.exe
EXEBOOT = _build/default/p4spec/bin/boot.exe
EXENANO = _build/default/p4spec/bin/nano.exe

build:
	rm -f ./$(SPEC)
	rm -f ./p4spec/lib/parsing/parser.ml ./p4spec/lib/parsing/parser.mli
	cd p4spec && opam exec --switch=5.1.0 -- dune build bin/main.exe && echo
	ln -f $(EXESPEC) ./$(SPEC)
	cd p4spec && opam exec --switch=5.1.0 -- dune build test/lang/test.exe test/run/test.exe test/sim/test.exe test/parse/test.exe test/boot/test.exe && echo

boot:
	cd p4spec && opam exec --switch=5.1.0 -- dune build bin/boot.exe && echo
	ln -f $(EXEBOOT) ./$(BOOT)
	cd p4spec && opam exec --switch=5.1.0 -- dune build test/lang/test.exe test/run/test.exe test/sim/test.exe test/parse/test.exe test/boot/test.exe && echo

release:
	rm -f ./$(SPEC)
	rm -f ./p4spec/lib/parsing/parser.ml ./p4spec/lib/parsing/parser.mli
	cd p4spec && opam exec --switch=5.1.0 -- dune build --profile=release bin/main.exe && echo
	ln -f $(EXESPEC) ./$(SPEC)
	cd p4spec && opam exec --switch=5.1.0 -- dune build --profile=release bin/boot.exe && echo
	ln -f $(EXEBOOT) ./$(BOOT)
	cd p4spec && opam exec -- dune build --profile=release bin/nano.exe && echo
	ln -f $(EXENANO) ./$(NANO)

# Spec

p4spec-draft:
	cd docs/p4 && make draft && cd ../..
p4spec-draft-html:
	cd docs/p4 && make draft-html && cd ../..

p4spec-release:
	cd docs/p4 && make release && cd ../..
p4spec-release-html:
	cd docs/p4 && make release-html && cd ../..

slspec:
	cd docs/sl && make spec && cd ../..
slspec-html:
	cd docs/sl && make spec-html && cd ../..

alspec:
	cd docs/al && make spec && cd ../..
alspec-html:
	cd docs/al && make spec-html && cd ../..

# Format

.PHONY: fmt

fmt:
	cd p4spec && opam exec --switch=5.1.0 -- dune fmt

# Tests

# Generate a test target: test-<alias> that runs `dune build @<alias>`
define dune-alias-test
.PHONY: test-$(1)
test-$(1):
	echo "#### Running (dune build @$(1))"
	cd p4spec && opam exec --switch=5.1.0 -- dune build @$(1) --profile=release && echo OK || \
	  (echo "####>" Failure running dune build @$(1). && \
	   echo "####>" Run \`make promote\` to accept changes in test expectations. && false)
endef

# Fast tests (no -det)
TEST_ALIASES := \
  backend-adoc \
  backend-latex \
  backend-splice \
  speclang \
  run run-al run-sl run-pl \
  sim-al sim-sl sim-pl \
  sim-v1model-p4c-al sim-v1model-p4c-sl sim-v1model-p4c-pl \
  sim-v1model-p4testgen-al sim-v1model-p4testgen-sl sim-v1model-p4testgen-pl \
  sim-v1model-custom-al sim-v1model-custom-sl sim-v1model-custom-pl \
  sim-ebpf-p4c-al sim-ebpf-p4c-sl sim-ebpf-p4c-pl \
  sim-ebpf-p4testgen-al sim-ebpf-p4testgen-sl sim-ebpf-p4testgen-pl \
  sim-psa-p4c-al sim-psa-p4c-sl sim-psa-p4c-pl \
  p4parse \
	boot

$(foreach a,$(TEST_ALIASES),$(eval $(call dune-alias-test,$(a))))

# Det tests (slow, with -det)
DET_ALIASES := \
	micro-det \
  run-det run-al-det run-sl-det \
  sim-al-det sim-sl-det sim-pl-det \
  sim-v1model-p4c-al-det sim-v1model-p4c-sl-det sim-v1model-p4c-pl-det \
  sim-v1model-p4testgen-al-det sim-v1model-p4testgen-sl-det sim-v1model-p4testgen-pl-det \
  sim-v1model-custom-al-det sim-v1model-custom-sl-det sim-v1model-custom-pl-det \
  sim-ebpf-p4c-al-det sim-ebpf-p4c-sl-det sim-ebpf-p4c-pl-det \
  sim-ebpf-p4testgen-al-det sim-ebpf-p4testgen-sl-det sim-ebpf-p4testgen-pl-det \
  sim-psa-p4c-al-det sim-psa-p4c-sl-det sim-psa-p4c-pl-det

$(foreach a,$(DET_ALIASES),$(eval $(call dune-alias-test,$(a))))

# Micro tier: fast shallow coverage of every test category.
.PHONY: test-micro
test-micro:
	echo "#### Running (dune build @speclang @micro @micro-det)"
	cd p4spec && opam exec --switch=5.1.0 -- dune build @speclang @micro @micro-det --profile=release && echo OK || \
	  (echo "####>" Failure running dune build @speclang @micro @micro-det. && \
	   echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

.PHONY: test-fast
test-fast:
	echo "#### Running fast tests (speclang, p4parse, run-sl, sim-sl)"
	cd p4spec && opam exec --switch=5.1.0 -- dune build @speclang @p4parse @run-sl @sim-sl --profile=release && echo OK || \
	  (echo "####>" Failure running fast tests. && \
	   echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

.PHONY: test-all
test-all:
	echo "#### Running all tests (without -det)"
	cd p4spec && opam exec --switch=5.1.0 -- dune build @backend-adoc @backend-latex @backend-splice @speclang @p4parse @boot @run @sim-al @sim-sl @sim-pl --profile=release && echo OK || \
	  (echo "####>" Failure running dune test. && \
	   echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

.PHONY: test-all-det
test-all-det:
	echo "#### Running all tests (with -det)"
	cd p4spec && opam exec --switch=5.1.0 -- dune build @speclang @p4parse @run-det @sim-al-det @sim-sl-det --profile=release && echo OK || \
	  (echo "####>" Failure running det tests. && \
	   echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

.PHONY: promote
promote:
	cd p4spec && opam exec --switch=5.1.0 -- dune promote

# Cleanup

.PHONY: clean

clean:
	rm -f ./$(SPEC)
	cd p4spec && opam exec --switch=5.1.0 -- dune clean
