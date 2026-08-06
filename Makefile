SPEC = p4spectec
BOOT = spectec-boot

# Compile

.PHONY: build stat perf spec-test

EXESPEC = _build/default/p4spec/bin/main.exe
EXEBOOT = _build/default/p4spec/bin/boot.exe

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
	cd p4spec && opam exec --switch=5.1.0 -- dune runtest test --profile=release && echo OK || \
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

# K specification (spec-meta-k)
#
# The K port runs a .watsup through the meta-language spec in spec-meta-k/,
# rather than through the OCaml interpreter.  See CROSS.md.
#
#   make k-build                         compile the K definition
#   make k-run TEC=examples/add.watsup   run one target
#   make k-clean                         drop al-kompiled/ and .tmp/
#
# All must run from the repo root: a spec that calls a builtin shells out to
# ./spectec-boot at that relative path.

KDEFDIR = al-kompiled
KENTRY = spec-meta-k/al/5-entry.k
KTMP = .tmp

.PHONY: k-build
k-build: $(BOOT)
	kompile $(KENTRY) --main-module AL --syntax-module AL-SYNTAX -o $(KDEFDIR)

# `boot` is a prerequisite of every K target: builtin calls shell out to it.
# Named as a file target so it is not rebuilt when already present.
$(BOOT):
	$(MAKE) boot

.PHONY: k-run
k-run: $(BOOT)
	@test -n "$(TEC)" || { echo "usage: make k-run TEC=examples/add.watsup"; exit 1; }
	@mkdir -p $(KTMP)
	KDEF=$(KDEFDIR) krun -d $(KDEFDIR) --parser ./kast-json.sh $(TEC)

# Cleanup

.PHONY: clean

clean:
	rm -f ./$(SPEC)
	cd p4spec && opam exec --switch=5.1.0 -- dune clean

.PHONY: k-clean
k-clean:
	rm -rf $(KDEFDIR) $(KTMP)
