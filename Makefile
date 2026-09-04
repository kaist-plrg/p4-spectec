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

# K specification

KDEFDIR = al-kompiled
KENTRY = spec-meta-k/al/6-entry.k
KSCRIPTS = spec-meta-k/scripts
KSCRATCH = spec-meta-k

KFFI_OBJ = _build/default/p4spec/bin/kffi.exe.o
KSHIM_SRC = spec-meta-k/ffi/shim.c
KSHIM_OBJ = spec-meta-k/ffi/shim.o
OCAMLWHERE = $(shell opam exec --switch=5.1.0 -- ocamlopt -where)
K_INC = $(shell dirname $$(dirname $$(readlink -f $$(which kompile))))/include/kframework/builtin

SPEC_K = spec
P4INCLUDE = p4c/p4include

.PHONY: k-spec
k-spec: boot $(KFFI_OBJ) $(KSHIM_OBJ)
	kompile $(KENTRY) --main-module AL --syntax-module AL-SYNTAX -o $(KDEFDIR) \
	  -ccopt $(KSHIM_OBJ) -ccopt $(KFFI_OBJ) \
	  -ccopt -L"$(OCAMLWHERE)" \
	  -ccopt -lasmrun -ccopt -lzstd -ccopt -lgmp \
	  -ccopt -lm -ccopt -ldl \
	  -ccopt -rdynamic

.PHONY: $(KFFI_OBJ)
$(KFFI_OBJ):
	cd p4spec && opam exec --switch=5.1.0 -- dune build bin/kffi.exe.o && echo

$(KSHIM_OBJ): $(KSHIM_SRC)
	gcc -c -fPIC -O2 -I "$(OCAMLWHERE)" -o $@ $<

.PHONY: k-run
k-run: boot
	@test -n "$(TEC)" || { echo "usage: make k-run TEC=examples/add.watsup"; exit 1; }
	KDEF=$(KDEFDIR) krun -d $(KDEFDIR) --parser ./$(KSCRIPTS)/kast-spec.sh $(TEC) \
	  -cP4= -pP4=./$(KSCRIPTS)/kast-p4.sh \
	  -cSPEC='"$(TEC)"' --output none

.PHONY: k-typecheck
k-typecheck: boot
	@test -n "$(P4)" || { echo "usage: make k-typecheck P4=p4c/testdata/p4_16_samples/action-bind.p4"; exit 1; }
	@trap 'rm -f $(KSCRATCH)/specdir' 0; \
	printf '@%s\n' "$(SPEC_K)" > $(KSCRATCH)/specdir; \
	KDEF=$(KDEFDIR) P4INCLUDE=$(P4INCLUDE) krun -d $(KDEFDIR) \
	  --parser ./$(KSCRIPTS)/kast-spec.sh $(KSCRATCH)/specdir \
	  -cP4=$(P4) -pP4=./$(KSCRIPTS)/kast-p4.sh \
	  -cSPEC='"$(SPEC_K)"' --output none

.PHONY: k-test
k-test:
	@status=0; \
	python3 $(KSCRIPTS)/run-k-typecheck.py || status=1; \
	python3 $(KSCRIPTS)/run-k-typecheck.py --neg || status=1; \
	exit $$status

# Cleanup

.PHONY: clean

clean:
	rm -f ./$(SPEC)
	cd p4spec && opam exec --switch=5.1.0 -- dune clean
	rm -rf $(KDEFDIR) $(KSHIM_OBJ)
	rm -f $(KSCRATCH)/specdir $(KSCRATCH)/spectec-k-*
	rm -rf $(KSCRATCH)/.kore-cache
