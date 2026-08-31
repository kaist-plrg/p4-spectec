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
# The K port runs a spec through the meta-language spec in spec-meta-k/,
# rather than through the OCaml interpreter.  See CROSS.md.
#
#   make k-build                         compile the K definition
#   make k-run TEC=examples/add.watsup   run one self-contained target
#   make k-typecheck P4=foo.p4           type-check a P4 program against spec/
#   make k-clean                         drop al-kompiled/
#
# Scratch files are mktemp'd directly into spec-meta-k/ by the two parser
# wrappers, which remove what they create; a run that dies on a stuck term
# deliberately leaves its own behind, since those are worth reading.
#
# kast-json.sh additionally caches the booted+parsed $PGM under
# spec-meta-k/.kore-cache/, keyed on a hash of the .watsup sources, the
# compiled definition and spectec-boot, since re-deriving it costs ~8s per run
# and `spec/` almost never changes.  `make k-clean` drops it.
#
# All must run from the repo root: the target spec path a builtin or extern call
# resolves against (`-cSPEC`, below) is relative to it.
#
# `al-kompiled/interpreter` embeds a *snapshot* of the OCaml implementation --
# kffi.exe.o is linked into it -- so after editing p4spec/ the workflow is
# `make boot && make k-build`.  `make boot` alone leaves a stale interpreter
# that silently keeps using the old builtins.

KDEFDIR = al-kompiled
KENTRY = spec-meta-k/al/6-entry.k
KSCRIPTS = spec-meta-k/scripts
# Scratch files (the k-typecheck spec stub, and whatever the wrappers mktemp)
# live here rather than in a scratch directory of their own.
KSCRATCH = spec-meta-k

# The FFI boundary, linked into the interpreter (CROSS.md §7).
#
#   kffi.exe.o  the whole OCaml implementation as one object, runtime included
#               (`(modes object)` in p4spec/bin/dune -- there is no .exe)
#   shim.o      the C ABI outside / caml_callback inside
#
# Recursive `=`, so `opam exec` and `readlink` only run when a K target
# actually expands them.
KFFI_OBJ = _build/default/p4spec/bin/kffi.exe.o
KSHIM_SRC = spec-meta-k/ffi/shim.c
KSHIM_OBJ = spec-meta-k/ffi/shim.o
OCAMLWHERE = $(shell opam exec --switch=5.1.0 -- ocamlopt -where)
K_INC = $(shell dirname $$(dirname $$(readlink -f $$(which kompile))))/include/kframework/builtin

# The P4 spec, and the include path its programs are preprocessed with.
# Program_ok needs 0-aux..5-typing only; 6-9 (instantiation, dynamic, arch) are
# not reachable from it, so SPEC may be narrowed to cut load time.
SPEC_K = spec
P4INCLUDE = p4c/p4include

# Link the OCaml implementation and the C shim into the interpreter.
#
# Gotchas, each of which otherwise produces a confusing failure
# (examples/k/README.md):
#
#   - `-rdynamic` is MANDATORY.  `#functionAddress` is `dlsym`, which searches
#     only the *dynamic* symbol table, and nothing in the interpreter
#     references these symbols, so without it the linker drops them and the run
#     segfaults immediately.  Hence the `nm -D` assertion below.
#   - `-ccopt` is a hidden kompile flag: `kompile --help-hidden`, not `--help`.
#   - `-lzstd` is required on OCaml 5.1 (its marshaller uses zstd); `-lgmp` for
#     zarith.
#   - `--backend llvm` is explicit: the FFI hooks are not implemented in the
#     Haskell backend.
.PHONY: k-build
k-build: $(BOOT) $(KFFI_OBJ) $(KSHIM_OBJ)
	kompile $(KENTRY) --main-module AL --syntax-module AL-SYNTAX -o $(KDEFDIR) \
	  --backend llvm -I "$(K_INC)" \
	  -ccopt $(KSHIM_OBJ) -ccopt $(KFFI_OBJ) \
	  -ccopt -L"$(OCAMLWHERE)" \
	  -ccopt -lasmrun -ccopt -lzstd -ccopt -lgmp \
	  -ccopt -lm -ccopt -ldl \
	  -ccopt -rdynamic
	@nm -D $(KDEFDIR)/interpreter | grep -q ml_eval_c \
	  || { echo "####> ml_eval_c not in dynamic symbol table -- -rdynamic missing?"; false; }

# Always delegated to dune, which decides whether anything actually changed.
.PHONY: $(KFFI_OBJ)
$(KFFI_OBJ):
	cd p4spec && opam exec --switch=5.1.0 -- dune build bin/kffi.exe.o && echo

$(KSHIM_OBJ): $(KSHIM_SRC)
	gcc -c -fPIC -O2 -I "$(OCAMLWHERE)" -o $@ $<

# `boot` is a prerequisite of every K target: $(KSCRIPTS)/kast-json.sh and
# $(KSCRIPTS)/kast-p4.sh still
# invoke ./spectec-boot to boot $PGM and parse $P4, entirely outside the
# interpreter.  Named as a file target so it is not rebuilt when already
# present.
$(BOOT):
	$(MAKE) boot

# `-cP4` supplies the `<p4prog>` cell.  K has no default for a configuration
# variable, so even the no-program case must name one.  `-cP4=VALUE` writes
# VALUE to a temp file and hands that file to `-pP4`'s command, so the value
# passed here is a *P4 program path* and kast-p4.sh reads it back out.
#
# The empty value selects `noP4()`, i.e. the `$main()` entry, leaving these runs
# behaving exactly as before.
#
# `-cSPEC` supplies `<specdir>`, the target spec a builtin or extern call is
# answered against (al/4.2-extern-ffi.k).  Here that is the script itself: a
# self-contained target is its own spec, and `$print_` unparses against it.
# Unlike `-cP4` it needs no `-p` wrapper -- krun parses a bare `String`, so the
# inner quotes are part of the value and the `'...'` keeps the shell off them.
# It has no default either, so a hand-written krun line must pass it too.
.PHONY: k-run
k-run: $(BOOT)
	@test -n "$(TEC)" || { echo "usage: make k-run TEC=examples/add.watsup"; exit 1; }
	KDEF=$(KDEFDIR) krun -d $(KDEFDIR) --parser ./$(KSCRIPTS)/kast-json.sh $(TEC) \
	  -cP4= -pP4=./$(KSCRIPTS)/kast-p4.sh \
	  -cSPEC='"$(TEC)"' --output none

# Type-check a P4 program: the spec is $PGM, the program is $P4, and the entry
# becomes `Program_ok` rather than `$main()`.
#
# krun requires $PGM to be a file, but the spec is a whole directory, so its
# path is handed over in a one-line stub (`@`-prefixed) that kast-json.sh
# resolves.  The creating shell removes the stub on exit in case another
# configuration parser fails before kast-json.sh runs.  `-cSPEC` names that
# same directory, this time as an ordinary value, so it needs no stub.
.PHONY: k-typecheck
k-typecheck: $(BOOT)
	@test -n "$(P4)" || { echo "usage: make k-typecheck P4=p4c/testdata/p4_16_samples/action-bind.p4"; exit 1; }
	@trap 'rm -f $(KSCRATCH)/specdir' 0; \
	printf '@%s\n' "$(SPEC_K)" > $(KSCRATCH)/specdir; \
	KDEF=$(KDEFDIR) P4INCLUDE=$(P4INCLUDE) krun -d $(KDEFDIR) \
	  --parser ./$(KSCRIPTS)/kast-json.sh $(KSCRATCH)/specdir \
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

.PHONY: k-clean
k-clean:
	rm -rf $(KDEFDIR) $(KSHIM_OBJ)
	rm -f $(KSCRATCH)/specdir $(KSCRATCH)/spectec-k-*
	rm -rf $(KSCRATCH)/.kore-cache
