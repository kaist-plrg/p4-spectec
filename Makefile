SPEC = p4spectec
BOOT = spectec-boot
COMP = p4spectec-comp
BOOTCOMP = spectec-boot-comp

# Compile

.PHONY: build stat perf spec-test ensure-interp-ml restore-stub build-compiled test-run-ml test-run-ml-inc test-sim-ml test-sim-ml-inc

# Executables

EXESPEC = p4spec/_build/default/bin/main.exe
EXEBOOT = p4spec/_build/default/bin/boot.exe
EXECOMP = p4spec/_build/default/bin/comp.exe
EXEBOOTCOMP = p4spec/_build/default/bin/compboot.exe

# Compiled spec paths

INTERP_ML      = p4spec/lib/backend-ocaml/interp_ml.ml
INTERP_ML_STUB = p4spec/lib/backend-ocaml/interp_ml_stub.ml
# The heavy generated spec is split across a part-library directory; restore-stub
# swaps in a tiny committed mirror so plain `make build` stays fast.
INTERP_ML_DIR      = p4spec/lib/backend-ocaml/compiled
INTERP_ML_STUB_DIR = p4spec/lib/backend-ocaml/compiled_stub
UNPARSE_COMPILED      = p4spec/lib/interface/p4/unparse_compiled.ml
UNPARSE_COMPILED_STUB = p4spec/lib/interface/p4/unparse_compiled_stub.ml

# Compiled meta-spec paths (spectec-boot-comp: spec-meta/il, spec-meta/sl)

INTERP_ML_IL      = p4spec/lib/backend-ocaml-il/interp_ml.ml
INTERP_ML_IL_STUB = p4spec/lib/backend-ocaml-il/interp_ml_stub.ml
INTERP_ML_IL_DIR      = p4spec/lib/backend-ocaml-il/compiled
INTERP_ML_IL_STUB_DIR = p4spec/lib/backend-ocaml-il/compiled_stub

INTERP_ML_SL      = p4spec/lib/backend-ocaml-sl/interp_ml.ml
INTERP_ML_SL_STUB = p4spec/lib/backend-ocaml-sl/interp_ml_stub.ml
INTERP_ML_SL_DIR      = p4spec/lib/backend-ocaml-sl/compiled
INTERP_ML_SL_STUB_DIR = p4spec/lib/backend-ocaml-sl/compiled_stub

# Restore the compiled spec to a stub version

restore-stub:
	rm -rf $(INTERP_ML_DIR)
	cp -R $(INTERP_ML_STUB_DIR) $(INTERP_ML_DIR)
	cp $(INTERP_ML_STUB) $(INTERP_ML)
	cp $(UNPARSE_COMPILED_STUB) $(UNPARSE_COMPILED)
	rm -rf $(INTERP_ML_IL_DIR)
	cp -R $(INTERP_ML_IL_STUB_DIR) $(INTERP_ML_IL_DIR)
	cp $(INTERP_ML_IL_STUB) $(INTERP_ML_IL)
	rm -rf $(INTERP_ML_SL_DIR)
	cp -R $(INTERP_ML_SL_STUB_DIR) $(INTERP_ML_SL_DIR)
	cp $(INTERP_ML_SL_STUB) $(INTERP_ML_SL)

# Build EXESPEC

build: restore-stub
	rm -f ./$(SPEC)
	rm -f ./p4spec/lib/parsing/parser.ml ./p4spec/lib/parsing/parser.mli
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build bin/main.exe && echo
	ln -f $(EXESPEC) ./$(SPEC)
	cd p4spec && opam exec -- dune build test/lang/test.exe test/run/test.exe test/sim/test.exe test/parse/test.exe test/boot/test.exe && echo

# Build EXEBOOT

boot: restore-stub
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build bin/boot.exe && echo
	ln -f $(EXEBOOT) ./$(BOOT)
	cd p4spec && opam exec -- dune build test/lang/test.exe test/run/test.exe test/sim/test.exe test/parse/test.exe test/boot/test.exe && echo

# Build INTERP_ML

# gen-ocaml compiles via the ML backend, so it uses the compile-optimized
# variant (defined-function stdlib maps). The interpreter test rules use spec/p4.
SPEC_PATHS ?= spec/p4-comp

ifeq ($(firstword $(MAKECMDGOALS)),gen-ocaml)
_gen_ocaml_paths := $(or $(filter-out gen-ocaml,$(MAKECMDGOALS)),$(SPEC_PATHS))
$(filter-out gen-ocaml,$(MAKECMDGOALS)):
	@:
else
_gen_ocaml_paths := $(SPEC_PATHS)
endif

gen-ocaml: restore-stub
	./$(SPEC) ocaml $(_gen_ocaml_paths) \
	  -o $(INTERP_ML) \
	  -o-unparse $(UNPARSE_COMPILED)

# Compile spec-meta/il and spec-meta/sl via spectec-boot's `compile` command.
# Unlike `gen-ocaml`, these do NOT depend on `restore-stub` (which would wipe
# out any already-generated compiled specs) — they only ensure every compiled
# dir exists in *some* form (real or stub) so bin/boot.exe still builds, then
# build+link spectec-boot idempotently before invoking `compile`.

.PHONY: gen-ocaml-il gen-ocaml-sl

gen-ocaml-il: ensure-interp-ml ensure-interp-ml-il ensure-interp-ml-sl
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build bin/boot.exe && echo
	ln -f $(EXEBOOT) ./$(BOOT)
	./$(BOOT) compile spec-meta/il -o $(INTERP_ML_IL) -name il

gen-ocaml-sl: ensure-interp-ml ensure-interp-ml-il ensure-interp-ml-sl
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build bin/boot.exe && echo
	ln -f $(EXEBOOT) ./$(BOOT)
	./$(BOOT) compile spec-meta/sl -o $(INTERP_ML_SL) -name sl

# Build EXECOMP with INTERP_ML

ensure-interp-ml:
	@test -d $(INTERP_ML_DIR) || cp -R $(INTERP_ML_STUB_DIR) $(INTERP_ML_DIR)
	@test -f $(INTERP_ML) || cp $(INTERP_ML_STUB) $(INTERP_ML)

build-compiled: ensure-interp-ml
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build bin/comp.exe && echo
	ln -f $(EXECOMP) ./$(COMP)

# Build EXEBOOTCOMP with INTERP_ML, INTERP_ML_IL, INTERP_ML_SL

.PHONY: ensure-interp-ml-il ensure-interp-ml-sl build-boot-comp

ensure-interp-ml-il:
	@test -d $(INTERP_ML_IL_DIR) || cp -R $(INTERP_ML_IL_STUB_DIR) $(INTERP_ML_IL_DIR)
	@test -f $(INTERP_ML_IL) || cp $(INTERP_ML_IL_STUB) $(INTERP_ML_IL)

ensure-interp-ml-sl:
	@test -d $(INTERP_ML_SL_DIR) || cp -R $(INTERP_ML_SL_STUB_DIR) $(INTERP_ML_SL_DIR)
	@test -f $(INTERP_ML_SL) || cp $(INTERP_ML_SL_STUB) $(INTERP_ML_SL)

build-boot-comp: ensure-interp-ml ensure-interp-ml-il ensure-interp-ml-sl
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build bin/compboot.exe && echo
	ln -f $(EXEBOOTCOMP) ./$(BOOTCOMP)

# Run ML-mode tests using the compiled spec.
# Does NOT call restore-stub — generates OCaml then rebuilds test.exe.

test-run-ml: gen-ocaml
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build test/run/test.exe && echo
	cd p4spec && opam exec -- dune build @run-ml --profile=release && echo OK

# Incremental: skip gen-ocaml, assume interp_ml.ml already up to date.

test-run-ml-inc:
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build test/run/test.exe && echo
	cd p4spec && opam exec -- dune build @run-ml --profile=release && echo OK

# sim ML-mode tests

test-sim-ml: gen-ocaml
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build test/sim/test.exe && echo
	cd p4spec && opam exec -- dune build @sim-ml --profile=release && echo OK

test-sim-ml-inc:
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build test/sim/test.exe && echo
	cd p4spec && opam exec -- dune build @sim-ml --profile=release && echo OK

# Per-arch/per-tool sim ML-mode tests.
# test-sim-<arch>-<tool>-ml      runs gen-ocaml first (two-pass).
# test-sim-<arch>-<tool>-ml-inc  skips gen-ocaml, assumes interp_ml.ml up to date.
define dune-sim-ml-test
.PHONY: test-sim-$(1)-ml test-sim-$(1)-ml-inc
test-sim-$(1)-ml: gen-ocaml
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build test/sim/test.exe && echo
	cd p4spec && opam exec -- dune build @sim-$(1)-ml --profile=release && echo OK

test-sim-$(1)-ml-inc:
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build test/sim/test.exe && echo
	cd p4spec && opam exec -- dune build @sim-$(1)-ml --profile=release && echo OK
endef

SIM_ML_TARGETS := \
  v1model-p4c v1model-p4testgen \
  ebpf-p4c ebpf-p4testgen \
  psa-p4c

$(foreach t,$(SIM_ML_TARGETS),$(eval $(call dune-sim-ml-test,$(t))))

# Release build for EXESPEC and EXEBOOT

release: restore-stub
	rm -f ./$(SPEC)
	rm -f ./p4spec/lib/parsing/parser.ml ./p4spec/lib/parsing/parser.mli
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build --profile=release bin/main.exe && echo
	ln -f $(EXESPEC) ./$(SPEC)
	cd p4spec && opam exec -- dune build --profile=release bin/boot.exe && echo
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

ilspec:
	cd docs/il && make spec && cd ../..
ilspec-html:
	cd docs/il && make spec-html && cd ../..

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
# NOTE: run-ml is intentionally excluded — it requires `make gen-ocaml` first (two-pass).
#       Use `make gen-ocaml-test-ml` or `make test-run-ml` to run ML tests in isolation.
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
