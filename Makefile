SPEC = p4spectec

# Compile

.PHONY: build

EXESPEC = p4spec/_build/default/bin/main.exe

build:
	rm -f ./$(SPEC)
	rm -f ./p4spec/lib/parsing/parser.ml ./p4spec/lib/parsing/parser.mli
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build bin/main.exe && echo
	ln -f $(EXESPEC) ./$(SPEC)

release:
	rm -f ./$(SPEC)
	rm -f ./p4spec/lib/parsing/parser.ml ./p4spec/lib/parsing/parser.mli
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build --profile=release bin/main.exe && echo
	ln -f $(EXESPEC) ./$(SPEC)

# Spec

spec:
	cd docs && make build && cd ..
spec-html:
	cd docs && make build-html && cd ..

# Format

.PHONY: fmt

fmt:
	opam switch 5.1.0
	cd p4spec && opam exec dune fmt

# Tests

.PHONY: test-all
.PHONY: test-speclang
.PHONY: test-p4static test-p4static-il test-p4static-sl
.PHONY: test-p4dynamic test-p4dynamic-v1model test-p4dynamic-ebpf test-p4dynamic-psa
.PHONY: test-p4parse
.PHONY: promote 

test-all:
	echo "#### Running (dune runtest)"
	opam switch 5.1.0
	cd p4spec && opam exec -- dune runtest test --profile=release && echo OK || (echo "####>" Failure running dune test. && echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

test-speclang:
	echo "#### Running (dune build @speclang)"
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build @speclang --profile=release && echo OK || (echo "####>" Failure running dune build @speclang. && echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

test-p4static:
	echo "#### Running (dune build @p4static)"
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build @p4static --profile=release && echo OK || (echo "####>" Failure running dune build @p4type. && echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

test-p4static-il:
	echo "#### Running (dune build @p4static-il)"
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build @p4static-il --profile=release && echo OK || (echo "####>" Failure running dune build @p4static. && echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

test-p4static-sl:
	echo "#### Running (dune build @p4static-sl)"
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build @p4static-sl --profile=release && echo OK || (echo "####>" Failure running dune build @p4static. && echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

test-p4dynamic:
	echo "#### Running (dune build @p4dynamic)"
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build @p4dynamic --profile=release && echo OK || (echo "####>" Failure running dune build @p4dynamic. && echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

test-p4dynamic-v1model:
	echo "#### Running (dune build @p4dynamic-v1model)"
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build @p4dynamic-v1model --profile=release && echo OK || (echo "####>" Failure running dune build @p4dynamic. && echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

test-p4dynamic-ebpf:
	echo "#### Running (dune build @p4dynamic-ebpf)"
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build @p4dynamic-ebpf --profile=release && echo OK || (echo "####>" Failure running dune build @p4dynamic. && echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

test-p4dynamic-psa:
	echo "#### Running (dune build @p4dynamic-psa)"
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build @p4dynamic-psa --profile=release && echo OK || (echo "####>" Failure running dune build @p4dynamic. && echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

test-p4parse:
	echo "#### Running (dune build @p4parse)"
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build @p4parse --profile=release && echo OK || (echo "####>" Failure running dune build @p4parse. && echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

promote:
	opam switch 5.1.0
	cd p4spec && opam exec -- dune promote

# Cleanup

.PHONY: clean

clean:
	rm -f ./$(SPEC)
	cd p4spec && dune clean
