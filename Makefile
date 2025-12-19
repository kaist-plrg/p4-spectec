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
.PHONY: test-speclang test-p4type test-p4inst test-p4ntt test-p4parse
.PHONY: promote 

test-all:
	echo "#### Running (dune runtest)"
	opam switch 5.1.0
	cd p4spec && opam exec -- dune runtest test --profile=release && echo OK || (echo "####>" Failure running dune test. && echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

test-speclang:
	echo "#### Running (dune build @speclang)"
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build @speclang --profile=release && echo OK || (echo "####>" Failure running dune build @speclang. && echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

test-p4type:
	echo "#### Running (dune build @p4type)"
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build @p4type --profile=release && echo OK || (echo "####>" Failure running dune build @p4type. && echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

test-p4type-il:
	echo "#### Running (dune build @p4type-il)"
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build @p4type-il --profile=release && echo OK || (echo "####>" Failure running dune build @p4type. && echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

test-p4type-sl:
	echo "#### Running (dune build @p4type-sl)"
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build @p4type-sl --profile=release && echo OK || (echo "####>" Failure running dune build @p4type. && echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

test-p4inst:
	echo "#### Running (dune build @p4inst)"
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build @p4inst --profile=release && echo OK || (echo "####>" Failure running dune build @p4inst. && echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

test-p4inst-il:
	echo "#### Running (dune build @p4inst-il)"
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build @p4inst-il --profile=release && echo OK || (echo "####>" Failure running dune build @p4inst. && echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

test-p4inst-sl:
	echo "#### Running (dune build @p4inst-sl)"
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build @p4inst-sl --profile=release && echo OK || (echo "####>" Failure running dune build @p4inst. && echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

test-p4sim:
	echo "#### Running (dune build @p4sim)"
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build @p4sim --profile=release && echo OK || (echo "####>" Failure running dune build @p4sim. && echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

test-p4ntt:
	echo "#### Running (dune build @p4ntt)"
	opam switch 5.1.0
	cd p4spec && opam exec -- dune build @p4ntt --profile=release && echo OK || (echo "####>" Failure running dune build @p4ntt. && echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

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
