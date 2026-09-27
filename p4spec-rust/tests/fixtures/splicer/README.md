# Splicer fixture

`expected.adoc` is the output of the OCaml backend at `99c4938a7`, covering all 18 marker kinds. Regenerate from the repository root with:

```sh
opam exec --switch=5.1.0 -- dune build p4spec/bin/main.exe
_build/default/p4spec/bin/main.exe splice p4spec-rust/tests/fixtures/splicer/spec.watsup -splice p4spec-rust/tests/fixtures/splicer/skeleton.adoc -out p4spec-rust/tests/fixtures/splicer/expected.adoc
```

The Rust test parses the same specification, runs elaboration through prosification, and compares the complete output without normalization.
