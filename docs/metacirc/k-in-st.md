# K-in-ST: implementer guide

K-in-ST runs K definitions on **spec-k**, the K semantics written in
P4-SpecTec. Given a K definition compiled to KORE and an initial term, it
should print the same final configuration as `krun`. The Rust side of the
tower is already built; you fill in a few marked places. This guide says which
places, in what order, and how to check each step. You do not need to know
the rest of the Rust code base.

ST-in-K is a separate task with its own guide (`st-in-k.md`). The two share no
code: do not use anything under `engine/k/` or `protocol/meta_k/`, and ST-in-K
will not use yours.

## 1. The picture

```
   input: imp-kompiled/definition.kore + init.kore (+ --depth N), as krun -d takes them
     |
     |  KoreInterface::parse_program -> parse_kore  -- YOU: interface/kore/
     |  (also loads imp-kompiled's K library)   -- YOU: offload.rs, ffi.rs
     v
   (kdefinition, kore) values
     |
+-----------------------------------+
| spec-k (P4-SpecTec)               |  the only level, the base  -- YOU: spec-k/
|   Krun: kdefinition |- kore nat?  |
|   hooks: builtin dec $...  -------+--> hooks.rs --> Offload --> K library's own hook
+-----------------------------------+                             (K framework, not Rust)
| SpecTecEngine (AL/SL interpreter) |                            -- unchanged
+-----------------------------------+
     |
     v
   runres  -->  KoreInterface::print_runres  -->  text, compared with krun's
```

The command is

```
bin/metacirc run-meta towers/k-in-st.json -p imp-kompiled/definition.kore -p init.kore --depth 100
```

* `imp-kompiled/` is a kompiled directory, as `krun -d` takes; it holds the
  definition and the K library compiled from it.
* `KoreInterface::parse_program` reads the two files and loads that library.
* `Tower::eval_krun` calls spec-k's `Krun` with them and the step bound.
* `KoreInterface::print_runres` prints the result.
* **Most K hooks are offloaded:** spec-k delegates them (`builtin dec $…`),
  and they run in the K framework's own implementation in that library,
  not in SpecTec and not in new Rust code.
* All of that wiring exists; the `KoreInterface` pieces, the offload, and
  spec-k itself are yours.

## 2. Your places

Add code only in these places; add new files inside the directories freely.

| place | what you fill |
|---|---|
| `spec-k/` | spec-k from #395, on the frontend of this branch |
| `p4spectec/src/interface/kore/` | `parse_kore` and `print_runres` (`mod.rs`), the offload (`offload.rs`, `ffi.rs`), the hook list (`hooks.rs`), and any new files they need (a KORE parser, ...) |
| `p4spectec/test-driver/src/metacirc/k_in_st.rs` | the comparison with `krun` |
| `p4spectec/test-driver/suites/metacirc/k-in-st.toml`, `testdata/k-in-st/` | the test cases, and deleting the `pending` line at the end |
| `towers/k-in-st.json` | the specification path, if yours differs |

Do not change any other file. If you think you have to, the template is
missing something: ask the template's owner to add it first.

`interface/kore/ffi.rs` is the only file in your places where `unsafe` is
allowed. Keep every `unsafe` block there.

## 3. Where #395's code goes

| #395 file | what it does | new place |
|---|---|---|
| `interface/kore/ast.ml`, `parse.ml` | KORE text to a syntax tree | `interface/kore/` (for example `parse.rs`) |
| `interface/kore/load.ml` | the tree to spec-k's `kdefinition` and `kore` values | `interface/kore/`, called by `parse_kore` |
| `interface/kore/unparse.ml` | spec-k values back to KORE text | `interface/kore/`, called by `print_runres` |
| `interface/kore/builtins.ml`, `floats.ml`, `secp256k1_stubs.c` | the hooks spec-k delegates, in OCaml | mostly replaced by offload (`offload.rs`); port to `hooks.rs` only a hook the K library cannot answer |
| `spec-k/*.watsup` | the K semantics | `spec-k/` |
| `interface/kore/k.ml`, `krun.ml`, `bin/boot.ml` | the interface setup and the `krun` command | already done: `metacirc run-meta ... --depth` |
| `spec-k/tests/` (`difftest.py`, `ktest.py`, `manual/`) | comparing with `krun` | `test-driver/src/metacirc/k_in_st.rs`, `k-in-st.toml`, `testdata/k-in-st/` |
| `interface/kore/step_check.ml` | step-by-step check against K | later, on the same K library |

## 4. The functions to fill

### `interface/kore/mod.rs`: `parse_kore`

`parse_program` is done: it calls `parse_kore`, then `Offload::load` with the
directory of `definition.kore`.

* Input: the paths of `definition.kore` and `init.kore`.
* Output: one tuple value `(kdefinition, kore)` whose parts are values of
  spec-k's syntax (`spec-k/1-syntax.watsup`).
* A value of a `syntax` case is made with `make::case_string!`, from the
  case's notation, its arguments, and its type name. For
  `syntax kore = ... | KAPP symid sort* kore* | ...`:

  ```rust
  use crate::lang::{common::source::Span, data::value::flat::make};

  let value_kapp = make::case_string! { arena: arena,
      shape: "KAPP symid sort* kore*",
      args: vec![value_symid, value_sorts, value_kores],
      typ: "kore",
      span: Span::default(),
  }?;
  ```

  Lists are `make::list`, options `make::opt`, texts `make::text`, numbers
  `make::nat` and `make::int`, and tuples `make::tuple`.
  `src/interface/p4/binary.rs` builds case values of the P4 specification the
  same way.

### `interface/kore/mod.rs`: `print_runres`

* Input: the `runres` that `Krun` returned: `FINAL n term`, `ERROR n term`,
  or `INITFAIL`.
* Output: the text `krun` would print. For `FINAL`, the final configuration
  `term` as KORE text. Read values with `get::case`, `get::list`, `get::text`,
  and so on.

### `interface/kore/offload.rs` and `ffi.rs`: `Offload`

* `Offload::load(dir_kompiled)`: open the K library in the kompiled directory
  with `libloading` (in `ffi.rs`), and look up the hook symbols you need.
  Kompile the test definitions so that the directory holds such a library.
* `Offload::call(arena, name, targs, values)`: convert `values` to K terms,
  call hook `name` of the library, and convert the result back to a spec-k
  value. The K LLVM backend's runtime (`runtime/strings/strings.cpp`, ...)
  defines the hooks; `ffi.rs` is where the calls happen.

### `interface/kore/hooks.rs`: `OFFLOADED`

* List every `builtin dec $name(...)` of `spec-k/3.1-hook-builtin.watsup`
  that the K library answers, by name without `$`; each then goes to
  `Offload::call`.

```rust
const OFFLOADED: [&str; 2] = ["string_length", "string_substr"];
```

* Only a hook the K library cannot answer gets a Rust entry of its own, ported
  from #395's `builtins.ml`, next to the offloaded ones in `hooks`.
* A hook in neither fails with `runtime/builtin-implementation-missing`, which
  tells you what is left.

### `test-driver/src/metacirc/k_in_st.rs`: `run`

* For each case under a suite's `root`, call
  `p4spectec::metacirc::krun(&tower, &[definition, init], config.runner(), depth)`
  and compare its output with `krun`'s saved output.
* You choose the layout of a case under `testdata/k-in-st/`; write it in the
  `//!` comment of `k_in_st.rs`.

## 5. Steps

Build with `make metacirc`, which also copies the binary to `bin/metacirc`.
Before each commit, run `make fmt` and `make lint`.

1. **Port spec-k.** Bring `spec-k/*.watsup` over from #395 and fix what the
   frontend of this branch rejects. If spec-k needs spec-meta's standard
   library, copy it into `spec-k/`, since a level has one `specdir`.
   *Check:* `bin/metacirc algo spec-k` prints AL without errors.
2. **Read KORE.** Fill `parse_kore`.
   *Check:* `bin/metacirc run-meta towers/k-in-st.json -p imp-kompiled/definition.kore -p init.kore --depth 1`
   gets past parsing and stops at `Offload::load is not implemented yet`.
3. **Offload.** Fill `Offload::load` and `Offload::call`, and list hooks in
   `OFFLOADED` until no `builtin-implementation-missing` remains for your
   test definitions. Port a hook to Rust only if the library cannot answer it.
   *Check:* the command of step 2 reaches `KoreInterface::print_runres is not implemented yet`.
4. **Print.** Fill `print_runres`.
   *Check:* the command of step 2 without `--depth` prints the same final
   configuration as `krun` on the same files.
5. **Test.** Add cases under `testdata/k-in-st/`, fill `k_in_st.rs`, and delete
   the `pending` line in `k-in-st.toml`.
   *Check:* `make test-metacirc-k-in-st`.

## 6. Done when

* `k-in-st.toml` has no `pending`.
* `make test-metacirc-k-in-st` passes.
* `make fmt-check lint` are clean, and every changed file is in section 2.

Review covers (a) the template functions implemented, (b) the test above
passing, and (c) a line-by-line reading of `spec-k/` only.

## 7. Rust in five lines

* A `Value` is a handle into an `Arena`; pass `&mut Arena` to make values
  (`make::...`) and `&Arena` to read them (`get::...`).
* Errors are `Result`; `?` returns the error to the caller.
* A builtin returns `Result<Value, BuiltinError>`; most value errors convert
  with `?` or `.map_err(Into::into)`.
* `make lint` runs clippy; fix what it reports in your files.
* `cargo doc --open` in `p4spectec/` shows every type and function with its
  comment.
