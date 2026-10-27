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
   input: definition.kore + init.kore (+ --depth N), as krun takes them
     |
     |  KoreInterface::parse_program            -- YOU: interface/kore/
     v
   (kdefinition, kore) values
     |
+-----------------------------------+
| spec-k (P4-SpecTec)               |  the only level, the base  -- YOU: spec-k/
|   Krun: kdefinition |- kore nat?  |
|   hooks: builtin dec $...         |  answered by hooks.rs      -- YOU: interface/kore/
+-----------------------------------+
| SpecTecEngine (AL/SL interpreter) |                            -- unchanged
+-----------------------------------+
     |
     v
   runres  -->  KoreInterface::print_runres  -->  text, compared with krun's
```

The command is

```
bin/metacirc run-meta towers/k-in-st.json -p definition.kore -p init.kore --depth 100
```

It reads the two files with `KoreInterface::parse_program`, calls spec-k's
`Krun` with them and the step bound (`Tower::eval_krun`), and prints the
result with `KoreInterface::print_runres`. All of that wiring exists; the
three `KoreInterface` pieces and spec-k itself are yours.

## 2. Your places

Add code only in these places; add new files inside the directories freely.

| place | what you fill |
|---|---|
| `spec-k/` | spec-k from #395, on the frontend of this branch |
| `p4spectec/src/interface/kore/` | `parse_program` and `print_runres` (`mod.rs`), the hooks (`hooks.rs`), and any new files they need (a KORE parser, ...) |
| `p4spectec/test-driver/src/metacirc/k_in_st.rs` | the comparison with `krun` |
| `p4spectec/test-driver/suites/metacirc/k-in-st.toml`, `testdata/k-in-st/` | the test cases, and deleting the `pending` line at the end |
| `towers/k-in-st.json` | the specification path, if yours differs |

Do not change any other file. If you think you have to, the template is
missing something: ask the template's owner to add it first.

## 3. Where #395's code goes

| #395 file | what it does | new place |
|---|---|---|
| `interface/kore/ast.ml`, `parse.ml` | KORE text to a syntax tree | `interface/kore/` (for example `parse.rs`) |
| `interface/kore/load.ml` | the tree to spec-k's `kdefinition` and `kore` values | `interface/kore/`, called by `parse_program` |
| `interface/kore/unparse.ml` | spec-k values back to KORE text | `interface/kore/`, called by `print_runres` |
| `interface/kore/builtins.ml`, `floats.ml`, `secp256k1_stubs.c` | the hooks spec-k delegates | `interface/kore/hooks.rs` (and helper files) |
| `spec-k/*.watsup` | the K semantics | `spec-k/` |
| `interface/kore/k.ml`, `krun.ml`, `bin/boot.ml` | the interface setup and the `krun` command | already done: `metacirc run-meta ... --depth` |
| `spec-k/tests/` (`difftest.py`, `ktest.py`, `manual/`) | comparing with `krun` | `test-driver/src/metacirc/k_in_st.rs`, `k-in-st.toml`, `testdata/k-in-st/` |
| `interface/kore/step_check.ml` | step-by-step check against K | later: it needs the K library, which is a separate task |

## 4. The functions to fill

### `interface/kore/mod.rs`: `parse_program`

* Input: `paths == [definition.kore, init.kore]`.
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

### `interface/kore/hooks.rs`: `hooks`

* One entry per `builtin dec $name(...)` in `spec-k/3.1-hook-builtin.watsup`:
  the name without `$`, and a function from argument values to the result.
* A hook may override a standard builtin of the same name. A hook without an
  entry fails with `runtime/builtin-implementation-missing`, which tells you
  what is left.

```rust
use crate::interface::builtin::{call::BuiltinImpl, extract};
use crate::lang::{common::source::Span, data::value::flat::{get, make}};
use num_bigint::BigInt;

pub(super) fn hooks() -> [(&'static str, BuiltinImpl); 1] {
    [("string_length", Box::new(|arena, _targs, values| {
        let text = get::text(arena, extract::one(values)?)?;
        make::int(arena, BigInt::from(text.chars().count()), Span::default())
            .map_err(Into::into)
    }))]
}
```

`src/interface/builtin/texts.rs` has more examples of builtins.

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
2. **Read KORE.** Fill `parse_program`.
   *Check:* `bin/metacirc run-meta towers/k-in-st.json -p definition.kore -p init.kore --depth 1`
   gets past parsing into spec-k; it stops at a missing hook or at
   `KoreInterface::print_runres is not implemented yet`.
3. **Hooks.** Add entries to `hooks.rs` until no
   `builtin-implementation-missing` remains for your test definitions.
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
