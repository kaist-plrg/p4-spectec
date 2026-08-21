# Calling OCaml from K via the C FFI

A minimal, working example of a K semantics invoking OCaml functions through
K's [FFI module](https://kframework.org/k-distribution/include/kframework/builtin/ffi/).

K's FFI is a libffi wrapper: it can call any function with a plain **C ABI**.
OCaml cannot expose such a function on its own, so a thin C shim sits in
between. The shim presents a normal C signature outwards and calls into OCaml
via `caml_callback` inwards.

```
K rules  --#ffiCall-->  C shim  --caml_callback-->  OCaml
```

## Files

| File | Role |
|---|---|
| `mymod.ml` | OCaml logic, exposed by name with `Callback.register` |
| `shim.c` | C ABI outside, `caml_callback` inside |
| `mlffi.k` | K semantics calling the shim with `#ffiCall` |

## Requirements

- K framework (tested with 7.1.337), **LLVM backend** — the FFI hooks are not
  implemented in the Haskell backend
- OCaml 5.1.0 (the P4-SpecTec switch), with `ocamlopt` on `PATH`
- `libzstd` development library

## Building

```sh
eval $(opam env --switch=5.1.0)

K_INC="$(dirname "$(dirname "$(readlink -f "$(which kompile)")")")/include/kframework/builtin"

# 1. OCaml module -> object file, with the OCaml runtime bundled in
ocamlopt -output-complete-obj -o mlcode.o mymod.ml

# 2. C shim -> object file
gcc -c -fPIC -I "$(ocamlopt -where)" -o shim.o shim.c

# 3. kompile, linking both objects into the interpreter
kompile mlffi.k --backend llvm -I "$K_INC" \
  -ccopt shim.o -ccopt mlcode.o \
  -ccopt -L"$(ocamlopt -where)" \
  -ccopt -lasmrun -ccopt -lzstd \
  -ccopt -lm -ccopt -ldl \
  -ccopt -rdynamic
```

### Build gotchas

Four traps, each of which produces a confusing failure:

1. `-ccopt` is a hidden flag: see `kompile --help-hidden`, not `--help`.

2. **`-rdynamic` is mandatory.** Without it the run segfaults immediately.
   `#functionAddress` is implemented with `dlsym`, which searches only the
   *dynamic* symbol table; the linker otherwise omits these symbols because
   nothing in the interpreter references them. Diagnose with:

   ```sh
   nm -D mlffi-kompiled/interpreter | grep ml_add_c   # must print a line
   ```

3. **The output object must not be named after the module.**
   `ocamlopt -output-complete-obj -o mymod.o mymod.ml` fails with
   *"input file 'mymod.o' is the same as output file"*. Hence `mlcode.o`.

4. **`-lzstd` is required on OCaml 5.1.** Its runtime uses zstd for
   marshalling; omitting it gives
   *"undefined reference to `ZSTD_createCCtx`"*.

## Running

The program syntax is a `;`-separated command list. Note there is **no
trailing separator** — `List{Cmd,";"}` is a separator-style list, and a
trailing `;` is a parse error.

**`init` must be the first command.** It starts the OCaml runtime and
resolves every callback; the other commands assume that has happened. It
returns `1`, which is why `1` heads the output below.

```sh
echo -n 'init ; add 17 25' > demo.mlffi
krun demo.mlffi
```

```
<generatedTop>
  <k>
    .K
  </k>
  <out>
    ListItem ( 1 )
    ListItem ( 42 )
  </out>
</generatedTop>
```

All commands together:

```sh
echo -n 'init ; add 1 2 ; fib 40 ; upper "hello, ocaml" ; describe "p4" ; add 100 -142' > demo.mlffi
krun demo.mlffi
```

```
<generatedTop>
  <k>
    .K
  </k>
  <out>
    ListItem ( 1 )
    ListItem ( 3 )
    ListItem ( 102334155 )
    ListItem ( "HELLO, OCAML" )
    ListItem ( "[ocaml] len=2 rev=4p" )
    ListItem ( -42 )
  </out>
</generatedTop>
```

Use `echo -n` (or `printf`): a trailing newline is fine, but a trailing `;`
is not.

### Forgetting `init`

The shim does **not** check that initialization happened. Reaching any other
entry point first dereferences a NULL closure pointer and segfaults, with no
indication of the real cause:

```
$ echo -n 'add 1 2' > noinit.mlffi && krun noinit.mlffi
...  Segmentation fault
[Error] krun: ./mlffi-kompiled/interpreter ...
```

If you hit that, check that `init` is the first command. Calling `init` more
than once is harmless — `caml_startup` is idempotent and re-resolving the
closures is a no-op.

## How it works

### OCaml side — registration by name

```ocaml
let ml_add a b = a + b
let () = Callback.register "ml_add" ml_add
```

`Callback.register` is what makes the function reachable. Compiled OCaml
symbols are name-mangled with a build-dependent stamp
(`camlMymod__ml_add_267`) and use OCaml's internal calling convention, so
they cannot be called by K directly. Registration sidesteps both problems.

### C side — the shim

All initialization is collected into one entry point. `ml_init_c` starts the
runtime and resolves every callback into a file-scope global:

```c
static const value *ml_add = NULL;
/* ... one per callback ... */

int64_t ml_init_c(void) {
    static char *argv[] = { "k_interpreter", NULL };
    caml_startup(argv);
    ml_add = caml_named_value("ml_add");
    /* ... */
    return 1;
}
```

Each call site is then just the call:

```c
int64_t ml_add_c(int64_t a, int64_t b) {
    return (int64_t) Int_val(caml_callback2(*ml_add, Val_long(a), Val_long(b)));
}
```

Details that matter:

- **Caching the closure pointers is safe.** `caml_named_value` returns a
  pointer into OCaml's registered-roots table. The GC moves the closure it
  points at, but the table slot itself is stable for the life of the process,
  so resolving once and keeping the pointer is correct.
- **Global state persists.** The shim is linked *into* the K interpreter,
  which owns `main()` and is a single long-lived process — one `pid` for the
  whole `krun`. That is what makes one-time initialization possible at all.
- **No safety checks.** Entry points assume `init` already ran and that
  every `caml_named_value` succeeded. Both assumptions fail as a NULL
  dereference rather than a diagnostic — a deliberate trade for a minimal
  per-call path. Add an initialized flag and NULL checks if you want the
  failure mode to be legible.
- **Never return an OCaml pointer.** The OCaml GC moves the heap, so results
  are `memcpy`'d into `malloc`'d memory, with `CAMLparam0`/`CAMLlocal2`
  registering intermediates as GC roots. K frees the buffer via `ml_free_c`.

The tradeoff is the one you would expect: the K semantics is now responsible
for calling `init` first, and nothing enforces it. In exchange, adding a
callback means adding one `caml_named_value` line rather than another
lazy-cache block, and every call site is one line of actual work. It is **not** a performance change — measured over
500k calls the two versions are indistinguishable (~0.13s either way), since
the per-call branch it removes was perfectly predicted.

### K side — `#ffiCall`

`init` is a zero-argument call, so both list arguments are `.List`:

```k
rule #callInit()
  => Bytes2Int(#ffiCall(#functionAddress("ml_init_c"),
                        .List, .List, #sint64),
               LE, Signed)
```

Scalars marshal in with `Int2Bytes` and out with `Bytes2Int`:

```k
rule #callInt2(F, A, B)
  => Bytes2Int(#ffiCall(#functionAddress(F),
                        ListItem(Int2Bytes(8, A, LE)) ListItem(Int2Bytes(8, B, LE)),
                        ListItem(#sint64) ListItem(#sint64),
                        #sint64),
               LE, Signed)
```

Widths and endianness are **not** checked against the real C prototype; a
mismatch is undefined behaviour, not a K error. `Signed` is what makes
negative results come back correctly.

Strings take five steps (`#callStr` in `mlffi.k`):

1. `#alloc` a buffer and `#nativeWrite` the NUL-terminated input
2. `#ffiCall` the shim, receiving a `#pointer`
3. call `ml_cstrlen_c` to size the result
4. `#nativeRead` that many bytes back
5. `ml_free_c` the shim's buffer, `#free` ours

### Forcing effect order

`#nativeWrite`, `#nativeRead` and `#free` are `function`s returning `K`, so
they are pure to K and have no guaranteed evaluation order. Ordering is
forced by threading each effect through an argument position that must be
evaluated before the result escapes:

```k
syntax String ::= "#seqString" "(" K "," String ")" [function]
rule #seqString(_, X) => X
```

This is why the string path is split across several small helper functions
rather than written as one expression.

## Notes for larger use

- **Cost is per-process, not per-call.** The OCaml runtime starts once, so
  unlike `#system` there is no `fork`/`exec` — and no file-descriptor
  consumption — per call. 3000 allocating round-trips run with flat memory.
- **One shim per boundary, not per function.** Real P4-SpecTec entry points
  take ASTs rather than scalars. The practical shape is a single
  `ml_eval_c(const char *) -> char *` that takes a serialized request and
  returns a serialized reply, with dispatch happening in OCaml.
- **Threading.** With explicit `init` the startup race largely goes away: if
  K calls `init` once before any other command, the globals are written
  before any concurrent reader exists. This was still not tested under
  `--enable-search` or any concurrent configuration, and a multi-threaded
  caller would need `caml_c_thread_register` for non-main threads.
- **Linking a dune-built library** (rather than a standalone `.ml`) is not
  covered here. Getting `-output-complete-obj` to work across existing dune
  targets and their transitive dependencies is the untested step.

## Cleaning

```sh
rm -rf mlffi-kompiled mlcode.o shim.o mymod.cmi mymod.cmx mymod.o *.mlffi
```
