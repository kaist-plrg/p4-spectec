# AL meta acceptance

Run `make test-metacirc` from the repository root. This independent crate uses
the pinned Rust 1.97.1 toolchain and depends only on `p4spectec`. It does not
import the compiler test driver and is not part of `make test` or CI defaults.

The executable checks:

- The complete `spec-meta` elaboration, AL, and structured output against the
  preserved snapshots in `expected/`. Comparisons ignore decorative trailing
  spaces. The runner never updates snapshots.
- DSL → source AL → encode → decode → re-encode for `fixtures/target.watsup`,
  all of `spec-meta`, and the P4 `spec`. Both encoded scripts and every source
  type definition pass the actual meta guards. The focused target checks
  canonical relation input positions and preservation of otherwise groups.
- Polymorphic and nested function type codec signatures, `valres` and `valsres`
  annotations, exactly one wrapper payload, empty/single/
  multiple relation outputs, incompatible guard rejection, and malformed
  constructor, arity, and payload rejection.
- DSL signature equivalence, higher-order signature extraction, capture-avoiding
  substitution, and function-value membership. Evaluator regressions require
  evaluated type arguments and list slice replacement, including zero-length,
  wrong-length, and scalar replacement cases. All semantic entries execute
  through guarded AL, SL, and PL runners with determinism checks enabled.

Round trips compare the canonical encoded representation. Source spans, type
notes, general hints, and printing-only signatures are not preserved, and
variable declarations are intentionally omitted. Decoded source AL is not
executed as a replacement for compiler-checked syntax. This suite does not
claim exhaustive synthetic AST coverage or execute target builtin/extern
relays; those are outside this codec boundary.
