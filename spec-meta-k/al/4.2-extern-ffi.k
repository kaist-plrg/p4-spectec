// Transport for the external interface: K -> C shim -> OCaml, over K's FFI.
//
// The *codec* -- what a request and a reply look like on the wire -- is in
// 4.1-extern-json.k, which this module re-exports.  Only the crossing itself
// lives here.  The wire format is unchanged.
//
//   K rules  --#ffiCall-->  spec-meta-k/ffi/shim.c  --caml_callback2-->
//   p4spec/bin/kffi.ml
//
// This replaces an earlier `#system("./spectec-boot extern ...")` shell-out,
// which forked a fresh process per call.  Three properties follow from the
// OCaml runtime now being *in* the interpreter process (CROSS.md §7, §8):
//
//   - No file descriptor is consumed per call.  `#system` leaked one (K
//     7.1.337, LLVM backend), so a run aborted after ~1024 external calls with
//     `*** bit out of range 0 FD_SETSIZE on fd_set ***` -- which, not
//     correctness, was the ceiling on program size under K.
//   - `$fresh_typeId` works.  It closes over a per-process counter, which a
//     fresh process per call reset every time.
//   - An extern call no longer re-elaborates the lower spec (~1.2 s); kffi.ml
//     memoizes the runner per spec path.
//
// RUNTIME DEPENDENCY: the C shim and the OCaml object must be linked into
// `al-kompiled/interpreter` -- `make k-build` does this, and asserts the
// symbols are in the *dynamic* table, since `#functionAddress` is `dlsym`.
// Editing `p4spec/` therefore requires re-running `make k-build`, not just
// `make boot`: the interpreter embeds a snapshot of the OCaml implementation.
//
// NOT SUPPORTED under `--enable-search`.  See `externEval` below.

requires "4.1-extern-json.k"
requires "ffi.md"

module AL-EXTERN-FFI
  imports public AL-EXTERN-JSON
  imports FFI
  imports BYTES
  imports STRING
  imports INT
  imports LIST

  // ==========================================================================
  // Initialization
  //
  // The shim assumes `ml_init_c` has run: it starts the OCaml runtime and
  // resolves the `ml_eval` closure, and no other entry point checks that this
  // happened (the trade examples/k/shim.c documents).  So K must guarantee it,
  // which it does by sequencing: `initFFI()` sits at the very head of `<k>` in
  // the configuration (al/0-config.k), ahead of `$PGM`, and nothing can be
  // touched until it rewrites to `.K`.  No flag is involved.
  //
  // Consequence worth stating: *every* run now starts the OCaml runtime, even
  // one making no external call at all (`examples/add.watsup`).  That is the
  // cost of explicit init.  `caml_startup` builds no spec, so it is small.
  // ==========================================================================

  //   The result -- always 1 -- is landed in `<ffiinit>` rather than dropped,
  //   for two reasons.  It is *required*: `#ffiCall` is a `[function]`, so a
  //   rule rewriting `initFFI() => .K` without consuming the result would let
  //   K discard the whole call as an unused pure term, exactly the hazard
  //   `#seqString` exists to prevent below.  And it makes the run
  //   self-documenting: `<ffiinit> 1 </ffiinit>` in the final configuration is
  //   proof the OCaml runtime came up, while `0` means `initFFI()` never
  //   reduced -- check it is still at the head of `<k>`.  If it is removed
  //   altogether the failure mode is a segfault with no diagnostic, and this
  //   cell is what makes that diagnosable after the fact.
  rule <k> initFFI() => .K ... </k>
       <ffiinit> _ => callInitFFI() </ffiinit>

  syntax Int ::= "callInitFFI" "(" ")"  [function, symbol(callInitFFI)]
  rule callInitFFI()
    => Bytes2Int(#ffiCall(#functionAddress("ml_init_c"), .List, .List, #sint64),
                 LE, Signed)

  // ==========================================================================
  // The transport
  //
  // Unlike its `#system` predecessor this is a chain of pure `[function]`s
  // rather than a chain of `<k>`-cell rules.  That was forced before -- the
  // K-IO hooks `#write`/`#close` have sort `K` and can only be sequenced in
  // `<k>`, and `#system` returns a `#systemResult` needing a continuation.
  // Neither constraint survives: `#ffiCall` is a `[function]` returning
  // `Bytes`, and the effectful hooks are ordered by the `#seqString`/
  // `#seqBytes` trick instead.  So an external call is one atomic rewrite step
  // rather than six, and the intermediate `KItem` sorts, the `<builtinreq>`
  // cell and its first-call/later-call split are all gone.
  //
  // The chain mirrors `#callStr` in examples/k/mlffi.k, but with *two* input
  // buffers -- the spec path and the request:
  //
  //   1. `#alloc` + `#nativeWrite` both, NUL-terminated
  //   2. `#ffiCall(ml_eval_c, specPtr, reqPtr)` -> a `#pointer`
  //   3. `ml_cstrlen_c` to size the reply
  //   4. `#nativeRead` that many bytes back
  //   5. `ml_free_c` the shim's buffer, `#free` both of ours
  //
  // Two invariants held deliberately:
  //
  //   - **`#alloc` keys must not alias.**  `#alloc`'s first argument is a key
  //     identifying the buffer, so the spec path and the request are wrapped
  //     (`allocKeySpec`/`allocKeyReq`) rather than used bare: otherwise a
  //     request that happened to equal its spec path would collide on one
  //     buffer.  Safety further relies on there never being two `externEval`s
  //     in flight, which holds because a `[function]` application reduces
  //     within a single rewrite step and the LLVM backend is single-threaded.
  //     **`--enable-search` is unsupported.**
  //
  //   - **Never carry the `Bytes` term forward, only its `#address` and its
  //     key.**  Freeing an `#alloc`'d buffer that a live `Bytes` still
  //     references is undefined behaviour (ffi.md, `#free`).  The read-back
  //     buffer is a plain `padRightBytes`, not `#alloc`'d, so `Bytes2String`
  //     on it is safe.
  // ==========================================================================

  syntax String ::= externEval(String, String)      [function, symbol(externEval)]

  //   `#alloc` keys.  `#alloc` is keyed by a `KItem`, and `#free` takes that
  //   same key back rather than the buffer, so the keys -- not the `Bytes` --
  //   are what travel down the chain.  Two distinct constructors, so the two
  //   buffers of one call can never collide on one key even if a request
  //   happened to equal its spec path.
  syntax KItem ::= allocKeySpec(String)             [symbol(allocKeySpec)]
                 | allocKeyReq(String)              [symbol(allocKeyReq)]

  rule externEval(Spec:String, Req:String)
    => evSpecAlloc(Spec, Req,
                   #alloc(allocKeySpec(Spec), lengthString(Spec) +Int 1, 1))

  syntax String ::= evSpecAlloc(String, String, Bytes) [function, symbol(evSpecAlloc)]
  rule evSpecAlloc(Spec:String, Req:String, BufSpec:Bytes)
    => evReqAlloc(Spec, Req, #address(BufSpec),
                  #nativeWrite(#address(BufSpec), String2Bytes(Spec +String "\x00")),
                  #alloc(allocKeyReq(Req), lengthString(Req) +Int 1, 1))

  //   From here on the buffers travel as an *address* plus the key needed to
  //   free them; the `Bytes` terms are dropped.  See the second invariant
  //   above: freeing a buffer some live `Bytes` still references is UB.
  syntax String ::= evReqAlloc(String, String, Int, K, Bytes)
                                                    [function, symbol(evReqAlloc)]
  rule evReqAlloc(Spec:String, Req:String, AddrSpec:Int, _, BufReq:Bytes)
    => evCall(Spec, Req, AddrSpec, #address(BufReq),
              #nativeWrite(#address(BufReq), String2Bytes(Req +String "\x00")))

  syntax String ::= evCall(String, String, Int, Int, K)
                                                    [function, symbol(evCall)]
  rule evCall(Spec:String, Req:String, AddrSpec:Int, AddrReq:Int, _)
    => evResult(Spec, Req,
         Bytes2Int(#ffiCall(#functionAddress("ml_eval_c"),
                            ListItem(Int2Bytes(8, AddrSpec, LE))
                              ListItem(Int2Bytes(8, AddrReq, LE)),
                            ListItem(#pointer) ListItem(#pointer),
                            #pointer),
                   LE, Unsigned))

  //   Ask the shim how long the reply is, so the read-back buffer can be sized.
  syntax String ::= evResult(String, String, Int)   [function, symbol(evResult)]
  rule evResult(Spec:String, Req:String, AddrOut:Int)
    => evRead(Spec, Req, AddrOut,
         Bytes2Int(#ffiCall(#functionAddress("ml_cstrlen_c"),
                            ListItem(Int2Bytes(8, AddrOut, LE)),
                            ListItem(#pointer),
                            #sint64),
                   LE, Signed))

  syntax String ::= evRead(String, String, Int, Int) [function, symbol(evRead)]
  rule evRead(Spec:String, Req:String, AddrOut:Int, Len:Int)
    => evFinish(Spec, Req, AddrOut, ffiReadN(AddrOut, Len))

  syntax Bytes ::= ffiReadN(Int, Int)               [function, symbol(ffiReadN)]
  rule ffiReadN(Addr:Int, N:Int) => ffiReadAux(padRightBytes(.Bytes, N, 0), Addr)

  syntax Bytes ::= ffiReadAux(Bytes, Int)           [function, symbol(ffiReadAux)]
  rule ffiReadAux(Mem:Bytes, Addr:Int) => #seqBytes(#nativeRead(Addr, Mem), Mem)

  //   Free all three buffers -- the shim's `malloc`'d reply, then our two
  //   `#alloc`'d inputs -- and only then let the decoded string escape.  `Out`
  //   is a plain `padRightBytes`, not `#alloc`'d, so reading it here is safe.
  syntax String ::= evFinish(String, String, Int, Bytes)
                                                    [function, symbol(evFinish)]
  rule evFinish(Spec:String, Req:String, AddrOut:Int, Out:Bytes)
    => #seqString(#free(allocKeySpec(Spec)),
         #seqString(#free(allocKeyReq(Req)),
           #seqString(#ffiCall(#functionAddress("ml_free_c"),
                               ListItem(Int2Bytes(8, AddrOut, LE)),
                               ListItem(#pointer),
                               #void),
                      Bytes2String(Out))))

  //   `#nativeWrite`, `#nativeRead`, `#free` and a `#void` `#ffiCall` are all
  //   `function`s returning `K`, hence pure to K with no guaranteed evaluation
  //   order.  Threading each through an argument position that must be
  //   evaluated before the result escapes is what forces the effect to happen,
  //   and is why the chain above is split into small helpers rather than
  //   written as one expression.  (examples/k/mlffi.k, "Forcing effect order".)
  syntax String ::= "#seqString" "(" K "," String ")" [function, symbol(seqString)]
  syntax Bytes  ::= "#seqBytes"  "(" K "," Bytes  ")" [function, symbol(seqBytes)]
  rule #seqString(_, X) => X
  rule #seqBytes(_, X)  => X

  // ==========================================================================
  // Entry: one rule per `ExternKind`
  //
  // The whole crossing is a single `[function]` application, so each kind is
  // one rewrite: serialize, call, decode.  What varies is only which decoder
  // reads the reply -- `respOk` for a builtin (which cannot fail), and the
  // `externFuncResult`/`externRelResult` pair for the two extern kinds, which
  // turn `{"fail": null}` into `FAIL`.
  //
  // A malformed reply -- including kffi.ml's `{"error": <diagnostic>}`, the
  // third reply shape -- deliberately has *no* rule, for all three kinds.  It
  // means the wire broke, which is a defect rather than something the spec can
  // recover from, so the term sticks visibly in `<k>` carrying the diagnostic.
  // That is the same invariant the non-zero-exit case used to hold, and better
  // placed: the diagnostic is now in the configuration dump rather than on a
  // lost child's stderr (kffi.ml also echoes it to stderr as it happens).
  // ==========================================================================

  syntax KItem ::= externReq(String, String, ExternKind)  [symbol(externReq)]

  rule <k> externReq(Req:String, Spec:String, builtinK())
        => valOfJson(respOk(String2JSON(externEval(Spec, Req))))
       ...
       </k>
  rule <k> externReq(Req:String, Spec:String, externFuncK())
        => externFuncResult(String2JSON(externEval(Spec, Req)))
       ...
       </k>
  rule <k> externReq(Req:String, Spec:String, externRelK())
        => externRelResult(String2JSON(externEval(Spec, Req)))
       ...
       </k>

  syntax ExternKind ::= "builtinK" "(" ")"    [symbol(builtinK)]
                      | "externFuncK" "(" ")" [symbol(externFuncK)]
                      | "externRelK" "(" ")"  [symbol(externRelK)]

  // ==========================================================================
  // Which spec a call resolves against
  //
  // The **target spec**: the SpecTec program this run is executing.  It reaches
  // K as the `$SPEC` configuration variable, landing in `<specdir>`
  // (al/0-config.k), and both call sites -- `callBuiltinFunc`
  // (5.5-eval-call-func.k) and `callExternFunc`/`callExternRel` -- read that
  // cell directly.  There is no function here any more, and nothing hardcoded:
  // the target is a property of the *invocation*, and the invocation is now
  // what supplies it.
  //
  // It was previously a `builtinSpec()`/`externSpec(P4Opt)` pair of rules
  // fixed to `"spec"`.  That was a wart -- running against any other target
  // meant editing this file and re-`kompile`ing -- and the `<p4prog>` dispatch
  // in `externSpec` was vestigial besides, since both cases answered alike.
  //
  // One value serves both kinds, because both are answered against the same
  // spec:
  //
  //   - A builtin is a static registry lookup and does not need a spec at all,
  //     except for `$print_`, which unparses against one.  Naming the target
  //     costs nothing for the other 46: kffi.ml loads it only when a request
  //     needs it.
  //
  //   - An extern is answered on the P4 interface, which kffi.ml always builds
  //     its runner on; for the P4 spec's own externs
  //     (`ExternFunctionCall_eval_lctk`, `ExternMethodCall_eval`, ...) that
  //     resolves them against the architecture model in
  //     `p4spec/lib/backend-sim/`.
  //
  // Getting the path wrong is quiet rather than loud: an extern name the runner
  // cannot resolve comes back as `{"fail": null}`, indistinguishable from a
  // spec-level failure (CROSS.md §8).  So a mistyped `-cSPEC=` surfaces as a
  // type error in the program under test, not as a diagnostic -- which is the
  // one cost of making this configurable, and the reason `make` fills it in
  // rather than leaving it to be typed.
  // ==========================================================================

endmodule
