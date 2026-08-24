// Transport for the external interface: K -> C shim -> OCaml, over K's FFI.
//
// The *codec* -- what a request and a reply look like on the wire -- is in
// 4.1-extern-json.k, which this module re-exports.  Only the crossing itself
// lives here.  The wire format is unchanged.
//
//   K rules  --#ffiCall-->  spec-meta-k/ffi/shim.c  --caml_callback-->
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
//     builds the runner once, at init, for the run's one target spec.
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
  // The shim assumes `ml_init_c` has run: it starts the OCaml runtime,
  // resolves the `ml_init`/`ml_eval` closures and builds the runner for the
  // target spec, and no other entry point checks that this happened (the trade
  // examples/k/shim.c documents).  So K must guarantee it, which it does by
  // sequencing: `initFFI()` sits at the very head of `<k>` in the
  // configuration (al/0-config.k), ahead of `$PGM`, and nothing can be touched
  // until it rewrites to `.K`.  No flag is involved.
  //
  // Consequence worth stating: *every* run now builds the runner for its
  // target spec up front, even one making no external call at all
  // (`examples/add.watsup`).  That is the cost of eager init, and it buys a
  // simpler wire: `ml_eval_c` then takes only the request, since the host
  // already knows which spec to answer against.
  // ==========================================================================

  //   `initFFI()` reads the target spec out of `<specdir>` and hands it to
  //   `ml_init_c`, which is the only point the path crosses the wire.
  //
  //   The `Int` result -- always 1 -- is *discarded*, but only after being
  //   forced: `#ffiCall` is a `[function]`, so `initFFI() => .K` on its own
  //   would let K drop the whole call as an unused pure term.  `#seqK` threads
  //   it through an argument position that must be evaluated before the `.K`
  //   escapes, exactly as `#seqString` does for the transport below.
  rule <k> initFFI() => #seqK(callInitFFI(Spec), .K) ... </k>
       <specdir> Spec:String </specdir>

  // `#seqK` itself now lives in 4.1-extern-json.k, which this module imports:
  // `logDebug` needs the same forcing trick and sits there, below this file in
  // the import order.

  //   Same `#alloc`/`#nativeWrite`/`#free` dance as the transport, for the one
  //   string argument.  See the two invariants stated there; both apply.
  syntax Int ::= callInitFFI(String)            [function, symbol(callInitFFI)]
  rule callInitFFI(Spec:String)
    => initAlloc(Spec, #alloc(allocKeySpec(Spec), lengthString(Spec) +Int 1, 1))

  syntax Int ::= initAlloc(String, Bytes)       [function, symbol(initAlloc)]
  rule initAlloc(Spec:String, Buf:Bytes)
    => initCall(Spec, #address(Buf),
                #nativeWrite(#address(Buf), String2Bytes(Spec +String "\x00")))

  syntax Int ::= initCall(String, Int, K)       [function, symbol(initCall)]
  rule initCall(Spec:String, Addr:Int, _)
    => initFree(Spec,
         Bytes2Int(#ffiCall(#functionAddress("ml_init_c"),
                            ListItem(Int2Bytes(8, Addr, LE)),
                            ListItem(#pointer),
                            #sint64),
                   LE, Signed))

  syntax Int ::= initFree(String, Int)          [function, symbol(initFree)]
  rule initFree(Spec:String, R:Int) => #seqInt(#free(allocKeySpec(Spec)), R)

  syntax Int ::= "#seqInt" "(" K "," Int ")"    [function, symbol(seqInt)]
  rule #seqInt(_, X) => X

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
  // The chain mirrors `#callStr` in examples/k/mlffi.k, over one input buffer
  // -- the request.  The spec path is not sent: `initFFI()` above already gave
  // it to `ml_init_c`, which built the runner the host answers against.
  //
  //   1. `#alloc` + `#nativeWrite` the request, NUL-terminated
  //   2. `#ffiCall(ml_eval_c, reqPtr)` -> a `#pointer`
  //   3. `ml_cstrlen_c` to size the reply
  //   4. `#nativeRead` that many bytes back
  //   5. `ml_free_c` the shim's buffer, `#free` ours
  //
  // Two invariants held deliberately:
  //
  //   - **`#alloc` keys must not alias.**  `#alloc`'s first argument is a key
  //     identifying the buffer, so the request is wrapped (`allocKeyReq`)
  //     rather than used bare: it must not collide with the `allocKeySpec`
  //     buffer of `initFFI()`, nor with any other live allocation.  Safety
  //     further relies on there never being two `externEval`s in flight, which
  //     holds because a `[function]` application reduces within a single
  //     rewrite step and the LLVM backend is single-threaded.
  //     **`--enable-search` is unsupported.**
  //
  //   - **Never carry the `Bytes` term forward, only its `#address` and its
  //     key.**  Freeing an `#alloc`'d buffer that a live `Bytes` still
  //     references is undefined behaviour (ffi.md, `#free`).  The read-back
  //     buffer is a plain `padRightBytes`, not `#alloc`'d, so `Bytes2String`
  //     on it is safe.
  // ==========================================================================

  syntax String ::= externEval(String)              [function, symbol(externEval)]

  //   `#alloc` keys.  `#alloc` is keyed by a `KItem`, and `#free` takes that
  //   same key back rather than the buffer, so the key -- not the `Bytes` --
  //   is what travels down the chain.
  syntax KItem ::= allocKeySpec(String)             [symbol(allocKeySpec)]
                 | allocKeyReq(String)              [symbol(allocKeyReq)]

  rule externEval(Req:String)
    => evReqAlloc(Req, #alloc(allocKeyReq(Req), lengthString(Req) +Int 1, 1))

  //   From here on the buffer travels as an *address* plus the key needed to
  //   free it; the `Bytes` term is dropped.  See the second invariant above:
  //   freeing a buffer some live `Bytes` still references is UB.
  syntax String ::= evReqAlloc(String, Bytes)       [function, symbol(evReqAlloc)]
  rule evReqAlloc(Req:String, BufReq:Bytes)
    => evCall(Req, #address(BufReq),
              #nativeWrite(#address(BufReq), String2Bytes(Req +String "\x00")))

  syntax String ::= evCall(String, Int, K)          [function, symbol(evCall)]
  rule evCall(Req:String, AddrReq:Int, _)
    => evResult(Req,
         Bytes2Int(#ffiCall(#functionAddress("ml_eval_c"),
                            ListItem(Int2Bytes(8, AddrReq, LE)),
                            ListItem(#pointer),
                            #pointer),
                   LE, Unsigned))

  //   Ask the shim how long the reply is, so the read-back buffer can be sized.
  syntax String ::= evResult(String, Int)           [function, symbol(evResult)]
  rule evResult(Req:String, AddrOut:Int)
    => evRead(Req, AddrOut,
         Bytes2Int(#ffiCall(#functionAddress("ml_cstrlen_c"),
                            ListItem(Int2Bytes(8, AddrOut, LE)),
                            ListItem(#pointer),
                            #sint64),
                   LE, Signed))

  syntax String ::= evRead(String, Int, Int)        [function, symbol(evRead)]
  rule evRead(Req:String, AddrOut:Int, Len:Int)
    => evFinish(Req, AddrOut, ffiReadN(AddrOut, Len))

  syntax Bytes ::= ffiReadN(Int, Int)               [function, symbol(ffiReadN)]
  rule ffiReadN(Addr:Int, N:Int) => ffiReadAux(padRightBytes(.Bytes, N, 0), Addr)

  syntax Bytes ::= ffiReadAux(Bytes, Int)           [function, symbol(ffiReadAux)]
  rule ffiReadAux(Mem:Bytes, Addr:Int) => #seqBytes(#nativeRead(Addr, Mem), Mem)

  //   Free both buffers -- the shim's `malloc`'d reply, then our `#alloc`'d
  //   input -- and only then let the decoded string escape.  `Out` is a plain
  //   `padRightBytes`, not `#alloc`'d, so reading it here is safe.
  syntax String ::= evFinish(String, Int, Bytes)    [function, symbol(evFinish)]
  rule evFinish(Req:String, AddrOut:Int, Out:Bytes)
    => #seqString(#free(allocKeyReq(Req)),
         #seqString(#ffiCall(#functionAddress("ml_free_c"),
                             ListItem(Int2Bytes(8, AddrOut, LE)),
                             ListItem(#pointer),
                             #void),
                    Bytes2String(Out)))

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

  syntax KItem ::= externReq(String, ExternKind)          [symbol(externReq)]

  rule <k> externReq(Req:String, builtinK())
        => valOfJson(respOk(String2JSON(externEval(Req))))
       ...
       </k>
  rule <k> externReq(Req:String, externFuncK())
        => externFuncResult(String2JSON(externEval(Req)))
       ...
       </k>
  rule <k> externReq(Req:String, externRelK())
        => externRelResult(String2JSON(externEval(Req)))
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
  // (al/0-config.k), and crosses the wire exactly once -- in `initFFI()` at the
  // top of this file, which hands it to `ml_init_c`.  From then on the host
  // holds the runner built for it, so no individual call carries a path and
  // neither `callBuiltinFunc` (5.5-eval-call-func.k) nor
  // `callExternFunc`/`callExternRel` (5.6-eval-call-rel.k) needs to read the
  // cell.  Nothing is hardcoded: the target is a property of the *invocation*,
  // and the invocation is what supplies it.
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
  //     costs nothing for the other 46.
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
