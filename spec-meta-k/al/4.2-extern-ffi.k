// Transport for the external interface: K -> C shim -> OCaml, over K's FFI.
//
// The codec is in 4.1-extern-json.k, which this module re-exports.
// Only the crossing itself lives here. The wire format is unchanged.
//
//   K rules  --#ffiCall-->  spec-meta-k/ffi/shim.c  --caml_callback-->
//   p4spec/bin/kffi.ml
//
// The OCaml runtime lives in the interpreter process.
// kffi.ml builds the runner once, at init, for the run's one target spec.

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
  // The shim assumes `ml_init_c` has run -- it starts the OCaml runtime,
  // resolves the closures and builds the runner -- and no other entry point
  // checks that it did.  K guarantees it by sequencing: `initFFI()` sits at the
  // very head of `<k>` (al/0-config.k), ahead of `$PGM`, and nothing else runs
  // until it rewrites to `.K`.
  // ==========================================================================

  rule <k> initFFI() => callInitFFI(Spec) ... </k>
       <specdir> Spec:String </specdir>

  syntax K ::= callInitFFI(String)              [function, symbol(callInitFFI)]
  rule callInitFFI(Spec:String)
    => initAlloc(Spec, #alloc(allocKeySpec(Spec), lengthString(Spec) +Int 1, 1))

  syntax K ::= initAlloc(String, Bytes)         [function, symbol(initAlloc)]
  rule initAlloc(Spec:String, Buf:Bytes)
    => initCall(Spec, #address(Buf),
                #nativeWrite(#address(Buf), String2Bytes(Spec +String "\x00")))

  syntax K ::= initCall(String, Int, K)         [function, symbol(initCall)]
  rule initCall(Spec:String, Addr:Int, _)
    => initFree(Spec,
         Bytes2Int(#ffiCall(#functionAddress("ml_init_c"),
                            ListItem(Int2Bytes(8, Addr, LE)),
                            ListItem(#pointer),
                            #sint64),
                   LE, Signed))

  syntax K ::= initFree(String, Int)            [function, symbol(initFree)]
  rule initFree(Spec:String, _:Int) => #free(allocKeySpec(Spec))

  // ==========================================================================
  // The transport
  //
  // A chain of pure `[function]`s rather than `<k>`-cell rules: `#ffiCall` is a
  // `[function]` returning `Bytes`, and the effectful hooks are ordered by the
  // `#seqString`/`#seqBytes` trick, so an external call is one atomic rewrite
  // step.
  //
  //   1. `#alloc` + `#nativeWrite` the request, NUL-terminated
  //   2. `#ffiCall(ml_eval_c, reqPtr)` -> a `#pointer`
  //   3. `ml_cstrlen_c` to size the reply
  //   4. `#nativeRead` that many bytes back
  //   5. `ml_free_c` the shim's buffer, `#free` ours
  // ==========================================================================

  syntax String ::= externEval(String)              [function, symbol(externEval)]

  syntax KItem ::= allocKeySpec(String)             [symbol(allocKeySpec)]
                 | allocKeyReq(String)              [symbol(allocKeyReq)]

  rule externEval(Req:String)
    => evReqAlloc(Req, #alloc(allocKeyReq(Req), lengthString(Req) +Int 1, 1))

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

  syntax String ::= evFinish(String, Int, Bytes)    [function, symbol(evFinish)]
  rule evFinish(Req:String, AddrOut:Int, Out:Bytes)
    => #seqString(#free(allocKeyReq(Req)),
         #seqString(#ffiCall(#functionAddress("ml_free_c"),
                             ListItem(Int2Bytes(8, AddrOut, LE)),
                             ListItem(#pointer),
                             #void),
                    Bytes2String(Out)))

  syntax String ::= "#seqString" "(" K "," String ")" [function, symbol(seqString)]
  syntax Bytes  ::= "#seqBytes"  "(" K "," Bytes  ")" [function, symbol(seqBytes)]
  rule #seqString(_, X) => X
  rule #seqBytes(_, X)  => X

  // ==========================================================================
  // Entry: one rule per `ExternKind`
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

endmodule
