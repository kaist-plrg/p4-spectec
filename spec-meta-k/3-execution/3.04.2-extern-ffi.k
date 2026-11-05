// Transport for the external interface: K -> Rust, over K's FFI.
//
//   K rules  --#ffiCall-->  bin/libp4spectec.so  (p4spectec::metacross::ffi)
//
// The interpreter links the library and resolves its host_* symbols at run
// time. The runner is built once on init.

requires "3.04.1-extern-json.k"
requires "ffi.md"

module EXTERN-FFI
  imports public EXTERN-JSON
  imports FFI
  imports BYTES
  imports STRING
  imports INT
  imports LIST

  // ==========================================================================
  // Initialization
  //
  // `host_init` must precede other FFI calls.
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
         Bytes2Int(#ffiCall(#functionAddress("host_init"),
                            ListItem(Int2Bytes(8, Addr, LE)),
                            ListItem(#pointer),
                            #sint64),
                   LE, Signed))

  syntax K ::= initFree(String, Int)            [function, symbol(initFree)]
  rule initFree(Spec:String, _:Int) => #free(allocKeySpec(Spec))

  // ==========================================================================
  // The transport
  //
  //   1. `#alloc` + `#nativeWrite` the request, NUL-terminated
  //   2. `#ffiCall(host_eval, reqPtr)` -> a `#pointer`
  //   3. `host_strlen` to size the reply
  //   4. `#nativeRead` that many bytes back
  //   5. `#free` our request buffer; the reply belongs to Rust until the next call
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
         Bytes2Int(#ffiCall(#functionAddress("host_eval"),
                            ListItem(Int2Bytes(8, AddrReq, LE)),
                            ListItem(#pointer),
                            #pointer),
                   LE, Unsigned))

  syntax String ::= evResult(String, Int)           [function, symbol(evResult)]
  rule evResult(Req:String, AddrOut:Int)
    => evRead(Req, AddrOut,
         Bytes2Int(#ffiCall(#functionAddress("host_strlen"),
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
  rule ffiReadAux(Mem:Bytes, Addr:Int)
    => #let _ = #nativeRead(Addr, Mem) #in Mem

  syntax String ::= evFinish(String, Int, Bytes)    [function, symbol(evFinish)]
  rule evFinish(Req:String, _AddrOut:Int, Out:Bytes)
    => #let _ = #free(allocKeyReq(Req)) #in
       Bytes2String(Out)

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
