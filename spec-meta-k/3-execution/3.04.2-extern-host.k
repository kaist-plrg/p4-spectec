// Transport for the external interface: K <-> the host, over K's FFI.
//
//   K rules  --#ffiCall-->  host_eval, host_serve  (p4spectec::metacirc::engine::k::ffi)
//
// The host runs this configuration on a thread of its own process and passes
// the addresses of its host functions as configuration variables
// (0.1-config.k):
//
//   char*       host_buffer(int64_t len);    // the send buffer, resized to len bytes
//   const char* host_eval(const char* req);  // a request; blocks for its response
//   const char* host_serve(const char* ans); // an answer; blocks for the next call
//   int64_t     host_strlen(const char* s);  // the length of a received message
//
// Both buffers belong to the host. A message is copied into the send buffer
// with #nativeWrite, NUL-terminated, and the received one is copied back with
// #nativeRead; it lives until the next exchange.

requires "3.04.1-extern-json.k"
requires "ffi.md"

module EXTERN-HOST
  imports public EXTERN-JSON
  imports FFI
  imports BYTES
  imports STRING
  imports INT
  imports LIST

  // ==========================================================================
  // Exchanges
  //
  // A function argument is evaluated before its body, which orders the calls.
  // ==========================================================================

  // Sends `Msg` through the host function at `F`; the message received back.
  // `B` and `L` are the addresses of host_buffer and host_strlen.
  syntax String ::= hostExchange(Int, Int, Int, String)
                                                [function, symbol(hostExchange)]
  rule hostExchange(B:Int, F:Int, L:Int, Msg:String)
    => hostReceived(L, hostPointer(F, hostWrite(B, String2Bytes(Msg +String "\x00"))))

  // The send buffer, holding `Bs`.
  syntax Int ::= hostWrite(Int, Bytes)          [function, symbol(hostWrite)]
  rule hostWrite(B:Int, Bs:Bytes) => hostWritten(hostPointer(B, lengthBytes(Bs)), Bs)

  syntax Int ::= hostWritten(Int, Bytes)        [function, symbol(hostWritten)]
  rule hostWritten(Addr:Int, Bs:Bytes) => #let _ = #nativeWrite(Addr, Bs) #in Addr

  // The received message at `Addr`.
  syntax String ::= hostReceived(Int, Int)      [function, symbol(hostReceived)]
  rule hostReceived(L:Int, Addr:Int)
    => Bytes2String(hostRead(Addr, padRightBytes(.Bytes, hostLength(L, Addr), 0)))

  syntax Bytes ::= hostRead(Int, Bytes)         [function, symbol(hostRead)]
  rule hostRead(Addr:Int, Mem:Bytes) => #let _ = #nativeRead(Addr, Mem) #in Mem

  // The host function at `F` on one argument, returning a pointer.
  syntax Int ::= hostPointer(Int, Int)          [function, symbol(hostPointer)]
  rule hostPointer(F:Int, Arg:Int)
    => Bytes2Int(#ffiCall(F, ListItem(Int2Bytes(8, Arg, LE)), ListItem(#sint64), #pointer),
                 LE, Unsigned)

  // host_strlen at `L` on a received message.
  syntax Int ::= hostLength(Int, Int)           [function, symbol(hostLength)]
  rule hostLength(L:Int, Addr:Int)
    => Bytes2Int(#ffiCall(L, ListItem(Int2Bytes(8, Addr, LE)), ListItem(#pointer), #sint64),
                 LE, Signed)

  // ==========================================================================
  // Requests: one rule per `ExternKind`
  // ==========================================================================

  syntax KItem ::= externReq(String, ExternKind)          [symbol(externReq)]

  rule <k> externReq(Req:String, builtinK())
        => builtinResult(String2JSON(hostExchange(B, F, L, Req)))
       ...
       </k>
       <hbuffer> B:Int </hbuffer>
       <heval> F:Int </heval>
       <hstrlen> L:Int </hstrlen>
  rule <k> externReq(Req:String, externFuncK())
        => externFuncResult(String2JSON(hostExchange(B, F, L, Req)))
       ...
       </k>
       <hbuffer> B:Int </hbuffer>
       <heval> F:Int </heval>
       <hstrlen> L:Int </hstrlen>
  rule <k> externReq(Req:String, externRelK())
        => externRelResult(String2JSON(hostExchange(B, F, L, Req)))
       ...
       </k>
       <hbuffer> B:Int </hbuffer>
       <heval> F:Int </heval>
       <hstrlen> L:Int </hstrlen>

  syntax ExternKind ::= "builtinK" "(" ")"    [symbol(builtinK)]
                      | "externFuncK" "(" ")" [symbol(externFuncK)]
                      | "externRelK" "(" ")"  [symbol(externRelK)]

endmodule
