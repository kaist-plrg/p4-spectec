requires "ffi.md"

module MLFFI-SYNTAX
  imports INT-SYNTAX
  imports STRING-SYNTAX

  syntax Cmd ::= "add" Int Int
               | "fib" Int
               | "upper" String
               | "describe" String
  syntax Cmds ::= List{Cmd, ";"} [symbol(cmds)]
endmodule

module MLFFI
  imports MLFFI-SYNTAX
  imports FFI
  imports BYTES
  imports INT
  imports STRING
  imports LIST

  configuration <k> $PGM:Cmds </k>
                <out> .List </out>

  rule <k> C:Cmd ; Cs:Cmds => C ~> Cs ... </k>
  rule <k> .Cmds => .K ... </k>

  //--------------------------------------------------------------------
  // Scalar OCaml calls. Identical in shape to plain-C FFI: the shim
  // presents a normal C ABI, so K cannot tell OCaml is behind it.
  //--------------------------------------------------------------------
  rule <k> add A B => .K ... </k>
       <out> ... .List => ListItem(#callInt2("ml_add_c", A, B)) </out>

  rule <k> fib N => .K ... </k>
       <out> ... .List => ListItem(#callInt1("ml_fib_c", N)) </out>

  syntax Int ::= "#callInt2" "(" String "," Int "," Int ")" [function]
  rule #callInt2(F, A, B)
    => Bytes2Int(#ffiCall(#functionAddress(F),
                          ListItem(Int2Bytes(8, A, LE)) ListItem(Int2Bytes(8, B, LE)),
                          ListItem(#sint64) ListItem(#sint64),
                          #sint64),
                 LE, Signed)

  syntax Int ::= "#callInt1" "(" String "," Int ")" [function]
  rule #callInt1(F, N)
    => Bytes2Int(#ffiCall(#functionAddress(F),
                          ListItem(Int2Bytes(8, N, LE)),
                          ListItem(#sint64),
                          #sint64),
                 LE, Signed)

  //--------------------------------------------------------------------
  // String -> String OCaml calls.
  //
  // The shim returns a malloc'd char*. We must:
  //   1. marshal the input into #alloc'd memory, NUL-terminated
  //   2. call, receiving a pointer as Bytes
  //   3. ask the shim how long the result is (ml_cstrlen_c)
  //   4. #nativeRead that many bytes out of the returned pointer
  //   5. ml_free_c the shim's buffer, and #free ours
  //--------------------------------------------------------------------
  rule <k> upper S => .K ... </k>
       <out> ... .List => ListItem(#callStr("ml_upper_c", S)) </out>

  rule <k> describe S => .K ... </k>
       <out> ... .List => ListItem(#callStr("ml_describe_c", S)) </out>

  syntax String ::= "#callStr" "(" String "," String ")" [function]
  rule #callStr(F, S) => #csAlloc(F, S, #alloc(S, lengthString(S) +Int 1, 1))

  syntax String ::= "#csAlloc" "(" String "," String "," Bytes ")" [function]
  rule #csAlloc(F, S, Buf)
    => #csCall(F, S, #address(Buf),
               #nativeWrite(#address(Buf), String2Bytes(S +String "\x00")))

  syntax String ::= "#csCall" "(" String "," String "," Int "," K ")" [function]
  rule #csCall(F, S, InAddr, _)
    => #csResult(S,
         Bytes2Int(#ffiCall(#functionAddress(F),
                            ListItem(Int2Bytes(8, InAddr, LE)),
                            ListItem(#pointer),
                            #pointer),
                   LE, Unsigned))

  // Ask the shim for the length of the returned C string.
  syntax String ::= "#csResult" "(" String "," Int ")" [function]
  rule #csResult(S, OutAddr)
    => #csRead(S, OutAddr,
         Bytes2Int(#ffiCall(#functionAddress("ml_cstrlen_c"),
                            ListItem(Int2Bytes(8, OutAddr, LE)),
                            ListItem(#pointer),
                            #sint64),
                   LE, Signed))

  syntax String ::= "#csRead" "(" String "," Int "," Int ")" [function]
  rule #csRead(S, OutAddr, Len)
    => #csFinish(S, OutAddr, #readN(OutAddr, Len))

  syntax Bytes ::= "#readN" "(" Int "," Int ")" [function]
  rule #readN(Addr, N) => #readAux(padRightBytes(.Bytes, N, 0), Addr)

  syntax Bytes ::= "#readAux" "(" Bytes "," Int ")" [function]
  rule #readAux(Mem, Addr) => #seqBytes(#nativeRead(Addr, Mem), Mem)

  // Free the shim's malloc'd buffer, then our own #alloc'd input buffer.
  syntax String ::= "#csFinish" "(" String "," Int "," Bytes ")" [function]
  rule #csFinish(S, OutAddr, Out)
    => #seqString(#free(S),
         #seqString(#ffiCall(#functionAddress("ml_free_c"),
                             ListItem(Int2Bytes(8, OutAddr, LE)),
                             ListItem(#pointer),
                             #void),
                    Bytes2String(Out)))

  syntax String ::= "#seqString" "(" K "," String ")" [function]
  syntax Bytes  ::= "#seqBytes"  "(" K "," Bytes  ")" [function]
  rule #seqString(_, X) => X
  rule #seqBytes(_, X)  => X
endmodule
