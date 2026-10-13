// A byte replacement past the end.
module REPLACEAT
  imports BYTES

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= replaceat(Int)

  rule <k> replaceat(I) => replaceAtBytes(b"ab", I, b"xyz") </k>
endmodule
