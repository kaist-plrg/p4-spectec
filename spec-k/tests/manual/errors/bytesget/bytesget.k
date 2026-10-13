// A byte index out of range.
module BYTESGET
  imports INT
  imports BYTES

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= bytesget(Int)

  rule <k> bytesget(I) => b"a" [ I ] </k>
endmodule
