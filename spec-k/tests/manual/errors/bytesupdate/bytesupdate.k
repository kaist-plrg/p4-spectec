// A byte update with a value above 255.
module BYTESUPDATE
  imports BYTES

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= bytesupdate(Int)

  rule <k> bytesupdate(I) => b"a" [ 0 <- I ] </k>
endmodule
