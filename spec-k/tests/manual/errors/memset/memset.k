// memsetBytes with a value above 127.
module MEMSET
  imports INT
  imports BYTES

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= memset(Int)

  rule <k> memset(I) => memsetBytes(b"abc", 0, 2, I) </k>
endmodule
