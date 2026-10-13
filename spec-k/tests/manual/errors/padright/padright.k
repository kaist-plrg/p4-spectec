// Padding with a value above 255.
module PADRIGHT
  imports INT
  imports BYTES

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= padright(Int)

  rule <k> padright(I) => padRightBytes(b"a", 3, I) </k>
endmodule
