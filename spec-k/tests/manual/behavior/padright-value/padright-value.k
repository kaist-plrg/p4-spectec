// padRightBytes with a value that is not a byte, when no byte is added
// (domains.md: the result is #False if the value is not in [0..255]).
module PADRIGHT-VALUE
  imports BYTES
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= run(Int)
  rule <k> run(V) => padRightBytes(Int2Bytes(2, 258, BE), 1, V) </k>
endmodule
