// Int2Bytes with a negative length.
module INT2BYTES-NEGATIVE
  imports BYTES
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= run(Int)
  rule <k> run(L) => Int2Bytes(L, 258, BE) </k>
endmodule
