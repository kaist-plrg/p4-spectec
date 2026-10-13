// Int2Bytes of a negative length.
module INT2BYTES
  imports BYTES

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= int2bytes(Int)

  rule <k> int2bytes(I) => Int2Bytes(I, 5, BE) </k>
endmodule
