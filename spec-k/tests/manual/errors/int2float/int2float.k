// Int2Float with a precision of 0.
module INT2FLOAT
  imports INT
  imports FLOAT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= int2float(Int)

  rule <k> int2float(I) => Int2Float(1, I, 8) </k>
endmodule
