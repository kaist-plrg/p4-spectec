// A negative exponent.
module POW
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= pow(Int)

  rule <k> pow(I) => 2 ^Int I </k>
endmodule
