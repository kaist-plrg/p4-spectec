// t-division by zero.
module TDIV
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= tdiv(Int)

  rule <k> tdiv(I) => 1 /Int I </k>
endmodule
