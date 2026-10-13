// t-modulus by zero.
module TMOD
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= tmod(Int)

  rule <k> tmod(I) => 1 %Int I </k>
endmodule
