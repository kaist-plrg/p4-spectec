// Euclidean modulus by zero.
module EMOD
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= emod(Int)

  rule <k> emod(I) => 1 modInt I </k>
endmodule
