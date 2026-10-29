// Euclidean division by zero.
module EDIV
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= ediv(Int)

  rule <k> ediv(I) => 1 divInt I </k>
endmodule
