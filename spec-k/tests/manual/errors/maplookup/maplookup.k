// A map lookup of a missing key.
module MAPLOOKUP
  imports MAP
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= maplookup(Int)

  rule <k> maplookup(I) => (1 |-> 2) [ I ] </k>
endmodule
