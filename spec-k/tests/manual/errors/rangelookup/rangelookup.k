// A range map lookup of a missing key.
module RANGELOOKUP
  imports RANGEMAP
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= rangelookup(Int)

  rule <k> rangelookup(I) => ([ 0, 2 ) r|-> 5) [ I ] </k>
endmodule
