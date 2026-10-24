// find_range of a missing key.
module RANGEFIND
  imports RANGEMAP
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= rangefind(Int)

  rule <k> rangefind(I) => find_range(([ 0, 2 ) r|-> 5), I) </k>
endmodule
