// A concatenation of overlapping range maps.
module RANGECONCAT
  imports RANGEMAP
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= rangeconcat(Int)

  rule <k> rangeconcat(I) => ([ 0, 2 ) r|-> 1) ([ I, 3 ) r|-> 2) </k>
endmodule
