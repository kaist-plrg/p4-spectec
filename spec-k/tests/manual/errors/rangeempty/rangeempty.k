// An empty range.
module RANGEEMPTY
  imports RANGEMAP
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= rangeempty(Int)

  rule <k> rangeempty(I) => ([ I, 3 ) r|-> 5) </k>
endmodule
