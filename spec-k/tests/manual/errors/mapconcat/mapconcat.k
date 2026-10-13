// A concatenation of maps with the same key.
module MAPCONCAT
  imports MAP
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= mapconcat(Int)

  rule <k> mapconcat(I) => (1 |-> 2) (I |-> 3) </k>
endmodule
