// choice of the empty map.
module MAPCHOICE
  imports INT
  imports MAP

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= mapchoice(Map)

  rule <k> mapchoice(M) => choice(M) </k>
endmodule
