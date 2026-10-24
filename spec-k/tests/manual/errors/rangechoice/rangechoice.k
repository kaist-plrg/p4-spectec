// choice of the empty range map.
module RANGECHOICE
  imports INT
  imports RANGEMAP

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= rangechoice(RangeMap)

  rule <k> rangechoice(M) => choice(M) </k>
endmodule
