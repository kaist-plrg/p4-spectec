// A list of negative length.
module LISTMAKE
  imports LIST

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= listmake(Int)

  rule <k> listmake(I) => makeList(I, 0) </k>
endmodule
