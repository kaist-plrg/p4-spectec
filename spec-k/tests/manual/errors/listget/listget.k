// A list index out of range.
module LISTGET
  imports INT
  imports LIST

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= listget(Int)

  rule <k> listget(I) => ListItem(1) [ I ] </k>
endmodule
