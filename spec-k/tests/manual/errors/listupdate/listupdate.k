// A list update out of range.
module LISTUPDATE
  imports INT
  imports LIST

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= listupdate(Int)

  rule <k> listupdate(I) => ListItem(1) [ I <- 2 ] </k>
endmodule
