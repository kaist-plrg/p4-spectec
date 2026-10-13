// updateList at index 0 with a source longer than the list (domains.md:
// "Having index + N > K yields an exception").
module UPDATELIST-INDEX0
  imports LIST
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= short(Int) | long(Int)
  rule <k> short(N) => updateList(ListItem(N), 0, ListItem(1) ListItem(2) ListItem(3)) </k>
  rule <k> long(N) => size(updateList(ListItem(N), 0, makeList(40, N))) </k>
endmodule
