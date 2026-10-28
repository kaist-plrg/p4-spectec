// A function in a list index (L[I <- V]) on a left-hand side, its variables
// bound: matching evaluates it, and the run ends where it has no equation.
module LISTINDEX
  imports LIST
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= listindex(Int)

  syntax Int ::= g(Int) [function]
  rule g(0) => 1
  syntax KItem ::= inList(Int, List)
  rule <k> listindex(I) => inList(I, ListItem(5) ListItem(6)) </k>
  rule <k> inList(I, _ [ g(I) <- V ]) => V </k>
  rule <k> inList(_, _) => 0 </k> [owise]
endmodule
