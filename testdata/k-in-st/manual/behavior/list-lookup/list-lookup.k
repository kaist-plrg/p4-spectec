// L[I <- V] on a left-hand side: outside the list it does not match.
module LIST-LOOKUP
  imports LIST
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= outside(Int) | inside(Int)
  syntax KItem ::= lk(List) | found(K, K) | "notfound"
  rule <k> outside(N) => lk(ListItem(N)) </k>
  rule <k> inside(N) => lk(ListItem(N) ListItem(N +Int 1) ListItem(N +Int 2)) </k>
  rule <k> lk(L [ 2 <- V ]) => found(V, L) </k>
  rule <k> lk(_) => notfound </k> [owise]
endmodule
