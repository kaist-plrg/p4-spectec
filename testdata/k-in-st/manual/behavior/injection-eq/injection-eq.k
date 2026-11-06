// ==K and map lookup with a composed injection.
module INJECTION-EQ
  imports MAP
  imports LIST
  imports INT
  imports K-EQUAL

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= run(Int)
  syntax A ::= a(Int)
  syntax B ::= A
  syntax C ::= B
  syntax B ::= toB(A) [function]
  rule toB(X) => X
  syntax C ::= toC(B) [function]
  rule toC(X) => X
  syntax C ::= toC2(A) [function]
  rule toC2(X) => X
  rule <k> run(N) => ListItem(toC(toB(a(N))) ==K toC2(a(N)))
                     ListItem((toC(toB(a(N))) |-> 1) [ toC2(a(N)) ] orDefault 0) </k>
endmodule
