// A function in a set element on a left-hand side, its variables bound:
// matching evaluates it, and the run ends where it has no equation.
module SETELEMENT
  imports SET
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= setelement(Int)

  syntax Int ::= g(Int) [function]
  rule g(0) => 1
  syntax KItem ::= inSet(Int, Set)
  rule <k> setelement(I) => inSet(I, SetItem(1)) </k>
  rule <k> inSet(I, SetItem(g(I)) _:Set) => 1 </k>
  rule <k> inSet(_, _) => 0 </k> [owise]
endmodule
