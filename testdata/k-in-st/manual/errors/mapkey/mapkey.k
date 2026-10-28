// A function in a map key on a left-hand side, its variables bound:
// matching evaluates it, and the run ends where it has no equation.
module MAPKEY
  imports MAP
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= mapkey(Int)

  syntax Int ::= g(Int) [function]
  rule g(0) => 1
  syntax KItem ::= inMap(Int, Map)
  rule <k> mapkey(I) => inMap(I, 1 |-> 3) </k>
  rule <k> inMap(I, g(I) |-> V _:Map) => V </k>
  rule <k> inMap(_, _) => 0 </k> [owise]
endmodule
