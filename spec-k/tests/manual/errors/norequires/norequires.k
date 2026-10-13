// A function with no equation for its argument, in the requires clause
// of a rule: the run ends, and the owise rule is not tried.
module NOREQUIRES
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= norequires(Int)

  syntax Int ::= g(Int) [function]
  rule g(0) => 1
  rule <k> norequires(I) => 1 </k> requires g(I) ==Int 1
  rule <k> norequires(_) => 2 </k> [owise]
endmodule
