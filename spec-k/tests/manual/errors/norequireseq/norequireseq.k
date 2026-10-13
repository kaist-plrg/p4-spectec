// A function with no equation for its argument, in the requires clause
// of an equation: the run ends, and the owise equation is not tried.
module NOREQUIRESEQ
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= norequireseq(Int)

  syntax Int ::= g(Int) [function]
  rule g(0) => 1
  syntax Int ::= h(Int) [function]
  rule h(I) => 1 requires g(I) ==Int 1
  rule h(_) => 2 [owise]
  rule <k> norequireseq(I) => h(I) </k>
endmodule
