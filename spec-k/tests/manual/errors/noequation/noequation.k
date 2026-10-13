// A function with no equation for its argument, on a right-hand side.
module NOEQUATION
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= noequation(Int)

  syntax Int ::= g(Int) [function]
  rule g(0) => 1
  rule <k> noequation(I) => g(I) </k>
endmodule
