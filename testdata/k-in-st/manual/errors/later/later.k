// The error comes after one step: 1 /Int 0 in the second configuration.
module LATER
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= later(Int) | step(Int)

  rule <k> later(I) => step(I) </k>
  rule <k> step(I) => 1 /Int I </k>
endmodule
