// The square root of -0.0 (domains.md: a generalization of IEEE 754, where
// it is -0.0).
module SQRT-NEGATIVE-ZERO
  imports FLOAT
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= run(Int)
  rule <k> run(N) => sqrtFloat(--Float Int2Float(N, 53, 11)) </k>
endmodule
