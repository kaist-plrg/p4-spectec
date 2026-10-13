// Or-patterns: which alternative binds X.
module OR-PATTERN
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= orLeft(Int) | orRight(Int)
  syntax KItem ::= g(K) | h1(K) | h2(K) | found(K)
  rule <k> orLeft(N) => h1(g(N)) </k>
  rule <k> h1(g(X:K) #Or X:K) => found(X) </k>
  rule <k> orRight(N) => h2(g(N)) </k>
  rule <k> h2(X:K #Or g(X:K)) => found(X) </k>
endmodule
