// Short-circuiting: the second argument fails (domains.md: only andThenBool
// and orElseBool are, andBool and orBool may be).
module SHORT-CIRCUIT
  imports INT
  imports BOOL

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= implies(Int) | and(Int)
  rule <k> implies(N) => false impliesBool (1 /Int N ==Int 1) </k>
  rule <k> and(N) => false andBool (1 /Int N ==Int 1) </k>
endmodule
