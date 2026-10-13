// #getenv, listed as supported in domains.md.
module GETENV
  imports K-REFLECTION
  imports STRING

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= run()
  rule <k> run() => #getenv("HOME") </k>
endmodule
