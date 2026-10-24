// A Float literal beyond the exponent range of binary64 (domains.md: rounded
// as an IEEE 754 type, to infinity here).
module FLOAT-LITERAL-RANGE
  imports FLOAT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= run()
  rule <k> run() => 1.0e400 </k>
endmodule
