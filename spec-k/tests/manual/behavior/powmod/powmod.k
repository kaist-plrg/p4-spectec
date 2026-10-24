// ^%Int (domains.md: equal in value to (A ^Int B) %Int C).
module POWMOD
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= run(Int, Int)
  rule <k> run(B, C) => 2 ^%Int B C </k>
endmodule
