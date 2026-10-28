// A negative left shift.
module SHL
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= shl(Int)

  rule <k> shl(I) => 1 <<Int I </k>
endmodule
