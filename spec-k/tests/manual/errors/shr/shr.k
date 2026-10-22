// A negative right shift.
module SHR
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= shr(Int)

  rule <k> shr(I) => 1 >>Int I </k>
endmodule
