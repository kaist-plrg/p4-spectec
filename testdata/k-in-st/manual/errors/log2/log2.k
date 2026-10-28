// The logarithm of zero.
module LOG2
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= log2(Int)

  rule <k> log2(I) => log2Int(I) </k>
endmodule
