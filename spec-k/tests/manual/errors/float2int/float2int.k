// Float2Int of NaN.
module FLOAT2INT
  imports INT
  imports FLOAT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= float2int(Float)

  rule <k> float2int(F) => Float2Int(F) </k>
endmodule
