// A string that is not an integer.
module STRING2INT
  imports STRING

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= string2int(String)

  rule <k> string2int(S) => String2Int(S) </k>
endmodule
