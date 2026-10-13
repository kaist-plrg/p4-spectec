// String2Int and String2Base (domains.md: a sign and digits, in a radix of
// 2 to 36).
module STRING2INT-SYNTAX
  imports STRING
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= int(String) | base(String, Int)
  rule <k> int(S) => String2Int(S) </k>
  rule <k> base(S, B) => String2Base(S, B) </k>
endmodule
