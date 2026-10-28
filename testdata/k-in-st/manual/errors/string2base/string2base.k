// A string that is not an integer in base 10.
module STRING2BASE
  imports INT
  imports STRING

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= string2base(String)

  rule <k> string2base(S) => String2Base(S, 10) </k>
endmodule
