// A character code above 255.
module CHR
  imports INT
  imports STRING

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= chr(Int)

  rule <k> chr(I) => chrChar(I) </k>
endmodule
