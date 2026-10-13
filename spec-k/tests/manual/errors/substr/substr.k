// A substring that ends before it starts.
module SUBSTR
  imports INT
  imports STRING

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= substr(Int)

  rule <k> substr(I) => substrString("abc", 2, I) </k>
endmodule
