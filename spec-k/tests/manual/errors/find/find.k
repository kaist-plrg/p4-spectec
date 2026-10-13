// A negative start for findString.
module FIND
  imports STRING

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= find(Int)

  rule <k> find(I) => findString("abc", "b", I) </k>
endmodule
