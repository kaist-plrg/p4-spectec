// A byte substring that ends before it starts.
module BYTESSUBSTR
  imports BYTES

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= bytessubstr(Int)

  rule <k> bytessubstr(I) => substrBytes(b"abc", 2, I) </k>
endmodule
