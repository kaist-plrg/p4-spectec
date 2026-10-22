// Machine-integer division by zero.
module MINT-DIV-ZERO
  imports MINT
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax MInt{8}
  syntax Pgm ::= run(Int)
  syntax KItem ::= res(MInt{8})
  rule <k> run(N) => res(Int2MInt(1)::MInt{8} /uMInt Int2MInt(N)::MInt{8}) </k>
endmodule
