// Two equations of the same priority that both match ff(aa, bb).
module SAME-PRIORITY
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= run(Int)
  syntax KItem ::= "aa" | "bb" | ff(KItem, KItem) [function]
  rule ff(aa, _) => 1
  rule ff(_, bb) => 2
  syntax KItem ::= mk(Int) [function]
  rule mk(0) => aa
  rule mk(_) => bb [owise]
  rule <k> run(N) => ff(mk(N), mk(N +Int 1)) </k>
endmodule
