// A fresh variable in a function equation (kast.md: a value distinct from
// any other generated).
module FRESH-IN-FUNCTION
  imports INT
  imports LIST

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= run(Int)
  syntax Int ::= fr(Int) [function]
  rule fr(_) => !_I:Int
  rule <k> run(N) => ListItem(fr(N)) ListItem(fr(N)) </k>
endmodule
