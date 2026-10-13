// A fresh variable in a function equation (kast.md: a value distinct from
// any other generated). krun gives the fresh value of a rule and the first
// fresh value of a function called on its right-hand side the same value: on
// alongside, 0, 0, 1. The function reads the counter of the configuration the
// rule matched, before the rule takes its own value from it.
module FRESH-IN-FUNCTION
  imports INT
  imports LIST

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= run(Int) | alongside(Int)
  syntax Int ::= fr(Int) [function]
  rule fr(_) => !_I:Int
  rule <k> run(N) => ListItem(fr(N)) ListItem(fr(N)) </k>
  rule <k> alongside(N) => ListItem(!_X:Int) ListItem(fr(N)) ListItem(fr(N)) </k>
endmodule
