// Rule and equation priorities: a smaller number is tried first, owise is 200,
// and rules without a priority are 50. Values come from the program, so that
// kompile does not fold them.
module PRIORITY
  imports INT
  imports LIST

  configuration <k> $PGM:Pgm </k> <log> .List </log>

  syntax Pgm ::= a(Int) | b(Int) | c(Int) | d(Int)

  rule <k> a(I) => .K </k> <log> L => L ListItem(I +Int 60) </log> [priority(60)]
  rule <k> a(I) => .K </k> <log> L => L ListItem(I +Int 40) </log> [priority(40)]

  rule <k> b(I) => .K </k> <log> L => L ListItem(I +Int 250) </log> [priority(250)]
  rule <k> b(I) => .K </k> <log> L => L ListItem(I +Int 200) </log> [owise]

  syntax Int ::= f(Int) [function]
  rule f(_) => 60 [priority(60)]
  rule f(0) => 40 [priority(40)]
  rule f(_) => 45 [priority(45)]
  rule <k> c(I) => .K </k> <log> L => L ListItem(f(I)) ListItem(f(I +Int 1)) </log>

  rule <k> d(I) => .K </k> <log> L => L ListItem(I +Int 51) </log> [priority(51)]
  rule <k> d(I) => .K </k> <log> L => L ListItem(I +Int 50) </log>
endmodule
