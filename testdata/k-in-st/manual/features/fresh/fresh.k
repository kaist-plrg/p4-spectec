// A fresh value !X of a function equation: two in one equation differ. Their
// numbers are not looked at: kast.md requires only that fresh values differ,
// and the LLVM backend numbers those of a function from the counter of the
// configuration its rule matched, which it then increments in place (k#4942).
// The calling rule takes a fresh value too, so it builds a new counter cell
// and the one incremented in place is dropped.
module FRESH
  imports INT
  imports BOOL

  configuration <k> $PGM:Pgm </k> <out> 0 </out>

  syntax Pgm ::= run(Int)
  syntax Bool ::= distinct(Int) [function]
  rule distinct(_) => !X:Int =/=Int !Y:Int
  rule <k> run(I) => distinct(I) </k> <out> _ => !Z:Int </out>
endmodule
