// replaceAll and countAllOccurrences where occurrences overlap (the
// equations of domains.md count them without overlap).
module OVERLAPPING-OCCURRENCES
  imports STRING
  imports LIST

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= replace(String, String, String) | count(String, String)
  rule <k> replace(S, A, B) => replaceAll(S, A, B) </k>
  rule <k> count(S, A) => countAllOccurrences(S, A) </k>
endmodule
