// kompile folds hooks on constants with its own implementation: here
// constant and run-time arguments give different results (N = 3).
module CONSTANT-FOLDING
  imports STRING
  imports LIST
  imports INT

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= run(Int)
  rule <k> run(N) => ListItem(countAllOccurrences("aaa", "aa"))
                     ListItem(countAllOccurrences(substrString("aaaa", 0, N), "aa"))
                     ListItem(lengthString("가"))
                     ListItem(lengthString("가" +String substrString("x", 0, N -Int 3)))
                     ListItem(replaceAll("aaa", "aa", "b"))
                     ListItem(replaceAll(substrString("aaaa", 0, N), "aa", "b")) </k>
endmodule
