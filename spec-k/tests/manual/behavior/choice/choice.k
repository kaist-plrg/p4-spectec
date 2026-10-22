// choice and Set2List on ==K-equal maps and sets built in different orders
// (domains.md: "The same key will always be returned for the same map", "Two
// equal sets are guaranteed to generate the same ordering").
module CHOICE
  imports MAP
  imports SET
  imports LIST
  imports INT
  imports K-EQUAL
  imports COLLECTIONS

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= run(Int)
  rule <k> run(N) => ListItem(.Map [ N <- 1 ] [ 0 -Int N <- 2 ] ==K .Map [ 0 -Int N <- 2 ] [ N <- 1 ])
                     ListItem(choice(.Map [ N <- 1 ] [ 0 -Int N <- 2 ]))
                     ListItem(choice(.Map [ 0 -Int N <- 2 ] [ N <- 1 ]))
                     ListItem(Set2List(SetItem(N) SetItem(0 -Int N)))
                     ListItem(Set2List(SetItem(0 -Int N) SetItem(N))) </k>
endmodule
