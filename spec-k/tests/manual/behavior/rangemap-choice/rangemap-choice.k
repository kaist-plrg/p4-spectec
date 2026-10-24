// choice on ==K-equal range maps built in different orders.
module RANGEMAP-CHOICE
  imports RANGEMAP
  imports LIST
  imports INT
  imports K-EQUAL

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= run(Int)
  rule <k> run(N) => ListItem(
         .RangeMap [ [ N , N +Int 1 ) <- 1 ] [ [ N +Int 2 , N +Int 3 ) <- 2 ] [ [ N +Int 4 , N +Int 5 ) <- 3 ] [ [ N +Int 6 , N +Int 7 ) <- 4 ]
         ==K .RangeMap [ [ N +Int 6 , N +Int 7 ) <- 4 ] [ [ N +Int 4 , N +Int 5 ) <- 3 ] [ [ N +Int 2 , N +Int 3 ) <- 2 ] [ [ N , N +Int 1 ) <- 1 ])
       ListItem(choice(.RangeMap [ [ N , N +Int 1 ) <- 1 ] [ [ N +Int 2 , N +Int 3 ) <- 2 ] [ [ N +Int 4 , N +Int 5 ) <- 3 ] [ [ N +Int 6 , N +Int 7 ) <- 4 ]))
       ListItem(choice(.RangeMap [ [ N +Int 6 , N +Int 7 ) <- 4 ] [ [ N +Int 4 , N +Int 5 ) <- 3 ] [ [ N +Int 2 , N +Int 3 ) <- 2 ] [ [ N , N +Int 1 ) <- 1 ])) </k>
endmodule
