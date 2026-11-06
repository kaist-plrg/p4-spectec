// <=Map and <=Set on equal collections: true (domains.md says "strict subset").
module INCLUSION
  imports MAP
  imports SET
  imports LIST
  imports INT
  imports BOOL

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= run(Int)
  rule <k> run(N) => ListItem((N |-> 1) <=Map (N |-> 1))
                     ListItem(SetItem(N) <=Set SetItem(N))
                     ListItem(((N |-> 1) (N +Int 1 |-> 2)) <=Map (N |-> 1)) </k>
endmodule
