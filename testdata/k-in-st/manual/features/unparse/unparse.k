// #unparseKORE: a term as the KORE text the LLVM backend's printer writes,
// with its symbols, sorts, injections, and escaped strings. Maps and sets of
// more than one element are left out: the printer writes them in the order of
// its hash trie. Each term goes in as a K, through u: the LLVM backend reads
// the argument as a term, so an Int, a Bool, or a collection given as itself
// is read wrong and runs it out of memory.
module UNPARSE
  imports INT
  imports STRING
  imports BOOL
  imports LIST
  imports MAP
  imports SET
  imports K-REFLECTION

  configuration <k> $PGM:Pgm </k> <out> .List </out>

  syntax Exp ::= Int | String | pr(Exp, Exp) | "nil"
  syntax Pgm ::= ints(Int) | strings(String) | terms(Int) | collections(Int)

  syntax String ::= u(K) [function]
  rule u(T) => #unparseKORE(T)

  rule <k> ints(I) => .K </k>
       <out> _ => ListItem(u(I)) ListItem(u(0 -Int I)) </out>

  // quotes, a backslash, a newline, a tab, and a byte past ASCII
  rule <k> strings(S) => .K </k>
       <out> _ => ListItem(u(S +String "\"\\\n\t\xe9")) </out>

  // a constructor, a constant, and an Int injected into Exp
  rule <k> terms(I) => .K </k>
       <out> _ => ListItem(u(pr(I, pr(nil, "a")))) ListItem(u(true)) </out>

  rule <k> collections(I) => .K </k>
       <out> _ => ListItem(u(ListItem(I) ListItem(pr(I, nil))))
                  ListItem(u(I |-> "v"))
                  ListItem(u(SetItem(I)))
                  ListItem(u(.List))
                  ListItem(u(.Map)) </out>
endmodule
