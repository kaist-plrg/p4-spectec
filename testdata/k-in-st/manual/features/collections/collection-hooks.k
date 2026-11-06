// Hooks of maps, lists, and sets that build a collection from others. Values
// come from the program, so that kompile does not fold them. keys_list and
// values give a list in the order of the map, which the LLVM backend keeps in
// its hash trie, so they are seen on maps of one entry, or by size and sum.
module COLLECTION-HOOKS
  imports INT
  imports LIST
  imports MAP
  imports SET
  imports COLLECTIONS

  configuration <k> $PGM:Pgm </k> <out> .List </out>

  syntax Pgm ::= remove(Int) | removeAll(Int) | difference(Int) | updateAll(Int)
               | keysList(Int) | values(Int) | fill(Int) | list2set(Int)

  // M[K <- undef]: a key of the map, and one that is not
  rule <k> remove(I) => .K </k>
       <out> _ => ListItem((I |-> 1 (I +Int 1) |-> 2) [ I <- undef ])
                  ListItem((I |-> 1) [ I +Int 5 <- undef ]) </out>

  // removeAll(M, S): keys of the set, in the map or not
  rule <k> removeAll(I) => .K </k>
       <out> _ => ListItem(removeAll(I |-> 1 (I +Int 1) |-> 2 (I +Int 2) |-> 3,
                                     SetItem(I) SetItem(I +Int 2) SetItem(I +Int 7))) </out>

  // M1 -Map M2: an entry goes only if its value is the same in M2
  rule <k> difference(I) => .K </k>
       <out> _ => ListItem((I |-> 1 (I +Int 1) |-> 2) -Map (I |-> 1 (I +Int 1) |-> 5)) </out>

  // updateMap(M1, M2): M2's value of a key in both wins
  rule <k> updateAll(I) => .K </k>
       <out> _ => ListItem(updateMap(I |-> 1 (I +Int 1) |-> 2, (I +Int 1) |-> 7 (I +Int 3) |-> 4)) </out>

  // keys_list: one entry, and the size of a list of three
  rule <k> keysList(I) => .K </k>
       <out> _ => ListItem(keys_list(I |-> 1))
                  ListItem(size(keys_list(I |-> 1 (I +Int 1) |-> 2 (I +Int 2) |-> 3))) </out>

  // values: one entry, and the sum of three
  syntax Int ::= sum(List) [function]
  rule sum(.List) => 0
  rule sum(ListItem(V:Int) L) => V +Int sum(L)
  rule <k> values(I) => .K </k>
       <out> _ => ListItem(values(I |-> 1))
                  ListItem(sum(values(I |-> 1 (I +Int 1) |-> 2 (I +Int 2) |-> 3))) </out>

  // fillList(L, index, length, value): inside the list, and of length 0
  rule <k> fill(I) => .K </k>
       <out> _ => ListItem(fillList(ListItem(0) ListItem(0) ListItem(0) ListItem(0), I, 2, 9))
                  ListItem(fillList(ListItem(0), I -Int 1, 0, 9)) </out>

  // List2Set: duplicates go
  rule <k> list2set(I) => .K </k>
       <out> _ => ListItem(List2Set(ListItem(I) ListItem(I +Int 1) ListItem(I))) </out>
endmodule
