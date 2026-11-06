// Range maps (RANGEMAP): inserting cuts the ranges it overlaps and merges
// the ones it touches with the same value; lookups by an Int key; removing,
// difference, and inclusion by ranges. Which key choice gives is the
// implementation's, so the case looks only at it being a key. Values come
// from the program, so that kompile does not fold them.
module RANGEMAP-OPS
  imports INT
  imports BOOL
  imports LIST
  imports SET
  imports RANGEMAP

  configuration <k> $PGM:Pgm </k> <out> .List </out>

  syntax Pgm ::= insert(Int) | lookup(Int) | remove(Int) | compare(Int) | views(Int)

  // [I, I+10) |-> 1, then [I+3, I+5) |-> 2 cuts it, then [I+10, I+12) |-> 1
  // touches the right part, which merges with it
  syntax RangeMap ::= m(Int) [function]
  rule m(I) => ((([ I, I +Int 10 ) r|-> 1) [ [ I +Int 3, I +Int 5 ) <- 2 ])
                 [ [ I +Int 10, I +Int 12 ) <- 1 ])

  rule <k> insert(I) => .K </k> <out> _ => ListItem(m(I)) ListItem(size(m(I))) </out>

  rule <k> lookup(I) => .K </k>
       <out> _ => ListItem(m(I) [ I +Int 4 ]) ListItem(m(I) [ I +Int 20 ] orDefault 0)
                  ListItem(find_range(m(I), I +Int 7)) ListItem((I +Int 11) in_keys(m(I)))
                  ListItem((I +Int 12) in_keys(m(I)))
                  ListItem(choice(m(I)) in_keys(m(I))) </out>

  rule <k> remove(I) => .K </k>
       <out> _ => ListItem(m(I) [ [ I +Int 1, I +Int 4 ) <- undef ])
                  ListItem(removeAll(m(I), SetItem([ I, I +Int 1 )) SetItem([ I +Int 9, I +Int 12 )))) </out>

  rule <k> compare(I) => .K </k>
       <out> _ => ListItem(m(I) -RangeMap ([ I, I +Int 2 ) r|-> 1))
                  ListItem(([ I, I +Int 2 ) r|-> 1) <=RangeMap m(I))
                  ListItem(m(I) <=RangeMap ([ I, I +Int 2 ) r|-> 1))
                  ListItem(updateRangeMap(m(I), [ I +Int 4, I +Int 6 ) r|-> 3)) </out>

  rule <k> views(I) => .K </k>
       <out> _ => ListItem(keys_list(m(I))) ListItem(values(m(I))) ListItem(size(keys(m(I)))) </out>
endmodule
