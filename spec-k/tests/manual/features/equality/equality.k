// ==K on collections, float and bytes literals on the left, lazy impliesBool,
// list patterns longer than the list, and hooks the languages do not use
// (or use on paths their tests do not reach). Values come from the program,
// so that kompile does not fold them.
module EQUALITY
  imports INT
  imports MAP
  imports SET
  imports LIST
  imports BOOL
  imports K-EQUAL
  imports FLOAT
  imports BYTES
  imports STRING

  configuration <k> $PGM:Pgm </k>

  syntax Pgm ::= Float | Bytes
               | maps(Int, Int) | sets(Int, Int) | lists(Int, Int) | implies(Int)
               | short(Int) | strings(String) | signed(Bytes)
               | ints(Int, Int) | compare(String, String) | single(Int)

  rule <k> maps(A, B) => ListItem((A |-> B B |-> A) ==K (B |-> A A |-> B))
                         ListItem((A |-> B) ==K (A |-> A))
                         ListItem((A |-> B) =/=K (A |-> A)) </k>
  rule <k> sets(A, B) => ListItem(SetItem(A) SetItem(B) ==K SetItem(B) SetItem(A))
                         ListItem(SetItem(A) ==K SetItem(B)) </k>
  rule <k> lists(A, B) => ListItem(ListItem(A) ListItem(B) ==K ListItem(A) ListItem(B))
                          ListItem(ListItem(A) ListItem(B) ==K ListItem(B) ListItem(A)) </k>

  rule <k> 1.5 => ListItem(true) </k>
  rule <k> F:Float => ListItem(F =/=Float 1.5) </k> [owise]
  rule <k> b"ab" => ListItem(true) </k>
  rule <k> _:Bytes => ListItem(false) </k> [owise]

  // the second argument would fail if it were evaluated
  rule <k> implies(I) => ListItem((I >Int 0) impliesBool (1 /Int I ==Int 0)) </k>

  syntax Int ::= two(List) [function]
  rule two(ListItem(A:Int) ListItem(_) _) => A
  rule two(_) => 0 [owise]
  rule <k> short(I) => ListItem(two(ListItem(I))) ListItem(two(ListItem(I) ListItem(6))) </k>

  rule <k> strings(S) => ListItem(replace(S, "a", "b", 2)) ListItem(countAllOccurrences(S, "aa")) </k>
  rule <k> signed(B) => ListItem(Bytes2Int(B, BE, Signed)) ListItem(Bytes2Int(B, LE, Signed)) </k>

  rule <k> ints(I, J) => ListItem(absInt(I)) ListItem(absInt(J)) ListItem(I modInt J) ListItem(J modInt I)
                         ListItem(signExtendBitRangeInt(I, 0, 8)) ListItem(signExtendBitRangeInt(J, 0, 8))
                         ListItem(signExtendBitRangeInt(I, 0, 0)) </k>
  rule <k> compare(S, T) => ListItem(S <String T) ListItem(S <=String T) ListItem(S >=String T)
                            ListItem(T <String S) ListItem(T <=String S) ListItem(T >=String S) </k>
  // one element, since K leaves open which of several choice gives
  rule <k> single(I) => ListItem(pushList(I, ListItem(1))) ListItem(choice(SetItem(I)))
                        ListItem(choice(I |-> 1)) </k>
endmodule
