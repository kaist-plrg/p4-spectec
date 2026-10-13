// K behaviours that the K documentation leaves open or states otherwise
// (k-findings.md in the k-in-p4 repository, by test number E1, E2, ...).
// Each program is t(Test, N); N comes from the program so that kompile does
// not fold the expressions, except where a test compares with folding.
// Where the spec follows the documentation and krun does not, the program is
// a known failure (suites.toml).
module BEHAVIOR-SYNTAX
  imports INT-SYNTAX
  syntax Pgm ::= t(Int, Int)

endmodule

module BEHAVIOR
  imports BEHAVIOR-SYNTAX
  imports DOMAINS
  imports RANGEMAP
  imports K-REFLECTION
  imports MINT
  imports BYTES
  imports FLOAT
  imports COLLECTIONS

  syntax MInt{8}

  configuration <k> $PGM:Pgm </k>

  syntax KItem ::= res(List) | out(K)
  syntax KItem ::= g(K) | h1(K) | h2(K) | lk(List) | found(K, K) | "notfound"

  syntax A ::= a(Int)
  syntax B ::= A
  syntax C ::= B
  syntax B ::= toB(A) [function]
  rule toB(X) => X
  syntax C ::= toC(B) [function]
  rule toC(X) => X
  syntax C ::= toC2(A) [function]
  rule toC2(X) => X

  // E1 inclusion of equal collections ("strict subset" in domains.md)
  rule <k> t(1, N) => res(ListItem((N |-> 1) <=Map (N |-> 1))
                          ListItem(SetItem(N) <=Set SetItem(N))
                          ListItem(((N |-> 1) (N +Int 1 |-> 2)) <=Map (N |-> 1))) </k>

  // E2 choice and Set2List on ==K-equal maps and sets built in different orders
  rule <k> t(2, N) => res(ListItem(.Map [ N <- 1 ] [ 0 -Int N <- 2 ] ==K .Map [ 0 -Int N <- 2 ] [ N <- 1 ])
                          ListItem(choice(.Map [ N <- 1 ] [ 0 -Int N <- 2 ]))
                          ListItem(choice(.Map [ 0 -Int N <- 2 ] [ N <- 1 ]))
                          ListItem(Set2List(SetItem(N) SetItem(0 -Int N)))
                          ListItem(Set2List(SetItem(0 -Int N) SetItem(N)))) </k>

  // E3 RangeMap.choice on ==K-equal range maps built in different orders
  rule <k> t(3, N) => res(
         ListItem(.RangeMap [ [ N , N +Int 1 ) <- 1 ] [ [ N +Int 2 , N +Int 3 ) <- 2 ] [ [ N +Int 4 , N +Int 5 ) <- 3 ] [ [ N +Int 6 , N +Int 7 ) <- 4 ]
              ==K .RangeMap [ [ N +Int 6 , N +Int 7 ) <- 4 ] [ [ N +Int 4 , N +Int 5 ) <- 3 ] [ [ N +Int 2 , N +Int 3 ) <- 2 ] [ [ N , N +Int 1 ) <- 1 ])
         ListItem(choice(.RangeMap [ [ N , N +Int 1 ) <- 1 ] [ [ N +Int 2 , N +Int 3 ) <- 2 ] [ [ N +Int 4 , N +Int 5 ) <- 3 ] [ [ N +Int 6 , N +Int 7 ) <- 4 ]))
         ListItem(choice(.RangeMap [ [ N +Int 6 , N +Int 7 ) <- 4 ] [ [ N +Int 4 , N +Int 5 ) <- 3 ] [ [ N +Int 2 , N +Int 3 ) <- 2 ] [ [ N , N +Int 1 ) <- 1 ]))) </k>

  // E4, E5 updateList at index 0 with a longer source ("yields an exception")
  rule <k> t(4, N) => res(ListItem(updateList(ListItem(N), 0, ListItem(1) ListItem(2) ListItem(3)))) </k>
  rule <k> t(5, N) => res(ListItem(size(updateList(ListItem(N), 0, makeList(40, N))))) </k>

  // E6 #getenv (listed as supported in domains.md)
  rule <k> t(6, _) => out(#getenv("HOME")) </k>

  // E7, E8 or-patterns: which alternative binds X
  rule <k> t(7, N) => h1(g(N)) </k>
  rule <k> h1(g(X:K) #Or X:K) => found(X, .K) </k>
  rule <k> t(8, N) => h2(g(N)) </k>
  rule <k> h2(X:K #Or g(X:K)) => found(X, .K) </k>

  // E9 ==K and map lookup with a composed injection
  rule <k> t(9, N) => res(ListItem(toC(toB(a(N))) ==K toC2(a(N)))
                          ListItem((toC(toB(a(N))) |-> 1) [ toC2(a(N)) ] orDefault 0)) </k>

  // E10, E11 L[I <- V] on a left-hand side
  rule <k> t(10, N) => lk(ListItem(N)) </k>
  rule <k> t(11, N) => lk(ListItem(N) ListItem(N +Int 1) ListItem(N +Int 2)) </k>
  rule <k> lk(L [ 2 <- V ]) => found(V, L) </k>
  rule <k> lk(_) => notfound </k> [owise]

  // E12 constant folding vs the runtime (N = 3)
  rule <k> t(12, N) => res(ListItem(countAllOccurrences("aaa", "aa"))
                           ListItem(countAllOccurrences(substrString("aaaa", 0, N), "aa"))
                           ListItem(lengthString("가"))
                           ListItem(lengthString("가" +String substrString("x", 0, N -Int 3)))
                           ListItem(replaceAll("aaa", "aa", "b"))
                           ListItem(replaceAll(substrString("aaaa", 0, N), "aa", "b"))) </k>

  // E22 replaceAll with overlapping occurrences (N = 3)
  rule <k> t(22, N) => res(ListItem(replaceAll(substrString("aaaa", 0, N), "aa", "b"))
                           ListItem(replaceAll(substrString("aaaa", 0, N), "aa", "xyz"))
                           ListItem(replaceAll(substrString("aaaaaaaa", 0, N +Int 4), "aa", "b"))) </k>

  // E13, E14 short-circuiting with N = 0
  rule <k> t(13, N) => res(ListItem(false impliesBool (1 /Int N ==Int 1))) </k>
  rule <k> t(14, N) => res(ListItem(false andBool (1 /Int N ==Int 1))) </k>

  // E15 padRightBytes with a value that is not a byte, when no padding is needed (N = 258)
  rule <k> t(15, N) => res(ListItem(padRightBytes(Int2Bytes(2, N, BE), 1, 999))) </k>

  // E16 MInt division by zero (N = 0)
  rule <k> t(16, N) => res(ListItem(Int2MInt(1)::MInt{8} /uMInt Int2MInt(N)::MInt{8})) </k>

  // E17 square root of -0.0 (N = 0)
  rule <k> t(17, N) => res(ListItem(sqrtFloat(--Float Int2Float(N, 53, 11)))) </k>

  // E18 String2Int with white space (N = 1)
  rule <k> t(18, N) => res(ListItem(String2Int(" 1 2" +String substrString("3", 0, N)))) </k>

  // E19 Int2Bytes with a negative length (N = 258)
  rule <k> t(19, N) => res(ListItem(Int2Bytes(0 -Int 2, N, BE))) </k>
  // E20 two rules of the same priority that both match ff(aa, bb)
  syntax KItem ::= "aa" | "bb" | ff(KItem, KItem) [function]
  rule ff(aa, _) => 1
  rule ff(_, bb) => 2
  syntax KItem ::= mk(Int) [function]
  rule mk(0) => aa
  rule mk(_) => bb [owise]
  rule <k> t(20, N) => res(ListItem(ff(mk(N), mk(N +Int 1)))) </k>

  // E21 a fresh variable in a function equation with one argument
  syntax Int ::= fr(Int) [function]
  rule fr(_) => !_I:Int
  rule <k> t(21, N) => res(ListItem(fr(N)) ListItem(fr(N))) </k>

  // E23 a Float literal beyond the exponent range of binary64 (rounded to
  // infinity in domains.md)
  rule <k> t(23, _) => res(ListItem(1.0e400)) </k>

  // E24, E25 ^%Int with a negative exponent (N = 1) and with modulus 0 (N = 0)
  // ("equal in value to (A ^Int B) %Int C")
  rule <k> t(24, N) => res(ListItem(2 ^%Int (0 -Int N) 5)) </k>
  rule <k> t(25, N) => res(ListItem(2 ^%Int 3 N)) </k>

  // E26 String2Base with a radix above 36 (N = 37)
  rule <k> t(26, N) => res(ListItem(String2Base("10", N))) </k>

  // E27 countAllOccurrences with overlapping occurrences at run time (N = 3)
  rule <k> t(27, N) => res(ListItem(countAllOccurrences(substrString("aaaa", 0, N), "aa"))) </k>
endmodule
