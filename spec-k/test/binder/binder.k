requires "substitution.md"

// Substitution into values of every kind, and renaming a binder whose
// variable occurs free in the substituted value. After a capture, krun prints
// the free variable renamed (y0), so check(E) compares it while rewriting.
module BINDER
  imports INT
  imports STRING
  imports BOOL
  imports FLOAT
  imports BYTES
  imports SUBSTITUTION
  imports K-EQUAL

  syntax Exp ::= KVar | Int | String | Bool | Float | Bytes
               | "lam" KVar "." Exp [binder]
               | app(Exp, Exp)
               | pair(Exp, Exp)
               | check(Exp)

  syntax KItem ::= "#check"

  configuration <k> $PGM:Exp </k>

  rule <k> app(lam X:KVar . E:Exp, V:Exp) => E[V / X] ... </k>
  rule <k> check(E) => E ~> #check ... </k>
  rule <k> lam _:KVar . pair(A, pair(_, R)) ~> #check => pair(R, A ==K String2KVar("y")) ... </k>
endmodule
