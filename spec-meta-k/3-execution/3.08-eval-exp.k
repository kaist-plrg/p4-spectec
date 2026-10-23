// Expression evaluation.
//
// Mirrors spec-meta/3-execution/3.07-eval-exp.watsup. An expression at the
// head of `<k>` evaluates its operands by the strictness attributes of
// 1.0-syntax.k; the rules here compute on values, and one with no applicable
// case gives `error` (`Eval_exp/error`).

requires "3.05-eval-ops.k"
requires "3.06.4-typ-cast.k"
requires "3.07-eval-assign.k"

module EVAL-EXP
  imports ASSIGN
  imports EVAL-OPS
  imports TYP-CAST

  // ==========================================================================
  // Eval_exp: ctx |- exp : expRes
  // ==========================================================================

  //   rulegroup Eval_exp/literal
  rule <k> boolE(B:Bool) => boolV(B) ... </k>
  rule <k> textE(S:String) => textV(S) ... </k>

  //   rule Eval_exp/variable
  rule <k> varE(X:Id) => V ... </k>
       <lvenv> ... varr(X, .IterList) |-> V:Val ... </lvenv>
  rule <k> varE(_:Id) => error ... </k> [owise]

  //   rulegroup Eval_exp/unary
  rule <k> unE(notOp(), boolV(B:Bool)) => boolV(notBool B) ... </k>
  rule <k> unE(O:NumUnOp, N:Num) => numUnop(O, N) ... </k>
  rule <k> unE(_:UnOp, _:Val) => error ... </k> [owise]

  //   rulegroup Eval_exp/binary and /compare
  rule <k> binE(O:BoolBinOp, boolV(Bl:Bool), boolV(Br:Bool)) => boolV(boolBinop(O, Bl, Br)) ... </k>
  rule <k> binE(O:NumBinOp, Nl:Num, Nr:Num) => unres(numBinop(O, Nl, Nr)) ... </k>
  rule <k> binE(_:BinOp, _:Val, _:Val) => error ... </k> [owise]

  rule <k> cmpE(O:PolyCmpOp, Vl:Val, Vr:Val) => boolV(polyCmpop(O, Vl, Vr)) ... </k>
  rule <k> cmpE(O:NumCmpOp, Nl:Num, Nr:Num) => unresBool(numCmpop(O, Nl, Nr)) ... </k>
  rule <k> cmpE(_:CmpOp, _:Val, _:Val) => error ... </k> [owise]

  //   rule Eval_exp/upcast, /downcast, /subtype
  rule <k> upCastE(T:Typ, V:Val) => unres(upcast(G, L, FE, T, V)) ... </k>
       <gtdenv> G:Map </gtdenv>
       <ltdenv> L:Map </ltdenv>
       <gfenv> FE:Map </gfenv>
  rule <k> downCastE(T:Typ, V:Val) => unres(downcast(G, L, FE, T, V)) ... </k>
       <gtdenv> G:Map </gtdenv>
       <ltdenv> L:Map </ltdenv>
       <gfenv> FE:Map </gfenv>
  rule <k> subE(V:Val, T:Typ) => unresBool(subtyp(FE, updateMap(G, L), T, V)) ... </k>
       <gtdenv> G:Map </gtdenv>
       <ltdenv> L:Map </ltdenv>
       <gfenv> FE:Map </gfenv>

  //   rulegroup Eval_exp/match
  rule <k> matchE(V:Val, P:Pat) => boolV(matches(V, P)) ... </k>

  //   rule Eval_exp/cons
  rule <k> consE(V:Val, listV(Vs:ValList)) => listV(V, Vs) ... </k>
  rule <k> consE(_:Val, _:Val) => error ... </k> [owise]

  //   rulegroup Eval_exp/concat
  rule <k> catE(textV(Sl:String), textV(Sr:String)) => textV(Sl +String Sr) ... </k>
  rule <k> catE(listV(Vl:ValList), listV(Vr:ValList)) => listV(appendVal(Vl, Vr)) ... </k>
  rule <k> catE(_:Val, _:Val) => error ... </k> [owise]

  //   rule Eval_exp/mem
  rule <k> memE(V:Val, listV(Vs:ValList)) => boolV(memVal(V, Vs)) ... </k>
  rule <k> memE(_:Val, _:Val) => error ... </k> [owise]

  syntax Bool ::= memVal(Val, ValList)      [function, total, symbol(memVal)]
  rule memVal(_:Val, .ValList) => false
  rule memVal(V:Val, (W:Val, Vs:ValList)) => V ==K W orBool memVal(V, Vs)

  //   rulegroup Eval_exp/len
  rule <k> lenE(textV(S:String)) => natN(lengthString(S)) ... </k>
  rule <k> lenE(listV(Vs:ValList)) => natN(lengthVals(Vs)) ... </k>
  rule <k> lenE(_:Val) => error ... </k> [owise]

  //   rule Eval_exp/dot, /idx, /slice, and Eval_path_acc/dot, /idx, /slice
  //   (a one-step path from the root, 3.09-eval-path.k)
  rule <k> dotE(strV(Fs:ValFieldList), A:Atom) => unopt(assocVal(A, Fs)) ... </k>
  rule <k> dotE(_:Val, _:Atom) => error ... </k> [owise]

  // The field found, back in flight.
  syntax KItem ::= unopt(ValOpt)
  rule <k> unopt(someVal(V:Val)) => V ... </k>
  rule <k> unopt(noVal()) => error ... </k>

  rule <k> idxE(textV(S:String), natN(N:Int)) => textV(substrString(S, N, N +Int 1)) ... </k>
    requires N >=Int 0 andBool N <Int lengthString(S)
  rule <k> idxE(listV(Vs:ValList), natN(N:Int)) => nthVal(Vs, N) ... </k>
    requires N >=Int 0 andBool N <Int lengthVals(Vs)
  rule <k> idxE(_:Val, _:Val) => error ... </k> [owise]

  syntax Val ::= nthVal(ValList, Int)       [function, symbol(nthVal)]
  rule nthVal((V:Val, _:ValList), 0) => V
  rule nthVal((_:Val, Vs:ValList), N:Int) => nthVal(Vs, N -Int 1) requires N >Int 0

  rule <k> sliceE(textV(S:String), natN(I:Int), natN(N:Int))
        => textV(substrString(S, I, I +Int N))
       ...
       </k>
    requires I >=Int 0 andBool N >=Int 0 andBool I +Int N <=Int lengthString(S)
  rule <k> sliceE(listV(Vs:ValList), natN(I:Int), natN(N:Int)) => listV(sliceVal(Vs, I, N)) ... </k>
    requires I >=Int 0 andBool N >=Int 0 andBool I +Int N <=Int lengthVals(Vs)
  rule <k> sliceE(_:Val, _:Val, _:Val) => error ... </k> [owise]

  syntax ValList ::= sliceVal(ValList, Int, Int)
                                            [function, symbol(sliceVal)]
  rule sliceVal(_:ValList, _:Int, 0) => .ValList
  rule sliceVal((V:Val, Vs:ValList), 0, N:Int) => V, sliceVal(Vs, 0, N -Int 1)
    requires N >Int 0
  rule sliceVal((_:Val, Vs:ValList), I:Int, N:Int) => sliceVal(Vs, I -Int 1, N)
    requires I >Int 0

  //   rule Eval_exp/upd (Eval_path_upd in 3.09-eval-path.k)
  rule <k> updE(Vb:Val, P:Path, Vn:Val) => upd(Vb, P, Vn) ... </k>

  //   rule Eval_exp/call: in 3.10-eval-arg.k, after its arguments

  //   rulegroup Eval_exp/iter
  syntax KItem ::= evalIter(VarrOpt, Exp, IterExp)
  rule <k> iterE(E:Exp, IE:IterExp) => evalIter(isIterOnVar(iterE(E, IE)), E, IE) ... </k>

  //   rule Eval_exp/simple
  rule <k> evalIter(someVarr(R:Varr), _:Exp, _:IterExp) => V ... </k>
       <lvenv> ... R |-> V:Val ... </lvenv>
  rule <k> evalIter(someVarr(_:Varr), _:Exp, _:IterExp) => error ... </k> [owise]

  //   rule Eval_exp/opt-succ: the body under the sub-context, if there is one
  rule <k> evalIter(noVarr(), E:Exp, iterExp(quest(), Xs:VariList))
        => optSub(Xs, E, subOpt(Xs, M))
       ...
       </k>
       <lvenv> M:Map </lvenv>

  syntax KItem ::= optSub(VariList, Exp, Res)
  rule <k> optSub(_:VariList, _:Exp, ok(noSub())) => optV(noVal()) ... </k>
  rule <k> optSub(Xs:VariList, E:Exp, ok(someSub(Vs:ValList)))
        => optE(someExp(inScope(bindRow(Xs, Vs) ~> E)))
       ...
       </k>
  rule <k> optSub(_:VariList, _:Exp, ERROR) => error ... </k>

  //   rule Eval_exp/list-succ: the body under each sub-context in turn
  rule <k> evalIter(noVarr(), E:Exp, iterExp(star(), Xs:VariList))
        => listSub(Xs, E, subList(Xs, M))
       ...
       </k>
       <lvenv> M:Map </lvenv>

  syntax KItem ::= listSub(VariList, Exp, Res)
  rule <k> listSub(Xs:VariList, E:Exp, ok(Rows:ValListList))
        => listE(rowExps(Xs, E, Rows))
       ...
       </k>
  rule <k> listSub(_:VariList, _:Exp, ERROR) => error ... </k>

  syntax ExpList ::= rowExps(VariList, Exp, ValListList)
                                            [function, total, symbol(rowExps)]
  rule rowExps(_:VariList, _:Exp, .ValListList) => .ExpList
  rule rowExps(Xs:VariList, E:Exp, (Row:ValList ; Rows:ValListList))
    => inScope(bindRow(Xs, Row) ~> E), rowExps(Xs, E, Rows)

endmodule
