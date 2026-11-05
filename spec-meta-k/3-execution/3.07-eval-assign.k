// Assignment (pattern matching a value against an expression).
//
// Mirrors spec-meta/4-execution/4.05-eval-assign.watsup. An assignment is a
// statement: it rewrites to `.K` with its bindings added to `<local>`, or
// to `error`.
//
//   Assign_exp:  ctx |- exp := val : assignRes    --> assignExp(Exp, Val)
//   Assign_exps: ctx |- exp* := val* : assignRes  --> assignExps(ExpList, ValList)
//   Assign_arg:  ctx |- arg := val : assignRes    --> assignArg(Arg, Val)
//   Assign_args: ctx |- arg* := val* : assignRes  --> assignArgs(ArgList, ValList)

requires "3.03-relation.k"

module ASSIGN
  imports RELATION
  imports BOOL

  // ==========================================================================
  // Assign_exp: ctx |- exp := val : assignRes
  // ==========================================================================

  //   rule Assign_exp/variable: C |- VAR id := val : C'
  rule <k> assignExp(varE(X:Id), V:Val) => .K ... </k>
       <lvenv> M:Map => M[varr(X, .IterList) <- V] </lvenv>

  //   rule Assign_exp/tup: C |- TUP exp* := TUP val* : C'
  rule <k> assignExp(tupE(Es:ExpList), tupV(Vs:ValList)) => assignExps(Es, Vs) ... </k>

  //   rule Assign_exp/inj: C |- INJ (mixop exp*) := INJ (mixop val*) : C'
  rule <k> assignExp(injE(expCase(M:MixOp, Es:ExpList)),
                     injV(valCase(M:MixOp, Vs:ValList)))
        => assignExps(Es, Vs)
       ...
       </k>

  //   rule Assign_exp/str: C |- STR expfield* := STR valfield* : C'
  rule <k> assignExp(strE(Fs:ExpFieldList), strV(Gs:ValFieldList))
        => assignFields(Fs, Gs)
       ...
       </k>

  //   rulegroup Assign_exp/opt
  rule <k> assignExp(optE(someExp(E:Exp)), optV(someVal(V:Val))) => assignExp(E, V) ... </k>
  rule <k> assignExp(optE(noExp()), optV(noVal())) => .K ... </k>

  //   rule Assign_exp/list: C |- LIST exp* := LIST val* : C'
  rule <k> assignExp(listE(Es:ExpList), listV(Vs:ValList)) => assignExps(Es, Vs) ... </k>

  //   rule Assign_exp/cons: C |- CONS exp_h exp_t := LIST (val_h :: val_t*) : C''
  rule <k> assignExp(consE(Eh:Exp, Et:Exp), listV(Vh:Val, Vt:ValList))
        => assignExp(Eh, Vh) ~> assignExp(Et, listV(Vt))
       ...
       </k>

  //   rulegroup Assign_exp/iter
  rule <k> assignExp(iterE(E:Exp, IE:IterExp), V:Val)
        => assignIter(isIterOnVar(iterE(E, IE)), E, IE, V)
       ...
       </k>

  rule <k> assignExp(_:Exp, _:Val) => error ... </k> [owise]

  // ==========================================================================
  // Assigning to an iterated expression
  // ==========================================================================

  syntax KItem ::= assignIter(VarrOpt, Exp, IterExp, Val)

  //   rule Assign_exp/simple
  rule <k> assignIter(someVarr(R:Varr), _:Exp, _:IterExp, V:Val) => .K ... </k>
       <lvenv> M:Map => M[R <- V] </lvenv>

  //   rule Assign_exp/opt-none
  rule <k> assignIter(noVarr(), _:Exp, iterExp(quest(), Vs:VariList), optV(noVal()))
        => .K
       ...
       </k>
       <lvenv> M:Map
            => addsMap(M, iterVaris(Vs, quest()),
                       repeatVal(optV(noVal()), lengthVaris(Vs)))
       </lvenv>

  //   rule Assign_exp/opt-some
  rule <k> assignIter(noVarr(), E:Exp, iterExp(quest(), Vs:VariList), optV(someVal(V:Val)))
        => assignExp(E, V) ~> assignIterVaris(Vs)
       ...
       </k>

  syntax KItem ::= assignIterVaris(VariList)
  rule <k> assignIterVaris(Vs:VariList) => bindOpts(Vs, readVaris(Vs)) ... </k>

  //   rule Assign_exp/list
  rule <k> assignIter(noVarr(), E:Exp, iterExp(star(), Vs:VariList), listV(Ws:ValList))
        => bindLists(Vs, assignRows(E, Ws, Vs))
       ...
       </k>

  // Each element is assigned under an empty value environment, and the
  // bindings it makes are read back as one row.
  syntax KItem ::= assignRows(Exp, ValList, VariList)
  rule <k> assignRows(_:Exp, .ValList, _:VariList) => .ValListList ... </k>
  rule <k> assignRows(E:Exp, (V:Val, Rest:ValList), Vs:VariList)
        => consRow(inScope(clearLocalVenv() ~> assignExp(E, V) ~> readVaris(Vs)),
                   assignRows(E, Rest, Vs))
       ...
       </k>

  // ==========================================================================
  // Assign_exps: ctx |- exp* := val* : assignRes
  //
  // The values may still be a computation (a relation call's outputs).
  // ==========================================================================

  rule <k> assignExps(.ExpList, .ValList) => .K ... </k>
  rule <k> assignExps((E:Exp, Es:ExpList), (V:Val, Vs:ValList))
        => assignExp(E, V) ~> assignExps(Es, Vs)
       ...
       </k>
  rule <k> assignExps(_:ExpList, _:ValList) => error ... </k> [owise]

  // Struct fields, matched positionally with their atoms compared.
  syntax KItem ::= assignFields(ExpFieldList, ValFieldList)

  rule <k> assignFields(.ExpFieldList, .ValFieldList) => .K ... </k>
  rule <k> assignFields((expField(A:Atom, E:Exp), Fs:ExpFieldList),
                        (valField(A:Atom, V:Val), Gs:ValFieldList))
        => assignExp(E, V) ~> assignFields(Fs, Gs)
       ...
       </k>
  rule <k> assignFields(_:ExpFieldList, _:ValFieldList) => error ... </k> [owise]

  // ==========================================================================
  // Assign_arg / Assign_args: ctx |- arg(*) := val(*) : assignRes
  // ==========================================================================

  //   rule Assign_arg/exp
  rule <k> assignArg(expA(E:Exp), V:Val) => assignExp(E, V) ... </k>

  //   rule Assign_arg/fun: the parameter is bound to the global function name
  rule <k> assignArg(funA(X:Id), funcV(Y:Id)) => .K ... </k>
       <lfenv> M:Map => M[X <- Y] </lfenv>

  rule <k> assignArg(_:Arg, _:Val) => error ... </k> [owise]

  //   rulegroup Assign_args
  rule <k> assignArgs(.ArgList, .ValList) => .K ... </k>
  rule <k> assignArgs((A:Arg, As:ArgList), (V:Val, Vs:ValList))
        => assignArg(A, V) ~> assignArgs(As, Vs)
       ...
       </k>
  rule <k> assignArgs(_:ArgList, _:ValList) => error ... </k> [owise]

  // ==========================================================================
  // Value-level helpers
  // ==========================================================================

  syntax Int ::= lengthVaris(VariList)      [function, total, symbol(lengthVaris)]
  rule lengthVaris(.VariList) => 0
  rule lengthVaris(_:Vari, Vs:VariList) => 1 +Int lengthVaris(Vs)

  // `$find_varis` on the current value environment: the values, or `error`.
  syntax KItem ::= readVaris(VariList)
  rule <k> readVaris(Vs:VariList) => unres(finds(varrsOfVaris(Vs), M)) ... </k>
       <lvenv> M:Map </lvenv>

  // `$add_varis` of the bindings of one sub-context, each as `OPT val`.
  syntax KItem ::= bindOpts(VariList, KItem)
                                            [strict(2)]
  rule <k> bindOpts(Vs:VariList, Ws:ValList) => .K ... </k>
       <lvenv> M:Map => addsMap(M, iterVaris(Vs, quest()), someVals(Ws)) </lvenv>

  // `$add_varis` of rows of bindings (one computation per row), one list
  // per variable.
  syntax KItem ::= bindLists(VariList, KItem)
                                            [strict(2)]
  rule <k> bindLists(Vs:VariList, Rows:ValListList) => .K ... </k>
       <lvenv> M:Map => addsMap(M, iterVaris(Vs, star()), columns(Vs, Rows)) </lvenv>

  syntax ValList ::= columns(VariList, ValListList)
                                            [function, total, symbol(columns)]
  rule columns(.VariList, _:ValListList) => .ValList
  rule columns((_:Vari, Vs:VariList), Rows:ValListList)
    => listV(headsOf(Rows)), columns(Vs, tailsOf(Rows))

  syntax ValList ::= someVals(ValList)      [function, total, symbol(someVals)]
  rule someVals(.ValList) => .ValList
  rule someVals(V:Val, Vs:ValList) => optV(someVal(V)), someVals(Vs)

endmodule
