// Path evaluation.
//
// Mirrors spec-meta/3-execution/3.08-eval-path.watsup. A path access is the
// expression steps from the root; an update rebuilds the value step by step.
// A step with no applicable case, an index out of bounds, or a text update
// of the wrong length gives `error`.

requires "3.08-eval-exp.k"

module EVAL-PATH
  imports EVAL-EXP

  // ==========================================================================
  // Eval_path_acc: ctx |- val path : pathRes
  //
  // A path is the chain of the `dotE`/`idxE`/`sliceE` steps from its root.
  // ==========================================================================

  rule <k> path(V:Val, rootPath()) => V ... </k>
  rule <k> path(V:Val, dotPath(P:Path, A:Atom)) => dotE(path(V, P), A) ... </k>
  rule <k> path(V:Val, idxPath(P:Path, Ei:Exp)) => idxE(path(V, P), Ei) ... </k>
  rule <k> path(V:Val, slicePath(P:Path, Ei:Exp, En:Exp)) => sliceE(path(V, P), Ei, En) ... </k>

  // ==========================================================================
  // Eval_path_upd: ctx |- val path := val : pathRes
  //
  // The innermost step rebuilds the value at its prefix, then the prefix is
  // updated with the result.
  // ==========================================================================

  syntax Exp ::= updIdx(Exp, Exp, Val)      [seqstrict(1, 2)]
               | updSlice(Exp, Exp, Exp, Val)
                                            [seqstrict(1, 2, 3)]
               | updDot(Exp, Atom, Val)     [strict(1)]

  //   rule Eval_path_upd/root
  rule <k> upd(_:Val, rootPath(), Vn:Val) => Vn ... </k>

  //   rulegroup Eval_path_upd/idx
  rule <k> upd(Vb:Val, idxPath(P:Path, Ei:Exp), Vn:Val)
        => upd(Vb, P, updIdx(path(Vb, P), Ei, Vn))
       ...
       </k>

  // Text index update replaces one character (`-- if |t_n| = 1`).
  rule <k> updIdx(textV(S:String), natN(N:Int), textV(Sn:String))
        => textV(replaceAtString(S, N, Sn))
       ...
       </k>
    requires lengthString(Sn) ==Int 1 andBool N >=Int 0 andBool N <Int lengthString(S)
  rule <k> updIdx(listV(Vs:ValList), natN(N:Int), Vn:Val) => listV(updateNthVal(Vs, N, Vn)) ... </k>
    requires N >=Int 0 andBool N <Int lengthVals(Vs)
  rule <k> updIdx(_:Val, _:Val, _:Val) => error ... </k> [owise]

  syntax ValList ::= updateNthVal(ValList, Int, Val)
                                            [function, symbol(updateNthVal)]
  rule updateNthVal((_:Val, Vs:ValList), 0, Vn:Val) => Vn, Vs
  rule updateNthVal((V:Val, Vs:ValList), N:Int, Vn:Val)
    => V, updateNthVal(Vs, N -Int 1, Vn)
    requires N >Int 0

  syntax String ::= replaceAtString(String, Int, String)
                                            [function, symbol(replaceAtString)]
  rule replaceAtString(S:String, N:Int, Sn:String)
    => substrString(S, 0, N) +String Sn +String substrString(S, N +Int 1, lengthString(S))

  //   rulegroup Eval_path_upd/slice
  rule <k> upd(Vb:Val, slicePath(P:Path, Ei:Exp, En:Exp), Vn:Val)
        => upd(Vb, P, updSlice(path(Vb, P), Ei, En, Vn))
       ...
       </k>

  rule <k> updSlice(textV(S:String), natN(I:Int), natN(N:Int), textV(Sn:String))
        => textV(substrString(S, 0, I) +String Sn +String substrString(S, I +Int N, lengthString(S)))
       ...
       </k>
    requires lengthString(Sn) ==Int N andBool I >=Int 0 andBool N >=Int 0
       andBool I +Int N <=Int lengthString(S)
  rule <k> updSlice(listV(Vs:ValList), natN(I:Int), natN(N:Int), listV(Ws:ValList))
        => listV(replaceSliceVal(Vs, I, N, Ws))
       ...
       </k>
    requires I >=Int 0 andBool N >=Int 0 andBool I +Int N <=Int lengthVals(Vs)
  rule <k> updSlice(_:Val, _:Val, _:Val, _:Val) => error ... </k> [owise]

  syntax ValList ::= replaceSliceVal(ValList, Int, Int, ValList)
                                            [function, symbol(replaceSliceVal)]
  rule replaceSliceVal(Vs:ValList, I:Int, N:Int, Ws:ValList)
    => appendVal(sliceVal(Vs, 0, I),
                 appendVal(Ws, sliceVal(Vs, I +Int N, lengthVals(Vs) -Int (I +Int N))))

  //   rule Eval_path_upd/dot
  rule <k> upd(Vb:Val, dotPath(P:Path, A:Atom), Vn:Val)
        => upd(Vb, P, updDot(path(Vb, P), A, Vn))
       ...
       </k>

  rule <k> updDot(strV(Fs:ValFieldList), A:Atom, Vn:Val) => strV(updateField(Fs, A, Vn)) ... </k>
  rule <k> updDot(_:Val, _:Atom, _:Val) => error ... </k> [owise]

  // Update every field selected by its atom.
  syntax ValFieldList ::= updateField(ValFieldList, Atom, Val)
                                            [function, total, symbol(updateField)]
  rule updateField(.ValFieldList, _:Atom, _:Val) => .ValFieldList
  rule updateField((valField(A, _:Val), Fs:ValFieldList), A:Atom, Vn:Val)
    => valField(A, Vn), updateField(Fs, A, Vn)
  rule updateField((F:ValField, Fs:ValFieldList), A:Atom, Vn:Val)
    => F, updateField(Fs, A, Vn) [owise]

endmodule
