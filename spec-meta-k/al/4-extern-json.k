// JSON codec and transport for the external interface to the host interpreter.
//
// `Call_builtin_func`, `Call_extern_func` and `Call_extern_rel` are all
// `extern relation`s in watsup: their meaning lives outside the
// meta-language.  Rather than mirror those implementations in K — which
// duplicates authority, and covers only the handful someone got round to
// porting — such a call is serialized here, handed to `spectec-boot`, and its
// result read back.  The OCaml stays the single authority.
//
// All three go to `spectec-boot extern -spec SPEC` — one subcommand, which
// tells the kinds apart by the request's own key — and differ only in *who*
// answers:
//
//   - `Call_builtin_func` -> the OCaml builtin registry
//     (`p4spec/lib/interface/builtin/`), a static lookup that does not load
//     SPEC at all (except for `$print_`, which unparses against it).
//   - `Call_extern_func` / `Call_extern_rel` -> answered against SPEC on the
//     P4 interface.  Two things can supply
//     the meaning there, and `externArgs()` (bottom of this file) says which
//     SPEC to load for each: a *lower spec*'s ordinary `dec`/`relation`
//     definitions, mirroring `Make_parametric`
//     (`p4spec/lib/backend-boot/spectec.ml:173-215`); or, for the P4 spec's own
//     externs, the architecture model in `p4spec/lib/backend-sim/`.
//
// Because that is the only difference, one transport serves all three: the
// pipeline below is parameterized by the subcommand string to run and an
// `ExternKind` saying how to decode the reply.
//
// The OCaml half of this codec is `p4spec/lib/interface/spectec/ali/extern.ml`,
// which documents the wire format in full.  In brief:
//
//   val   ::= ["boolV", <bool>]
//           | ["natN", "<decimal>"] | ["intN", "<decimal>"]
//           | ["textV", <string>]
//           | ["strV", [[<atom>, val], ...]]
//           | ["injV", mixop, [val, ...]]
//           | ["tupV", [val, ...]]
//           | ["optV", null] | ["optV", val]
//           | ["listV", [val, ...]]
//           | ["funcV", <id>]
//           | ["extV", <json>]
//
//   mixop ::= [[<atom>, ...], ...]     // an atoms matrix, exactly K's MixOp
//
//   typ   ::= ["natT"] | ["intT"] | ["boolT"] | ["textT"]
//           | ["varT", <id>, [typ, ...]] | ["tupT", [typ, ...]]
//           | ["iterT", typ, "?"|"*"] | ["funcT"]
//
//   request  ::= {"builtin":     <id>, "targs": [typ, ...], "args": [val, ...]}
//              | {"extern-func": <id>, "targs": [typ, ...], "args": [val, ...]}
//              | {"extern-rel":  <id>, "args": [val, ...]}
//
//   response ::= {"ok": val}         // builtin and extern-func
//              | {"ok": [val, ...]}  // extern-rel: a relation yields val*
//              | {"fail": null}      // recoverable failure; extern only
//
// The request kinds are told apart by *which key is present*, so a builtin
// request is byte-identical to what it was before externs existed.
//
// `{"fail": null}` becomes K's `FAIL`, which backtracks through `tryNextRul`
// / `tryNextClause` exactly like a failed rule.  It is a spec-level outcome,
// so it arrives on stdout with exit status 0; a non-zero exit stays reserved
// for "the wire broke" and still has no rule.  An extern relation takes no
// `targs` (`spec-meta/common/4-relation.watsup:26-30`).
//
// Numbers cross as decimal *strings*, not JSON numbers: `natN`/`intN` hold
// arbitrary-precision integers on both sides (K's `Int`, OCaml's `Bigint.t`)
// and P4 routinely exceeds 64 bits.
//
// A `JSON` *term* is built and stringified once with `JSON2String`, rather
// than concatenating strings by hand: atoms are rendered with quotes (`':'`)
// and identifiers may contain characters that need escaping, and `JSON2String`
// already escapes correctly.
//
// None of these functions is `total`, deliberately.  A malformed response
// leaves a visible non-`Val` stuck in the `<k>` cell — which is what you want
// to read — rather than quietly rewriting to a wrong answer.  This matches the
// convention used for the partial helpers in 5.5-eval-call-func.k.

requires "json.md"
requires "0-config.k"
requires "../common/3-relation.k"

module AL-EXTERN-JSON
  imports AL-CONFIG
  imports COMMON-RELATION
  imports JSON
  imports K-IO
  imports STRING
  imports INT

  // ==========================================================================
  // Outbound: K terms -> JSON
  //
  // `Id` and `Atom` are subsorts of `String` (common/1-syntax.k:34-35), and
  // `String` is a `JSON`, so they drop straight into a JSON string position
  // with no coercion.
  // ==========================================================================

  syntax JSON ::= jsonOfVal(Val)              [function, symbol(jsonOfVal)]
  syntax JSONs ::= jsonOfVals(ValList)        [function, symbol(jsonOfVals)]
  syntax JSONs ::= jsonOfValFields(ValFieldList)
                                              [function, symbol(jsonOfValFields)]
  syntax JSON ::= jsonOfMixOp(MixOp)          [function, symbol(jsonOfMixOp)]
  syntax JSONs ::= jsonOfAtomRows(MixOp)      [function, symbol(jsonOfAtomRows)]
  syntax JSONs ::= jsonOfAtoms(AtomList)      [function, symbol(jsonOfAtoms)]
  syntax JSON ::= jsonOfTyp(Typ)              [function, symbol(jsonOfTyp)]
  syntax JSONs ::= jsonOfTyps(TypList)        [function, symbol(jsonOfTyps)]

  //   `[ Js ]` cannot be written with a `JSONs`-valued call directly inside
  //   it: the parser cannot tell that from a one-element list whose element is
  //   that call.  This wrapper pins the `JSONs` position.
  syntax JSON ::= jsonList(JSONs)             [function, total, symbol(jsonList)]
  rule jsonList(Js:JSONs) => [ Js ]

  rule jsonOfVal(boolV(B:Bool))   => [ "boolV", B, .JSONs ]
  rule jsonOfVal(natN(N:Int))     => [ "natN", Int2String(N), .JSONs ]
  rule jsonOfVal(intN(N:Int))     => [ "intN", Int2String(N), .JSONs ]
  rule jsonOfVal(textV(S:String)) => [ "textV", S, .JSONs ]
  rule jsonOfVal(strV(Fs:ValFieldList))
    => [ "strV", jsonList(jsonOfValFields(Fs)), .JSONs ]
  rule jsonOfVal(injV(valCase(M:MixOp, Vs:ValList)))
    => [ "injV", jsonOfMixOp(M), jsonList(jsonOfVals(Vs)), .JSONs ]
  rule jsonOfVal(tupV(Vs:ValList))  => [ "tupV", jsonList(jsonOfVals(Vs)), .JSONs ]
  rule jsonOfVal(optV(noVal()))     => [ "optV", null, .JSONs ]
  rule jsonOfVal(optV(someVal(V:Val))) => [ "optV", jsonOfVal(V), .JSONs ]
  rule jsonOfVal(listV(Vs:ValList)) => [ "listV", jsonList(jsonOfVals(Vs)), .JSONs ]
  rule jsonOfVal(funcV(X:Id))       => [ "funcV", X, .JSONs ]

  // `extV(Json)` gets no rule on purpose: `Json` is declared abstract
  // (common/1-syntax.k:159) and has no K-reachable inhabitants, so a value
  // that somehow reaches here should stick visibly rather than be invented.

  rule jsonOfVals(.ValList) => .JSONs
  rule jsonOfVals(V:Val, Vs:ValList) => jsonOfVal(V), jsonOfVals(Vs)

  rule jsonOfValFields(.ValFieldList) => .JSONs
  rule jsonOfValFields(valField(A:Atom, V:Val), Fs:ValFieldList)
    => [ A, jsonOfVal(V), .JSONs ], jsonOfValFields(Fs)

  rule jsonOfMixOp(M:MixOp) => jsonList(jsonOfAtomRows(M))

  rule jsonOfAtomRows(.MixOp) => .JSONs
  rule jsonOfAtomRows(As:AtomList ; M:MixOp)
    => jsonList(jsonOfAtoms(As)), jsonOfAtomRows(M)

  rule jsonOfAtoms(.AtomList) => .JSONs
  rule jsonOfAtoms(A:Atom, As:AtomList) => A, jsonOfAtoms(As)

  rule jsonOfTyp(boolT()) => [ "boolT", .JSONs ]
  rule jsonOfTyp(natT())  => [ "natT", .JSONs ]
  rule jsonOfTyp(intT())  => [ "intT", .JSONs ]
  rule jsonOfTyp(textT()) => [ "textT", .JSONs ]
  rule jsonOfTyp(varT(X:Id, Ts:TypList))
    => [ "varT", X, jsonList(jsonOfTyps(Ts)), .JSONs ]
  rule jsonOfTyp(tupT(Ts:TypList)) => [ "tupT", jsonList(jsonOfTyps(Ts)), .JSONs ]
  rule jsonOfTyp(iterT(T:Typ, quest())) => [ "iterT", jsonOfTyp(T), "?", .JSONs ]
  rule jsonOfTyp(iterT(T:Typ, star()))  => [ "iterT", jsonOfTyp(T), "*", .JSONs ]
  rule jsonOfTyp(funcT()) => [ "funcT", .JSONs ]

  rule jsonOfTyps(.TypList) => .JSONs
  rule jsonOfTyps(T:Typ, Ts:TypList) => jsonOfTyp(T), jsonOfTyps(Ts)

  // ==========================================================================
  // Requests
  // ==========================================================================

  syntax JSON ::= jsonOfRequest(Id, TypList, ValList)
                                              [function, symbol(jsonOfRequest)]
  rule jsonOfRequest(X:Id, Ts:TypList, Vs:ValList)
    => { "builtin" : X,
         "targs" : jsonList(jsonOfTyps(Ts)),
         "args" : jsonList(jsonOfVals(Vs)),
         .JSONs }

  //   An extern *function* request carries `targs`, like a builtin's.  They
  //   are forwarded faithfully even though only one of the two OCaml routes
  //   reads them: `Make_parametric.call_extern_func` unboots and passes them
  //   to `eval_func`, while `interp.ml:1451` passes `[]` to
  //   `eval_extern_func`.  Sending them costs nothing and keeps the request
  //   honest about what was called.
  syntax JSON ::= jsonOfExternFuncRequest(Id, TypList, ValList)
                                    [function, symbol(jsonOfExternFuncRequest)]
  rule jsonOfExternFuncRequest(X:Id, Ts:TypList, Vs:ValList)
    => { "extern-func" : X,
         "targs" : jsonList(jsonOfTyps(Ts)),
         "args" : jsonList(jsonOfVals(Vs)),
         .JSONs }

  //   An extern *relation* takes only `id val*`, so no `targs` field.
  syntax JSON ::= jsonOfExternRelRequest(Id, ValList)
                                     [function, symbol(jsonOfExternRelRequest)]
  rule jsonOfExternRelRequest(X:Id, Vs:ValList)
    => { "extern-rel" : X,
         "args" : jsonList(jsonOfVals(Vs)),
         .JSONs }

  // ==========================================================================
  // Inbound: JSON -> K terms
  //
  // Only the `val` half is needed: a response never carries a type.
  // ==========================================================================

  syntax Val ::= valOfJson(JSON)              [function, symbol(valOfJson)]
  syntax ValList ::= valsOfJsons(JSONs)       [function, symbol(valsOfJsons)]
  syntax ValFieldList ::= valFieldsOfJsons(JSONs)
                                              [function, symbol(valFieldsOfJsons)]
  syntax MixOp ::= mixOpOfJson(JSON)          [function, symbol(mixOpOfJson)]
  syntax MixOp ::= atomRowsOfJsons(JSONs)     [function, symbol(atomRowsOfJsons)]
  syntax AtomList ::= atomsOfJsons(JSONs)     [function, symbol(atomsOfJsons)]

  rule valOfJson([ "boolV", B:Bool, .JSONs ]) => boolV(B)
  rule valOfJson([ "natN", S:String, .JSONs ]) => natN(String2Int(S))
  rule valOfJson([ "intN", S:String, .JSONs ]) => intN(String2Int(S))
  rule valOfJson([ "textV", S:String, .JSONs ]) => textV(S)
  rule valOfJson([ "strV", [ Js:JSONs ], .JSONs ]) => strV(valFieldsOfJsons(Js))
  rule valOfJson([ "injV", Jm:JSON, [ Js:JSONs ], .JSONs ])
    => injV(valCase(mixOpOfJson(Jm), valsOfJsons(Js)))
  rule valOfJson([ "tupV", [ Js:JSONs ], .JSONs ]) => tupV(valsOfJsons(Js))
  rule valOfJson([ "optV", null, .JSONs ]) => optV(noVal())
  //   `null` is itself a `JSON`, so the two `optV` shapes overlap; an explicit
  //   side condition says which is which, rather than leaving it to `[owise]`.
  rule valOfJson([ "optV", J:JSON, .JSONs ]) => optV(someVal(valOfJson(J)))
    requires J =/=K null
  rule valOfJson([ "listV", [ Js:JSONs ], .JSONs ]) => listV(valsOfJsons(Js))
  rule valOfJson([ "funcV", X:String, .JSONs ]) => funcV(X)

  rule valsOfJsons(.JSONs) => .ValList
  rule valsOfJsons(J:JSON, Js:JSONs) => valOfJson(J), valsOfJsons(Js)

  rule valFieldsOfJsons(.JSONs) => .ValFieldList
  rule valFieldsOfJsons([ A:String, J:JSON, .JSONs ], Js:JSONs)
    => valField(A, valOfJson(J)), valFieldsOfJsons(Js)

  rule mixOpOfJson([ Js:JSONs ]) => atomRowsOfJsons(Js)

  rule atomRowsOfJsons(.JSONs) => .MixOp
  rule atomRowsOfJsons([ Jas:JSONs ], Js:JSONs)
    => atomsOfJsons(Jas) ; atomRowsOfJsons(Js)

  rule atomsOfJsons(.JSONs) => .AtomList
  rule atomsOfJsons(A:String, Js:JSONs) => A, atomsOfJsons(Js)

  // ==========================================================================
  // Responses
  // ==========================================================================

  syntax JSON ::= respOk(JSON)                [function, symbol(respOk)]
  rule respOk({ "ok" : J:JSON, .JSONs }) => J

  //   An extern reply resolves to *either* a payload or `FAIL`, so unlike
  //   `respOk` these decode all the way to a `KItem` that can land in `<k>`
  //   directly.  `FAIL` is an ordinary KItem here (common/3-relation.k), which
  //   is exactly what makes a failed extern backtrack like a failed rule.
  syntax KItem ::= externFuncResult(JSON)     [function, symbol(externFuncResult)]
                 | externRelResult(JSON)      [function, symbol(externRelResult)]

  rule externFuncResult({ "ok" : J:JSON, .JSONs }) => valOfJson(J)
  rule externFuncResult({ "fail" : null, .JSONs }) => FAIL

  //   A relation's outputs are `val*`, so its `ok` payload is a JSON array;
  //   `valsOfJsons` yields the `ValList` that `relCallDone()` expects.
  rule externRelResult({ "ok" : [ Js:JSONs ], .JSONs }) => valsOfJsons(Js)
  rule externRelResult({ "fail" : null, .JSONs }) => FAIL

  // ==========================================================================
  // Transport: write the request to a file, shell out, decode the reply.
  //
  // Shared by all three call kinds.  Two things vary, and are threaded through
  // the pipeline states rather than duplicated: the subcommand *arguments*
  // string, and an `ExternKind` telling the final step which decoder to use.
  //
  // RUNTIME DEPENDENCY: `./spectec-boot` must exist and runs must start from
  // the repo root.  K rules cannot read environment variables, so unlike
  // `kast-json.sh` there is no `$SPECTEC_BOOT` override available here.  The
  // request file lives under `./.tmp/` for the same reason — a path relative
  // to the working directory, kept inside the project rather than in `/tmp`.
  // ==========================================================================

  syntax ExternKind ::= "builtinK" "(" ")"    [symbol(builtinK)]
                      | "externFuncK" "(" ")" [symbol(externFuncK)]
                      | "externRelK" "(" ")"  [symbol(externRelK)]

  syntax KItem ::= externReq(String, String, ExternKind)
                 | externFile(IOFile, String, String, ExternKind)
                 | externWrite(IOInt, String, String, String, ExternKind)
                 | externRun(String, String, ExternKind)
                 | externResp(Int, String, String, ExternKind)

  //   The request goes through a *file*, never through argv: operator atoms
  //   render with quotes (`':'`), and `#system` passes its argument through a
  //   shell, which fails on those with an unterminated quoted string.  Only
  //   the `#mkstemp` path — `./.tmp/spectec-k-` plus six alphanumerics — is
  //   ever interpolated into the command line.
  //
  //   One file serves the whole run.  `#mkstemp` mints it on the first call
  //   and `<builtinreq>` remembers the path; every later call reuses it, and
  //   K removes it when rewriting ends.  Calling `#mkstemp` per call would
  //   instead leave one file behind per invocation.
  //
  //   First call: no path yet, so make the file and write through the fd
  //   `#mkstemp` already opened.
  rule <k> externReq(Req:String, Args:String, K:ExternKind)
        => externFile(#mkstemp(builtinTemplate()), Req, Args, K)
       ...
       </k>
       <builtinreq> "" </builtinreq>

  rule <k> externFile(#tempFile(P:String, FD:Int), Req:String, Args:String, K:ExternKind)
        => externWrite(FD, Req, P, Args, K)
       ...
       </k>
       <builtinreq> _ => P </builtinreq>

  //   Later calls: reopen the same path in mode `"w"`, which truncates (C
  //   `fopen` semantics), so a short request can never read back the tail of a
  //   longer predecessor.
  rule <k> externReq(Req:String, Args:String, K:ExternKind)
        => externWrite(#open(P, "w"), Req, P, Args, K)
       ...
       </k>
       <builtinreq> P:String </builtinreq>
    requires P =/=String ""

  //   `#write`/`#close` have sort `K`, so they can only be sequenced directly
  //   in the `<k>` cell — a `strict(1)` wrapper around them does not kompile.
  rule <k> externWrite(FD:Int, Req:String, P:String, Args:String, K:ExternKind)
        => #write(FD, Req) ~> #close(FD) ~> externRun(P, Args, K)
       ...
       </k>

  rule <k> externRun(P:String, Args:String, K:ExternKind)
        => #system(builtinCmd() +String " " +String Args
                   +String " -i '" +String P +String "'")
           ~> externKindK(K)
       ...
       </k>

  //   `#system` is a *function* hook, so its result lands in the `<k>` cell as
  //   a `#systemResult` that a continuation has to pick up.  The kind travels
  //   in a following KItem rather than inside `#systemResult`, whose shape is
  //   fixed by K-IO.
  syntax KItem ::= externKindK(ExternKind)
  rule <k> #systemResult(C:Int, Out:String, Err:String) ~> externKindK(K:ExternKind)
        => externResp(C, Out, Err, K)
       ...
       </k>

  rule <k> externResp(0, Out:String, _:String, builtinK())
        => valOfJson(respOk(String2JSON(Out)))
       ...
       </k>
  rule <k> externResp(0, Out:String, _:String, externFuncK())
        => externFuncResult(String2JSON(Out))
       ...
       </k>
  rule <k> externResp(0, Out:String, _:String, externRelK())
        => externRelResult(String2JSON(Out))
       ...
       </k>

  //   A non-zero exit deliberately has no rule, for all three kinds.  It means
  //   the wire broke — a malformed request, a missing `./spectec-boot`, a
  //   lower spec that would not load — which is a defect rather than something
  //   the spec can recover from.  That is distinct from `{"fail": null}`,
  //   which *is* recoverable and arrives with exit 0.  The stuck term carries
  //   the child's stderr, which is what you want to read.

  syntax String ::= "builtinCmd" "(" ")"      [function, total, symbol(builtinCmd)]
                  | "builtinTemplate" "(" ")" [function, total, symbol(builtinTemplate)]
                  | "builtinArgs" "(" ")"     [function, total, symbol(builtinArgs)]
                  | externArgs(P4Opt)         [function, total, symbol(externArgs)]
  rule builtinCmd() => "./spectec-boot"
  //   Builtins go to the same `extern` subcommand as externs — one command
  //   serves all three request kinds, told apart by the request's own key.
  //   `-spec` names the spec that `$print_` unparses against: it is the one
  //   builtin needing more than a registry lookup, and the OCaml side loads the
  //   spec only when a request actually needs it, so naming it costs nothing
  //   for the other 46 builtins.  Hardcoded like the paths around it.
  rule builtinArgs() => "extern -spec spec"
  //   `#mkstemp` fills the `XXXXXX` in, so concurrent runs in one working
  //   directory get distinct request files.  `./.tmp/` must exist — the
  //   `kast-json.sh` wrapper creates it, since K rules cannot mkdir.
  rule builtinTemplate() => "./.tmp/spectec-k-XXXXXX"
  //   The spec an extern call resolves against.  Only the *path* varies, and
  //   `<p4prog>` — the cell that already tells the two entries of 6-entry.k
  //   apart — is what selects it:
  //
  //   - `noP4()`: a self-contained script (`examples/extern-call.watsup`),
  //     whose externs mean `examples/lower`'s ordinary `dec`/`relation`
  //     definitions.  That is `Make_parametric`'s route into a lower runner
  //     (`p4spec/lib/backend-boot/spectec.ml:173-215`).
  //
  //   - `someP4(_)`: the P4 spec type-checking a program.  Its own externs
  //     (`ExternFunctionCall_eval_lctk`, `ExternMethodCall_eval`, ...) mean the
  //     architecture model in `p4spec/lib/backend-sim/`, so the spec to load is
  //     `spec` itself.
  //
  //   `spectec-boot extern` always builds its runner on the P4 interface, which
  //   is what resolves the second kind; the first is unaffected, since the
  //   interface changes only how *extern* names are resolved and
  //   `examples/lower` declares none of its own.  So neither an interface nor a
  //   mode flag appears here.  Getting this wrong is quiet rather than loud: an
  //   extern name the runner cannot resolve comes back as `{"fail": null}`,
  //   indistinguishable from a spec-level failure (CROSS.md §8).
  //
  //   Both paths are hardcoded for the same reason `builtinCmd()` is: K rules
  //   cannot read the environment.  `<p4prog>` is the one channel that does
  //   vary, so a spec that is neither of these two still has nowhere to come
  //   from.
  rule externArgs(noP4())    => "extern -spec examples/lower"
  rule externArgs(someP4(_)) => "extern -spec spec"

endmodule
