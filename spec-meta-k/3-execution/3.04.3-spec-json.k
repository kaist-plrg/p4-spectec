// JSON codec of AL specifications, as the host sends them (3.14-entry.k).
//
// Each node is tagged with the symbol of its constructor in 1.0-syntax.k;
// lists are arrays, and an absent option is null. Mixops and types are those
// of 3.04.1-extern-json.k, and a number is a value there.
//
//   spec     ::= [defn, ...]
//   defn     ::= ["extTypD", <id>] | ["typD", <id>, [<id>, ...], deftyp]
//              | ["extRelD", <id>, [typ, ...], [typ, ...]]
//              | ["relD", <id>, [typ, ...], [typ, ...], [rulGroup, ...], elsGroup | null]
//              | ["extFuncD", <id>, [<id>, ...], [param, ...], typ]
//              | ["builtinFuncD", <id>, [<id>, ...], [param, ...], typ]
//              | ["tableFuncD", <id>, [param, ...], typ, [tblRow, ...]]
//              | ["funcD", <id>, [<id>, ...], [param, ...], typ, [clause, ...], clause | null]
//   deftyp   ::= ["aliasDT", typ] | ["structDT", [[<atom>, typ], ...]]
//              | ["variantDT", [[mixop, [typ, ...]], ...]]
//   vari     ::= [<id>, typ, [iter, ...]]
//   iter     ::= "?" | "*"
//   exp      ::= ["natN", "<decimal>"] | ["intN", "<decimal>"]
//              | ["boolE", <bool>] | ["textE", <string>] | ["varE", <id>]
//              | ["unE", <op>, exp] | ["binE", <op>, exp, exp] | ["cmpE", <op>, exp, exp]
//              | ["upCastE", typ, exp] | ["downCastE", typ, exp] | ["subE", exp, typ]
//              | ["matchE", exp, pat] | ["tupE", [exp, ...]] | ["injE", mixop, [exp, ...]]
//              | ["strE", [[<atom>, exp], ...]] | ["optE", exp | null] | ["listE", [exp, ...]]
//              | ["consE", exp, exp] | ["catE", exp, exp] | ["memE", exp, exp] | ["lenE", exp]
//              | ["dotE", exp, <atom>] | ["idxE", exp, exp] | ["sliceE", exp, exp, exp]
//              | ["updE", exp, path, exp] | ["callE", <id>, [typ, ...], [arg, ...]]
//              | ["iterE", exp, iter, [vari, ...]]
//   <op>     ::= "notOp" | "plusOp" | "minusOp" | "andOp" | ... | "eqOp" | "neOp"
//   pat      ::= ["injPat", mixop] | ["consPat"] | ["fixedPat", <int>] | ["nilPat"]
//              | ["somePat"] | ["nonePat"]
//   path     ::= ["rootPath"] | ["idxPath", path, exp] | ["slicePath", path, exp, exp]
//              | ["dotPath", path, <atom>]
//   arg      ::= ["expA", exp] | ["funA", <id>]
//   param    ::= ["expParam", typ] | ["funParam", <id>, [<id>, ...], [param, ...], typ]
//   prem     ::= ["relPr", <id>, [exp, ...], [exp, ...]] | ["ifPr", exp]
//              | ["ifHoldPr", <id>, [exp, ...]] | ["ifNotHoldPr", <id>, [exp, ...]]
//              | ["letPr", exp, exp] | ["iterPr", prem, iter, [vari, ...], [vari, ...]]
//              | ["debugPr", exp]
//   rulGroup ::= [<id>, rulMatch, [rulPath, ...]]
//   elsGroup ::= [<id>, rulMatch, rulPath]
//   rulMatch ::= [[exp, ...], [prem, ...]]
//   rulPath  ::= [<id>, [exp, ...], [prem, ...]]
//   clause   ::= [[arg, ...], exp, [prem, ...]]       // and a table row

requires "3.04.1-extern-json.k"

module SPEC-JSON
  imports EXTERN-JSON

  // ==========================================================================
  // Spec and definitions
  // ==========================================================================

  syntax Spec ::= specOfJsons(JSONs)          [function, symbol(specOfJsons)]
  rule specOfJsons(.JSONs) => .Spec
  rule specOfJsons(J:JSON, Js:JSONs) => defnOfJson(J), specOfJsons(Js)

  syntax Defn ::= defnOfJson(JSON)            [function, symbol(defnOfJson)]
  rule defnOfJson([ "extTypD", X:String, .JSONs ]) => extTypD(X)
  rule defnOfJson([ "typD", X:String, [ Ps:JSONs ], J:JSON, .JSONs ])
    => typD(X, tparamsOfJsons(Ps), defTypOfJson(J))
  rule defnOfJson([ "extRelD", X:String, [ Ti:JSONs ], [ To:JSONs ], .JSONs ])
    => extRelD(X, typsOfJsons(Ti), typsOfJsons(To))
  rule defnOfJson([ "relD", X:String, [ Ti:JSONs ], [ To:JSONs ], [ Gs:JSONs ], G:JSON, .JSONs ])
    => relD(X, typsOfJsons(Ti), typsOfJsons(To), rulGroupsOfJsons(Gs), elsGroupOfJson(G))
  rule defnOfJson([ "extFuncD", X:String, [ Ps:JSONs ], [ Qs:JSONs ], T:JSON, .JSONs ])
    => extFuncD(X, tparamsOfJsons(Ps), paramsOfJsons(Qs), typOfJson(T))
  rule defnOfJson([ "builtinFuncD", X:String, [ Ps:JSONs ], [ Qs:JSONs ], T:JSON, .JSONs ])
    => builtinFuncD(X, tparamsOfJsons(Ps), paramsOfJsons(Qs), typOfJson(T))
  rule defnOfJson([ "tableFuncD", X:String, [ Qs:JSONs ], T:JSON, [ Rs:JSONs ], .JSONs ])
    => tableFuncD(X, paramsOfJsons(Qs), typOfJson(T), tblRowsOfJsons(Rs))
  rule defnOfJson([ "funcD", X:String, [ Ps:JSONs ], [ Qs:JSONs ], T:JSON, [ Cs:JSONs ], C:JSON, .JSONs ])
    => funcD(X, tparamsOfJsons(Ps), paramsOfJsons(Qs), typOfJson(T),
             clausesOfJsons(Cs), elsClauseOfJson(C))

  // ==========================================================================
  // Type definitions
  // ==========================================================================

  syntax DefTyp ::= defTypOfJson(JSON)        [function, symbol(defTypOfJson)]
  rule defTypOfJson([ "aliasDT", T:JSON, .JSONs ]) => aliasDT(typOfJson(T))
  rule defTypOfJson([ "structDT", [ Fs:JSONs ], .JSONs ]) => structDT(typFieldsOfJsons(Fs))
  rule defTypOfJson([ "variantDT", [ Cs:JSONs ], .JSONs ]) => variantDT(typCasesOfJsons(Cs))

  syntax TypFieldList ::= typFieldsOfJsons(JSONs)
                                              [function, symbol(typFieldsOfJsons)]
  rule typFieldsOfJsons(.JSONs) => .TypFieldList
  rule typFieldsOfJsons([ A:String, T:JSON, .JSONs ], Js:JSONs)
    => typField(A, typOfJson(T)), typFieldsOfJsons(Js)

  syntax TypCaseList ::= typCasesOfJsons(JSONs)
                                              [function, symbol(typCasesOfJsons)]
  rule typCasesOfJsons(.JSONs) => .TypCaseList
  rule typCasesOfJsons([ M:JSON, [ Ts:JSONs ], .JSONs ], Js:JSONs)
    => typCase(mixOpOfJson(M), typsOfJsons(Ts)), typCasesOfJsons(Js)

  // ==========================================================================
  // Iterators and variables
  // ==========================================================================

  syntax Iter ::= iterOfJson(JSON)            [function, symbol(iterOfJson)]
  rule iterOfJson("?") => quest()
  rule iterOfJson("*") => star()

  syntax IterList ::= itersOfJsons(JSONs)     [function, symbol(itersOfJsons)]
  rule itersOfJsons(.JSONs) => .IterList
  rule itersOfJsons(J:JSON, Js:JSONs) => iterOfJson(J), itersOfJsons(Js)

  syntax VariList ::= varisOfJsons(JSONs)     [function, symbol(varisOfJsons)]
  rule varisOfJsons(.JSONs) => .VariList
  rule varisOfJsons([ X:String, T:JSON, [ Is:JSONs ], .JSONs ], Js:JSONs)
    => vari(X, typOfJson(T), itersOfJsons(Is)), varisOfJsons(Js)

  // ==========================================================================
  // Operators and patterns
  // ==========================================================================

  syntax UnOp ::= unOpOfJson(JSON)            [function, symbol(unOpOfJson)]
  rule unOpOfJson("notOp") => notOp()
  rule unOpOfJson("plusOp") => plusOp()
  rule unOpOfJson("minusOp") => minusOp()

  syntax BinOp ::= binOpOfJson(JSON)          [function, symbol(binOpOfJson)]
  rule binOpOfJson("andOp") => andOp()
  rule binOpOfJson("orOp") => orOp()
  rule binOpOfJson("implOp") => implOp()
  rule binOpOfJson("equivOp") => equivOp()
  rule binOpOfJson("addOp") => addOp()
  rule binOpOfJson("subOp") => subOp()
  rule binOpOfJson("mulOp") => mulOp()
  rule binOpOfJson("divOp") => divOp()
  rule binOpOfJson("modOp") => modOp()
  rule binOpOfJson("powOp") => powOp()

  syntax CmpOp ::= cmpOpOfJson(JSON)          [function, symbol(cmpOpOfJson)]
  rule cmpOpOfJson("eqOp") => eqOp()
  rule cmpOpOfJson("neOp") => neOp()
  rule cmpOpOfJson("ltOp") => ltOp()
  rule cmpOpOfJson("gtOp") => gtOp()
  rule cmpOpOfJson("leOp") => leOp()
  rule cmpOpOfJson("geOp") => geOp()

  syntax Pat ::= patOfJson(JSON)              [function, symbol(patOfJson)]
  rule patOfJson([ "injPat", M:JSON, .JSONs ]) => injPat(mixOpOfJson(M))
  rule patOfJson([ "consPat", .JSONs ]) => consPat()
  rule patOfJson([ "fixedPat", N:Int, .JSONs ]) => fixedPat(N)
  rule patOfJson([ "nilPat", .JSONs ]) => nilPat()
  rule patOfJson([ "somePat", .JSONs ]) => somePat()
  rule patOfJson([ "nonePat", .JSONs ]) => nonePat()

  // ==========================================================================
  // Expressions
  // ==========================================================================

  syntax Exp ::= expOfJson(JSON)              [function, symbol(expOfJson)]
  rule expOfJson([ "natN", S:String, .JSONs ]) => natN(String2Int(S))
  rule expOfJson([ "intN", S:String, .JSONs ]) => intN(String2Int(S))
  rule expOfJson([ "boolE", B:Bool, .JSONs ]) => boolE(B)
  rule expOfJson([ "textE", S:String, .JSONs ]) => textE(S)
  rule expOfJson([ "varE", X:String, .JSONs ]) => varE(X)
  rule expOfJson([ "unE", O:JSON, E:JSON, .JSONs ]) => unE(unOpOfJson(O), expOfJson(E))
  rule expOfJson([ "binE", O:JSON, El:JSON, Er:JSON, .JSONs ])
    => binE(binOpOfJson(O), expOfJson(El), expOfJson(Er))
  rule expOfJson([ "cmpE", O:JSON, El:JSON, Er:JSON, .JSONs ])
    => cmpE(cmpOpOfJson(O), expOfJson(El), expOfJson(Er))
  rule expOfJson([ "upCastE", T:JSON, E:JSON, .JSONs ]) => upCastE(typOfJson(T), expOfJson(E))
  rule expOfJson([ "downCastE", T:JSON, E:JSON, .JSONs ])
    => downCastE(typOfJson(T), expOfJson(E))
  rule expOfJson([ "subE", E:JSON, T:JSON, .JSONs ]) => subE(expOfJson(E), typOfJson(T))
  rule expOfJson([ "matchE", E:JSON, P:JSON, .JSONs ]) => matchE(expOfJson(E), patOfJson(P))
  rule expOfJson([ "tupE", [ Es:JSONs ], .JSONs ]) => tupE(expsOfJsons(Es))
  rule expOfJson([ "injE", M:JSON, [ Es:JSONs ], .JSONs ])
    => injE(expCase(mixOpOfJson(M), expsOfJsons(Es)))
  rule expOfJson([ "strE", [ Fs:JSONs ], .JSONs ]) => strE(expFieldsOfJsons(Fs))
  rule expOfJson([ "optE", null, .JSONs ]) => optE(noExp())
  rule expOfJson([ "optE", E:JSON, .JSONs ]) => optE(someExp(expOfJson(E)))
    requires E =/=K null
  rule expOfJson([ "listE", [ Es:JSONs ], .JSONs ]) => listE(expsOfJsons(Es))
  rule expOfJson([ "consE", Eh:JSON, Et:JSON, .JSONs ]) => consE(expOfJson(Eh), expOfJson(Et))
  rule expOfJson([ "catE", El:JSON, Er:JSON, .JSONs ]) => catE(expOfJson(El), expOfJson(Er))
  rule expOfJson([ "memE", Ee:JSON, Es:JSON, .JSONs ]) => memE(expOfJson(Ee), expOfJson(Es))
  rule expOfJson([ "lenE", E:JSON, .JSONs ]) => lenE(expOfJson(E))
  rule expOfJson([ "dotE", E:JSON, A:String, .JSONs ]) => dotE(expOfJson(E), A)
  rule expOfJson([ "idxE", Eb:JSON, Ei:JSON, .JSONs ]) => idxE(expOfJson(Eb), expOfJson(Ei))
  rule expOfJson([ "sliceE", Eb:JSON, Ei:JSON, En:JSON, .JSONs ])
    => sliceE(expOfJson(Eb), expOfJson(Ei), expOfJson(En))
  rule expOfJson([ "updE", Eb:JSON, P:JSON, En:JSON, .JSONs ])
    => updE(expOfJson(Eb), pathOfJson(P), expOfJson(En))
  rule expOfJson([ "callE", X:String, [ Ts:JSONs ], [ As:JSONs ], .JSONs ])
    => callE(X, targsOfJsons(Ts), argsOfJsons(As))
  rule expOfJson([ "iterE", E:JSON, I:JSON, [ Xs:JSONs ], .JSONs ])
    => iterE(expOfJson(E), iterExp(iterOfJson(I), varisOfJsons(Xs)))

  syntax ExpList ::= expsOfJsons(JSONs)       [function, symbol(expsOfJsons)]
  rule expsOfJsons(.JSONs) => .ExpList
  rule expsOfJsons(J:JSON, Js:JSONs) => expOfJson(J), expsOfJsons(Js)

  syntax ExpFieldList ::= expFieldsOfJsons(JSONs)
                                              [function, symbol(expFieldsOfJsons)]
  rule expFieldsOfJsons(.JSONs) => .ExpFieldList
  rule expFieldsOfJsons([ A:String, E:JSON, .JSONs ], Js:JSONs)
    => expField(A, expOfJson(E)), expFieldsOfJsons(Js)

  syntax Path ::= pathOfJson(JSON)            [function, symbol(pathOfJson)]
  rule pathOfJson([ "rootPath", .JSONs ]) => rootPath()
  rule pathOfJson([ "idxPath", P:JSON, Ei:JSON, .JSONs ]) => idxPath(pathOfJson(P), expOfJson(Ei))
  rule pathOfJson([ "slicePath", P:JSON, Ei:JSON, En:JSON, .JSONs ])
    => slicePath(pathOfJson(P), expOfJson(Ei), expOfJson(En))
  rule pathOfJson([ "dotPath", P:JSON, A:String, .JSONs ]) => dotPath(pathOfJson(P), A)

  // ==========================================================================
  // Arguments and parameters
  // ==========================================================================

  syntax Arg ::= argOfJson(JSON)              [function, symbol(argOfJson)]
  rule argOfJson([ "expA", E:JSON, .JSONs ]) => expA(expOfJson(E))
  rule argOfJson([ "funA", X:String, .JSONs ]) => funA(X)

  syntax ArgList ::= argsOfJsons(JSONs)       [function, symbol(argsOfJsons)]
  rule argsOfJsons(.JSONs) => .ArgList
  rule argsOfJsons(J:JSON, Js:JSONs) => argOfJson(J), argsOfJsons(Js)

  syntax Param ::= paramOfJson(JSON)          [function, symbol(paramOfJson)]
  rule paramOfJson([ "expParam", T:JSON, .JSONs ]) => expParam(typOfJson(T))
  rule paramOfJson([ "funParam", X:String, [ Ps:JSONs ], [ Qs:JSONs ], T:JSON, .JSONs ])
    => funParam(X, tparamsOfJsons(Ps), paramsOfJsons(Qs), typOfJson(T))

  syntax ParamList ::= paramsOfJsons(JSONs)   [function, symbol(paramsOfJsons)]
  rule paramsOfJsons(.JSONs) => .ParamList
  rule paramsOfJsons(J:JSON, Js:JSONs) => paramOfJson(J), paramsOfJsons(Js)

  // ==========================================================================
  // Premises
  // ==========================================================================

  syntax Prem ::= premOfJson(JSON)            [function, symbol(premOfJson)]
  rule premOfJson([ "relPr", X:String, [ Ei:JSONs ], [ Eo:JSONs ], .JSONs ])
    => relPr(X, expsOfJsons(Ei), expsOfJsons(Eo))
  rule premOfJson([ "ifPr", E:JSON, .JSONs ]) => ifPr(expOfJson(E))
  rule premOfJson([ "ifHoldPr", X:String, [ Es:JSONs ], .JSONs ]) => ifHoldPr(X, expsOfJsons(Es))
  rule premOfJson([ "ifNotHoldPr", X:String, [ Es:JSONs ], .JSONs ])
    => ifNotHoldPr(X, expsOfJsons(Es))
  rule premOfJson([ "letPr", El:JSON, Er:JSON, .JSONs ]) => letPr(expOfJson(El), expOfJson(Er))
  rule premOfJson([ "iterPr", P:JSON, I:JSON, [ Xs:JSONs ], [ Ys:JSONs ], .JSONs ])
    => iterPr(premOfJson(P), iterPrem(iterOfJson(I), varisOfJsons(Xs), varisOfJsons(Ys)))
  rule premOfJson([ "debugPr", E:JSON, .JSONs ]) => debugPr(expOfJson(E))

  syntax PremList ::= premsOfJsons(JSONs)     [function, symbol(premsOfJsons)]
  rule premsOfJsons(.JSONs) => .PremList
  rule premsOfJsons(J:JSON, Js:JSONs) => premOfJson(J), premsOfJsons(Js)

  // ==========================================================================
  // Rules, clauses, and table rows
  // ==========================================================================

  syntax RulMatch ::= rulMatchOfJson(JSON)    [function, symbol(rulMatchOfJson)]
  rule rulMatchOfJson([ [ Es:JSONs ], [ Ps:JSONs ], .JSONs ])
    => rulMatch(expsOfJsons(Es), premsOfJsons(Ps))

  syntax RulPath ::= rulPathOfJson(JSON)      [function, symbol(rulPathOfJson)]
  rule rulPathOfJson([ X:String, [ Es:JSONs ], [ Ps:JSONs ], .JSONs ])
    => rulPath(X, expsOfJsons(Es), premsOfJsons(Ps))

  syntax RulPathList ::= rulPathsOfJsons(JSONs)
                                              [function, symbol(rulPathsOfJsons)]
  rule rulPathsOfJsons(.JSONs) => .RulPathList
  rule rulPathsOfJsons(J:JSON, Js:JSONs) => rulPathOfJson(J), rulPathsOfJsons(Js)

  syntax RulGroupList ::= rulGroupsOfJsons(JSONs)
                                              [function, symbol(rulGroupsOfJsons)]
  rule rulGroupsOfJsons(.JSONs) => .RulGroupList
  rule rulGroupsOfJsons([ X:String, M:JSON, [ Ps:JSONs ], .JSONs ], Js:JSONs)
    => rulGroup(X, rulMatchOfJson(M), rulPathsOfJsons(Ps)), rulGroupsOfJsons(Js)

  syntax ElsGroupOpt ::= elsGroupOfJson(JSON) [function, symbol(elsGroupOfJson)]
  rule elsGroupOfJson(null) => noElsGroup()
  rule elsGroupOfJson([ X:String, M:JSON, P:JSON, .JSONs ])
    => someElsGroup(elsGroup(X, rulMatchOfJson(M), rulPathOfJson(P)))

  syntax Clause ::= clauseOfJson(JSON)        [function, symbol(clauseOfJson)]
  rule clauseOfJson([ [ As:JSONs ], E:JSON, [ Ps:JSONs ], .JSONs ])
    => clause(argsOfJsons(As), expOfJson(E), premsOfJsons(Ps))

  syntax ClauseList ::= clausesOfJsons(JSONs) [function, symbol(clausesOfJsons)]
  rule clausesOfJsons(.JSONs) => .ClauseList
  rule clausesOfJsons(J:JSON, Js:JSONs) => clauseOfJson(J), clausesOfJsons(Js)

  syntax ElsClauseOpt ::= elsClauseOfJson(JSON)
                                              [function, symbol(elsClauseOfJson)]
  rule elsClauseOfJson(null) => noElsClause()
  rule elsClauseOfJson(J:JSON) => someElsClause(clauseOfJson(J)) requires J =/=K null

  syntax TblRowList ::= tblRowsOfJsons(JSONs) [function, symbol(tblRowsOfJsons)]
  rule tblRowsOfJsons(.JSONs) => .TblRowList
  rule tblRowsOfJsons([ [ As:JSONs ], E:JSON, [ Ps:JSONs ], .JSONs ], Js:JSONs)
    => tblRow(argsOfJsons(As), expOfJson(E), premsOfJsons(Ps)), tblRowsOfJsons(Js)

endmodule
