// Syntax of AL, the algorithmic layer of the P4-SpecTec meta-language.
//
// Mirrors spec-meta/al/1-syntax.watsup; the sorts it builds on (types,
// expressions, values, arguments, ...) come from spec-meta-k/common/1-syntax.k,
// just as spec-meta/al/0-common symlinks spec-meta/common.
//
// As in the common module this is an *abstract* syntax: productions are
// constructor applications only.  Watsup's quoted delimiters (':', '->', '=',
// '-') are pure notation and carry no information, so they are dropped; the
// remaining fields become constructor arguments in the same order.

requires "../common/1-syntax.k"

module AL-SYNTAX
  imports COMMON-SYNTAX

  // ==========================================================================
  // Parameters
  //
  //   syntax param =
  //     | EXP typ
  //     | FUN id ':' tparam* param* '->' typ
  // ==========================================================================

  syntax Param ::= expParam(Typ)            [symbol(expParam)]
                 | funParam(Id, TParamList, ParamList, Typ)
                                            [symbol(funParam)]

  syntax ParamList ::= List{Param, ","}     [symbol(paramList), terminator-symbol(.paramList)]

  // ==========================================================================
  // Premises
  //
  //   syntax iterprem = iter vari* vari*
  //   syntax prem =
  //     | REL id ':' exp* '->' exp*
  //     | IF exp
  //     | IFHOLD id ':' exp*
  //     | IFNOTHOLD id ':' exp*
  //     | LET exp '=' exp
  //     | ITER prem iterprem
  //     | DEBUG exp
  // ==========================================================================

  syntax IterPrem ::= iterPrem(Iter, VariList, VariList)
                                            [symbol(iterPrem)]

  syntax Prem ::= relPr(Id, ExpList, ExpList)
                                            [symbol(relPr)]
                | ifPr(Exp)                 [symbol(ifPr)]
                | ifHoldPr(Id, ExpList)     [symbol(ifHoldPr)]
                | ifNotHoldPr(Id, ExpList)  [symbol(ifNotHoldPr)]
                | letPr(Exp, Exp)           [symbol(letPr)]
                | iterPr(Prem, IterPrem)    [symbol(iterPr)]
                | debugPr(Exp)              [symbol(debugPr)]

  syntax PremList ::= List{Prem, ","}       [symbol(premList), terminator-symbol(.premList)]

  // ==========================================================================
  // Rules
  //
  //   syntax rulmatch = exp* '-' prem*
  //   syntax rulpath = id '=' exp* '-' prem*
  //   syntax rulgroup = id ':' rulmatch '=' rulpath*
  //   syntax elsgroup = id ':' rulmatch '=' rulpath
  // ==========================================================================

  syntax RulMatch ::= rulMatch(ExpList, PremList)
                                            [symbol(rulMatch)]
  syntax RulPath ::= rulPath(Id, ExpList, PremList)
                                            [symbol(rulPath)]
  syntax RulGroup ::= rulGroup(Id, RulMatch, RulPathList)
                                            [symbol(rulGroup)]
  syntax ElsGroup ::= elsGroup(Id, RulMatch, RulPath)
                                            [symbol(elsGroup)]

  syntax RulPathList ::= List{RulPath, ","} [symbol(rulPathList), terminator-symbol(.rulPathList)]
  syntax RulGroupList ::= List{RulGroup, ","}
                                            [symbol(rulGroupList), terminator-symbol(.rulGroupList)]

  // `elsgroup?`
  syntax ElsGroupOpt ::= noElsGroup()       [symbol(noElsGroup)]
                       | someElsGroup(ElsGroup)
                                            [symbol(someElsGroup)]

  // ==========================================================================
  // Clauses and table rows
  //
  //   syntax clause = arg* '=' exp '-' prem*
  //   syntax elsclause = clause
  //   syntax tblrow = arg* '=' exp '-' prem*
  // ==========================================================================

  syntax Clause ::= clause(ArgList, Exp, PremList)
                                            [symbol(clause)]
  syntax TblRow ::= tblRow(ArgList, Exp, PremList)
                                            [symbol(tblRow)]

  syntax ClauseList ::= List{Clause, ","}   [symbol(clauseList), terminator-symbol(.clauseList)]
  syntax TblRowList ::= List{TblRow, ","}   [symbol(tblRowList), terminator-symbol(.tblRowList)]

  // `elsclause?`; `elsclause` is an alias of `clause`, so it is inlined.
  syntax ElsClauseOpt ::= noElsClause()     [symbol(noElsClause)]
                        | someElsClause(Clause)
                                            [symbol(someElsClause)]

  // ==========================================================================
  // Definitions
  //
  //   syntax defn =
  //     | EXTTYP id
  //     | TYP id tparam* '=' deftyp
  //     | EXTREL id ':' typ* '->' typ*
  //     | REL id ':' typ* '->' typ* '=' rulgroup* elsgroup?
  //     | EXTFUNC id tparam* param* ':' typ
  //     | BUILTINFUNC id tparam* param* ':' typ
  //     | TABLEFUNC id param* ':' typ '=' tblrow*
  //     | FUNC id tparam* param* ':' typ '=' clause* elsclause?
  // ==========================================================================

  syntax Defn ::= extTypD(Id)               [symbol(extTypD)]
                | typD(Id, TParamList, DefTyp)
                                            [symbol(typD)]
                | extRelD(Id, TypList, TypList)
                                            [symbol(extRelD)]
                | relD(Id, TypList, TypList, RulGroupList, ElsGroupOpt)
                                            [symbol(relD)]
                | extFuncD(Id, TParamList, ParamList, Typ)
                                            [symbol(extFuncD)]
                | builtinFuncD(Id, TParamList, ParamList, Typ)
                                            [symbol(builtinFuncD)]
                | tableFuncD(Id, ParamList, Typ, TblRowList)
                                            [symbol(tableFuncD)]
                | funcD(Id, TParamList, ParamList, Typ, ClauseList, ElsClauseOpt)
                                            [symbol(funcD)]

  // ==========================================================================
  // Scripts
  //
  //   syntax script = defn*
  //
  // `script` is an alias of `defn*`, so `Script` *is* the list sort: a script
  // is matched directly as `.Script` or `D:Defn, Ds:Script`.
  // ==========================================================================

  syntax Script ::= List{Defn, ","}         [symbol(script), terminator-symbol(.script)]

  // ==========================================================================
  // Target programs
  //
  // The input a *language* spec is run on, as opposed to a self-contained
  // meta-language target that defines its own `$main()`.  Running the P4 spec
  // binds `someP4(val)` here and the entry becomes `Program_ok` (al/6-entry.k).
  //
  // This lives in the syntax module rather than next to the configuration cell
  // it fills, because it is *parsed*: it is what `krun -pP4` hands back, so it
  // has to be reachable from the definition's `--syntax-module`.
  // ==========================================================================

  syntax P4Opt ::= "noP4" "(" ")"           [symbol(noP4)]
                 | someP4(Val)              [symbol(someP4)]

endmodule
