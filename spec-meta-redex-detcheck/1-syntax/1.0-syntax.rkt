#lang racket/base
;; Mirrors spec-meta/1-syntax/1.0-common.watsup and 1.2-al.watsup.

(require redex/reduction-semantics
         "../0-aux/0.0-stdlib.rkt")
(provide syntax)

(define-extended-language syntax stdlib
  ;; Identifiers
  (id ::= text)
  (atom ::= text)
  (mixop ::= ((atom ...) ...))

  ;; Numbers
  (num ::= (NAT nat) (INT int))

  ;; Types
  (numtyp ::= NAT INT)
  (optyp ::= BOOL numtyp TEXT)
  (typ ::=
       optyp
       (VAR id (targ ...))
       (TUP (typ ...))
       (ITER typ iter)
       (FUNC (tparam ...) (typ ...) typ))
  (deftyp ::=
          (ALIAS typ)
          (STRUCT (typField ...))
          (VARIANT (typCase ...)))
  (typField ::= (atom typ))
  (typCase ::= (mixop (typ ...)))

  ;; Iterators
  (iter ::= QUEST STAR)

  ;; Variables
  (vari ::= (id typ (iter ...)))

  ;; Operators
  (boolUnop ::= NOT)
  (numUnop ::= PLUS MINUS)
  (unop ::= boolUnop numUnop)
  (boolBinop ::= AND OR IMPL EQUIV)
  (numBinop ::= ADD SUB MUL DIV MOD POW)
  (binop ::= boolBinop numBinop)
  (numCmpop ::= LT GT LE GE)
  (polyCmpop ::= EQ NE)
  (cmpop ::= polyCmpop numCmpop)

  ;; Patterns
  (listPat ::= CONS (FIXED nat) NIL)
  (optPat ::= SOME NONE)
  (pat ::= (INJ mixop) listPat optPat)

  ;; Expressions
  (exp ::=
       (BOOL bool)
       num
       (TEXT text)
       (VAR id)
       (UN unop exp)
       (BIN binop exp exp)
       (CMP cmpop exp exp)
       (UPCAST typ exp)
       (DOWNCAST typ exp)
       (SUB exp typ)
       (MATCH exp pat)
       (TUP (exp ...))
       (INJ expCase)
       (STR (expField ...))
       (OPT ()) (OPT (exp))
       (LIST (exp ...))
       (CONS exp exp)
       (CAT exp exp)
       (MEM exp exp)
       (LEN exp)
       (DOT exp atom)
       (IDX exp exp)
       (SLICE exp exp exp)
       (UPD exp path exp)
       (CALL id (targ ...) (arg ...))
       (ITER exp iterExp))
  (expCase ::= (mixop (exp ...)))
  (expField ::= (atom exp))
  (iterExp ::= (iter (vari ...)))

  ;; Paths
  (path ::=
        ROOT
        (IDX path exp)
        (SLICE path exp exp)
        (DOT path atom))

  ;; Arguments
  (targ ::= typ)
  (arg ::= (EXP exp) (FUN id))

  ;; Type parameters
  (tparam ::= id)

  ;; Parameters
  (param ::=
         (EXP typ)
         (FUN id (tparam ...) (param ...) typ))

  ;; Premises
  (prem ::=
        (REL id (exp ...) (exp ...))
        (IF exp)
        (IFHOLD id (exp ...))
        (IFNOTHOLD id (exp ...))
        (LET exp exp)
        (ITER prem iterPrem)
        (DEBUG exp))
  (iterPrem ::= (iter (vari ...) (vari ...)))

  ;; Type definitions
  (externTypDefn ::= (EXTTYP id))
  (definedTypDefn ::= (TYP id (tparam ...) deftyp))
  (typDefn ::= externTypDefn definedTypDefn)

  ;; Relation definitions
  (externRelDefn ::= (EXTREL id (typ ...) (typ ...)))
  (rulMatch ::= ((exp ...) (prem ...)))
  (rulPath ::= (id (exp ...) (prem ...)))
  (rulGroup ::= (id rulMatch (rulPath ...)))
  (elsGroup ::= (id rulMatch rulPath))
  (definedRelDefn ::=
                  (REL id (typ ...) (typ ...) (rulGroup ...) ())
                  (REL id (typ ...) (typ ...) (rulGroup ...) (elsGroup)))
  (relDefn ::= externRelDefn definedRelDefn)

  ;; Meta-functions
  (externFuncDefn ::= (EXTFUNC id (tparam ...) (param ...) typ))
  (builtinFuncDefn ::= (BUILTINFUNC id (tparam ...) (param ...) typ))
  (tblRow ::= ((arg ...) exp (prem ...)))
  (tableFuncDefn ::= (TABLEFUNC id (param ...) typ (tblRow ...)))
  (clause ::= ((arg ...) exp (prem ...)))
  (elsClause ::= clause)
  (definedFuncDefn ::=
                   (FUNC id (tparam ...) (param ...) typ (clause ...) ())
                   (FUNC id (tparam ...) (param ...) typ (clause ...) (elsClause)))
  (funcDefn ::= externFuncDefn builtinFuncDefn tableFuncDefn definedFuncDefn)

  ;; Definitions
  (defn ::= typDefn relDefn funcDefn)

  ;; Spec
  (spec ::= (defn ...)))
