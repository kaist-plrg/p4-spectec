#lang racket/base
;; spec-meta/1-syntax.watsup.

(require json
         redex/reduction-semantics
         "0.0-stdlib.rkt")
(provide syntax)

(define-extended-language syntax stdlib
  ;; Identifiers
  (id ::= text)
  (atom ::= text)
  (mixop ::= ((atom ...) ...))

  ;; Types
  (numtyp ::= NAT INT)
  (optyp ::= BOOL numtyp TEXT)
  (typ ::=
       optyp
       (VAR id (targ ...))
       (TUP (typ ...))
       (ITER typ iter)
       FUNC)
  (deftyp ::=
          (ALIAS typ)
          (STRUCT (typfield ...))
          (VARIANT (typcase ...)))
  (typfield ::= (atom typ))
  (typcase ::= (mixop (typ ...)))

  ;; Iterators
  (iter ::= QUEST STAR)

  ;; Variables
  (vari ::= (id typ (iter ...)))

  ;; Numbers and operators
  (num ::= (NAT nat) (INT int))
  (boolunop ::= NOT)
  (boolbinop ::= AND OR IMPL EQUIV)
  (numunop ::= PLUS MINUS)
  (numbinop ::= ADD SUB MUL DIV MOD POW)
  (numcmpop ::= LT GT LE GE)
  (polycmpop ::= EQ NE)
  (unop ::= boolunop numunop)
  (binop ::= boolbinop numbinop)
  (cmpop ::= polycmpop numcmpop)

  ;; Values
  (json ::= (side-condition any_json (jsexpr? (term any_json))))
  (val ::=
       (BOOL bool)
       num
       (TEXT text)
       (STR (valfield ...))
       (INJ valcase)
       (TUP (val ...))
       (OPT ()) (OPT (val))
       (LIST (val ...))
       (FUNC id)
       (EXT json))
  (valfield ::= (atom val))
  (valcase ::= (mixop (val ...)))

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
       (MATCH exp pattern)
       (TUP (exp ...))
       (INJ expcase)
       (STR (expfield ...))
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
       (ITER exp iterexp))
  (expcase ::= (mixop (exp ...)))
  (expfield ::= (atom exp))
  (iterexp ::= (iter (vari ...)))

  ;; Patterns
  (listpattern ::= CONS (FIXED nat) NIL)
  (optpattern ::= SOME NONE)
  (pattern ::= (INJ mixop) listpattern optpattern)

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
  (iterprem ::= (iter (vari ...) (vari ...)))
  (prem ::=
        (REL id (exp ...) (exp ...))
        (IF exp)
        (IFHOLD id (exp ...))
        (IFNOTHOLD id (exp ...))
        (LET exp exp)
        (ITER prem iterprem)
        (DEBUG exp))

  ;; Definitions
  (rulmatch ::= ((exp ...) (prem ...)))
  (rulpath ::= (id (exp ...) (prem ...)))
  (rulgroup ::= (id rulmatch (rulpath ...)))
  (elsgroup ::= (id rulmatch rulpath))

  (clause ::= ((arg ...) exp (prem ...)))
  (elsclause ::= clause)

  (tblrow ::= ((arg ...) exp (prem ...)))

  (defn ::=
        (EXTTYP id)
        (TYP id (tparam ...) deftyp)
        (EXTREL id (typ ...) (typ ...))
        (REL id (typ ...) (typ ...) (rulgroup ...) ())
        (REL id (typ ...) (typ ...) (rulgroup ...) (elsgroup))
        (EXTFUNC id (tparam ...) (param ...) typ)
        (BUILTINFUNC id (tparam ...) (param ...) typ)
        (TABLEFUNC id (param ...) typ (tblrow ...))
        (FUNC id (tparam ...) (param ...) typ (clause ...) ())
        (FUNC id (tparam ...) (param ...) typ (clause ...) (elsclause)))

  ;; Scripts
  (script ::= (defn ...)))
