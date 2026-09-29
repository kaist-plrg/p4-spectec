#lang racket/base
;; spec-meta/common/1-syntax.watsup, plus the `var`s of common/0-stdlib.

(require "0-prelude.rkt")
(provide Common)

(define-language Common
  ;; Metavariables (common/0-stdlib)
  (bool b ::= boolean)
  (int i ::= integer)
  (nat n ::= natural)
  (text t ::= string)

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
  (json ::= any)
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
  (tparam ::= id))
