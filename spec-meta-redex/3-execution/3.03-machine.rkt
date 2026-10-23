#lang racket/base
;; spec-meta/3-execution/3.03-relation.watsup, as the terms of the machine.
;;
;; The three extern relations are reduction rules on machine forms. Their
;; rules call the host procedures at the end of this module, which go to the
;; Rust host over the extern wire. They are impure, so only a reduction rule
;; may call them, never a metafunction.
;;
;; An AL phrase evaluates in place, and every other relation has a machine
;; form. Frames give their evaluation positions. An evaluated expression is
;; its value, and a constructor whose parts are all values is a value, so a
;; frame writes val for an evaluated subterm and exp for one still to
;; evaluate. watsup's res<ctx> is (IN L OK) or FAIL: premises update the
;; innermost IN's layer in place.
;;
;; An intermediate form is named after the rule, or the group, that builds
;; it; seq and eval-exps, which several rules build, after what they do.

(require json
         redex/reduction-semantics
         "3.04.1-extern-json.rkt"
         "3.04.2-extern-ffi.rkt"
         "3.02-context.rkt")
(provide al
         host-call-extern-func
         host-call-builtin-func
         host-call-extern-rel)

(define-extended-language al context
  ;; Configurations
  (conf ::= (G e))

  ;; Results to represent backtracking in evaluation. Redex has no parametric
  ;; nonterminals, so each res<X> is written out.
  (unitres ::= OK FAIL)
  (valres ::= val FAIL)
  (valsres ::= (OK (val ...)) FAIL)
  (res ::= unitres valres valsres)

  ;; Machine terms: the forms that rules start from, and the frames around a
  ;; machine term
  (e ::=
     res
     (IN L e)
     exp arg prem
     (assign-exp exp val) (assign-exps (exp ...) (val ...))
     (assign-arg L arg val) (assign-args L (arg ...) (val ...))
     (seq (e ...)) (eval-exps (exp ...))
     (eval-path val path) (eval-path-upd val path val)
     (eval-clause L clause (val ...))
     (eval-tblrow tblrow (val ...)) (eval-tblrows (tblrow ...) (val ...))
     (call-table-func tableFuncDef (val ...))
     (call-defined-func definedFuncDef (typ ...) (val ...))
     (call-func-dispatch funcdef (typ ...) (val ...))
     (call-func id (typ ...) (val ...))
     (call-extern-func id (typ ...) (val ...))
     (call-builtin-func id (typ ...) (val ...))
     (eval-rul rulmatch rulpath (val ...))
     (call-defined-rel definedRelDef (val ...))
     (call-rel-dispatch reldef (val ...))
     (call-rel id (val ...))
     (call-extern-rel id (val ...))
     ;; iterations over nothing, which have no evaluation position
     (assign-exp/iter/list () (vari ...))
     ;; calls with no clause or rule path, which have no evaluation position
     (call-defined-func/clauses () L (elsclause ...) (val ...))
     (call-defined-rel/ruls () (elsgroup ...) (val ...))
     (in-hole Fr e))

  ;; Finished subterms
  (done ::= res (IN L OK))

  ;; An element of a list expression: under an iteration, each element
  ;; evaluates in a sub-context of its own
  (elem ::= exp (IN L exp))

  ;; Frames, one level each: those through which FAIL passes, and those
  ;; where a rule looks at FAIL
  (Fr-pass ::=
           ;; Assign_exp(s), Assign_arg(s)
           (assign-exp/iter/opt-some hole (vari ...))
           (assign-exp/iter/list ((IN L OK) ... hole (IN L (assign-exp exp val)) ...) (vari ...))
           ;; a statement sequence: assignments, premises, and an expression
           (seq (OK ... hole e ...))
           ;; Eval_exp
           (UN unop hole)
           (BIN binop hole exp)
           (BIN binop val hole)
           (CMP cmpop hole exp)
           (CMP cmpop val hole)
           (UPCAST typ hole)
           (DOWNCAST typ hole)
           (SUB hole typ)
           (MATCH hole pattern)
           (TUP (val ... hole exp ...))
           (INJ (mixop (val ... hole exp ...)))
           (STR ((atom val) ... (atom hole) expfield ...))
           (OPT (hole))
           (LIST (val ... hole elem ...))
           (CONS hole exp)
           (CONS val hole)
           (CAT hole exp)
           (CAT val hole)
           (MEM hole exp)
           (MEM val hole)
           (LEN hole)
           (DOT hole atom)
           (IDX hole exp)
           (IDX val hole)
           (SLICE hole exp exp)
           (SLICE val hole exp)
           (SLICE val val hole)
           (UPD hole path exp)
           (UPD val path hole)
           (CALL id (targ ...) (val ... hole arg ...))
           ;; Eval_path_upd
           (eval-path-upd val path hole)
           (eval-path-upd/idx hole exp val)
           (eval-path-upd/idx val hole val)
           (eval-path-upd/slice hole exp exp val)
           (eval-path-upd/slice val hole exp val)
           (eval-path-upd/slice val val hole val)
           (eval-path-upd/dot hole atom val)
           ;; Eval_arg
           (EXP hole)
           ;; Eval_prem
           (REL id (val ... hole exp ...) (exp ...))
           (eval-prem/relpr hole (exp ...))
           (IF hole)
           (IFHOLD id (val ... hole exp ...))
           (eval-prem/ifholdpr/hold hole)
           (IFNOTHOLD id (val ... hole exp ...))
           (LET exp hole)
           (eval-prem/iterpr-opt/some hole (vari ...))
           (eval-prem/iterpr-list/list ((IN L OK) ... hole (IN L prem) ...) (vari ...))
           (DEBUG hole)
           ;; Eval_rul
           (eval-exps (val ... hole exp ...)))
  (Fr-catch ::=
            ;; Eval_prem/nothold
            (eval-prem/ifholdpr/nothold hole)
            ;; the alternatives of a call: every clause or rule path is
            ;; evaluated, and a FAIL is one result among them
            (call-defined-func/clauses (valres ... hole (IN L_callee (eval-clause L_caller clause (val ...))) ...)
                                       L (elsclause ...) (val ...))
            (call-defined-rel/ruls (valsres ... hole (IN L (eval-rul rulmatch rulpath (val ...))) ...)
                                   (elsgroup ...) (val ...))
            ;; Eval_tblrows/cons-fail, which tries the next row
            (eval-tblrows/cons hole (tblrow ...) (val ...)))
  (Fr ::= Fr-pass Fr-catch)

  ;; Frames within one local context, and across local contexts
  (F ::= hole (in-hole Fr F))
  (E ::= F (in-hole F (IN L E))))

;;
;; The host procedures of the extern relations
;;

;; Sends a request to the host, and gives its response as a jsexpr.
(define (host-request request)
  (string->jsexpr (host-eval (jsexpr->string request))))

;;; Extern meta-function invocation

;; |- id `< typ* `> `( val* `) : res<val>, as a valres
(define (host-call-extern-func id typs vals)
  (response->valres (host-request (extern-func-request id typs vals))))

;;; Builtin meta-function invocation

;; |- id '@' `< typ* `> `( val* `) : res<val>, as a valres
(define (host-call-builtin-func id typs vals)
  (response->valres (host-request (builtin-request id typs vals))))

;;; Extern relations

;; |- id val* : res<val*>, as a valsres
(define (host-call-extern-rel id vals)
  (response->valsres (host-request (extern-rel-request id vals))))
