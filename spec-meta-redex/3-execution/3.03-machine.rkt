#lang racket/base
;; Mirrors spec-meta/4-execution/4.02-relation.watsup, as the terms of the
;; machine, with the result syntaxes of spec-meta's 4.03-eval-ops, 2.4 and 2.5,
;; and 4.04.x, whose metafunctions are over al.
;;
;; An AL phrase evaluates in place, and every other relation has a machine
;; form; the three extern relations are reduction rules on theirs, which call
;; the host procedures of 3.04.2-extern-host.rkt. Frames give the evaluation
;; positions. An evaluated expression is its value, and a constructor whose
;; parts are all values is a value, so a frame writes val for an evaluated
;; subterm and exp for one still to evaluate. watsup's ok<ctx> is (IN L OK):
;; premises update the innermost IN's layer in place. A failure (UNMATCH or
;; ERROR) passes through the frames of Fr-pass, and a rule looks at it in
;; those of Fr-catch.
;;
;; An intermediate form is named after the rule, or the group, that builds
;; it; seq and eval-exps, which several rules build, after what they do.

(require redex/reduction-semantics
         "3.02-context.rkt")
(provide al)

(define-extended-language al context
  ;; Configurations
  (conf ::= (G e))

  ;; Results. Redex has no parametric nonterminals, so each ok<X> is written
  ;; out, and a result of val is the val itself.
  (valres ::= val failure)
  (valsres ::= (OK (val ...)) failure)
  (res ::= unitRes valres valsres)

  ;; The results, and the substitution environment, of 4.03-eval-ops, 2.4,
  ;; 2.5, and 4.04.x
  (numBinopRes ::= (OK val) ERROR)
  (numCmpRes ::= (OK bool) ERROR)
  (theta ::= ((id typ) ...))
  (typRes ::= (OK typ) ERROR)
  (typsRes ::= (OK (typ ...)) ERROR)
  (equivRes ::= (OK bool) ERROR)
  (subtypRes ::= (OK bool) ERROR)
  (castRes ::= (OK val) ERROR)

  ;; A rule path with its group's match (3.12)
  (rul ::= (rulMatch rulPath))

  ;; Machine terms: the forms that rules start from, and the frames around a
  ;; machine term
  (e ::=
     res
     (IN L e)
     exp arg prem
     (assign-exp exp val) (assign-exps (exp ...) (val ...))
     (assign-arg arg val) (assign-args (arg ...) (val ...))
     (seq (e ...)) (eval-exps (exp ...))
     (eval-path-acc val path) (eval-path-upd val path val)
     (eval-clause clause (val ...))
     (eval-clauses ((IN L (eval-clause clause (val ...))) ...))
     (eval-clauses-on-ok val ((IN L (eval-clause clause (val ...))) ...))
     (eval-tblrow tblRow (val ...)) (eval-tblrows (tblRow ...) (val ...))
     (call-table-func tableFuncDefn (val ...))
     (call-defined-func definedFuncDefn (typ ...) (val ...))
     (call-func id (typ ...) (val ...))
     (call-extern-func id (typ ...) (val ...))
     (call-builtin-func id (typ ...) (val ...))
     (eval-rul rul (val ...))
     (eval-ruls ((IN L (eval-rul rul (val ...))) ...))
     (eval-ruls-on-ok (OK (val ...)) ((IN L (eval-rul rul (val ...))) ...))
     (call-defined-rel definedRelDefn (val ...))
     (call-rel id (val ...))
     (call-extern-rel id (val ...))
     ;; an iteration over nothing, which has no evaluation position
     (assign-exp/iter/list () (vari ...))
     (in-hole Fr e))

  ;; Finished subterms
  (done ::= res (IN L OK))

  ;; An element of a list expression: under an iteration, each element
  ;; evaluates in a sub-context of its own
  (elem ::= exp (IN L exp))

  ;; Frames, one level each: those through which a failure passes, and those
  ;; where a rule looks at it
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
           (MATCH hole pat)
           (TUP (val ... hole exp ...))
           (INJ (mixop (val ... hole exp ...)))
           (STR ((atom val) ... (atom hole) expField ...))
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
           (eval-prem/ifholdpr hole)
           (IFNOTHOLD id (val ... hole exp ...))
           (LET exp hole)
           (eval-prem/iterpr-opt/some hole (vari ...))
           (eval-prem/iterpr-list/list ((IN L OK) ... hole (IN L prem) ...) (vari ...))
           (DEBUG hole)
           ;; Eval_rul
           (eval-exps (val ... hole exp ...)))
  (Fr-catch ::=
            ;; Eval_prem/ifnotholdpr
            (eval-prem/ifnotholdpr hole)
            ;; the clauses of a call, in order: an unmatched one is passed
            ;; over, and the ones after a success must be unmatched too
            (eval-clauses (UNMATCH ... hole (IN L (eval-clause clause (val ...))) ...))
            (eval-clauses-on-ok val (UNMATCH ... hole (IN L (eval-clause clause (val ...))) ...))
            (call-defined-func/clauses hole (elsClause ...) (val ...))
            ;; Eval_tblrows/unmatch, which tries the next row
            (eval-tblrows/cons hole (tblRow ...) (val ...))
            ;; the rule paths of a call, as the clauses
            (eval-ruls (UNMATCH ... hole (IN L (eval-rul rul (val ...))) ...))
            (eval-ruls-on-ok (OK (val ...)) (UNMATCH ... hole (IN L (eval-rul rul (val ...))) ...))
            (call-defined-rel/ruls hole (elsGroup ...) (val ...)))
  (Fr ::= Fr-pass Fr-catch)

  ;; Frames within one local context, and across local contexts
  (F ::= hole (in-hole Fr F))
  (E ::= F (in-hole F (IN L E))))
