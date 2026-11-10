#lang racket/base
;; Mirrors spec-meta/3-execution/3.02-relation.watsup.
;;
;; The machine runs on configurations (e G L K).

(require redex/reduction-semantics
         "3.02-context.rkt")
(provide al)

(define-extended-language al context
  ;; Results
  (valres ::= val failure)
  (valsres ::= (OK (val ...)) failure)
  (res ::= unitRes valres valsres)

  (numBinopRes ::= (OK val) ERROR)
  (numCmpRes ::= (OK bool) ERROR)
  (theta ::= ((id typ) ...))
  (typRes ::= (OK typ) ERROR)
  (typsRes ::= (OK (typ ...)) ERROR)
  (equivRes ::= (OK bool) ERROR)
  (subtypRes ::= (OK bool) ERROR)
  (castRes ::= (OK val) ERROR)

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
     ;; Determinism check (3.12-eval-call-func.rkt):
     ;; (eval-clauses-on-ok val ((IN L (eval-clause clause (val ...))) ...))
     (eval-tblrow tblRow (val ...)) (eval-tblrows (tblRow ...) (val ...))
     (call-table-func tableFuncDefn (val ...))
     (call-defined-func definedFuncDefn (typ ...) (val ...))
     (call-func id (typ ...) (val ...))
     (call-extern-func id (typ ...) (val ...))
     (call-builtin-func id (typ ...) (val ...))
     (eval-rul rul (val ...))
     (eval-ruls ((IN L (eval-rul rul (val ...))) ...))
     ;; Determinism check (3.13-eval-call-rel.rkt):
     ;; (eval-ruls-on-ok (OK (val ...)) ((IN L (eval-rul rul (val ...))) ...))
     (call-defined-rel definedRelDefn (val ...))
     (call-rel id (val ...))
     (call-extern-rel id (val ...))
     ;; an iteration over nothing
     (assign-exp/iter/list () (vari ...))
     (in-hole Fr e))

  ;; Finished subterms
  (done ::= res (IN L OK))

  ;; An element of a list expression
  (elem ::= exp (IN L exp))

  ;; Frames: failures are passed in `Fr-pass`, caught by `Fr-catch`
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
            ;; Eval_clauses/unmatch
            (eval-clauses (UNMATCH ... hole (IN L (eval-clause clause (val ...))) ...))
            ;; Determinism check: the clauses after a success
            ;; (eval-clauses-on-ok val (UNMATCH ... hole (IN L (eval-clause clause (val ...))) ...))
            (call-defined-func/clauses hole (elsClause ...) (val ...))
            ;; Eval_tblrows/unmatch
            (eval-tblrows/cons hole (tblRow ...) (val ...))
            ;; Eval_ruls/unmatch
            (eval-ruls (UNMATCH ... hole (IN L (eval-rul rul (val ...))) ...))
            ;; Determinism check: the rule paths after a success
            ;; (eval-ruls-on-ok (OK (val ...)) (UNMATCH ... hole (IN L (eval-rul rul (val ...))) ...))
            (call-defined-rel/ruls hole (elsGroup ...) (val ...)))
  (Fr ::= Fr-pass Fr-catch)

  ;; (K ::= () (frame K))
  ;; (frame ::= Fr (RET L))
  (K ::= any))
