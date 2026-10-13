#lang racket/base
;; spec-meta/al/4-relation.watsup, as the terms of the machine.
;;
;; An AL phrase evaluates in place, and every other relation has a machine
;; form. Frames give their evaluation positions. watsup's res<ctx> is
;; (IN L OK) or FAIL: premises update the innermost IN's layer in place.
;;
;; An intermediate form is named after the rule, or the group, that builds it.

(require "../common/0.0-prelude.rkt"
         "3-context.rkt")
(provide al)

(define-extended-language al al-context
  ;; Configurations
  (conf ::= (G e))

  ;; Results
  (typsres ::= (OK (typ ...)) FAIL)
  (res ::= unitres valres valsres typsres)

  ;; Machine terms: the forms that rules start from, and the frames around a
  ;; machine term
  (e ::=
     res
     (IN L e)
     exp arg prem
     (assign-exp exp val) (assign-exps (exp ...) (val ...))
     (assign-arg L arg val) (assign-args L (arg ...) (val ...))
     (eval-targs (targ ...)) (eval-prems (prem ...))
     (eval-path val path) (eval-path-upd val path val)
     (eval-clause L clause (val ...)) (eval-clauses L (clause ...) (val ...))
     (eval-tblrow tblrow (val ...)) (eval-tblrows (tblrow ...) (val ...))
     (call-table-func tableFuncDef (val ...))
     (call-defined-func definedFuncDef (typ ...) (val ...))
     (call-func-dispatch funcdef (typ ...) (val ...))
     (call-func id (typ ...) (val ...))
     (call-extern-func id (typ ...) (val ...))
     (call-builtin-func id (typ ...) (val ...))
     (eval-rul rulmatch rulpath (val ...)) (eval-ruls rulmatch (rulpath ...) (val ...))
     (eval-rulgroup rulgroup (val ...)) (eval-rulgroups (rulgroup ...) (val ...))
     (call-defined-rel definedRelDef (val ...))
     (call-rel-dispatch reldef (val ...))
     (call-rel id (val ...))
     (call-extern-rel id (val ...))
     ;; iterations over nothing, which have no evaluation position
     (eval-exp/iter/opt ()) (eval-exp/iter/list ()) (assign-exp/iter/list () (vari ...))
     (in-hole Fr e))

  ;; Finished subterms
  (done ::= res (IN L OK))

  ;; Frames, one level each: those through which FAIL passes, and those
  ;; where a rule looks at FAIL
  (Fr-pass ::=
           ;; Assign_exp(s), Assign_arg(s)
           (assign-exp/cons hole exp (LIST (val ...)))
           (assign-exp/iter/opt-some hole (vari ...))
           (assign-exp/iter/list ((IN L OK) ... hole (IN L (assign-exp exp val)) ...) (vari ...))
           (assign-exps/cons hole (exp ...) (val ...))
           (assign-args/cons L hole (arg ...) (val ...))
           ;; Eval_exp
           (UN unop hole)
           (BIN binop hole exp)
           (BIN boolbinop (OK (BOOL b)) hole)
           (BIN numbinop (OK num) hole)
           (CMP cmpop hole exp)
           (CMP polycmpop (OK val) hole)
           (CMP numcmpop (OK num) hole)
           (UPCAST typ hole)
           (DOWNCAST typ hole)
           (SUB hole typ)
           (MATCH hole pattern)
           (TUP ((OK val) ... hole exp ...))
           (INJ (mixop ((OK val) ... hole exp ...)))
           (STR ((atom (OK val)) ... (atom hole) expfield ...))
           (OPT (hole))
           (LIST ((OK val) ... hole exp ...))
           (CONS hole exp)
           (CONS (OK val) hole)
           (CAT hole exp)
           (CAT (OK (TEXT t)) hole)
           (CAT (OK (LIST (val ...))) hole)
           (MEM hole exp)
           (MEM (OK val) hole)
           (LEN hole)
           (DOT hole atom)
           (IDX hole exp)
           (SLICE hole exp exp)
           (UPD hole path exp)
           (UPD (OK val) path hole)
           (eval-exp/call id hole (arg ...))
           (eval-exp/call id (OK (typ ...)) ((OK val) ... hole arg ...))
           (eval-exp/iter/opt (hole))
           (eval-exp/iter/list ((OK val) ... hole (IN L exp) ...))
           ;; Eval_path
           (eval-path/dot hole atom)
           (eval-path/idx hole exp)
           (eval-path/idx (OK (TEXT t)) hole)
           (eval-path/idx (OK (LIST (val ...))) hole)
           (eval-path/slice hole exp exp)
           (eval-path/slice (OK (TEXT t)) hole exp)
           (eval-path/slice (OK (LIST (val ...))) hole exp)
           (eval-path/slice (OK (TEXT t)) (OK (NAT n)) hole)
           (eval-path/slice (OK (LIST (val ...))) (OK (NAT n)) hole)
           ;; Eval_path_upd
           (eval-path-upd/idx hole exp val path val)
           (eval-path-upd/idx (OK (TEXT t)) hole val path (TEXT t))
           (eval-path-upd/idx (OK (LIST (val ...))) hole val path val)
           (eval-path-upd/slice hole exp exp val path val)
           (eval-path-upd/slice (OK (TEXT t)) hole exp val path (TEXT t))
           (eval-path-upd/slice (OK (LIST (val ...))) hole exp val path (LIST (val ...)))
           (eval-path-upd/slice (OK (TEXT t)) (OK (NAT n)) hole val path (TEXT t))
           (eval-path-upd/slice (OK (LIST (val ...))) (OK (NAT n)) hole val path (LIST (val ...)))
           (eval-path-upd/dot hole atom val path val)
           ;; Eval_arg
           (EXP hole)
           ;; Eval_prem, Eval_prems
           (REL id ((OK val) ... hole exp ...) (exp ...))
           (eval-prem/relpr hole (exp ...))
           (IF hole)
           (IFHOLD id ((OK val) ... hole exp ...))
           (eval-prem/ifholdpr/hold hole)
           (IFNOTHOLD id ((OK val) ... hole exp ...))
           (LET exp hole)
           (eval-prem/iterpr-opt/some hole (vari ...))
           (eval-prem/iterpr-list/list ((IN L OK) ... hole (IN L prem) ...) (vari ...))
           (DEBUG hole)
           (eval-prems/head hole (prem ...))
           ;; Eval_clause, Eval_tblrow
           (eval-clause/succ hole (eval-prems (prem ...)) exp)
           (eval-clause/succ OK hole exp)
           (eval-clause/succ OK OK hole)
           (eval-tblrow/succ hole (eval-prems (prem ...)) exp)
           (eval-tblrow/succ OK hole exp)
           (eval-tblrow/succ OK OK hole)
           ;; Eval_rul
           (eval-rul/succ hole (eval-prems (prem ...)) (exp ...))
           (eval-rul/succ OK hole (exp ...))
           (eval-rul/succ OK OK ((OK val) ... hole exp ...)))
  (Fr-catch ::=
            ;; Eval_prem/nothold
            (eval-prem/ifholdpr/nothold hole)
            ;; the cons-fail rules, which try the next alternative
            (eval-clauses/cons hole L (clause ...) (val ...))
            (eval-tblrows/cons hole (tblrow ...) (val ...))
            (eval-ruls/cons hole rulmatch (rulpath ...) (val ...))
            (eval-rulgroups/cons hole (rulgroup ...) (val ...)))
  (Fr ::= Fr-pass Fr-catch)

  ;; Frames within one local context, and across local contexts
  (F ::= hole (in-hole Fr F))
  (E ::= F (in-hole F (IN L E))))
