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

  ;; Machine terms
  (e ::=
     res
     (IN L e)
     ;; Assign_exp(s), Assign_arg(s)
     (assign-exp exp val)
     (assign-exp/cons e exp (LIST (val ...)))
     (assign-exp/iter/opt-some e (vari ...))
     (assign-exp/iter/list ((IN L OK) ...) (vari ...))
     (assign-exp/iter/list ((IN L OK) ... e (IN L (assign-exp exp val)) ...) (vari ...))
     (assign-exps (exp ...) (val ...))
     (assign-exps/cons e (exp ...) (val ...))
     (assign-arg L arg val)
     (assign-args L (arg ...) (val ...))
     (assign-args/cons L e (arg ...) (val ...))
     ;; Eval_exp
     exp
     (UN unop e)
     (BIN binop e exp)
     (BIN boolbinop (OK (BOOL b)) e)
     (BIN numbinop (OK num) e)
     (CMP cmpop e exp)
     (CMP polycmpop (OK val) e)
     (CMP numcmpop (OK num) e)
     (UPCAST typ e)
     (DOWNCAST typ e)
     (SUB e typ)
     (MATCH e pattern)
     (TUP ((OK val) ... e exp ...))
     (INJ (mixop ((OK val) ... e exp ...)))
     (STR ((atom (OK val)) ... (atom e) expfield ...))
     (OPT (e))
     (LIST ((OK val) ... e exp ...))
     (CONS e exp)
     (CONS (OK val) e)
     (CAT e exp)
     (CAT (OK (TEXT t)) e)
     (CAT (OK (LIST (val ...))) e)
     (MEM e exp)
     (MEM (OK val) e)
     (LEN e)
     (DOT e atom)
     (IDX e exp)
     (SLICE e exp exp)
     (UPD e path exp)
     (UPD (OK val) path e)
     (eval-exp/iter/opt ())
     (eval-exp/iter/opt (e))
     (eval-exp/iter/list ((OK val) ...))
     (eval-exp/iter/list ((OK val) ... e (IN L exp) ...))
     ;; Eval_path: the base path, then the indices
     (eval-path val path)
     (eval-path/dot e atom)
     (eval-path/idx e exp)
     (eval-path/idx (OK (TEXT t)) e)
     (eval-path/idx (OK (LIST (val ...))) e)
     (eval-path/slice e exp exp)
     (eval-path/slice (OK (TEXT t)) e exp)
     (eval-path/slice (OK (LIST (val ...))) e exp)
     (eval-path/slice (OK (TEXT t)) (OK (NAT n)) e)
     (eval-path/slice (OK (LIST (val ...))) (OK (NAT n)) e)
     ;; Eval_path_upd: as Eval_path, then the base, the inner path, and the new
     ;; value, for the update of the inner path
     (eval-path-upd val path val)
     (eval-path-upd/idx e exp val path val)
     (eval-path-upd/idx (OK (TEXT t)) e val path (TEXT t))
     (eval-path-upd/idx (OK (LIST (val ...))) e val path val)
     (eval-path-upd/slice e exp exp val path val)
     (eval-path-upd/slice (OK (TEXT t)) e exp val path (TEXT t))
     (eval-path-upd/slice (OK (LIST (val ...))) e exp val path (LIST (val ...)))
     (eval-path-upd/slice (OK (TEXT t)) (OK (NAT n)) e val path (TEXT t))
     (eval-path-upd/slice (OK (LIST (val ...))) (OK (NAT n)) e val path (LIST (val ...)))
     (eval-path-upd/dot e atom val path val)
     ;; Eval_arg, Eval_targs
     arg
     (EXP e)
     (eval-targs (targ ...)))

  ;; Finished subterms
  (done ::= res (IN L OK))

  ;; Frames, one level each, through which FAIL passes. Redex has no empty
  ;; nonterminals, so Fr-catch comes with the first frame that catches FAIL.
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
           (EXP hole))
  (Fr ::= Fr-pass)

  ;; Frames within one local context, and across local contexts
  (F ::= hole (in-hole Fr F))
  (E ::= F (in-hole F (IN L E))))
