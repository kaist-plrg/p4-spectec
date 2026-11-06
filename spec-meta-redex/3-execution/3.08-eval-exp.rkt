#lang racket/base
;; Mirrors spec-meta/4-execution/4.06-eval-exp.watsup.
;;
;; An exp evaluates in place: each evaluated subterm is replaced by its value,
;; and a constructor whose parts are all values is a value already. The rules
;; of Eval_path_acc on an evaluated DOT, IDX, or SLICE are in 3.09-eval-path.rkt.
;; Eval_exp/error becomes complement rules next to the rules they cover:
;; "<group>/error" where the evaluated subterms fit no rule, and
;; "<group>/<rule>/error" where a rule's own premise fails.

(require redex/reduction-semantics
         "../0-aux/0.0-stdlib.rkt"
         "../2-runtime/2.1-variable.rkt"
         "3.02-context.rkt"
         "3.03-machine.rkt"
         "3.05-eval-ops.rkt"
         "3.06.3-typ-match.rkt"
         "3.06.4-typ-cast.rkt")
(provide ->term/eval-exp
         ->term+ctx/eval-exp)

;; The rules on a term
(define ->term/eval-exp
  (reduction-relation
   al

   ;;; Unary, binary, and comparison evaluation rules

   ;; rulegroup Eval_exp/unary
   (--> (UN NOT (BOOL b)) (BOOL b_res)
        (where b_res ,(not (term b)))
        "eval-exp/unary/boolean-succ")
   (--> (UN numUnop num) (num-unop numUnop num)
        "eval-exp/unary/number-succ")
   (--> (UN unop val) ERROR
        ;; otherwise
        (side-condition (not (redex-match? al (NOT (BOOL b)) (term (unop val)))))
        (side-condition (not (redex-match? al (numUnop num) (term (unop val)))))
        "eval-exp/unary/error")

   ;; rulegroup Eval_exp/binary
   (--> (BIN boolBinop (BOOL b_l) (BOOL b_r)) (BOOL (bool-binop boolBinop b_l b_r))
        "eval-exp/binary/boolean-succ")
   (--> (BIN numBinop num_l num_r) val_res
        (where (OK val_res) (num-binop numBinop num_l num_r))
        "eval-exp/binary/number-succ")
   (--> (BIN numBinop num_l num_r) ERROR
        (where ERROR (num-binop numBinop num_l num_r))
        "eval-exp/binary/number-error")
   (--> (BIN binop val_l val_r) ERROR
        ;; otherwise
        (side-condition
         (not (redex-match? al (boolBinop (BOOL b_l) (BOOL b_r)) (term (binop val_l val_r)))))
        (side-condition
         (not (redex-match? al (numBinop num_l num_r) (term (binop val_l val_r)))))
        "eval-exp/binary/error")

   ;; rulegroup Eval_exp/compare
   (--> (CMP polyCmpop val_l val_r) (BOOL (poly-cmpop polyCmpop val_l val_r))
        "eval-exp/compare/poly-succ")
   (--> (CMP numCmpop num_l num_r) (BOOL b_res)
        (where (OK b_res) (num-cmpop numCmpop num_l num_r))
        "eval-exp/compare/number-succ")
   (--> (CMP numCmpop num_l num_r) ERROR
        (where ERROR (num-cmpop numCmpop num_l num_r))
        "eval-exp/compare/number-error")
   (--> (CMP numCmpop val_l val_r) ERROR
        ;; otherwise
        (side-condition (not (redex-match? al (num_l num_r) (term (val_l val_r)))))
        "eval-exp/compare/error")

   ;;; Match evaluation rules

   ;; rulegroup Eval_exp/match
   (--> (MATCH val pat) (BOOL (matches val pat))
        "eval-exp/match/succ")

   ;;; Cons-list evaluation rules

   ;; rulegroup Eval_exp/cons
   (--> (CONS val_h (LIST (val_t ...))) (LIST (val_h val_t ...))
        "eval-exp/cons/succ")
   (--> (CONS val_h val) ERROR
        ;; otherwise: the tail is not a list
        (side-condition (not (redex-match? al (LIST (val ...)) (term val))))
        "eval-exp/cons/error")

   ;;; Concatenation evaluation rules

   ;; rulegroup Eval_exp/concat
   (--> (CAT (TEXT t_l) (TEXT t_r)) (TEXT t_res)
        (where t_res ,(string-append (term t_l) (term t_r)))
        "eval-exp/concat/text-succ")
   (--> (CAT (LIST (val_l ...)) (LIST (val_r ...))) (LIST (val_l ... val_r ...))
        "eval-exp/concat/list-succ")
   (--> (CAT val_l val_r) ERROR
        ;; otherwise
        (side-condition (not (redex-match? al ((TEXT t_l) (TEXT t_r)) (term (val_l val_r)))))
        (side-condition
         (not (redex-match? al ((LIST (val_l ...)) (LIST (val_r ...))) (term (val_l val_r)))))
        "eval-exp/concat/error")

   ;;; Membership evaluation rules

   ;; rulegroup Eval_exp/mem
   (--> (MEM val_e (LIST (val ...))) (BOOL b)
        (where b ,(and (member (term val_e) (term (val ...))) #t))
        "eval-exp/mem/succ")
   (--> (MEM val_e val) ERROR
        ;; otherwise: not a list
        (side-condition (not (redex-match? al (LIST (val ...)) (term val))))
        "eval-exp/mem/error")

   ;;; Length evaluation rules

   ;; rulegroup Eval_exp/len
   (--> (LEN (TEXT t)) (NAT n)
        (where n ,(text-len (term t)))
        "eval-exp/len/text")
   (--> (LEN (LIST (val ...))) (NAT n)
        (where n ,(length (term (val ...))))
        "eval-exp/len/list")
   (--> (LEN val) ERROR
        ;; otherwise: neither a text nor a list
        (side-condition (not (redex-match? al (TEXT t) (term val))))
        (side-condition (not (redex-match? al (LIST (val ...)) (term val))))
        "eval-exp/len/error")

   ;;; Update evaluation rules

   ;; rulegroup Eval_exp/upd
   (--> (UPD val_b path val_f) (eval-path-upd val_b path val_f)
        "eval-exp/upd/succ")

   ;;; Expression sequences

   ;; Eval_exps on a sequence of expressions, for Eval_rul's outputs. Not a
   ;; watsup relation of its own here: the expressions evaluate in place.
   (--> (eval-exps (val ...)) (OK (val ...))
        "eval-exps")))

;; The rules on a term and its context (r G L)
(define ->term+ctx/eval-exp
  (reduction-relation
   al

   ;;; Variable evaluation rules

   ;; rule Eval_exp/variable
   (--> ((VAR id) G L) (val G L)
        (where (val) (find-varr L (id ())))
        "eval-exp/variable")
   (--> ((VAR id) G L) (ERROR G L)
        ;; otherwise: the variable is unbound
        (where () (find-varr L (id ())))
        "eval-exp/variable/error")

   ;;; Upcasting, downcasting, and subtyping evaluation rules

   ;; rulegroup Eval_exp/upcast
   (--> ((UPCAST typ val) G L) (val_cast G L)
        (where (OK val_cast) (upcast G L typ val))
        "eval-exp/upcast/succ")
   (--> ((UPCAST typ val) G L) (ERROR G L)
        (where ERROR (upcast G L typ val))
        "eval-exp/upcast/error")

   ;; rulegroup Eval_exp/downcast
   (--> ((DOWNCAST typ val) G L) (val_cast G L)
        (where (OK val_cast) (downcast G L typ val))
        "eval-exp/downcast/succ")
   (--> ((DOWNCAST typ val) G L) (ERROR G L)
        (where ERROR (downcast G L typ val))
        "eval-exp/downcast/error")

   ;; rulegroup Eval_exp/subtype. $subtyp takes G and the local tdenv, in
   ;; place of their extension (see subtyp).
   (--> ((SUB val typ) G (name L {TYP tdenv FUNC fnenv VAL venv})) ((BOOL b) G L)
        (where (OK b) (subtyp G tdenv typ val))
        "eval-exp/subtype/succ")
   (--> ((SUB val typ) G (name L {TYP tdenv FUNC fnenv VAL venv})) (ERROR G L)
        (where ERROR (subtyp G tdenv typ val))
        "eval-exp/subtype/error")

   ;;; Iterated expression evaluation rules

   ;; rulegroup Eval_exp/iter. An iteration over sub-contexts is the option
   ;; or list of exp under each of them, which evaluates as one.
   (--> ((ITER exp iterExp) G L) (val G L)
        (where (varr) (is-iter-on-var (ITER exp iterExp)))
        (where (val) (find-varr L varr))
        "eval-exp/iter/simple")
   (--> ((ITER exp iterExp) G L) (ERROR G L)
        ;; otherwise: the variable is unbound
        (where (varr) (is-iter-on-var (ITER exp iterExp)))
        (where () (find-varr L varr))
        "eval-exp/iter/simple/error")
   (--> ((ITER exp (QUEST (vari ...))) G L) ((OPT ((IN L_sub exp) ...)) G L)
        (where () (is-iter-on-var (ITER exp (QUEST (vari ...)))))
        (where (OK (L_sub ...)) (sub-opt L (vari ...)))
        "eval-exp/iter/opt-succ")
   (--> ((ITER exp (QUEST (vari ...))) G L) (ERROR G L)
        (where () (is-iter-on-var (ITER exp (QUEST (vari ...)))))
        (where ERROR (sub-opt L (vari ...)))
        "eval-exp/iter/opt/error")
   (--> ((ITER exp (STAR (vari ...))) G L) ((LIST ((IN L_sub exp) ...)) G L)
        (where () (is-iter-on-var (ITER exp (STAR (vari ...)))))
        (where (OK (L_sub ...)) (sub-list L (vari ...)))
        "eval-exp/iter/list-succ")
   (--> ((ITER exp (STAR (vari ...))) G L) (ERROR G L)
        (where () (is-iter-on-var (ITER exp (STAR (vari ...)))))
        (where ERROR (sub-list L (vari ...)))
        "eval-exp/iter/list/error")))
