#lang racket/base
;; spec-meta/3-execution/3.07-eval-exp.watsup.
;;
;; An exp evaluates in place: each evaluated subterm is replaced by its value,
;; and a constructor whose parts are all values is a value already. Eval_exp/
;; fail, Eval_path/fail, and Eval_path_upd/fail become complement rules next
;; to the rules they cover: "<group>/fail" where the evaluated subterms fit
;; no rule, and "<group>/<rule>/fail" where a rule's own premise fails.

(require redex/reduction-semantics
         "../0-aux/0.0-stdlib.rkt"
         "3.01-env.rkt"
         "3.02-context.rkt"
         "3.03-machine.rkt"
         "3.05-eval-ops.rkt"
         "3.06.1-typ-subst.rkt")
(provide ->term/eval-exp
         ->term+ctx/eval-exp)

;; A field of the struct that Eval_path_upd/dot updates: val if its atom is
;; atom, and unchanged otherwise
(define-metafunction al
  update-field : atom val valfield -> valfield
  [(update-field atom val (atom val_field)) (atom val)]
  [(update-field atom val valfield) valfield])

;; The rules on a term
(define ->term/eval-exp
  (reduction-relation
   al

   ;;; Unary, binary, and comparison evaluation rules

   ;; rulegroup Eval_exp/unary
   (--> (UN NOT (BOOL b)) (BOOL b_res)
        (where b_res ,(not (term b)))
        "eval-exp/unary/boolean")
   (--> (UN numunop num) (unop-number numunop num)
        "eval-exp/unary/number")
   (--> (UN unop val) FAIL
        ;; otherwise
        (side-condition (not (redex-match? al (NOT (BOOL b)) (term (unop val)))))
        (side-condition (not (redex-match? al (numunop num) (term (unop val)))))
        "eval-exp/unary/fail")

   ;; rulegroup Eval_exp/binary
   (--> (BIN boolbinop (BOOL b_l) (BOOL b_r)) (BOOL (binop-bool boolbinop b_l b_r))
        "eval-exp/binary/boolean")
   (--> (BIN numbinop num_l num_r) val_res
        (judgment-holds (binop-number numbinop num_l num_r val_res))
        "eval-exp/binary/number")
   (--> (BIN numbinop num_l num_r) FAIL
        ;; otherwise: one operand is a nat and the other an int
        (side-condition (not (judgment-holds (binop-number numbinop num_l num_r any))))
        "eval-exp/binary/number/fail")
   (--> (BIN binop val_l val_r) FAIL
        ;; otherwise
        (side-condition
         (not (redex-match? al (boolbinop (BOOL b_l) (BOOL b_r)) (term (binop val_l val_r)))))
        (side-condition
         (not (redex-match? al (numbinop num_l num_r) (term (binop val_l val_r)))))
        "eval-exp/binary/fail")

   ;; rulegroup Eval_exp/compare
   (--> (CMP polycmpop val_l val_r) (BOOL (cmpop-poly polycmpop val_l val_r))
        "eval-exp/compare/poly")
   (--> (CMP numcmpop num_l num_r) (BOOL b_res)
        (judgment-holds (cmpop-number numcmpop num_l num_r b_res))
        "eval-exp/compare/number")
   (--> (CMP numcmpop num_l num_r) FAIL
        ;; otherwise: one operand is a nat and the other an int
        (side-condition (not (judgment-holds (cmpop-number numcmpop num_l num_r any))))
        "eval-exp/compare/number/fail")
   (--> (CMP numcmpop val_l val_r) FAIL
        ;; otherwise
        (side-condition (not (redex-match? al (num_l num_r) (term (val_l val_r)))))
        "eval-exp/compare/fail")

   ;;; Match evaluation rules

   ;; rulegroup Eval_exp/match
   (--> (MATCH (INJ (mixop_v (val ...))) (INJ mixop_p)) (BOOL b)
        (where b ,(equal? (term mixop_p) (term mixop_v)))
        "eval-exp/match/inj")
   (--> (MATCH (LIST (val ...)) CONS) (BOOL b)
        (where b ,(> (length (term (val ...))) 0))
        "eval-exp/match/list-cons")
   (--> (MATCH (LIST (val ...)) (FIXED n)) (BOOL b)
        (where b ,(= (length (term (val ...))) (term n)))
        "eval-exp/match/list-fixed")
   (--> (MATCH (LIST (val ...)) NIL) (BOOL b)
        (where b ,(= (length (term (val ...))) 0))
        "eval-exp/match/list-nil")
   (--> (MATCH (OPT (val ...)) SOME) (BOOL b)
        (where b ,(pair? (term (val ...))))
        "eval-exp/match/opt-some")
   (--> (MATCH (OPT (val ...)) NONE) (BOOL b)
        (where b ,(null? (term (val ...))))
        "eval-exp/match/opt-none")
   (--> (MATCH val pattern) FAIL
        ;; otherwise: the value does not have the pattern's kind
        (side-condition (not (redex-match? al ((INJ valcase) (INJ mixop)) (term (val pattern)))))
        (side-condition (not (redex-match? al ((LIST (val ...)) listpattern) (term (val pattern)))))
        (side-condition (not (redex-match? al ((OPT (val ...)) optpattern) (term (val pattern)))))
        "eval-exp/match/fail")

   ;;; Cons-list evaluation rules

   ;; rule Eval_exp/cons
   (--> (CONS val_h (LIST (val_t ...))) (LIST (val_h val_t ...))
        "eval-exp/cons")
   (--> (CONS val_h val) FAIL
        ;; otherwise: the tail is not a list
        (side-condition (not (redex-match? al (LIST (val ...)) (term val))))
        "eval-exp/cons/fail")

   ;;; Concatenation evaluation rules

   ;; rulegroup Eval_exp/concat
   (--> (CAT (TEXT t_l) (TEXT t_r)) (TEXT t_res)
        (where t_res ,(string-append (term t_l) (term t_r)))
        "eval-exp/concat/text")
   (--> (CAT (LIST (val_l ...)) (LIST (val_r ...))) (LIST (val_l ... val_r ...))
        "eval-exp/concat/list")
   (--> (CAT val_l val_r) FAIL
        ;; otherwise
        (side-condition (not (redex-match? al ((TEXT t_l) (TEXT t_r)) (term (val_l val_r)))))
        (side-condition
         (not (redex-match? al ((LIST (val_l ...)) (LIST (val_r ...))) (term (val_l val_r)))))
        "eval-exp/concat/fail")

   ;;; Membership evaluation rules

   ;; rule Eval_exp/mem
   (--> (MEM val_e (LIST (val ...))) (BOOL b)
        (where b ,(and (member (term val_e) (term (val ...))) #t))
        "eval-exp/mem")
   (--> (MEM val_e val) FAIL
        ;; otherwise: not a list
        (side-condition (not (redex-match? al (LIST (val ...)) (term val))))
        "eval-exp/mem/fail")

   ;;; Length evaluation rules

   ;; rulegroup Eval_exp/len
   (--> (LEN (TEXT t)) (NAT n)
        (where n ,(text-len (term t)))
        "eval-exp/len/text")
   (--> (LEN (LIST (val ...))) (NAT n)
        (where n ,(length (term (val ...))))
        "eval-exp/len/list")
   (--> (LEN val) FAIL
        ;; otherwise: neither a text nor a list
        (side-condition (not (redex-match? al (TEXT t) (term val))))
        (side-condition (not (redex-match? al (LIST (val ...)) (term val))))
        "eval-exp/len/fail")

   ;;; Dot, indexing, and slicing evaluation rules, which Eval_path shares

   ;; rules Eval_exp/dot and Eval_path/dot
   (--> (DOT (STR (valfield ...)) atom) val
        (where (atom val) ,(assoc (term atom) (term (valfield ...))))
        "eval-exp/dot/field")
   (--> (DOT (STR (valfield ...)) atom) FAIL
        ;; otherwise: no field atom
        (where #f ,(assoc (term atom) (term (valfield ...))))
        "eval-exp/dot/field/fail")
   (--> (DOT val atom) FAIL
        ;; otherwise: not a struct
        (side-condition (not (redex-match? al (STR (valfield ...)) (term val))))
        "eval-exp/dot/fail")

   ;; rules Eval_exp/idx and Eval_path/idx
   (--> (IDX (TEXT t) (NAT n)) (TEXT t_res)
        (side-condition (< (term n) (text-len (term t))))
        (where t_res ,(text-idx (term t) (term n)))
        "eval-exp/idx/text")
   (--> (IDX (LIST (val ...)) (NAT n)) val_res
        (side-condition (< (term n) (length (term (val ...)))))
        (where val_res ,(list-idx (term (val ...)) (term n)))
        "eval-exp/idx/list")
   (--> (IDX val val_i) FAIL
        ;; otherwise: neither a text nor a list, or the index is not a nat
        ;; below the length
        (side-condition
         (not (redex-match? al (side-condition ((TEXT t) (NAT n)) (< (term n) (text-len (term t))))
                            (term (val val_i)))))
        (side-condition
         (not (redex-match? al (side-condition ((LIST (val ...)) (NAT n))
                                               (< (term n) (length (term (val ...)))))
                            (term (val val_i)))))
        "eval-exp/idx/fail")

   ;; rules Eval_exp/slice and Eval_path/slice
   (--> (SLICE (TEXT t) (NAT n_i) (NAT n_n)) (TEXT t_res)
        (where t_res ,(text-slice (term t) (term n_i) (term n_n)))
        "eval-exp/slice/text")
   (--> (SLICE (LIST (val ...)) (NAT n_i) (NAT n_n)) (LIST (val_res ...))
        (where (val_res ...) ,(list-slice (term (val ...)) (term n_i) (term n_n)))
        "eval-exp/slice/list")
   (--> (SLICE val val_i val_n) FAIL
        ;; otherwise
        (side-condition
         (not (redex-match? al ((TEXT t) (NAT n_i) (NAT n_n)) (term (val val_i val_n)))))
        (side-condition
         (not (redex-match? al ((LIST (val ...)) (NAT n_i) (NAT n_n)) (term (val val_i val_n)))))
        "eval-exp/slice/fail")

   ;;; Path evaluation rules

   ;; rulegroup Eval_path: a path unfolds into the expression that reads it
   (--> (eval-path val_b ROOT) val_b
        "eval-path/root")
   (--> (eval-path val_b (DOT path atom)) (DOT (eval-path val_b path) atom)
        "eval-path/dot")
   (--> (eval-path val_b (IDX path exp_i)) (IDX (eval-path val_b path) exp_i)
        "eval-path/idx")
   (--> (eval-path val_b (SLICE path exp_i exp_n)) (SLICE (eval-path val_b path) exp_i exp_n)
        "eval-path/slice")

   ;;; Update evaluation rules

   ;; rule Eval_exp/upd
   (--> (UPD val_b path val_f) (eval-path-upd val_b path val_f)
        "eval-exp/upd")

   ;; rulegroup Eval_path_upd: the value at the path's prefix is read,
   ;; updated at the path's last step, and written back along the prefix
   (--> (eval-path-upd val_b ROOT val_n) val_n
        "eval-path-upd/root")
   (--> (eval-path-upd val_b (IDX path exp_i) val_n)
        (eval-path-upd val_b path (eval-path-upd/idx (eval-path val_b path) exp_i val_n))
        "eval-path-upd/idx")
   (--> (eval-path-upd val_b (SLICE path exp_i exp_n) val_n)
        (eval-path-upd val_b path (eval-path-upd/slice (eval-path val_b path) exp_i exp_n val_n))
        "eval-path-upd/slice")
   (--> (eval-path-upd val_b (DOT path atom) val_n)
        (eval-path-upd val_b path (eval-path-upd/dot (eval-path val_b path) atom val_n))
        "eval-path-upd/dot")

   ;; Eval_path_upd/idx, on the value at the prefix
   (--> (eval-path-upd/idx (TEXT t) (NAT n) (TEXT t_n)) (TEXT t_upd)
        (side-condition (= (text-len (term t_n)) 1))
        (where t_upd ,(text-upd (term t) (term n) (term t_n)))
        "eval-path-upd/idx/text")
   (--> (eval-path-upd/idx (LIST (val ...)) (NAT n) val_n) (LIST (val_upd ...))
        (where (val_upd ...) ,(list-upd (term (val ...)) (term n) (term val_n)))
        "eval-path-upd/idx/list")
   (--> (eval-path-upd/idx val val_i val_n) FAIL
        ;; otherwise: neither a text updated by a text of length 1 nor a
        ;; list, or the index is not a nat
        (side-condition
         (not (redex-match? al (side-condition ((TEXT t) (NAT n) (TEXT t_n))
                                               (= (text-len (term t_n)) 1))
                            (term (val val_i val_n)))))
        (side-condition (not (redex-match? al ((LIST (val ...)) (NAT n)) (term (val val_i)))))
        "eval-path-upd/idx/fail")

   ;; Eval_path_upd/slice, on the value at the prefix: a text or a list, by
   ;; the new value's kind
   (--> (eval-path-upd/slice (TEXT t) (NAT n_i) (NAT n_n) (TEXT t_n)) (TEXT t_upd)
        (side-condition (= (text-len (term t_n)) (term n_n)))
        (where t_upd ,(text-upd-slice (term t) (term n_i) (term n_n) (term t_n)))
        "eval-path-upd/slice/text")
   ;; The new value is LIST val_n*, whose elements replace the slice, where
   ;; spec-meta replaces it with the one element val_n.
   (--> (eval-path-upd/slice (LIST (val ...)) (NAT n_i) (NAT n_n) (LIST (val_n ...)))
        (LIST (val_upd ...))
        (where (val_upd ...)
               ,(list-upd-slice (term (val ...)) (term n_i) (term n_n) (term (val_n ...))))
        "eval-path-upd/slice/list")
   (--> (eval-path-upd/slice val val_i val_l val_n) FAIL
        ;; otherwise: not a text and a text of the slice's length, nor two
        ;; lists, or an index is not a nat
        (side-condition
         (not (redex-match? al (side-condition ((TEXT t) (NAT n_i) (NAT n_n) (TEXT t_n))
                                               (= (text-len (term t_n)) (term n_n)))
                            (term (val val_i val_l val_n)))))
        (side-condition
         (not (redex-match? al ((LIST (val ...)) (NAT n_i) (NAT n_n) (LIST (val_n ...)))
                            (term (val val_i val_l val_n)))))
        "eval-path-upd/slice/fail")

   ;; Eval_path_upd/dot, on the value at the prefix
   (--> (eval-path-upd/dot (STR (valfield ...)) atom val_n)
        (STR ((update-field atom val_n valfield) ...))
        "eval-path-upd/dot/field")
   (--> (eval-path-upd/dot val atom val_n) FAIL
        ;; otherwise: not a struct
        (side-condition (not (redex-match? al (STR (valfield ...)) (term val))))
        "eval-path-upd/dot/fail")

   ;;; Expression sequences

   ;; Eval_exp on each of a sequence of expressions, for Eval_rul's outputs.
   ;; Not a watsup relation: the premise (Eval_exp: C |- exp : OK val)*
   ;; evaluates in place.
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
   (--> ((VAR id) G L) (FAIL G L)
        ;; otherwise
        (where () (find-varr L (id ())))
        "eval-exp/variable/fail")

   ;;; Upcasting, downcasting, and subtyping evaluation rules

   ;; rule Eval_exp/upcast
   (--> ((UPCAST typ val) G L) ((upcast G L typ val) G L)
        "eval-exp/upcast")

   ;; rule Eval_exp/downcast
   (--> ((DOWNCAST typ val) G L) ((downcast G L typ val) G L)
        "eval-exp/downcast")

   ;; rule Eval_exp/subtype. $subtyp takes G and the local tdenv, in place of
   ;; their extension (see subtyp).
   (--> ((SUB val typ) G (name L {TYP tdenv_l REL renv_l FUNC fenv_l VAL venv_l}))
        ((BOOL (subtyp G tdenv_l typ val)) G L)
        "eval-exp/subtype")

   ;;; Meta-function call evaluation rules

   ;; rule Eval_exp/call, with Eval_targs inline: once the arguments are
   ;; evaluated, the type arguments are substituted through the local tdenv.
   ;; It passes typ_input*, where spec-meta passes targ*.
   (--> ((CALL id (targ ...) (val_input ...)) G (name L {TYP tdenv REL renv FUNC fenv VAL venv}))
        ((call-func id (typ_input ...) (val_input ...)) G L)
        (judgment-holds (theta-of-tdenv tdenv theta))
        (judgment-holds (subst-typs theta (targ ...) (typ_input ...)))
        "eval-exp/call")
   (--> ((CALL id (targ ...) (val_input ...)) G (name L {TYP tdenv REL renv FUNC fenv VAL venv}))
        (FAIL G L)
        ;; otherwise: the local types are not all aliases
        (side-condition (not (judgment-holds (theta-of-tdenv tdenv any))))
        "eval-exp/call/fail-theta")
   (--> ((CALL id (targ ...) (val_input ...)) G (name L {TYP tdenv REL renv FUNC fenv VAL venv}))
        (FAIL G L)
        ;; otherwise: some substitution fails
        (judgment-holds (theta-of-tdenv tdenv theta))
        (side-condition (not (judgment-holds (subst-typs theta (targ ...) any))))
        "eval-exp/call/fail-subst")

   ;;; Iterated expression evaluation rules

   ;; rulegroup Eval_exp/iter. An iteration over sub-contexts is the option
   ;; or list of exp under each of them, which evaluates as one.
   (--> ((ITER exp iterexp) G L) (val G L)
        (where (varr) (is-iter-on-var (ITER exp iterexp)))
        (where (val) (find-varr L varr))
        "eval-exp/iter/simple")
   (--> ((ITER exp iterexp) G L) (FAIL G L)
        ;; otherwise: the variable is unbound
        (where (varr) (is-iter-on-var (ITER exp iterexp)))
        (where () (find-varr L varr))
        "eval-exp/iter/simple/fail")
   (--> ((ITER exp (QUEST (vari ...))) G L) ((OPT ((IN L_sub exp) ...)) G L)
        (where () (is-iter-on-var (ITER exp (QUEST (vari ...)))))
        (judgment-holds (sub-opt L (vari ...) (L_sub ...)))
        "eval-exp/iter/opt")
   (--> ((ITER exp (QUEST (vari ...))) G L) (FAIL G L)
        ;; otherwise: there is no sub-context
        (where () (is-iter-on-var (ITER exp (QUEST (vari ...)))))
        (side-condition (not (judgment-holds (sub-opt L (vari ...) any))))
        "eval-exp/iter/opt/fail")
   (--> ((ITER exp (STAR (vari ...))) G L) ((LIST ((IN L_sub exp) ...)) G L)
        (where () (is-iter-on-var (ITER exp (STAR (vari ...)))))
        (judgment-holds (sub-list L (vari ...) (L_sub ...)))
        "eval-exp/iter/list")
   (--> ((ITER exp (STAR (vari ...))) G L) (FAIL G L)
        ;; otherwise: there are no sub-contexts
        (where () (is-iter-on-var (ITER exp (STAR (vari ...)))))
        (side-condition (not (judgment-holds (sub-list L (vari ...) any))))
        "eval-exp/iter/list/fail")))
