#lang racket/base
;; spec-meta/al/5.3-eval-exp.watsup.
;;
;; An exp evaluates in place: each evaluated subterm is replaced by its result.
;; Eval_exp/fail, Eval_path/fail, and Eval_path_upd/fail become complement
;; rules next to the rules they cover: "<group>/fail-<subterm>" where a
;; subterm just evaluated fits no rule, "<group>/fail" where the last one
;; does not, and "<group>/<rule>/fail" where a rule's own premise fails.

(require redex/reduction-semantics
         "../common/0.0-stdlib.rkt"
         "../common/2-env.rkt"
         "../common/4.1-eval-ops.rkt"
         "3-context.rkt"
         "4-relation.rkt"
         "5.1-eval-typ.rkt")
(provide ->term/eval-exp
         ->term+ctx/eval-exp)

;; A field of the struct that Eval_path_upd/dot updates: val if its atom is
;; atom, and unchanged otherwise
(define-metafunction al
  update-field : atom val valfield -> valfield
  [(update-field atom val (atom val_field)) (atom val)]
  [(update-field atom val (atom_field val_field)) (atom_field val_field)
   (side-condition (not (equal? (term atom) (term atom_field))))])

;; The rules on a term
(define ->term/eval-exp
  (reduction-relation
   al

   ;;; Boolean, number, and text evaluation rules

   ;; rulegroup Eval_exp/literal
   (--> (BOOL b) (OK (BOOL b))
        "eval-exp/literal/boolean")
   (--> num (OK num)
        "eval-exp/literal/number")
   (--> (TEXT t) (OK (TEXT t))
        "eval-exp/literal/string")

   ;;; Unary, binary, and comparison evaluation rules

   ;; rulegroup Eval_exp/unary
   (--> (UN NOT (OK (BOOL b))) (OK (BOOL b_res))
        (where b_res ,(not (term b)))
        "eval-exp/unary/boolean")
   (--> (UN numunop (OK num)) (OK (unop-number numunop num))
        "eval-exp/unary/number")
   (--> (UN unop (OK val)) FAIL
        ;; otherwise
        (side-condition (not (redex-match? al (NOT (OK (BOOL b))) (term (unop (OK val))))))
        (side-condition (not (redex-match? al (numunop (OK num)) (term (unop (OK val))))))
        "eval-exp/unary/fail")

   ;; rulegroup Eval_exp/binary
   (--> (BIN boolbinop (OK (BOOL b_l)) (OK (BOOL b_r))) (OK (BOOL (binop-bool boolbinop b_l b_r)))
        "eval-exp/binary/boolean")
   (--> (BIN numbinop (OK num_l) (OK num_r)) (OK val_res)
        (judgment-holds (binop-number numbinop num_l num_r val_res))
        "eval-exp/binary/number")
   (--> (BIN numbinop (OK num_l) (OK num_r)) FAIL
        ;; otherwise: one operand is a nat and the other an int
        (side-condition (not (judgment-holds (binop-number numbinop num_l num_r any))))
        "eval-exp/binary/number/fail")
   (--> (BIN binop (OK val_l) exp_r) FAIL
        ;; otherwise: the right operand is not evaluated
        (side-condition (not (redex-match? al (boolbinop (BOOL b)) (term (binop val_l)))))
        (side-condition (not (redex-match? al (numbinop num) (term (binop val_l)))))
        "eval-exp/binary/fail-left")
   (--> (BIN binop (OK val_l) (OK val_r)) FAIL
        ;; otherwise
        (side-condition
         (not (redex-match? al (boolbinop (BOOL b_l) (BOOL b_r)) (term (binop val_l val_r)))))
        (side-condition
         (not (redex-match? al (numbinop num_l num_r) (term (binop val_l val_r)))))
        "eval-exp/binary/fail")

   ;; rulegroup Eval_exp/compare
   (--> (CMP polycmpop (OK val_l) (OK val_r)) (OK (BOOL (cmpop-poly polycmpop val_l val_r)))
        "eval-exp/compare/poly")
   (--> (CMP numcmpop (OK num_l) (OK num_r)) (OK (BOOL b_res))
        (judgment-holds (cmpop-number numcmpop num_l num_r b_res))
        "eval-exp/compare/number")
   (--> (CMP numcmpop (OK num_l) (OK num_r)) FAIL
        ;; otherwise: one operand is a nat and the other an int
        (side-condition (not (judgment-holds (cmpop-number numcmpop num_l num_r any))))
        "eval-exp/compare/number/fail")
   (--> (CMP numcmpop (OK val_l) exp_r) FAIL
        ;; otherwise: the right operand is not evaluated
        (side-condition (not (redex-match? al num (term val_l))))
        "eval-exp/compare/fail-left")
   (--> (CMP numcmpop (OK val_l) (OK val_r)) FAIL
        ;; otherwise
        (side-condition (not (redex-match? al (num_l num_r) (term (val_l val_r)))))
        "eval-exp/compare/fail")

   ;;; Match evaluation rules

   ;; rulegroup Eval_exp/match
   (--> (MATCH (OK (INJ (mixop_v (val ...)))) (INJ mixop_p)) (OK (BOOL b))
        (where b ,(equal? (term mixop_p) (term mixop_v)))
        "eval-exp/match/inj")
   (--> (MATCH (OK (LIST (val ...))) CONS) (OK (BOOL b))
        (where b ,(> (length (term (val ...))) 0))
        "eval-exp/match/list-cons")
   (--> (MATCH (OK (LIST (val ...))) (FIXED n)) (OK (BOOL b))
        (where b ,(= (length (term (val ...))) (term n)))
        "eval-exp/match/list-fixed")
   (--> (MATCH (OK (LIST (val ...))) NIL) (OK (BOOL b))
        (where b ,(= (length (term (val ...))) 0))
        "eval-exp/match/list-nil")
   (--> (MATCH (OK (OPT (val ...))) SOME) (OK (BOOL b))
        (where b ,(pair? (term (val ...))))
        "eval-exp/match/opt-some")
   (--> (MATCH (OK (OPT (val ...))) NONE) (OK (BOOL b))
        (where b ,(null? (term (val ...))))
        "eval-exp/match/opt-none")
   (--> (MATCH (OK val) pattern) FAIL
        ;; otherwise: the value does not have the pattern's kind
        (side-condition (not (redex-match? al ((INJ valcase) (INJ mixop)) (term (val pattern)))))
        (side-condition (not (redex-match? al ((LIST (val ...)) listpattern) (term (val pattern)))))
        (side-condition (not (redex-match? al ((OPT (val ...)) optpattern) (term (val pattern)))))
        "eval-exp/match/fail")

   ;;; Tuple evaluation rules

   ;; rule Eval_exp/tuple
   (--> (TUP ((OK val) ...)) (OK (TUP (val ...)))
        "eval-exp/tuple")

   ;;; Case evaluation rules

   ;; rule Eval_exp/case
   (--> (INJ (mixop ((OK val) ...))) (OK (INJ (mixop (val ...))))
        "eval-exp/case")

   ;;; Struct evaluation rules

   ;; rule Eval_exp/struct
   (--> (STR ((atom (OK val)) ...)) (OK (STR ((atom val) ...)))
        "eval-exp/struct")

   ;;; Option evaluation rules

   ;; rulegroup Eval_exp/opt, as one rule whose premise is
   ;; (Eval_exp: C |- exp : OK val)?
   (--> (OPT ((OK val) ...)) (OK (OPT (val ...)))
        "eval-exp/opt")

   ;;; List evaluation rules

   ;; rule Eval_exp/list
   (--> (LIST ((OK val) ...)) (OK (LIST (val ...)))
        "eval-exp/list")

   ;;; Cons-list evaluation rules

   ;; rule Eval_exp/cons
   (--> (CONS (OK val_h) (OK (LIST (val_t ...)))) (OK (LIST (val_h val_t ...)))
        "eval-exp/cons")
   (--> (CONS (OK val_h) (OK val)) FAIL
        ;; otherwise: the tail is not a list
        (side-condition (not (redex-match? al (LIST (val ...)) (term val))))
        "eval-exp/cons/fail")

   ;;; Concatenation evaluation rules

   ;; rulegroup Eval_exp/concat
   (--> (CAT (OK (TEXT t_l)) (OK (TEXT t_r))) (OK (TEXT t_res))
        (where t_res ,(string-append (term t_l) (term t_r)))
        "eval-exp/concat/text")
   (--> (CAT (OK (LIST (val_l ...))) (OK (LIST (val_r ...)))) (OK (LIST (val_l ... val_r ...)))
        "eval-exp/concat/list")
   (--> (CAT (OK val_l) exp_r) FAIL
        ;; otherwise: the right operand is not evaluated
        (side-condition (not (redex-match? al (TEXT t) (term val_l))))
        (side-condition (not (redex-match? al (LIST (val ...)) (term val_l))))
        "eval-exp/concat/fail-left")
   (--> (CAT (OK val_l) (OK val_r)) FAIL
        ;; otherwise
        (side-condition (not (redex-match? al ((TEXT t_l) (TEXT t_r)) (term (val_l val_r)))))
        (side-condition
         (not (redex-match? al ((LIST (val_l ...)) (LIST (val_r ...))) (term (val_l val_r)))))
        "eval-exp/concat/fail")

   ;;; Membership evaluation rules

   ;; rule Eval_exp/mem
   (--> (MEM (OK val_e) (OK (LIST (val ...)))) (OK (BOOL b))
        (where b ,(and (member (term val_e) (term (val ...))) #t))
        "eval-exp/mem")
   (--> (MEM (OK val_e) (OK val)) FAIL
        ;; otherwise: not a list
        (side-condition (not (redex-match? al (LIST (val ...)) (term val))))
        "eval-exp/mem/fail")

   ;;; Length evaluation rules

   ;; rulegroup Eval_exp/len
   (--> (LEN (OK (TEXT t))) (OK (NAT n))
        (where n ,(text-len (term t)))
        "eval-exp/len/text")
   (--> (LEN (OK (LIST (val ...)))) (OK (NAT n))
        (where n ,(length (term (val ...))))
        "eval-exp/len/list")
   (--> (LEN (OK val)) FAIL
        ;; otherwise: neither a text nor a list
        (side-condition (not (redex-match? al (TEXT t) (term val))))
        (side-condition (not (redex-match? al (LIST (val ...)) (term val))))
        "eval-exp/len/fail")

   ;;; Dot, indexing, and slicing evaluation rules

   ;; rule Eval_path/root
   (--> (eval-path val_b ROOT) (OK val_b)
        "eval-path/root")

   ;; rule Eval_path/dot
   (--> (eval-path val_b (DOT path atom))
        (eval-path/dot (eval-path val_b path) atom)
        "eval-path/dot")
   (--> (eval-path/dot (OK (STR (valfield ...))) atom) (OK val)
        (where (atom val) ,(assoc (term atom) (term (valfield ...))))
        "eval-path/dot/field")
   (--> (eval-path/dot (OK (STR (valfield ...))) atom) FAIL
        ;; otherwise: no field atom
        (where #f ,(assoc (term atom) (term (valfield ...))))
        "eval-path/dot/field/fail")
   (--> (eval-path/dot (OK val) atom) FAIL
        ;; otherwise: not a struct
        (side-condition (not (redex-match? al (STR (valfield ...)) (term val))))
        "eval-path/dot/fail")

   ;; rulegroup Eval_path/idx
   (--> (eval-path val_b (IDX path exp_i))
        (eval-path/idx (eval-path val_b path) exp_i)
        "eval-path/idx")
   (--> (eval-path/idx (OK (TEXT t)) (OK (NAT n))) (OK (TEXT t_res))
        (side-condition (< (term n) (text-len (term t))))
        (where t_res ,(text-idx (term t) (term n)))
        "eval-path/idx/text")
   (--> (eval-path/idx (OK (LIST (val ...))) (OK (NAT n))) (OK val_res)
        (side-condition (< (term n) (length (term (val ...)))))
        (where val_res ,(list-idx (term (val ...)) (term n)))
        "eval-path/idx/list")
   (--> (eval-path/idx (OK val) exp_i) FAIL
        ;; otherwise: neither a text nor a list, and the index is not evaluated
        (side-condition (not (redex-match? al (TEXT t) (term val))))
        (side-condition (not (redex-match? al (LIST (val ...)) (term val))))
        "eval-path/idx/fail-base")
   (--> (eval-path/idx (OK val) (OK val_i)) FAIL
        ;; otherwise: the index is not a nat below the length
        (side-condition
         (not (redex-match? al (side-condition ((TEXT t) (NAT n)) (< (term n) (text-len (term t))))
                            (term (val val_i)))))
        (side-condition
         (not (redex-match? al (side-condition ((LIST (val ...)) (NAT n))
                                               (< (term n) (length (term (val ...)))))
                            (term (val val_i)))))
        "eval-path/idx/fail")

   ;; rulegroup Eval_path/slice
   (--> (eval-path val_b (SLICE path exp_i exp_n))
        (eval-path/slice (eval-path val_b path) exp_i exp_n)
        "eval-path/slice")
   (--> (eval-path/slice (OK (TEXT t)) (OK (NAT n_i)) (OK (NAT n_n))) (OK (TEXT t_res))
        (where t_res ,(text-slice (term t) (term n_i) (term n_n)))
        "eval-path/slice/text")
   (--> (eval-path/slice (OK (LIST (val ...))) (OK (NAT n_i)) (OK (NAT n_n)))
        (OK (LIST (val_res ...)))
        (where (val_res ...) ,(list-slice (term (val ...)) (term n_i) (term n_n)))
        "eval-path/slice/list")
   (--> (eval-path/slice (OK val) exp_i exp_n) FAIL
        ;; otherwise: neither a text nor a list, and no index is evaluated
        (side-condition (not (redex-match? al (TEXT t) (term val))))
        (side-condition (not (redex-match? al (LIST (val ...)) (term val))))
        "eval-path/slice/fail-base")
   (--> (eval-path/slice (OK val) (OK val_i) exp_n) FAIL
        ;; otherwise: the start is not a nat, and the length is not evaluated
        (side-condition (not (redex-match? al (NAT n) (term val_i))))
        "eval-path/slice/fail-index")
   (--> (eval-path/slice (OK val) (OK val_i) (OK val_n)) FAIL
        ;; otherwise
        (side-condition
         (not (redex-match? al ((TEXT t) (NAT n_i) (NAT n_n)) (term (val val_i val_n)))))
        (side-condition
         (not (redex-match? al ((LIST (val ...)) (NAT n_i) (NAT n_n)) (term (val val_i val_n)))))
        "eval-path/slice/fail")

   ;; rule Eval_exp/dot
   (--> (DOT (OK val) atom) (eval-path val (DOT ROOT atom))
        "eval-exp/dot")

   ;; rule Eval_exp/idx
   (--> (IDX (OK val_b) exp_i) (eval-path val_b (IDX ROOT exp_i))
        "eval-exp/idx")

   ;; rule Eval_exp/slice
   (--> (SLICE (OK val_b) exp_i exp_n) (eval-path val_b (SLICE ROOT exp_i exp_n))
        "eval-exp/slice")

   ;;; Update evaluation rules

   ;; rule Eval_path_upd/root
   (--> (eval-path-upd val_b ROOT val_n) (OK val_n)
        "eval-path-upd/root")

   ;; rulegroup Eval_path_upd/idx
   (--> (eval-path-upd val_b (IDX path exp_i) val_n)
        (eval-path-upd/idx (eval-path val_b path) exp_i val_b path val_n)
        "eval-path-upd/idx")
   (--> (eval-path-upd/idx (OK (TEXT t)) (OK (NAT n_target)) val_b path (TEXT t_n))
        (eval-path-upd val_b path (TEXT t_upd))
        (side-condition (= (text-len (term t_n)) 1))
        (where t_upd ,(text-upd (term t) (term n_target) (term t_n)))
        "eval-path-upd/idx/text")
   (--> (eval-path-upd/idx (OK (LIST (val ...))) (OK (NAT n_target)) val_b path val_n)
        (eval-path-upd val_b path (LIST (val_upd ...)))
        (where (val_upd ...) ,(list-upd (term (val ...)) (term n_target) (term val_n)))
        "eval-path-upd/idx/list")
   (--> (eval-path-upd/idx (OK val) exp_i val_b path val_n) FAIL
        ;; otherwise: neither a text updated by a text nor a list, and the index
        ;; is not evaluated
        (side-condition (not (redex-match? al ((TEXT t) (TEXT t_n)) (term (val val_n)))))
        (side-condition (not (redex-match? al (LIST (val ...)) (term val))))
        "eval-path-upd/idx/fail-base")
   (--> (eval-path-upd/idx (OK val) (OK val_i) val_b path val_n) FAIL
        ;; otherwise: the index is not a nat, or the text not of length 1
        (side-condition
         (not (redex-match? al (side-condition ((TEXT t) (NAT n) (TEXT t_n))
                                               (= (text-len (term t_n)) 1))
                            (term (val val_i val_n)))))
        (side-condition (not (redex-match? al ((LIST (val ...)) (NAT n)) (term (val val_i)))))
        "eval-path-upd/idx/fail")

   ;; rulegroup Eval_path_upd/slice: both rules take a new value of the
   ;; base's kind first, a text or a list
   (--> (eval-path-upd val_b (SLICE path exp_i exp_n) val_n)
        (eval-path-upd/slice (eval-path val_b path) exp_i exp_n val_b path val_n)
        (side-condition (or (redex-match? al (TEXT t) (term val_n))
                            (redex-match? al (LIST (val ...)) (term val_n))))
        "eval-path-upd/slice")
   (--> (eval-path-upd/slice (OK (TEXT t)) (OK (NAT n_i)) (OK (NAT n_n)) val_b path (TEXT t_n))
        (eval-path-upd val_b path (TEXT t_upd))
        (side-condition (= (text-len (term t_n)) (term n_n)))
        (where t_upd ,(text-upd-slice (term t) (term n_i) (term n_n) (term t_n)))
        "eval-path-upd/slice/text")
   ;; The new value is LIST val_n*, whose elements replace the slice, where
   ;; spec-meta replaces it with the one element val_n.
   (--> (eval-path-upd/slice (OK (LIST (val ...))) (OK (NAT n_i)) (OK (NAT n_n))
                             val_b path (LIST (val_n ...)))
        (eval-path-upd val_b path (LIST (val_upd ...)))
        (where (val_upd ...)
               ,(list-upd-slice (term (val ...)) (term n_i) (term n_n) (term (val_n ...))))
        "eval-path-upd/slice/list")
   (--> (eval-path-upd val_b (SLICE path exp_i exp_n) val_n) FAIL
        ;; otherwise: the new value is neither a text nor a list, and nothing
        ;; is evaluated
        (side-condition (not (redex-match? al (TEXT t) (term val_n))))
        (side-condition (not (redex-match? al (LIST (val ...)) (term val_n))))
        "eval-path-upd/slice/fail-value")
   (--> (eval-path-upd/slice (OK val) exp_i exp_n val_b path val_n) FAIL
        ;; otherwise: not of the new value's kind, and no index is evaluated
        (side-condition (not (redex-match? al ((TEXT t) (TEXT t_n)) (term (val val_n)))))
        (side-condition
         (not (redex-match? al ((LIST (val ...)) (LIST (val_n ...))) (term (val val_n)))))
        "eval-path-upd/slice/fail-base")
   (--> (eval-path-upd/slice (OK val) (OK val_i) exp_n val_b path val_n) FAIL
        ;; otherwise: the start is not a nat, and the length is not evaluated
        (side-condition (not (redex-match? al (NAT n) (term val_i))))
        "eval-path-upd/slice/fail-index")
   (--> (eval-path-upd/slice (OK val) (OK val_i) (OK val_l) val_b path val_n) FAIL
        ;; otherwise: the length is not a nat, or not the text's length
        (side-condition
         (not (redex-match? al (side-condition ((TEXT t) (NAT n_i) (NAT n_n) (TEXT t_n))
                                               (= (text-len (term t_n)) (term n_n)))
                            (term (val val_i val_l val_n)))))
        (side-condition
         (not (redex-match? al ((LIST (val ...)) (NAT n_i) (NAT n_n) (LIST (val_n ...)))
                            (term (val val_i val_l val_n)))))
        "eval-path-upd/slice/fail")

   ;; rule Eval_path_upd/dot
   (--> (eval-path-upd val_b (DOT path atom) val_n)
        (eval-path-upd/dot (eval-path val_b path) atom val_b path val_n)
        "eval-path-upd/dot")
   (--> (eval-path-upd/dot (OK (STR (valfield ...))) atom val_b path val_n)
        (eval-path-upd val_b path (STR ((update-field atom val_n valfield) ...)))
        "eval-path-upd/dot/field")
   (--> (eval-path-upd/dot (OK val) atom val_b path val_n) FAIL
        ;; otherwise: not a struct
        (side-condition (not (redex-match? al (STR (valfield ...)) (term val))))
        "eval-path-upd/dot/fail")

   ;; rule Eval_exp/upd
   (--> (UPD (OK val_b) path (OK val_f)) (eval-path-upd val_b path val_f)
        "eval-exp/upd")

   ;;; Meta-function call evaluation rules

   ;; rule Eval_exp/call: the type arguments, then the arguments
   (--> (CALL id (targ ...) (arg ...)) (eval-exp/call id (eval-targs (targ ...)) (arg ...))
        "eval-exp/call")
   ;; It passes typ_input*, where spec-meta passes targ*.
   (--> (eval-exp/call id (OK (typ_input ...)) ((OK val_input) ...))
        (call-func id (typ_input ...) (val_input ...))
        "eval-exp/call/func")

   ;;; Iterated expression evaluation rules

   ;; Eval_exp/iter/opt and Eval_exp/iter/list, once exp is evaluated in each
   ;; sub-context
   (--> (eval-exp/iter/opt ((OK val) ...)) (OK (OPT (val ...)))
        "eval-exp/iter/opt/collect")
   (--> (eval-exp/iter/list ((OK val) ...)) (OK (LIST (val ...)))
        "eval-exp/iter/list/collect")))

;; The rules on a term and its context (r G L)
(define ->term+ctx/eval-exp
  (reduction-relation
   al

   ;;; Variable evaluation rules

   ;; rule Eval_exp/variable
   (--> ((VAR id) G L) ((OK val) G L)
        (where (val) (find-varr L (id ())))
        "eval-exp/variable")
   (--> ((VAR id) G L) (FAIL G L)
        ;; otherwise
        (where () (find-varr L (id ())))
        "eval-exp/variable/fail")

   ;;; Upcasting, downcasting, and subtyping evaluation rules

   ;; rule Eval_exp/upcast
   (--> ((UPCAST typ (OK val)) G L) ((upcast G L typ val) G L)
        "eval-exp/upcast")

   ;; rule Eval_exp/downcast
   (--> ((DOWNCAST typ (OK val)) G L) ((downcast G L typ val) G L)
        "eval-exp/downcast")

   ;; rule Eval_exp/subtype. $subtyp takes G and the local tdenv, in place of
   ;; their extension (see subtyp).
   (--> ((SUB (OK val) typ) G (name L {TYP tdenv_l REL renv_l FUNC fenv_l VAL venv_l}))
        ((OK (BOOL (subtyp G tdenv_l typ val))) G L)
        "eval-exp/subtype")

   ;;; Iterated expression evaluation rules

   ;; rulegroup Eval_exp/iter
   (--> ((ITER exp iterexp) G L) ((OK val) G L)
        (where (varr) (is-iter-on-var (ITER exp iterexp)))
        (where (val) (find-varr L varr))
        "eval-exp/iter/simple")
   (--> ((ITER exp iterexp) G L) (FAIL G L)
        ;; otherwise: the variable is unbound
        (where (varr) (is-iter-on-var (ITER exp iterexp)))
        (where () (find-varr L varr))
        "eval-exp/iter/simple/fail")
   (--> ((ITER exp (QUEST (vari ...))) G L)
        ((eval-exp/iter/opt ((IN L_sub exp) ...)) G L)
        (where () (is-iter-on-var (ITER exp (QUEST (vari ...)))))
        (judgment-holds (sub-opt L (vari ...) (L_sub ...)))
        "eval-exp/iter/opt")
   (--> ((ITER exp (QUEST (vari ...))) G L) (FAIL G L)
        ;; otherwise: there is no sub-context
        (where () (is-iter-on-var (ITER exp (QUEST (vari ...)))))
        (side-condition (not (judgment-holds (sub-opt L (vari ...) any))))
        "eval-exp/iter/opt/fail")
   (--> ((ITER exp (STAR (vari ...))) G L)
        ((eval-exp/iter/list ((IN L_sub exp) ...)) G L)
        (where () (is-iter-on-var (ITER exp (STAR (vari ...)))))
        (judgment-holds (sub-list L (vari ...) (L_sub ...)))
        "eval-exp/iter/list")
   (--> ((ITER exp (STAR (vari ...))) G L) (FAIL G L)
        ;; otherwise: there are no sub-contexts
        (where () (is-iter-on-var (ITER exp (STAR (vari ...)))))
        (side-condition (not (judgment-holds (sub-list L (vari ...) any))))
        "eval-exp/iter/list/fail")))
