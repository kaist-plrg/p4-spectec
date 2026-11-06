#lang racket/base
;; Mirrors spec-meta/4-execution/4.07-eval-path.watsup.
;;
;; A path unfolds into the expressions that read it: val_b (DOT path atom) is
;; (DOT (eval-path-acc val_b path) atom). So the rules on an evaluated DOT,
;; IDX, or SLICE are Eval_path_acc's last step, and Eval_exp/dot, idx, and
;; slice are them on the path ROOT. An update reads the value at the path's
;; prefix, updates it at the last step, and writes it back along the prefix.
;; Eval_path_acc/error and Eval_path_upd/error have no rules: the rules keep
;; the bounds the primitives need, and a term out of them is stuck, which is
;; ERROR.

(require redex/reduction-semantics
         "../0-aux/0.0-stdlib.rkt"
         "3.03-machine.rkt")
(provide ->term/eval-path)

;; A field of the struct that Eval_path_upd/dot updates: val if its atom is
;; atom, and unchanged otherwise
(define-metafunction al
  update-field : atom val valField -> valField
  [(update-field atom val (atom val_field)) (atom val)]
  [(update-field atom val valField) valField])

;; The rules on a term
(define ->term/eval-path
  (reduction-relation
   al

   ;;; Access evaluation rules

   ;; rules Eval_path_acc/root, dot, idx, slice: the path's last step reads
   ;; the value at its prefix
   (--> (eval-path-acc val_b ROOT) val_b
        "eval-path-acc/root")
   (--> (eval-path-acc val_b (DOT path atom)) (DOT (eval-path-acc val_b path) atom)
        "eval-path-acc/dot")
   (--> (eval-path-acc val_b (IDX path exp_i)) (IDX (eval-path-acc val_b path) exp_i)
        "eval-path-acc/idx")
   (--> (eval-path-acc val_b (SLICE path exp_i exp_n)) (SLICE (eval-path-acc val_b path) exp_i exp_n)
        "eval-path-acc/slice")

   ;; rulegroup Eval_path_acc/dot, at the last step
   (--> (DOT (STR (valField ...)) atom) val
        (where (atom val) ,(assoc (term atom) (term (valField ...))))
        "eval-path-acc/dot/succ")

   ;; rulegroup Eval_path_acc/idx, at the last step
   (--> (IDX (LIST (val ...)) (NAT n)) val_idx
        (side-condition (< (term n) (length (term (val ...)))))
        (where val_idx ,(list-idx (term (val ...)) (term n)))
        "eval-path-acc/idx/list-succ")
   (--> (IDX (TEXT t) (NAT n)) (TEXT t_idx)
        (side-condition (< (term n) (text-len (term t))))
        (where t_idx ,(text-idx (term t) (term n)))
        "eval-path-acc/idx/text-succ")

   ;; rulegroup Eval_path_acc/slice, at the last step
   (--> (SLICE (TEXT t) (NAT n_i) (NAT n_n)) (TEXT t_slice)
        (side-condition (<= (+ (term n_i) (term n_n)) (text-len (term t))))
        (where t_slice ,(text-slice (term t) (term n_i) (term n_n)))
        "eval-path-acc/slice/text-succ")
   (--> (SLICE (LIST (val ...)) (NAT n_i) (NAT n_n)) (LIST (val_slice ...))
        (side-condition (<= (+ (term n_i) (term n_n)) (length (term (val ...)))))
        (where (val_slice ...) ,(list-slice (term (val ...)) (term n_i) (term n_n)))
        "eval-path-acc/slice/list-succ")

   ;;; Update evaluation rules

   ;; rules Eval_path_upd/root, idx, slice, dot: the value at the path's
   ;; prefix is read, updated at the last step, and written back along the
   ;; prefix
   (--> (eval-path-upd val_b ROOT val_n) val_n
        "eval-path-upd/root")
   (--> (eval-path-upd val_b (IDX path exp_i) val_n)
        (eval-path-upd val_b path (eval-path-upd/idx (eval-path-acc val_b path) exp_i val_n))
        "eval-path-upd/idx")
   (--> (eval-path-upd val_b (SLICE path exp_i exp_n) val_n)
        (eval-path-upd val_b path (eval-path-upd/slice (eval-path-acc val_b path) exp_i exp_n val_n))
        "eval-path-upd/slice")
   (--> (eval-path-upd val_b (DOT path atom) val_n)
        (eval-path-upd val_b path (eval-path-upd/dot (eval-path-acc val_b path) atom val_n))
        "eval-path-upd/dot")

   ;; rulegroup Eval_path_upd/idx, on the value at the prefix
   (--> (eval-path-upd/idx (LIST (val ...)) (NAT n) val_n) (LIST (val_upd ...))
        (side-condition (< (term n) (length (term (val ...)))))
        (where (val_upd ...) ,(list-upd (term (val ...)) (term n) (term val_n)))
        "eval-path-upd/idx/list-succ")
   (--> (eval-path-upd/idx (TEXT t) (NAT n) (TEXT t_n)) (TEXT t_upd)
        (side-condition (< (term n) (text-len (term t))))
        (side-condition (= (text-len (term t_n)) 1))
        (where t_upd ,(text-upd (term t) (term n) (term t_n)))
        "eval-path-upd/idx/text-succ")

   ;; rulegroup Eval_path_upd/slice, on the value at the prefix: a text or a
   ;; list, by the new value's kind
   (--> (eval-path-upd/slice (TEXT t) (NAT n_i) (NAT n_n) (TEXT t_n)) (TEXT t_upd)
        (side-condition (<= (+ (term n_i) (term n_n)) (text-len (term t))))
        (side-condition (= (text-len (term t_n)) (term n_n)))
        (where t_upd ,(text-upd-slice (term t) (term n_i) (term n_n) (term t_n)))
        "eval-path-upd/slice/text-succ")
   (--> (eval-path-upd/slice (LIST (val ...)) (NAT n_i) (NAT n_n) (LIST (val_n ...)))
        (LIST (val_upd ...))
        (side-condition (<= (+ (term n_i) (term n_n)) (length (term (val ...)))))
        (side-condition (= (length (term (val_n ...))) (term n_n)))
        (where (val_upd ...)
               ,(list-upd-slice (term (val ...)) (term n_i) (term n_n) (term (val_n ...))))
        "eval-path-upd/slice/list-succ")

   ;; rulegroup Eval_path_upd/dot, on the value at the prefix
   (--> (eval-path-upd/dot (STR (valField ...)) atom val_n)
        (STR ((update-field atom val_n valField) ...))
        "eval-path-upd/dot/succ")))
