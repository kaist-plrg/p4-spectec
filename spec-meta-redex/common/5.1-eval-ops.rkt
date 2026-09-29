#lang racket/base
;; spec-meta/common/5.1-eval-ops.watsup.
;;
;; Arithmetic follows p4spec/lib/lang/xl/num.ml: DIV and MOD truncate. An
;; undefined result (division by zero, a negative exponent) raises an error, as
;; num.ml's `assert false` does.

(require "0.0-prelude.rkt"
         "4-relation.rkt")
(provide unop_number
         binop_bool
         binop_number
         cmpop_poly
         cmpop_number
         is_tup
         is_fun)

;; num.ml has no POW, so this follows watsup's `^` on integers.
(define (pow base exponent)
  (when (negative? exponent)
    (error 'binop_number "negative exponent: ~a" exponent))
  (expt base exponent))

;; Binary operations

(define-dec Common-relation
  unop_number : numunop num -> val
  [(unop_number PLUS (NAT n)) (NAT n)]
  [(unop_number PLUS (INT i)) (INT i)]
  [(unop_number MINUS (NAT n)) (INT ,(- (term n)))]
  [(unop_number MINUS (INT i)) (INT ,(- (term i)))])

(define-dec Common-relation
  binop_bool : boolbinop bool bool -> bool
  [(binop_bool AND b_l b_r) ,(and (term b_l) (term b_r))]
  [(binop_bool OR b_l b_r) ,(or (term b_l) (term b_r))]
  [(binop_bool IMPL b_l b_r) ,(or (not (term b_l)) (term b_r))]
  [(binop_bool EQUIV b_l b_r) ,(eq? (term b_l) (term b_r))])

(define-dec Common-relation
  binop_number : numbinop num num -> val
  [(binop_number ADD (NAT n_l) (NAT n_r)) (NAT ,(+ (term n_l) (term n_r)))]
  [(binop_number ADD (INT i_l) (INT i_r)) (INT ,(+ (term i_l) (term i_r)))]
  [(binop_number SUB (NAT n_l) (NAT n_r)) (INT ,(- (term n_l) (term n_r)))]
  [(binop_number SUB (INT i_l) (INT i_r)) (INT ,(- (term i_l) (term i_r)))]
  [(binop_number MUL (NAT n_l) (NAT n_r)) (NAT ,(* (term n_l) (term n_r)))]
  [(binop_number MUL (INT i_l) (INT i_r)) (INT ,(* (term i_l) (term i_r)))]
  [(binop_number DIV (NAT n_l) (NAT n_r)) (NAT ,(quotient (term n_l) (term n_r)))]
  [(binop_number DIV (INT i_l) (INT i_r)) (INT ,(quotient (term i_l) (term i_r)))]
  [(binop_number MOD (NAT n_l) (NAT n_r)) (NAT ,(remainder (term n_l) (term n_r)))]
  [(binop_number MOD (INT i_l) (INT i_r)) (INT ,(remainder (term i_l) (term i_r)))]
  [(binop_number POW (NAT n_l) (NAT n_r)) (NAT ,(pow (term n_l) (term n_r)))]
  [(binop_number POW (INT i_l) (INT i_r)) (INT ,(pow (term i_l) (term i_r)))])

(define-dec Common-relation
  cmpop_poly : polycmpop val val -> bool
  [(cmpop_poly EQ val_l val_r) ,(equal? (term val_l) (term val_r))]
  [(cmpop_poly NE val_l val_r) ,(not (equal? (term val_l) (term val_r)))])

(define-dec Common-relation
  cmpop_number : numcmpop num num -> bool
  [(cmpop_number LT (NAT n_l) (NAT n_r)) ,(< (term n_l) (term n_r))]
  [(cmpop_number LT (INT i_l) (INT i_r)) ,(< (term i_l) (term i_r))]
  [(cmpop_number GT (NAT n_l) (NAT n_r)) ,(> (term n_l) (term n_r))]
  [(cmpop_number GT (INT i_l) (INT i_r)) ,(> (term i_l) (term i_r))]
  [(cmpop_number LE (NAT n_l) (NAT n_r)) ,(<= (term n_l) (term n_r))]
  [(cmpop_number LE (INT i_l) (INT i_r)) ,(<= (term i_l) (term i_r))]
  [(cmpop_number GE (NAT n_l) (NAT n_r)) ,(>= (term n_l) (term n_r))]
  [(cmpop_number GE (INT i_l) (INT i_r)) ,(>= (term i_l) (term i_r))])

;; Constructor checks

(define-dec Common-relation
  is_tup : val -> bool
  [(is_tup (TUP (val ...))) #t]
  [(is_tup val) #f
   ;; otherwise
   (side-condition (not (redex-match? Common-relation (TUP (val ...)) (term val))))])

(define-dec Common-relation
  is_fun : val -> bool
  [(is_fun (FUNC id)) #t]
  [(is_fun val) #f
   ;; otherwise
   (side-condition (not (redex-match? Common-relation (FUNC id) (term val))))])
