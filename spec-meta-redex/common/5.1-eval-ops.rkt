#lang racket/base
;; spec-meta/common/5.1-eval-ops.watsup.
;;
;; Arithmetic follows p4spec/lib/lang/xl/num.ml: DIV and MOD truncate. An
;; undefined result (division by zero, a negative exponent) raises an error, as
;; num.ml's `assert false` does.

(require "0.0-prelude.rkt"
         "4-relation.rkt")
(provide unop-number
         binop-bool
         binop-number
         cmpop-poly
         cmpop-number
         is-tup
         is-fun)

;; num.ml has no POW, so this follows watsup's `^` on integers.
(define (pow base exponent)
  (when (negative? exponent)
    (error 'binop-number "negative exponent: ~a" exponent))
  (expt base exponent))

;; Binary operations

(define-dec common-relation
  unop-number : numunop num -> val
  [(unop-number PLUS (NAT n)) (NAT n)]
  [(unop-number PLUS (INT i)) (INT i)]
  [(unop-number MINUS (NAT n)) (INT ,(- (term n)))]
  [(unop-number MINUS (INT i)) (INT ,(- (term i)))])

(define-dec common-relation
  binop-bool : boolbinop bool bool -> bool
  [(binop-bool AND b_l b_r) ,(and (term b_l) (term b_r))]
  [(binop-bool OR b_l b_r) ,(or (term b_l) (term b_r))]
  [(binop-bool IMPL b_l b_r) ,(or (not (term b_l)) (term b_r))]
  [(binop-bool EQUIV b_l b_r) ,(eq? (term b_l) (term b_r))])

(define-dec common-relation
  binop-number : numbinop num num -> val
  [(binop-number ADD (NAT n_l) (NAT n_r)) (NAT ,(+ (term n_l) (term n_r)))]
  [(binop-number ADD (INT i_l) (INT i_r)) (INT ,(+ (term i_l) (term i_r)))]
  [(binop-number SUB (NAT n_l) (NAT n_r)) (INT ,(- (term n_l) (term n_r)))]
  [(binop-number SUB (INT i_l) (INT i_r)) (INT ,(- (term i_l) (term i_r)))]
  [(binop-number MUL (NAT n_l) (NAT n_r)) (NAT ,(* (term n_l) (term n_r)))]
  [(binop-number MUL (INT i_l) (INT i_r)) (INT ,(* (term i_l) (term i_r)))]
  [(binop-number DIV (NAT n_l) (NAT n_r)) (NAT ,(quotient (term n_l) (term n_r)))]
  [(binop-number DIV (INT i_l) (INT i_r)) (INT ,(quotient (term i_l) (term i_r)))]
  [(binop-number MOD (NAT n_l) (NAT n_r)) (NAT ,(remainder (term n_l) (term n_r)))]
  [(binop-number MOD (INT i_l) (INT i_r)) (INT ,(remainder (term i_l) (term i_r)))]
  [(binop-number POW (NAT n_l) (NAT n_r)) (NAT ,(pow (term n_l) (term n_r)))]
  [(binop-number POW (INT i_l) (INT i_r)) (INT ,(pow (term i_l) (term i_r)))])

(define-dec common-relation
  cmpop-poly : polycmpop val val -> bool
  [(cmpop-poly EQ val_l val_r) ,(equal? (term val_l) (term val_r))]
  [(cmpop-poly NE val_l val_r) ,(not (equal? (term val_l) (term val_r)))])

(define-dec common-relation
  cmpop-number : numcmpop num num -> bool
  [(cmpop-number LT (NAT n_l) (NAT n_r)) ,(< (term n_l) (term n_r))]
  [(cmpop-number LT (INT i_l) (INT i_r)) ,(< (term i_l) (term i_r))]
  [(cmpop-number GT (NAT n_l) (NAT n_r)) ,(> (term n_l) (term n_r))]
  [(cmpop-number GT (INT i_l) (INT i_r)) ,(> (term i_l) (term i_r))]
  [(cmpop-number LE (NAT n_l) (NAT n_r)) ,(<= (term n_l) (term n_r))]
  [(cmpop-number LE (INT i_l) (INT i_r)) ,(<= (term i_l) (term i_r))]
  [(cmpop-number GE (NAT n_l) (NAT n_r)) ,(>= (term n_l) (term n_r))]
  [(cmpop-number GE (INT i_l) (INT i_r)) ,(>= (term i_l) (term i_r))])

;; Constructor checks

(define-dec common-relation
  is-tup : val -> bool
  [(is-tup (TUP (val ...))) #t]
  [(is-tup val) #f
   ;; otherwise
   (side-condition (not (redex-match? common-relation (TUP (val ...)) (term val))))])

(define-dec common-relation
  is-fun : val -> bool
  [(is-fun (FUNC id)) #t]
  [(is-fun val) #f
   ;; otherwise
   (side-condition (not (redex-match? common-relation (FUNC id) (term val))))])
