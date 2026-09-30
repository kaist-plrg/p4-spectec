#lang racket/base

(require rackunit
         "../common/0.0-prelude.rkt"
         "../common/5.1-eval-ops.rkt")

;;
;; $unop_number
;;

(test-equal (term (unop-number PLUS (NAT 3))) '(NAT 3))
(test-equal (term (unop-number PLUS (INT -3))) '(INT -3))
(test-equal (term (unop-number MINUS (NAT 3))) '(INT -3))
(test-equal (term (unop-number MINUS (NAT 0))) '(INT 0))
(test-equal (term (unop-number MINUS (INT -3))) '(INT 3))

;;
;; $binop_bool
;;

(for* ([b_l (in-list '(#t #f))]
       [b_r (in-list '(#t #f))])
  (test-equal (term (binop-bool AND ,b_l ,b_r)) (and b_l b_r))
  (test-equal (term (binop-bool OR ,b_l ,b_r)) (or b_l b_r))
  (test-equal (term (binop-bool IMPL ,b_l ,b_r)) (or (not b_l) b_r))
  (test-equal (term (binop-bool EQUIV ,b_l ,b_r)) (eq? b_l b_r)))

;;
;; $binop_number
;;

(test-equal (term (binop-number ADD (NAT 2) (NAT 3))) '(NAT 5))
(test-equal (term (binop-number ADD (INT 2) (INT -3))) '(INT -1))
(test-equal (term (binop-number SUB (NAT 2) (NAT 3))) '(INT -1))
(test-equal (term (binop-number SUB (INT 2) (INT -3))) '(INT 5))
(test-equal (term (binop-number MUL (NAT 2) (NAT 3))) '(NAT 6))
(test-equal (term (binop-number MUL (INT 2) (INT -3))) '(INT -6))
(test-equal (term (binop-number POW (NAT 2) (NAT 10))) '(NAT 1024))
(test-equal (term (binop-number POW (INT -2) (INT 3))) '(INT -8))
(test-equal (term (binop-number POW (INT 0) (INT 0))) '(INT 1))
(test-equal (term (binop-number ADD (NAT ,(expt 2 64)) (NAT 1)))
            `(NAT ,(+ (expt 2 64) 1)))

;; DIV and MOD truncate, as bignum's Bigint does.
(test-equal (term (binop-number DIV (NAT 7) (NAT 2))) '(NAT 3))
(test-equal (term (binop-number MOD (NAT 7) (NAT 2))) '(NAT 1))
(test-equal (term (binop-number DIV (INT -7) (INT 2))) '(INT -3))
(test-equal (term (binop-number MOD (INT -7) (INT 2))) '(INT -1))
(test-equal (term (binop-number DIV (INT 7) (INT -2))) '(INT -3))
(test-equal (term (binop-number MOD (INT 7) (INT -2))) '(INT 1))
(test-equal (term (binop-number DIV (INT -7) (INT -2))) '(INT 3))
(test-equal (term (binop-number MOD (INT -7) (INT -2))) '(INT -1))

;; Undefined results raise.
(check-exn exn:fail? (λ () (term (binop-number DIV (NAT 1) (NAT 0)))))
(check-exn exn:fail? (λ () (term (binop-number MOD (INT 1) (INT 0)))))
(check-exn #rx"negative exponent" (λ () (term (binop-number POW (INT 2) (INT -1)))))

;; No clause for mixed NAT and INT: ⊥
(test-equal (term (binop-number ADD (NAT 1) (INT 1))) '⊥)
(test-equal (term (binop-number DIV (INT 1) (NAT 1))) '⊥)

;;
;; $cmpop_poly
;;

(test-equal (term (cmpop-poly EQ (NAT 1) (NAT 1))) #t)
(test-equal (term (cmpop-poly EQ (NAT 1) (INT 1))) #f)
(test-equal (term (cmpop-poly EQ (TUP ((TEXT "a") (OPT ()))) (TUP ((TEXT "a") (OPT ())))))
            #t)
(test-equal (term (cmpop-poly EQ (TUP ((TEXT "a") (OPT ()))) (TUP ((TEXT "b") (OPT ())))))
            #f)
(test-equal (term (cmpop-poly NE (NAT 1) (NAT 1))) #f)
(test-equal (term (cmpop-poly NE (LIST ()) (OPT ()))) #t)

;;
;; $cmpop_number
;;

(for* ([l (in-list '(1 2 3))]
       [r (in-list '(1 2 3))])
  (test-equal (term (cmpop-number LT (NAT ,l) (NAT ,r))) (< l r))
  (test-equal (term (cmpop-number GT (NAT ,l) (NAT ,r))) (> l r))
  (test-equal (term (cmpop-number LE (NAT ,l) (NAT ,r))) (<= l r))
  (test-equal (term (cmpop-number GE (NAT ,l) (NAT ,r))) (>= l r))
  (test-equal (term (cmpop-number LT (INT ,(- l)) (INT ,r))) (< (- l) r))
  (test-equal (term (cmpop-number GT (INT ,(- l)) (INT ,r))) (> (- l) r))
  (test-equal (term (cmpop-number LE (INT ,l) (INT ,(- r)))) (<= l (- r)))
  (test-equal (term (cmpop-number GE (INT ,l) (INT ,(- r)))) (>= l (- r))))

;; No clause for mixed NAT and INT: ⊥
(test-equal (term (cmpop-number LT (NAT 1) (INT 2))) '⊥)

;;
;; $is_tup, $is_fun
;;

(test-equal (term (is-tup (TUP ()))) #t)
(test-equal (term (is-tup (TUP ((NAT 1) (NAT 2))))) #t)
(test-equal (term (is-tup (LIST ()))) #f)
(test-equal (term (is-tup (FUNC "f"))) #f)

(test-equal (term (is-fun (FUNC "f"))) #t)
(test-equal (term (is-fun (TEXT "f"))) #f)
(test-equal (term (is-fun (TUP ()))) #f)

;;
;; Contracts
;;

(when contracts?
  (check-exn #rx"not in my domain" (λ () (term (binop-number AND (NAT 1) (NAT 2)))))
  (check-exn #rx"not in my domain" (λ () (term (is-tup 3)))))

(test-results)
