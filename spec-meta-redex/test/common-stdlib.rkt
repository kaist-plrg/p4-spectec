#lang racket/base

(require rackunit
         "../common/0-prelude.rkt"
         "../common/0-stdlib.rkt")

;;
;; Language
;;

(test-match Stdlib map (term ()))
(test-match Stdlib map (term ((a 1) (b 2))))
(test-no-match Stdlib map (term ((a 1 2))))
(test-no-match Stdlib map (term (a)))
(test-match Stdlib set (term (a b)))

;;
;; General bool functions
;;

(test-equal (term (ite #t 1 2)) 1)
(test-equal (term (ite #f 1 2)) 2)

;;
;; General option functions
;;

(test-equal (term (opt_as_seq_ ())) '())
(test-equal (term (opt_as_seq_ (x))) '(x))
(test-equal (term (opt_as_seq_ (x y))) '⊥)

;;
;; General sequence functions
;;

(test-equal (term (exists_ ())) #f)
(test-equal (term (exists_ (#f #f))) #f)
(test-equal (term (exists_ (#f #t))) #t)
(test-equal (term (exists_ (#t #f))) #t)

(test-equal (term (forall_ ())) #t)
(test-equal (term (forall_ (#t #t))) #t)
(test-equal (term (forall_ (#t #f))) #f)
(test-equal (term (forall_ (#f #t))) #f)

(test-equal (term (repeat_ x 0)) '())
(test-equal (term (repeat_ x 1)) '(x))
(test-equal (term (repeat_ (TUP ()) 3)) '((TUP ()) (TUP ()) (TUP ())))

(test-equal (term (rev_ ())) '())
(test-equal (term (rev_ (1 2 3))) '(3 2 1))

(test-equal (term (assoc_ a ())) '())
(test-equal (term (assoc_ a ((b 1)))) '())
(test-equal (term (assoc_ a ((b 1) (a 2) (a 3)))) '(2))
(test-equal (term (assoc_ (NAT 1) (((INT 1) x) ((NAT 1) y)))) '(y))

(test-equal (term (transpose_ ())) '())
(test-equal (term (transpose_ (()))) '())
(test-equal (term (transpose_ (() ()))) '())
(test-equal (term (transpose_ ((1)))) '((1)))
(test-equal (term (transpose_ ((1 2 3)))) '((1) (2) (3)))
(test-equal (term (transpose_ ((1 2) (3 4) (5 6)))) '((1 3 5) (2 4 6)))
(check-exn #rx"cannot transpose" (λ () (term (transpose_ ((1 2) (3))))))
(check-exn #rx"cannot transpose" (λ () (term (transpose_ (() (1))))))

;;
;; General set and map functions
;;

(test-equal (term (empty_set)) '())
(test-equal (term (empty_map)) '())

(test-equal (term (find_map () a)) '())
(test-equal (term (find_map ((a 1) (b 2)) b)) '(2))
(test-equal (term (find_map ((a 1) (a 2)) a)) '(1))
(test-equal (term (find_map ((("x" (STAR)) 1)) ("x" ()))) '())
(test-equal (term (find_map ((("x" (STAR)) 1)) ("x" (STAR)))) '(1))

(test-equal (term (find_maps () a)) '())
(test-equal (term (find_maps (() ((b 1))) a)) '())
(test-equal (term (find_maps (((a 1)) ((a 2))) a)) '(1))
(test-equal (term (find_maps (((b 1)) ((a 2))) a)) '(2))

(test-equal (term (add_map () a 1)) '((a 1)))
(test-equal (term (add_map ((a 1) (b 2)) c 3)) '((a 1) (b 2) (c 3)))
(test-equal (term (add_map ((a 1) (b 2)) a 3)) '((a 3) (b 2)))
(test-equal (term (add_map ((a 1) (b 2) (a 4)) a 3)) '((a 3) (b 2) (a 4)))

(test-equal (term (adds_map ((a 1)) () ())) '((a 1)))
(test-equal (term (adds_map ((a 1)) (b a) (2 3))) '((a 3) (b 2)))
(test-equal (term (adds_map () (a a) (1 2))) '((a 2)))
(check-exn #rx"adds_map" (λ () (term (adds_map () (a b) (1)))))

(test-results)
