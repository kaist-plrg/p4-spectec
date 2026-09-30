#lang racket/base

(require rackunit
         "../common/0.0-prelude.rkt"
         "../common/0.1-stdlib.rkt")

;;
;; Language
;;

(test-match stdlib map (term ()))
(test-match stdlib map (term ((a 1) (b 2))))
(test-no-match stdlib map (term ((a 1 2))))
(test-no-match stdlib map (term (a)))
(test-match stdlib set (term (a b)))

;;
;; General bool functions
;;

(test-equal (term (ite #t 1 2)) 1)
(test-equal (term (ite #f 1 2)) 2)

;;
;; General option functions
;;

(test-equal (term (opt-as-seq- ())) '())
(test-equal (term (opt-as-seq- (x))) '(x))
(test-equal (term (opt-as-seq- (x y))) '⊥)

;;
;; General sequence functions
;;

(test-equal (term (exists- ())) #f)
(test-equal (term (exists- (#f #f))) #f)
(test-equal (term (exists- (#f #t))) #t)
(test-equal (term (exists- (#t #f))) #t)

(test-equal (term (forall- ())) #t)
(test-equal (term (forall- (#t #t))) #t)
(test-equal (term (forall- (#t #f))) #f)
(test-equal (term (forall- (#f #t))) #f)

(test-equal (term (repeat- x 0)) '())
(test-equal (term (repeat- x 1)) '(x))
(test-equal (term (repeat- (TUP ()) 3)) '((TUP ()) (TUP ()) (TUP ())))

(test-equal (term (rev- ())) '())
(test-equal (term (rev- (1 2 3))) '(3 2 1))

(test-equal (term (assoc- a ())) '())
(test-equal (term (assoc- a ((b 1)))) '())
(test-equal (term (assoc- a ((b 1) (a 2) (a 3)))) '(2))
(test-equal (term (assoc- (NAT 1) (((INT 1) x) ((NAT 1) y)))) '(y))

(test-equal (term (transpose- ())) '())
(test-equal (term (transpose- (()))) '())
(test-equal (term (transpose- (() ()))) '())
(test-equal (term (transpose- ((1)))) '((1)))
(test-equal (term (transpose- ((1 2 3)))) '((1) (2) (3)))
(test-equal (term (transpose- ((1 2) (3 4) (5 6)))) '((1 3 5) (2 4 6)))
(check-exn #rx"cannot transpose" (λ () (term (transpose- ((1 2) (3))))))
(check-exn #rx"cannot transpose" (λ () (term (transpose- (() (1))))))

;;
;; General set and map functions
;;

(test-equal (term (empty-set)) '())
(test-equal (term (empty-map)) '())

(test-equal (term (find-map () a)) '())
(test-equal (term (find-map ((a 1) (b 2)) b)) '(2))
(test-equal (term (find-map ((a 1) (a 2)) a)) '(1))
(test-equal (term (find-map ((("x" (STAR)) 1)) ("x" ()))) '())
(test-equal (term (find-map ((("x" (STAR)) 1)) ("x" (STAR)))) '(1))

(test-equal (term (find-maps () a)) '())
(test-equal (term (find-maps (() ((b 1))) a)) '())
(test-equal (term (find-maps (((a 1)) ((a 2))) a)) '(1))
(test-equal (term (find-maps (((b 1)) ((a 2))) a)) '(2))

(test-equal (term (add-map () a 1)) '((a 1)))
(test-equal (term (add-map ((a 1) (b 2)) c 3)) '((a 1) (b 2) (c 3)))
(test-equal (term (add-map ((a 1) (b 2)) a 3)) '((a 3) (b 2)))
(test-equal (term (add-map ((a 1) (b 2) (a 4)) a 3)) '((a 3) (b 2) (a 4)))

(test-equal (term (adds-map ((a 1)) () ())) '((a 1)))
(test-equal (term (adds-map ((a 1)) (b a) (2 3))) '((a 3) (b 2)))
(test-equal (term (adds-map () (a a) (1 2))) '((a 2)))
(check-exn #rx"adds-map" (λ () (term (adds-map () (a b) (1)))))

;;
;; Racket helpers for indexing, slicing, and updating
;;

(test-equal (list-idx '(a b c) 0) 'a)
(test-equal (list-idx '(a b c) 2) 'c)
(check-exn #rx"out of bounds" (λ () (list-idx '(a b c) 3)))

;; x*[i : n] is the n elements from i.
(test-equal (list-slice '(a b c d) 1 2) '(b c))
(test-equal (list-slice '(a b c d) 0 4) '(a b c d))
(test-equal (list-slice '(a b c d) 4 0) '())
(check-exn #rx"out of bounds" (λ () (list-slice '(a b c d) 3 2)))
(check-exn #rx"out of bounds" (λ () (list-slice '(a b c d) 5 0)))

(test-equal (list-upd '(a b c) 1 'x) '(a x c))
(check-exn #rx"out of bounds" (λ () (list-upd '(a b c) 3 'x)))

(test-equal (list-upd-slice '(a b c d) 1 2 '(x y)) '(a x y d))
(test-equal (list-upd-slice '(a b c d) 4 0 '()) '(a b c d))
(check-exn #rx"out of bounds" (λ () (list-upd-slice '(a b c d) 3 2 '(x y))))
(check-exn #rx"length 2 updated with one of length 1"
           (λ () (list-upd-slice '(a b c d) 1 2 '(x))))

;; Texts are indexed by UTF-8 bytes, as in OCaml: "é" is two bytes.
(test-equal (text-length "") 0)
(test-equal (text-length "abc") 3)
(test-equal (text-length "é") 2)

(test-equal (text-idx "abc" 1) "b")
(test-equal (text-idx "aé" 0) "a")
(check-exn #rx"out of bounds" (λ () (text-idx "abc" 3)))
(check-exn #rx"splits a UTF-8 character" (λ () (text-idx "é" 0)))

(test-equal (text-slice "hello" 1 3) "ell")
(test-equal (text-slice "hello" 5 0) "")
(test-equal (text-slice "aéb" 1 2) "é")
(check-exn #rx"out of bounds" (λ () (text-slice "hello" 3 3)))
(check-exn #rx"splits a UTF-8 character" (λ () (text-slice "aéb" 1 1)))

(test-equal (text-upd "abc" 1 "x") "axc")
(check-exn #rx"out of bounds" (λ () (text-upd "abc" 3 "x")))
(check-exn #rx"length 1 updated with one of length 2" (λ () (text-upd "abc" 1 "xy")))
(check-exn #rx"length 1 updated with one of length 2" (λ () (text-upd "abc" 1 "é")))

(test-equal (text-upd-slice "hello" 1 3 "ELL") "hELLo")
(test-equal (text-upd-slice "hello" 1 2 "é") "hélo")
(check-exn #rx"out of bounds" (λ () (text-upd-slice "hello" 4 2 "xy")))
(check-exn #rx"length 3 updated with one of length 2" (λ () (text-upd-slice "hello" 1 3 "xy")))

;; debug writes to stderr.
(test-equal (let ([err (open-output-string)])
              (parameterize ([current-error-port err]) (debug '(NAT 1)))
              (get-output-string err))
            "(NAT 1)\n")

(test-results)
