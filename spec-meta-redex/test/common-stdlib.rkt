#lang racket/base

(require racket/port
         rackunit
         "../common/0.0-prelude.rkt"
         "../common/0.1-stdlib.rkt")

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

;; The first pair with the key
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

;; The first map with the key
(test-equal (term (find-maps () a)) '())
(test-equal (term (find-maps (() ((b 1))) a)) '())
(test-equal (term (find-maps (((a 1)) ((a 2))) a)) '(1))
(test-equal (term (find-maps (((b 1)) ((a 2))) a)) '(2))

;; A new key goes last; an existing key is replaced where it first stands.
(test-equal (term (add-map () a 1)) '((a 1)))
(test-equal (term (add-map ((a 1) (b 2)) c 3)) '((a 1) (b 2) (c 3)))
(test-equal (term (add-map ((a 1) (b 2)) a 3)) '((a 3) (b 2)))
(test-equal (term (add-map ((a 1) (b 2) (a 4)) a 3)) '((a 3) (b 2) (a 4)))

(test-equal (term (adds-map ((a 1)) () ())) '((a 1)))
(test-equal (term (adds-map ((a 1)) (b a) (2 3))) '((a 3) (b 2)))
(test-equal (term (adds-map () (a a) (1 2))) '((a 2)))
(check-exn #rx"adds-map" (λ () (term (adds-map () (a b) (1)))))

;;
;; Texts and lists
;;

;; Texts are measured and indexed in UTF-8 bytes.
(check-equal? (text-len "") 0)
(check-equal? (text-len "aé") 3)

(check-equal? (text-idx "abc" 2) "c")
(check-equal? (text-idx "éa" 2) "a")
(check-exn #rx"text-idx: index 3 out of bounds \\[0, 3\\)" (λ () (text-idx "abc" 3)))
(check-exn #rx"text-idx: the result splits a UTF-8 character" (λ () (text-idx "é" 1)))

;; A slice is a start and a length.
(check-equal? (text-slice "abcd" 1 2) "bc")
(check-equal? (text-slice "abcd" 4 0) "")
(check-equal? (text-slice "aéb" 1 2) "é")
(check-exn #rx"text-slice: slice \\[3, 5\\) out of bounds \\[0, 4\\)" (λ () (text-slice "abcd" 3 2)))
(check-exn #rx"text-slice: the result splits" (λ () (text-slice "aéb" 0 2)))

(check-equal? (text-upd "abc" 1 "z") "azc")
(check-equal? (text-upd "aé" 0 "x") "xé")
(check-exn #rx"text-upd: index 3 out of bounds" (λ () (text-upd "abc" 3 "z")))
(check-exn #rx"text-upd: the replacement has length 2 instead of 1" (λ () (text-upd "abc" 0 "zz")))
(check-exn #rx"text-upd: the result splits" (λ () (text-upd "é" 1 "x")))

(check-equal? (text-upd-slice "abcd" 1 2 "xy") "axyd")
(check-equal? (text-upd-slice "abcd" 0 0 "") "abcd")
(check-equal? (text-upd-slice "aéb" 1 2 "ü") "aüb")
(check-exn #rx"text-upd-slice: slice \\[3, 5\\) out of bounds" (λ () (text-upd-slice "abcd" 3 2 "xy")))
(check-exn #rx"text-upd-slice: the replacement has length 1 instead of 2"
           (λ () (text-upd-slice "abcd" 1 2 "x")))
(check-exn #rx"text-upd-slice: the result splits" (λ () (text-upd-slice "aé" 1 1 "x")))

(check-equal? (list-idx '(a b c) 0) 'a)
(check-exn #rx"list-idx: index 3 out of bounds \\[0, 3\\)" (λ () (list-idx '(a b c) 3)))

(check-equal? (list-slice '(a b c d) 1 2) '(b c))
(check-equal? (list-slice '(a b) 2 0) '())
(check-exn #rx"list-slice: slice \\[1, 3\\) out of bounds \\[0, 2\\)" (λ () (list-slice '(a b) 1 2)))

(check-equal? (list-upd '(a b c) 2 'z) '(a b z))
(check-exn #rx"list-upd: index 3 out of bounds" (λ () (list-upd '(a b c) 3 'z)))

(check-equal? (list-upd-slice '(a b c d) 1 2 '(x y)) '(a x y d))
(check-equal? (list-upd-slice '() 0 0 '()) '())
(check-exn #rx"list-upd-slice: slice \\[1, 3\\) out of bounds" (λ () (list-upd-slice '(a b) 1 2 '(x y))))
(check-exn #rx"list-upd-slice: the replacement has length 1 instead of 2"
           (λ () (list-upd-slice '(a b) 0 2 '(x))))

;;
;; Debugging
;;

(check-equal? (with-output-to-string
                (λ ()
                  (parameterize ([current-error-port (current-output-port)])
                    (debug (term (TEXT "a\"b"))))))
              "(TEXT \"a\\\"b\")\n")

;;
;; Contracts
;;

(when contracts?
  (check-exn #rx"not in my domain" (λ () (term (ite 1 x y))))
  (check-exn #rx"not in my domain" (λ () (term (exists- (1)))))
  (check-exn #rx"not in my domain" (λ () (term (repeat- x -1))))
  (check-exn #rx"not in my domain" (λ () (term (assoc- a ((a 1 2))))))
  (check-exn #rx"not in my domain" (λ () (term (transpose- (1)))))
  (check-exn #rx"not in my domain" (λ () (term (find-map (a) a))))
  (check-exn #rx"not in my domain" (λ () (term (find-maps ((a)) a))))
  (check-exn #rx"not in my domain" (λ () (term (add-map ((a)) a 1)))))
