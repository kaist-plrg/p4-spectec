#lang racket/base

(require rackunit
         "../common/0.0-prelude.rkt"
         "../common/0.1-stdlib.rkt")

(check-true (caching-enabled?))

;; The contract switch the macros were compiled with is the one in the
;; environment, so no stale compiled code is in use.
(check-equal? contracts? (not (equal? (getenv "SPECTEC_REDEX_CONTRACTS") "0")))

;; ⊥ when no clause applies.
(define-dec stdlib
  partial : nat -> nat
  [(partial 0) 1]
  [(partial 1) 2])

(check-equal? (term (partial 0)) 1)
(check-equal? (term (partial 1)) 2)
(check-equal? (term (partial 2)) '⊥)

;; Contracts reject inputs outside the domain.
(when contracts?
  (check-exn #rx"not in my domain" (λ () (term (partial -1))))
  (check-exn #rx"not in my domain" (λ () (term (partial "0")))))

;; A result outside the range breaks the contract; ⊥ does not.
(define-dec stdlib
  bad-range : nat -> nat
  [(bad-range 0) "0"])

(when contracts?
  (check-exn #rx"codomain test failed" (λ () (term (bad-range 0))))
  (check-equal? (term (bad-range 1)) '⊥))
