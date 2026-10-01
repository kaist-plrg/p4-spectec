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

;; reduction-relation/forms: the rules for a head symbol, and the rules
;; without a literal head, which apply to every head.
(define-language forms-lang
  (e ::= (A e) (B e) (C e ...) n)
  (op ::= A B)
  (n ::= natural))

(define ->forms
  (reduction-relation/forms
   forms-lang
   (--> (A n) n "a")
   (--> ((B n) any_G any_L) n "b/triple")
   (--> (A (B n)) n "a/b")
   (--> n (A n) "bare")
   (--> (op 0) 0 "nonterminal")
   (--> (e ... (C)) (C) "ellipsis")
   (--> (in-hole (C hole) 0) 0 "in-hole")))

(define (rule-names-for rel head)
  (define rel_h (relation-for-head rel head))
  (if rel_h (sort (map symbol->string (reduction-relation->rule-names rel_h)) string<?) '()))

(define any-head '("bare" "ellipsis" "in-hole" "nonterminal"))
(check-equal? (rule-names-for ->forms 'A) (sort (list* "a" "a/b" any-head) string<?))
(check-equal? (rule-names-for ->forms 'B) (sort (list* "b/triple" any-head) string<?))
(check-equal? (rule-names-for ->forms 'C) any-head)
(check-equal? (rule-names-for ->forms #f) any-head)
(check-equal? (length (reduction-relation->rule-names ->forms)) 7)

(check-equal? (term-head (term (A 1))) 'A)
(check-equal? (term-head (term ((B 1) G L))) 'B)
(check-equal? (term-head (term 1)) #f)

;; A union keeps the heads of its relations. A relation that
;; reduction-relation/forms did not build applies to every head.
(define ->plain (reduction-relation forms-lang (--> (C) 0 "plain")))
(define ->union
  (union-reduction-relations/forms
   ->forms
   (reduction-relation/forms forms-lang (--> (A 0) 0 "a/zero"))
   ->plain))

(check-equal? (rule-names-for ->union 'A)
              (sort (list* "a" "a/b" "a/zero" "plain" any-head) string<?))
(check-equal? (rule-names-for ->union 'B)
              (sort (list* "b/triple" "plain" any-head) string<?))
(check-eq? (relation-for-head ->plain 'A) ->plain)

;; A head with no rules, in a relation where every rule has a head
(check-false (relation-for-head (reduction-relation/forms forms-lang (--> (A 0) 0 "only-a")) 'B))
