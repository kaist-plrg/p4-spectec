#lang racket/base

(require "../common/0.0-prelude.rkt"
         "../common/5.0-eval-typ.rkt")

(define-term theta-X (("X" INT)))

;;
;; $subst_typ
;;

;; An empty theta leaves every type as it is, even ones $subst_type_inner rejects.
(test-equal (term (subst-typ () (VAR "X" ()))) '(VAR "X" ()))
(test-equal (term (subst-typ () (VAR "X" (NAT)))) '(VAR "X" (NAT)))

;; otherwise: $subst_type_inner
(test-equal (term (subst-typ theta-X (VAR "X" ()))) 'INT)
(test-equal (term (subst-typ theta-X (VAR "X" (NAT)))) '⊥)

;;
;; $subst_type_inner
;;

(test-equal (term (subst-type-inner theta-X NAT)) 'NAT)
(test-equal (term (subst-type-inner theta-X INT)) 'INT)
(test-equal (term (subst-type-inner theta-X TEXT)) 'TEXT)
(test-equal (term (subst-type-inner theta-X BOOL)) 'BOOL)
(test-equal (term (subst-type-inner theta-X FUNC)) 'FUNC)
(test-equal (term (subst-type-inner () NAT)) 'NAT)

;; VAR: bound without type arguments, or unbound
(test-equal (term (subst-type-inner theta-X (VAR "X" ()))) 'INT)
(test-equal (term (subst-type-inner theta-X (VAR "Y" ()))) '(VAR "Y" ()))
(test-equal (term (subst-type-inner theta-X (VAR "list" ((VAR "X" ()) NAT))))
            '(VAR "list" (INT NAT)))

;; No clause for a bound VAR with type arguments: ⊥
(test-equal (term (subst-type-inner theta-X (VAR "X" (NAT)))) '⊥)

;; Substitution does not apply to its own result.
(test-equal (term (subst-type-inner (("X" (VAR "Y" ())) ("Y" NAT)) (VAR "X" ())))
            '(VAR "Y" ()))

(test-equal (term (subst-type-inner theta-X (TUP ()))) '(TUP ()))
(test-equal (term (subst-type-inner theta-X (TUP ((VAR "X" ()) BOOL)))) '(TUP (INT BOOL)))
(test-equal (term (subst-type-inner theta-X (ITER (VAR "X" ()) STAR))) '(ITER INT STAR))
(test-equal (term (subst-type-inner theta-X (ITER (ITER (VAR "X" ()) QUEST) STAR)))
            '(ITER (ITER INT QUEST) STAR))

;; ⊥ inside a component
(test-equal (term (subst-type-inner theta-X (TUP (NAT (VAR "X" (NAT)))))) '⊥)
(test-equal (term (subst-type-inner theta-X (ITER (VAR "X" (NAT)) STAR))) '⊥)
(test-equal (term (subst-type-inner theta-X (VAR "list" ((VAR "X" (NAT)))))) '⊥)

(test-results)
