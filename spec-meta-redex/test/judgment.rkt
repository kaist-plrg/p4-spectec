#lang racket/base
;; Helpers for testing judgment forms.

(require racket/list
         racket/set
         rackunit
         "../common/0.0-prelude.rkt")
(provide outputs
         check-rules-used)

;; (judgment-name . rule-name) of each derivation `outputs` has built
(define used (mutable-set))

(define (record! d)
  (set-add! used (cons (car (derivation-term d)) (derivation-name d)))
  (for-each record! (derivation-subs d)))

;; (outputs (J input ... output-pattern))
;;
;; The last position of each derivation of J, as a list of at most one. More
;; than one derivation means overlapping rules, and raises an error.
(define-syntax-rule (outputs judgment)
  (let ([ds (build-derivations judgment)])
    (unless (<= (length ds) 1)
      (error 'outputs "~a derivations, by rules ~s, of ~s"
             (length ds) (map derivation-name ds) 'judgment))
    (for-each record! ds)
    (for/list ([d (in-list ds)])
      (last (derivation-term d)))))

;; (check-rules-used J)
;;
;; Checks that every rule of J appears in some derivation `outputs` has built.
(define-syntax-rule (check-rules-used J)
  (check-equal?
   (for/list ([name (in-list (judgment-form->rule-names J))]
              #:unless (set-member? used (cons 'J (symbol->string name))))
     name)
   '()
   (format "rules of ~a in no derivation" 'J)))
