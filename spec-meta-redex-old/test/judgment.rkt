#lang racket/base
;; Helpers for testing judgment forms.

(require racket/list
         racket/port
         racket/set
         rackunit
         "../common/0.0-prelude.rkt")
(provide outputs
         check-rules-used
         count-calls
         ctx-of)

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
;; (check-rules-used J #:except (rule-name ...))
;;
;; Checks that every rule of J, except the ones named, appears in some
;; derivation `outputs` has built. Derivations under a `judgment-holds` premise
;; are not built, so their rules need tests of their own.
(define-syntax check-rules-used
  (syntax-rules ()
    [(_ J) (check-rules-used J #:except ())]
    [(_ J #:except (name ...))
     (check-equal?
      (for/list ([rule (in-list (judgment-form->rule-names J))]
                 #:unless (member (symbol->string rule) '(name ...))
                 #:unless (set-member? used (cons 'J (symbol->string rule))))
        rule)
      '()
      (format "rules of ~a in no derivation" 'J))]))

;; The number of calls to the judgment named J while running thunk, counted in
;; its trace
(define (count-calls J thunk)
  (define trace
    (parameterize ([current-traced-metafunctions (list J)])
      (with-output-to-string thunk)))
  (length (regexp-match* (pregexp (format "(?m:^ *>[ >]*(?:\\[[0-9]+\\] *)?\\(~a\\s)" J))
                         trace)))

;; A context with the given global and local layers, each a list of the
;; TYP, REL, FUNC, and VAL maps.
(define (ctx-of global local)
  (define (layer maps) (append-map list '(TYP REL FUNC VAL) maps))
  (list 'GLOBAL (layer global) 'LOCAL (layer local)))
