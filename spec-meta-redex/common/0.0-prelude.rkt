#lang racket/base
;; Required by every module in place of Redex itself.

(require (for-syntax racket/base
                     racket/list
                     syntax/parse)
         racket/match
         redex/reduction-semantics)
(provide (all-from-out redex/reduction-semantics)
         define-dec
         contracts?
         reduction-relation/forms
         union-reduction-relations/forms
         term-head
         relation-for-head)

;; Safe only while impure operations stay in reduction rules, which Redex
;; does not cache. See CROSS_REDEX.md, "Caching".
(caching-enabled? #t)

;; SPECTEC_REDEX_CONTRACTS=0 drops the contracts of define-dec. It is read when
;; a module using define-dec is compiled.
(begin-for-syntax
  (define contracts-on? (not (equal? (getenv "SPECTEC_REDEX_CONTRACTS") "0"))))

(define-syntax (compiled-contracts? _stx)
  #`#,contracts-on?)
(define contracts? (compiled-contracts?))

;; (define-dec lang f : dom ... -> range (∨ range) ... clause ...)
;;
;; A watsup `dec` and its `def`s, as a metafunction. A last clause returns ⊥
;; when no other applies, so a call is partial rather than an error. A caller
;; binds the result with `where` against a pattern narrower than `any`, or
;; returns it as its own result.
(define-syntax (define-dec stx)
  (syntax-parse stx
    [(_ lang f (~datum :) dom ... (~datum ->)
        range (~seq (~datum ∨) range-alt) ...
        clause ...)
     (with-syntax ([(contract ...)
                    (if contracts-on?
                        #'(f : dom ... -> range (~@ ∨ range-alt) ... ∨ ⊥)
                        #'())])
       #'(define-metafunction lang
           contract ...
           clause ...
           [(f any (... ...)) ⊥]))]))
;; (reduction-relation/forms lang rule ...)
;;
;; The reduction-relation of the rules, built as the union of one relation per
;; run of consecutive rules whose left-hand sides have the same head symbol, so
;; relation-for-head can pick the rules for a term's head. A left-hand side
;; (s ...) has the head s, and ((s ...) ...), as a focus triple, has the head
;; of its first element. A rule without a literal head applies to every head.
(define-syntax (reduction-relation/forms stx)
  (syntax-parse stx
    [(_ lang rule ...)
     (define rules (syntax->list #'(rule ...)))
     (define heads
       (for/list ([rule (in-list rules)])
         (syntax-parse rule
           [((~datum -->) lhs _ ...) (pattern-head (syntax->datum #'lhs))]
           [_ (raise-syntax-error #f "expected a --> rule" stx rule)])))
     ;; Runs of consecutive rules with the same head, as (head rule ...)
     (define runs
       (reverse
        (for/fold ([runs '()]) ([rule (in-list rules)] [head (in-list heads)])
          (if (and (pair? runs) (eq? (caar runs) head))
              (cons (append (car runs) (list rule)) (cdr runs))
              (cons (list head rule) runs)))))
     (with-syntax ([((head run-rule ...) ...) runs]
                   [(literal-head ...) (remove-duplicates (filter values heads))])
       #'(make-forms-relation
          (list (cons 'head (reduction-relation lang run-rule ...)) ...)
          (list (cons 'literal-head (literal-pattern? (redex-match lang literal-head)
                                                      'literal-head))
                ...)))]))

(begin-for-syntax
  ;; Patterns that are lists headed by a keyword, not by a literal
  (define pattern-keywords
    '(name in-hole hide-hole side-condition cross compatible-closure-context
           variable-except variable-prefix mismatch-name nt))

  (define (ellipsis? d)
    (and (symbol? d) (regexp-match? #rx"^[.][.][.]" (symbol->string d))))

  ;; The symbol that every term matching the pattern d starts with, if d says
  ;; so by its shape. Whether it is a literal is checked at run time.
  (define (pattern-head d)
    (and (pair? d)
         (not (and (pair? (cdr d)) (ellipsis? (cadr d))))
         (cond
           [(symbol? (car d)) (and (not (memq (car d) pattern-keywords)) (car d))]
           [(pair? (car d)) (pattern-head (car d))]
           [else #f]))))

;; Whether the pattern of the matcher m is the literal symbol s
(define (literal-pattern? m s)
  (and (m s) (not (m (string->uninterned-symbol (symbol->string s)))) #t))

;; The head of a term, as pattern-head gives it for a pattern
(define (term-head t)
  (and (pair? t)
       (cond
         [(symbol? (car t)) (car t)]
         [(pair? (car t)) (term-head (car t))]
         [else #f])))

;; The relations of each literal head, in order, the ones of every head, and
;; the relations already built by relation-for-head
(struct forms (by-head any-head cache))

;; The forms of the relations that reduction-relation/forms and
;; union-reduction-relations/forms build
(define forms-registry (make-weak-hasheq))

(define (make-forms-relation runs literal?)
  (define-values (by-head any-head)
    (for/fold ([by-head (hasheq)] [any-head '()] #:result (values by-head (reverse any-head)))
              ([run (in-list runs)])
      (match-define (cons head rel) run)
      (if (and head (cdr (assq head literal?)))
          (values (hash-update by-head head (λ (rels) (append rels (list rel))) '()) any-head)
          (values by-head (cons rel any-head)))))
  (register-forms (union-all (map cdr runs)) by-head any-head))

(define (register-forms rel by-head any-head)
  (hash-set! forms-registry rel (forms by-head any-head (make-hasheq)))
  rel)

(define (union-all rels)
  (match rels
    ['() #f]
    [(list rel) rel]
    [_ (apply union-reduction-relations rels)]))

;; The union of the relations, with their forms. A relation without forms
;; applies to every head.
(define (union-reduction-relations/forms . rels)
  (define-values (by-head any-head)
    (for/fold ([by-head (hasheq)] [any-head '()])
              ([rel (in-list rels)])
      (match (hash-ref forms-registry rel #f)
        [(forms by-head_r any-head_r _)
         (values (for/fold ([by-head by-head]) ([(head rels_h) (in-hash by-head_r)])
                   (hash-update by-head head (λ (rels) (append rels rels_h)) '()))
                 (append any-head any-head_r))]
        [#f (values by-head (append any-head (list rel)))])))
  (register-forms (union-all rels) by-head any-head))

;; The rules of rel that can apply to a term with the head symbol head, as one
;; relation, or #f if there are none. A relation without forms is all of rel.
(define (relation-for-head rel head)
  (match (hash-ref forms-registry rel #f)
    [#f rel]
    [(forms by-head any-head cache)
     (hash-ref! cache head
                (λ () (union-all (append (hash-ref by-head head '()) any-head))))]))
