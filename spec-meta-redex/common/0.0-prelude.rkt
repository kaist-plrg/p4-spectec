#lang racket/base
;; Required by every module in place of Redex itself.

(require (for-syntax racket/base
                     racket/list
                     syntax/parse)
         racket/match
         redex/reduction-semantics)
(provide (all-from-out redex/reduction-semantics)
         reduction-relation/forms
         union-reduction-relations/forms
         apply-first)

;; Safe only while impure operations stay in reduction rules, which Redex
;; does not cache. See CROSS_REDEX.md, "Caching".
(caching-enabled? #t)

;; (reduction-relation/forms lang rule ...)
;;
;; The reduction-relation of the rules. It keeps each rule as a relation of its
;; own, with the head symbol of its left-hand side, so apply-first can try the
;; rules for a term's head one by one. A left-hand side (s ...) has the head s,
;; and ((s ...) ...), as a focus triple, has the head of its first element. A
;; rule without a literal head applies to every head.
(define-syntax (reduction-relation/forms stx)
  (syntax-parse stx
    [(_ lang rule ...)
     (define heads
       (for/list ([rule (in-list (syntax->list #'(rule ...)))])
         (syntax-parse rule
           [((~datum -->) lhs _ ...) (pattern-head (syntax->datum #'lhs))]
           [_ (raise-syntax-error #f "expected a --> rule" stx rule)])))
     (with-syntax ([(head ...) heads]
                   [(literal-head ...) (remove-duplicates (filter values heads))])
       #'(make-forms-relation
          (list (cons 'head (reduction-relation lang rule)) ...)
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

;; The rules of a relation, as (head . relation) pairs in order, where the head
;; #f applies to every head, and the rules already picked for each head
(struct forms (rules by-head))

;; The forms of the relations that reduction-relation/forms and
;; union-reduction-relations/forms build
(define forms-registry (make-weak-hasheq))

(define (make-forms-relation rules literal?)
  (register-forms
   (for/list ([head+rel (in-list rules)])
     (match-define (cons head rel) head+rel)
     (cons (and head (cdr (assq head literal?)) head) rel))))

;; The union of the rules' relations, registered with the rules
(define (register-forms rules)
  (define rel
    (match (map cdr rules)
      [(list rel) rel]
      [rels (apply union-reduction-relations rels)]))
  (hash-set! forms-registry rel (forms rules (make-hasheq)))
  rel)

;; The union of the relations, with their rules in order
(define (union-reduction-relations/forms . rels)
  (register-forms
   (for*/list ([rel (in-list rels)]
               [head+rel (in-list (forms-rules (hash-ref forms-registry rel)))])
     head+rel)))

;; The first result of the first rule of rel that applies to t, as (name t_1),
;; or #f if none applies. Only the rules for t's head are tried, in order.
(define (apply-first rel t)
  (match-define (forms rules by-head) (hash-ref forms-registry rel))
  (define head (term-head t))
  (define rels_h
    (hash-ref! by-head head
               (λ ()
                 (for/list ([head+rel (in-list rules)]
                            #:when (memq (car head+rel) (list #f head)))
                   (cdr head+rel)))))
  (for*/first ([rel_r (in-list rels_h)]
               [result (in-list (apply-reduction-relation/tag-with-names rel_r t))])
    result))
