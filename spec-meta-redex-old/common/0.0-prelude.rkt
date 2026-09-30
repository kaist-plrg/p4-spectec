#lang racket/base
;; Required by every module in place of Redex itself.

(require (for-syntax racket/base
                     syntax/parse)
         redex/reduction-semantics)
(provide (all-from-out redex/reduction-semantics)
         define-dec
         define-relation
         contracts?)

;; AL relations reach host state (externs, `$fresh_typeId`, `debug`), and
;; Redex's cache does not know it. See CROSS_REDEX.md, "Caching".
(caching-enabled? #f)

;; SPECTEC_REDEX_CONTRACTS=0 drops the contracts of define-dec and
;; define-relation. It is read when a module using them is compiled.
(begin-for-syntax
  (define contracts-on? (not (equal? (getenv "SPECTEC_REDEX_CONTRACTS") "0"))))

(define-syntax (compiled-contracts? _stx)
  #`#,contracts-on?)
(define contracts? (compiled-contracts?))

;; (define-dec lang f : dom ... -> range clause ...)
;;
;; A watsup `dec` and its `def`s, as a metafunction. A last clause returns ⊥
;; when no other applies, so a call is partial rather than an error. A caller
;; binds the result with `where` against a pattern narrower than `any`, or
;; returns it as its own result.
(define-syntax (define-dec stx)
  (syntax-parse stx
    [(_ lang f (~datum :) dom ... (~datum ->) range clause ...)
     (with-syntax ([(contract ...)
                    (if contracts-on? #'(f : dom ... -> range ∨ ⊥) #'())])
       #'(define-metafunction lang
           contract ...
           clause ...
           [(f any (... ...)) ⊥]))]))

;; (define-relation lang #:mode mode #:contract contract rule ...)
;;
;; A watsup `relation` and its `rule`s, as a judgment form.
(define-syntax (define-relation stx)
  (syntax-parse stx
    [(_ lang #:mode mode #:contract contract rule ...)
     (with-syntax ([(contract-kw ...)
                    (if contracts-on? #'(#:contract contract) #'())])
       #'(define-judgment-form lang
           #:mode mode
           contract-kw ...
           rule ...))]))
