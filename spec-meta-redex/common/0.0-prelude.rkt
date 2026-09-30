#lang racket/base
;; Required by every module in place of Redex itself.

(require (for-syntax racket/base
                     syntax/parse)
         redex/reduction-semantics)
(provide (all-from-out redex/reduction-semantics)
         define-dec
         contracts?)

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
