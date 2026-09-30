#lang racket/base
;; spec-meta/al/5.3-eval-exp.watsup to 5.7-eval-call-rel.watsup.
;;
;; The relations are mutually recursive, so the five files are fragments
;; included here.

(require racket/include
         "../common/0.0-prelude.rkt"
         "../common/0.1-stdlib.rkt"
         "../common/2-env.rkt"
         "../common/4-relation.rkt"
         "../common/5.0-eval-typ.rkt"
         "../common/5.1-eval-ops.rkt"
         "3-context.rkt"
         "4-relation.rkt"
         "5.1-eval-typ.rkt"
         "5.2-eval-assign.rkt")
;; The auxiliary judgments too, for the tests
(provide (all-defined-out))

(include "5.3-eval-exp.rktl")
(include "5.4-eval-arg.rktl")
(include "5.5-eval-prem.rktl")
(include "5.6-eval-call-func.rktl")
(include "5.7-eval-call-rel.rktl")
