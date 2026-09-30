#lang racket/base
;; `$main()` of the examples that need no builtins, evaluated under the
;; context their script loads to. The expected values are the meta-circular
;; oracle's. fibo, iter-sequence, and mutual-recursion also agree, but take
;; 10 to 50 s each, so they are left out.

(require racket/runtime-path
         "../common/0.0-prelude.rkt"
         "../al/0-boot.rkt"
         "../al/3-context.rkt"
         "../al/5-eval.rkt"
         "judgment.rkt")

(define-runtime-path examples "../../examples")

;; The results of $main(), and what its debug premises wrote
(define (run-main/debug name)
  (define C (term (load (empty-ctx) ,(boot-script (build-path examples name)))))
  (define err (open-output-string))
  (define results
    (parameterize ([current-error-port err])
      (outputs (eval-exp ,C (CALL "main" () ()) any))))
  (list results (get-output-string err)))

(define (run-main name) (car (run-main/debug name)))

(test-equal (run-main/debug "add.watsup") '(((OK (INT 119))) "(TEXT \"Add\")\n"))
(test-equal (run-main "iter-nontrivial.watsup") '((OK (INT -42))))
(test-equal (run-main "relation-typing.watsup") '((OK (INT 110))))
(test-equal (run-main "variant-tree.watsup") '((OK (INT 6))))

(test-results)
