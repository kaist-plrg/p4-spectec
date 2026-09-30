#lang racket/base

(require "../common/0.0-prelude.rkt"
         "../al/5-eval.rkt"
         "judgment.rkt")

;; Globally, the function id. Locally, the aliases X and Y, a parameter P,
;; and x.
(define C-arg
  (ctx-of '(() () (("id" (DEF () ((((EXP (VAR "x"))) (VAR "x") ())) ()))) ())
          '((("X" (DEF () (ALIAS NAT))) ("Y" (DEF () (ALIAS (VAR "X" ())))) ("P" PARAM))
            ()
            ()
            ((("x" ()) (NAT 1))))))

(define boom '(BIN DIV (NAT 1) (NAT 0)))
(define none '(VAR "none"))

;;
;; eval-arg
;;

(define (run-eval-arg arg) (outputs (eval-arg ,C-arg ,arg any)))

(test-equal (run-eval-arg '(EXP (VAR "x"))) '((OK (NAT 1))))
(test-equal (run-eval-arg `(EXP ,none)) '(FAIL))
;; A function argument is not looked up.
(test-equal (run-eval-arg '(FUN "id")) '((OK (FUNC "id"))))
(test-equal (run-eval-arg '(FUN "none")) '((OK (FUNC "none"))))

;;
;; eval-args
;;

(define (run-eval-args args) (outputs (eval-args ,C-arg ,args any)))

(test-equal (run-eval-args '()) '((OK ())))
(test-equal (run-eval-args '((EXP (VAR "x")) (FUN "id"))) '((OK ((NAT 1) (FUNC "id")))))
;; It stops at the first FAIL.
(test-equal (run-eval-args `((EXP (VAR "x")) (EXP ,none) (EXP ,boom))) '(FAIL))

;;
;; eval-targs
;;

(define (run-eval-targs targs [C C-arg]) (outputs (eval-targs ,C ,targs any)))

(test-equal (run-eval-targs '()) '(()))
(test-equal (run-eval-targs '(BOOL (VAR "X" ()) (ITER (VAR "X" ()) STAR)))
            '((BOOL NAT (ITER NAT STAR))))
;; Substitution is not repeated, so Y becomes X.
(test-equal (run-eval-targs '((VAR "Y" ()))) '(((VAR "X" ()))))
;; A parameter, or a type not in the local layer, stays.
(test-equal (run-eval-targs '((VAR "P" ()) (VAR "Z" (NAT)))) '(((VAR "P" ()) (VAR "Z" (NAT)))))
;; Without local types, nothing is substituted.
(test-equal (run-eval-targs '((VAR "X" ())) (ctx-of '(() () () ()) '(() () () ())))
            '(((VAR "X" ()))))
;; No derivation: an alias with type arguments, or local types that
;; $theta_of_tdenv does not take
(test-equal (run-eval-targs '((VAR "X" (NAT)))) '())
(test-equal (run-eval-targs '(NAT)
                            (ctx-of '(() () () ()) '((("F" (DEF ("A") (ALIAS NAT)))) () () ())))
            '())

;; Nesting: each argument is evaluated once.
(define (nested-call depth)
  (for/fold ([exp '(VAR "x")]) ([_ (in-range depth)])
    `(CALL "id" () ((EXP ,exp)))))

(define (arg-calls depth)
  (count-calls 'eval-arg (λ () (outputs (eval-exp ,C-arg ,(nested-call depth) any)))))

(test-equal (outputs (eval-exp ,C-arg ,(nested-call 3) any)) '((OK (NAT 1))))
(test-equal (arg-calls 10) 10)

;; Every rule appears in some derivation above.
(check-rules-used eval-arg)
(check-rules-used eval-arg/exp)
(check-rules-used eval-args)
(check-rules-used eval-args/cons)
(check-rules-used eval-targs)

(test-results)
