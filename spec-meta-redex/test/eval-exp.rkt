#lang racket/base

(require rackunit
         "../common/0.0-prelude.rkt"
         "../al/5.3-eval-exp.rkt"
         "machine.rkt")

(define coverage (start-coverage ->redex/eval-exp ->ctx/eval-exp))

;; G binds a value too, which no variable finds.
(define-term G-vals {TYP () REL () FUNC () VAL ((("g" ()) (NAT 0)))})

(define-term L-vals
  {TYP () REL () FUNC ()
       VAL ((("x" ()) (NAT 1))
            (("b" ()) (BOOL #t))
            (("i" ()) (INT -2))
            (("f" ()) (FUNC "f"))
            (("xs" (STAR)) (LIST ((NAT 2)))))})

(define (eval-exp e)
  (eval-in (term G-vals) (term L-vals) e))

(define (trace-exp e)
  (trace-in (term G-vals) (term L-vals) e))

;;
;; Literals
;;

(test-equal (eval-exp (term (BOOL #t))) (term (OK (BOOL #t))))
(test-equal (eval-exp (term (BOOL #f))) (term (OK (BOOL #f))))
(test-equal (eval-exp (term (NAT 3))) (term (OK (NAT 3))))
(test-equal (eval-exp (term (INT -3))) (term (OK (INT -3))))
(test-equal (eval-exp (term (TEXT "a"))) (term (OK (TEXT "a"))))
(test-equal (trace-exp (term (NAT 3))) '("eval-exp/literal/number"))

;;
;; Variables
;;

(test-equal (eval-exp (term (VAR "x"))) (term (OK (NAT 1))))
(test-equal (eval-exp (term (VAR "f"))) (term (OK (FUNC "f"))))
;; otherwise: unbound, bound only with iterators, or bound only in G
(test-equal (eval-exp (term (VAR "y"))) 'FAIL)
(test-equal (eval-exp (term (VAR "xs"))) 'FAIL)
(test-equal (eval-exp (term (VAR "g"))) 'FAIL)
(test-equal (trace-exp (term (VAR "y"))) '("eval-exp/variable/fail"))

;;
;; Unary operators
;;

(test-equal (eval-exp (term (UN NOT (BOOL #t)))) (term (OK (BOOL #f))))
(test-equal (eval-exp (term (UN NOT (VAR "b")))) (term (OK (BOOL #f))))
(test-equal (eval-exp (term (UN PLUS (NAT 1)))) (term (OK (NAT 1))))
(test-equal (eval-exp (term (UN PLUS (VAR "i")))) (term (OK (INT -2))))
(test-equal (eval-exp (term (UN MINUS (VAR "x")))) (term (OK (INT -1))))
(test-equal (eval-exp (term (UN MINUS (INT -2)))) (term (OK (INT 2))))
(test-equal (eval-exp (term (UN NOT (UN NOT (BOOL #f))))) (term (OK (BOOL #f))))
(test-equal (eval-exp (term (UN MINUS (UN MINUS (NAT 2))))) (term (OK (INT 2))))
(test-equal (trace-exp (term (UN MINUS (VAR "x"))))
            '("eval-exp/variable" "eval-exp/unary/number"))
;; otherwise: an operand of the wrong kind
(test-equal (eval-exp (term (UN NOT (NAT 1)))) 'FAIL)
(test-equal (eval-exp (term (UN NOT (TUP ())))) 'FAIL)
(test-equal (eval-exp (term (UN PLUS (BOOL #t)))) 'FAIL)
(test-equal (eval-exp (term (UN MINUS (TEXT "1")))) 'FAIL)
(test-equal (eval-exp (term (UN MINUS (VAR "f")))) 'FAIL)
(test-equal (trace-exp (term (UN NOT (NAT 1))))
            '("eval-exp/literal/number" "eval-exp/unary/fail"))
;; A failing operand fails the operator.
(test-equal (eval-exp (term (UN NOT (VAR "y")))) 'FAIL)
(test-equal (trace-exp (term (UN NOT (VAR "y"))))
            '("eval-exp/variable/fail" "frame/fail"))

;;
;; Tuples
;;

(test-equal (eval-exp (term (TUP ()))) (term (OK (TUP ()))))
(test-equal (eval-exp (term (TUP ((NAT 1) (VAR "x") (UN NOT (BOOL #f))))))
            (term (OK (TUP ((NAT 1) (NAT 1) (BOOL #t))))))
(test-equal (eval-exp (term (TUP ((TUP ()) (TUP ((TEXT "a") (VAR "b")))))))
            (term (OK (TUP ((TUP ()) (TUP ((TEXT "a") (BOOL #t))))))))
;; Left to right
(test-equal (trace-exp (term (TUP ((VAR "x") (NAT 1) (UN MINUS (NAT 2))))))
            '("eval-exp/variable" "eval-exp/literal/number"
              "eval-exp/literal/number" "eval-exp/unary/number" "eval-exp/tuple"))
;; The first failing element fails the tuple, and no later one is evaluated.
(test-equal (eval-exp (term (TUP ((NAT 1) (VAR "y") (BIN DIV (NAT 1) (NAT 0)))))) 'FAIL)
(test-equal (trace-exp (term (TUP ((NAT 1) (VAR "y") (BIN DIV (NAT 1) (NAT 0))))))
            '("eval-exp/literal/number" "eval-exp/variable/fail" "frame/fail"))
(test-equal (eval-exp (term (TUP ((TUP ((UN NOT (NAT 0)))) (BIN DIV (NAT 1) (NAT 0))))))
            'FAIL)

(check-coverage coverage)
