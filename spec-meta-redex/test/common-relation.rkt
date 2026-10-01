#lang racket/base

(require rackunit
         "../common/0.0-prelude.rkt"
         "../common/0.3-extern-ffi.rkt"
         "../common/4-relation.rkt"
         "machine.rkt")

;;
;; Language
;;

(test-match common-relation unitres (term OK))
(test-match common-relation unitres (term FAIL))
(test-no-match common-relation unitres (term (OK (NAT 1))))

(test-match common-relation valres (term (OK (NAT 1))))
(test-match common-relation valres (term FAIL))
(test-no-match common-relation valres (term OK))
(test-no-match common-relation valres (term (OK ((NAT 1)))))

(test-match common-relation valsres (term (OK ())))
(test-match common-relation valsres (term (OK ((NAT 1) (BOOL #t)))))
(test-match common-relation valsres (term FAIL))
(test-no-match common-relation valsres (term (OK (NAT 1))))

;;
;; Host procedures
;;

(define host-path
  (text-file #<<EOF
var n : nat

dec $twice(nat) : nat
def $twice(n) = $(n + n)

relation Halves: |- nat ':' nat '+' nat
  hint(input %0)

rule Halves:
  |- n ':' n_q '+' n_r
  -- if n_q = $(n / 2)
  -- if n_r = $(n \ 2)
EOF
             ))

(parameterize ([host-spec host-path])
  (test-equal (host-call-extern-func "twice" '() (term ((NAT 4)))) (term (OK (NAT 8))))
  (test-equal (host-call-extern-func "nope" '() '()) 'FAIL)
  (test-equal (host-call-builtin-func "rev_" (term (NAT)) (term ((LIST ((NAT 1) (NAT 2))))))
              (term (OK (LIST ((NAT 2) (NAT 1))))))
  (check-exn #rx"^host: Builtin error: arity mismatch"
             (λ () (host-call-builtin-func "rev_" (term (NAT)) '())))
  (test-equal (host-call-extern-rel "Halves" (term ((NAT 7)))) (term (OK ((NAT 3) (NAT 1)))))
  (test-equal (host-call-extern-rel "Nope" '()) 'FAIL))
