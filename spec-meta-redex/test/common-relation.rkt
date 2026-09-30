#lang racket/base

(require rackunit
         "../common/0.0-prelude.rkt"
         "../common/4-relation.rkt")

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
;; Host procedures: stubs until the host is reachable
;;

(check-exn #rx"host-call-extern-func"
           (λ () (host-call-extern-func "f" '(NAT) '((NAT 1)))))
(check-exn #rx"host-call-builtin-func"
           (λ () (host-call-builtin-func "f" '() '())))
(check-exn #rx"host-call-extern-rel"
           (λ () (host-call-extern-rel "R" '((NAT 1)))))
