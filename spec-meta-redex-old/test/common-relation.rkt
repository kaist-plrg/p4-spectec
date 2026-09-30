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
;; Extern relations: stubs until the host is reachable
;;

(check-exn #rx"call-extern-func"
           (λ () (judgment-holds (call-extern-func "f" (NAT) ((NAT 1)) valres)
                                 valres)))
(check-exn #rx"call-builtin-func"
           (λ () (judgment-holds (call-builtin-func "f" () () valres) valres)))
(check-exn #rx"call-extern-rel"
           (λ () (judgment-holds (call-extern-rel "R" ((NAT 1)) valsres)
                                 valsres)))

(test-results)
