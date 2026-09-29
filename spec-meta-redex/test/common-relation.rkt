#lang racket/base

(require rackunit
         "../common/0-prelude.rkt"
         "../common/4-relation.rkt")

;;
;; Language
;;

(test-match Common-relation unitres (term OK))
(test-match Common-relation unitres (term FAIL))
(test-no-match Common-relation unitres (term (OK (NAT 1))))

(test-match Common-relation valres (term (OK (NAT 1))))
(test-match Common-relation valres (term FAIL))
(test-no-match Common-relation valres (term OK))
(test-no-match Common-relation valres (term (OK ((NAT 1)))))

(test-match Common-relation valsres (term (OK ())))
(test-match Common-relation valsres (term (OK ((NAT 1) (BOOL #t)))))
(test-match Common-relation valsres (term FAIL))
(test-no-match Common-relation valsres (term (OK (NAT 1))))

;;
;; Extern relations: stubs until the host is reachable
;;

(check-exn #rx"Call_extern_func"
           (λ () (judgment-holds (Call_extern_func "f" (NAT) ((NAT 1)) valres)
                                 valres)))
(check-exn #rx"Call_builtin_func"
           (λ () (judgment-holds (Call_builtin_func "f" () () valres) valres)))
(check-exn #rx"Call_extern_rel"
           (λ () (judgment-holds (Call_extern_rel "R" ((NAT 1)) valsres)
                                 valsres)))

(test-results)
