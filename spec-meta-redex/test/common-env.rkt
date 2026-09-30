#lang racket/base

(require rackunit
         "../common/0.0-prelude.rkt"
         "../common/2-env.rkt"
         "../common/3-context.rkt")

;;
;; Languages
;;

(test-match common-env varr (term ("x" ())))
(test-match common-env varr (term ("x" (STAR QUEST))))
(test-no-match common-env varr (term ("x" NAT ())))

(test-match common-env venv (term ()))
(test-match common-env venv (term ((("x" ()) (NAT 1)) (("y" (STAR)) (LIST ())))))
(test-no-match common-env venv (term ((("x" ()) 1))))

(test-match common-env typdef (term PARAM))
(test-match common-env typdef (term EXT))
(test-match common-env typdef (term (DEF () (ALIAS NAT))))
(test-match common-env typdef (term (DEF ("X") (VARIANT ()))))
(test-no-match common-env typdef (term (DEF (ALIAS NAT))))
(test-no-match common-env typdef (term (EXT (1))))

(test-match common-env tdenv (term (("t" PARAM) ("u" (DEF () (STRUCT ()))))))
(test-no-match common-env tdenv (term (("t" NAT))))

(test-match common-env theta (term (("X" NAT) ("Y" (VAR "t" ())))))
(test-no-match common-env theta (term (("X" PARAM))))

(test-match common-context cursor (term GLOBAL))
(test-match common-context cursor (term LOCAL))
(test-no-match common-context cursor (term (GLOBAL)))

;;
;; $extend_tdenv
;;

(define-term tdenv-g (("t" PARAM) ("u" EXT)))

(test-equal (term (extend-tdenv tdenv-g ())) (term tdenv-g))
(test-equal (term (extend-tdenv () ())) '())

;; otherwise: a non-empty extension
(test-equal (term (extend-tdenv tdenv-g (("v" PARAM))))
            '(("t" PARAM) ("u" EXT) ("v" PARAM)))
(test-equal (term (extend-tdenv tdenv-g (("t" (DEF () (ALIAS NAT))))))
            '(("t" (DEF () (ALIAS NAT))) ("u" EXT)))

;;
;; $theta_of_tdenv
;;

(test-equal (term (theta-of-tdenv ())) '())
(test-equal (term (theta-of-tdenv (("X" (DEF () (ALIAS NAT))))))
            '(("X" NAT)))
(test-equal (term (theta-of-tdenv (("X" PARAM) ("Y" EXT))))
            '())
(test-equal (term (theta-of-tdenv (("X" (DEF () (ALIAS NAT)))
                                   ("P" PARAM)
                                   ("Y" (DEF () (ALIAS (VAR "X" ())))))))
            '(("Y" (VAR "X" ())) ("X" NAT)))
;; The head is added after the tail, so it replaces a later duplicate.
(test-equal (term (theta-of-tdenv (("X" (DEF () (ALIAS NAT)))
                                   ("X" (DEF () (ALIAS INT))))))
            '(("X" NAT)))

;; No clause for a DEF with type parameters or without an alias: ⊥, also from
;; the tail.
(test-equal (term (theta-of-tdenv (("X" (DEF ("Z") (ALIAS NAT)))))) '⊥)
(test-equal (term (theta-of-tdenv (("X" (DEF () (STRUCT ())))))) '⊥)
(test-equal (term (theta-of-tdenv (("X" (DEF () (ALIAS NAT)))
                                   ("Y" (DEF () (VARIANT ()))))))
            '⊥)
(test-equal (term (theta-of-tdenv (("P" PARAM) ("Y" (DEF ("Z") (ALIAS NAT))))))
            '⊥)

;; A caller's premise on ⊥ fails, whether the caller is a metafunction or a
;; reduction rule, and whether it dispatches on ⊥ or not.
(define-dec common-env
  theta-or-empty : tdenv -> theta
  [(theta-or-empty tdenv) theta
   (where theta (theta-of-tdenv tdenv))]
  [(theta-or-empty tdenv) ()
   (where ⊥ (theta-of-tdenv tdenv))])

(test-equal (term (theta-or-empty (("X" (DEF () (ALIAS NAT)))))) '(("X" NAT)))
(test-equal (term (theta-or-empty (("X" (DEF ("Z") (ALIAS NAT)))))) '())

(define ->theta
  (reduction-relation
   common-env
   (--> (theta-of tdenv) theta
        (where theta (theta-of-tdenv tdenv)))))

(test-equal (apply-reduction-relation ->theta (term (theta-of (("X" (DEF () (ALIAS NAT)))))))
            '((("X" NAT))))
(test-equal (apply-reduction-relation ->theta (term (theta-of (("X" (DEF ("Z") (ALIAS NAT)))))))
            '())

;;
;; $is_iter_on_var
;;

(test-equal (term (is-iter-on-var (VAR "x"))) '(("x" ())))
(test-equal (term (is-iter-on-var (ITER (VAR "x") (STAR (("x" NAT ()))))))
            '(("x" (STAR))))
(test-equal (term (is-iter-on-var
                   (ITER (ITER (VAR "x") (QUEST (("x" NAT ()))))
                         (STAR (("x" NAT (QUEST)))))))
            '(("x" (QUEST STAR))))

;; otherwise: neither VAR nor ITER
(test-equal (term (is-iter-on-var (NAT 1))) '())
(test-equal (term (is-iter-on-var (CALL "f" () ((EXP (VAR "x")))))) '())

;; otherwise: an ITER whose premises fail
(test-equal (term (is-iter-on-var (ITER (NAT 1) (STAR ())))) '())
(test-equal (term (is-iter-on-var (ITER (VAR "x") (STAR ())))) '())
(test-equal (term (is-iter-on-var
                   (ITER (VAR "x") (STAR (("x" NAT ()) ("y" NAT ()))))))
            '())
(test-equal (term (is-iter-on-var (ITER (VAR "x") (STAR (("y" NAT ()))))))
            '())
(test-equal (term (is-iter-on-var (ITER (VAR "x") (STAR (("x" NAT (STAR)))))))
            '())
(test-equal (term (is-iter-on-var
                   (ITER (ITER (VAR "x") (QUEST (("x" NAT ()))))
                         (STAR (("x" NAT ()))))))
            '())

;;
;; $iter_vari, $iter_varr
;;

(test-equal (term (iter-vari ("x" NAT ()) STAR)) '("x" NAT (STAR)))
(test-equal (term (iter-vari ("x" NAT (QUEST)) STAR)) '("x" NAT (QUEST STAR)))
(test-equal (term (iter-varr ("x" ()) QUEST)) '("x" (QUEST)))
(test-equal (term (iter-varr ("x" (STAR)) STAR)) '("x" (STAR STAR)))

;;
;; Contracts
;;

(when contracts?
  (check-exn #rx"not in my domain" (λ () (term (extend-tdenv (("t" NAT)) ()))))
  (check-exn #rx"not in my domain" (λ () (term (theta-of-tdenv (("X" NAT))))))
  (check-exn #rx"not in my domain" (λ () (term (is-iter-on-var (NAT -1)))))
  (check-exn #rx"not in my domain" (λ () (term (iter-varr ("x" ()) PLUS)))))
