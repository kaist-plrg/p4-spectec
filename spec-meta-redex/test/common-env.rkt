#lang racket/base

(require "../common/0.0-prelude.rkt"
         "../common/2-env.rkt"
         "../common/3-context.rkt")

;;
;; Languages
;;

(test-match Common-env varr (term ("x" ())))
(test-match Common-env varr (term ("x" (STAR QUEST))))
(test-no-match Common-env varr (term ("x" NAT ())))

(test-match Common-env venv (term ()))
(test-match Common-env venv (term ((("x" ()) (NAT 1)) (("y" (STAR)) (LIST ())))))
(test-no-match Common-env venv (term ((("x" ()) 1))))

(test-match Common-env typdef (term PARAM))
(test-match Common-env typdef (term EXT))
(test-match Common-env typdef (term (DEF () (ALIAS NAT))))
(test-match Common-env typdef (term (DEF ("X") (VARIANT ()))))
(test-no-match Common-env typdef (term (DEF (ALIAS NAT))))
(test-no-match Common-env typdef (term (EXT (1))))

(test-match Common-env tdenv (term (("t" PARAM) ("u" (DEF () (STRUCT ()))))))
(test-no-match Common-env tdenv (term (("t" NAT))))

(test-match Common-env theta (term (("X" NAT) ("Y" (VAR "t" ())))))
(test-no-match Common-env theta (term (("X" PARAM))))

(test-match Common-context cursor (term GLOBAL))
(test-match Common-context cursor (term LOCAL))
(test-no-match Common-context cursor (term (GLOBAL)))

;;
;; $extend_tdenv
;;

(define-term tdenv-g (("t" PARAM) ("u" EXT)))

(test-equal (term (extend_tdenv tdenv-g ())) (term tdenv-g))
(test-equal (term (extend_tdenv () ())) '())
(test-equal (term (extend_tdenv tdenv-g (("v" PARAM))))
            '(("t" PARAM) ("u" EXT) ("v" PARAM)))
(test-equal (term (extend_tdenv tdenv-g (("t" (DEF () (ALIAS NAT))))))
            '(("t" (DEF () (ALIAS NAT))) ("u" EXT)))

;;
;; $theta_of_tdenv
;;

(test-equal (term (theta_of_tdenv ())) '())
(test-equal (term (theta_of_tdenv (("X" (DEF () (ALIAS NAT))))))
            '(("X" NAT)))
(test-equal (term (theta_of_tdenv (("X" PARAM) ("Y" EXT))))
            '())
(test-equal (term (theta_of_tdenv (("X" (DEF () (ALIAS NAT)))
                                   ("P" PARAM)
                                   ("Y" (DEF () (ALIAS (VAR "X" ())))))))
            '(("Y" (VAR "X" ())) ("X" NAT)))
;; The head is added after the tail, so it replaces a later duplicate.
(test-equal (term (theta_of_tdenv (("X" (DEF () (ALIAS NAT)))
                                   ("X" (DEF () (ALIAS INT))))))
            '(("X" NAT)))

;; No clause for a DEF with type parameters or without an alias: ⊥, also from
;; the tail.
(test-equal (term (theta_of_tdenv (("X" (DEF ("Z") (ALIAS NAT)))))) '⊥)
(test-equal (term (theta_of_tdenv (("X" (DEF () (STRUCT ())))))) '⊥)
(test-equal (term (theta_of_tdenv (("X" (DEF () (ALIAS NAT)))
                                   ("Y" (DEF () (VARIANT ()))))))
            '⊥)
(test-equal (term (theta_of_tdenv (("P" PARAM) ("Y" (DEF ("Z") (ALIAS NAT))))))
            '⊥)

;; A caller's premise on ⊥ fails, whether it dispatches on it or not.
(define-dec Common-env
  theta_or_empty : tdenv -> theta
  [(theta_or_empty tdenv) theta
   (where theta (theta_of_tdenv tdenv))]
  [(theta_or_empty tdenv) ()
   (where ⊥ (theta_of_tdenv tdenv))])

(test-equal (term (theta_or_empty (("X" (DEF () (ALIAS NAT)))))) '(("X" NAT)))
(test-equal (term (theta_or_empty (("X" (DEF ("Z") (ALIAS NAT)))))) '())

(define-relation Common-env
  #:mode (Theta I O)
  #:contract (Theta tdenv theta)
  [(where theta (theta_of_tdenv tdenv))
   ------------------------------------ "theta"
   (Theta tdenv theta)])

(test-equal (judgment-holds (Theta (("X" (DEF () (ALIAS NAT)))) theta) theta)
            '((("X" NAT))))
(test-equal (judgment-holds (Theta (("X" (DEF ("Z") (ALIAS NAT)))) theta) theta)
            '())

;;
;; $is_iter_on_var
;;

(test-equal (term (is_iter_on_var (VAR "x"))) '(("x" ())))
(test-equal (term (is_iter_on_var (ITER (VAR "x") (STAR (("x" NAT ()))))))
            '(("x" (STAR))))
(test-equal (term (is_iter_on_var
                   (ITER (ITER (VAR "x") (QUEST (("x" NAT ()))))
                         (STAR (("x" NAT (QUEST)))))))
            '(("x" (QUEST STAR))))

;; otherwise: neither VAR nor ITER
(test-equal (term (is_iter_on_var (NAT 1))) '())
(test-equal (term (is_iter_on_var (CALL "f" () ((EXP (VAR "x")))))) '())

;; otherwise: an ITER whose premises fail
(test-equal (term (is_iter_on_var (ITER (NAT 1) (STAR ())))) '())
(test-equal (term (is_iter_on_var (ITER (VAR "x") (STAR ())))) '())
(test-equal (term (is_iter_on_var
                   (ITER (VAR "x") (STAR (("x" NAT ()) ("y" NAT ()))))))
            '())
(test-equal (term (is_iter_on_var (ITER (VAR "x") (STAR (("y" NAT ()))))))
            '())
(test-equal (term (is_iter_on_var (ITER (VAR "x") (STAR (("x" NAT (STAR)))))))
            '())
(test-equal (term (is_iter_on_var
                   (ITER (ITER (VAR "x") (QUEST (("x" NAT ()))))
                         (STAR (("x" NAT ()))))))
            '())

;;
;; $iter_vari, $iter_varr
;;

(test-equal (term (iter_vari ("x" NAT ()) STAR)) '("x" NAT (STAR)))
(test-equal (term (iter_vari ("x" NAT (QUEST)) STAR)) '("x" NAT (QUEST STAR)))
(test-equal (term (iter_varr ("x" ()) QUEST)) '("x" (QUEST)))
(test-equal (term (iter_varr ("x" (STAR)) STAR)) '("x" (STAR STAR)))

(test-results)
