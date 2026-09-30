;; spec-meta/al/5.4-eval-arg.watsup, included in 5-eval.rkt.

;;
;; Argument evaluation
;;

;; ctx |- arg : res<val>
(define-relation al
  #:mode (eval-arg I I O)
  #:contract (eval-arg ctx arg valres)
  [(eval-exp C exp valres_e)
   (eval-arg/exp valres_e valres)
   ---------------------------- "exp"
   (eval-arg C (EXP exp) valres)]
  [------------------------------------ "fun"
   (eval-arg C (FUN id) (OK (FUNC id)))])

;; rule Eval_arg/exp, given the expression
(define-relation al
  #:mode (eval-arg/exp I O)
  #:contract (eval-arg/exp valres valres)
  [-------------------------------- "exp"
   (eval-arg/exp (OK val) (OK val))]
  [------------------------ "fail"
   (eval-arg/exp FAIL FAIL)])

;; (Eval_arg: C |- arg : OK val)*, which stops at the first FAIL
(define-relation al
  #:mode (eval-args I I O)
  #:contract (eval-args ctx (arg ...) valsres)
  [------------------------ "nil"
   (eval-args C () (OK ()))]
  [(eval-arg C arg_h valres_h)
   (eval-args/cons C valres_h (arg_t ...) valsres)
   --------------------------------------------- "cons"
   (eval-args C (arg_h arg_t ...) valsres)])

(define-relation al
  #:mode (eval-args/cons I I I O)
  #:contract (eval-args/cons ctx valres (arg ...) valsres)
  [(eval-args C (arg_t ...) valsres_t)
   (where valsres (cons-valsres val_h valsres_t))
   ------------------------------------------------- "cons-succ"
   (eval-args/cons C (OK val_h) (arg_t ...) valsres)]
  [----------------------------------------- "cons-fail"
   (eval-args/cons C FAIL (arg_t ...) FAIL)])

;;
;; Type argument evaluation
;;

;; ctx |- targ* : typ*
(define-relation al
  #:mode (eval-targs I I O)
  #:contract (eval-targs ctx (targ ...) (typ ...))
  [(where {GLOBAL _ LOCAL {TYP tdenv REL _ FUNC _ VAL _}} C)
   (where theta (theta-of-tdenv tdenv))
   (where (targ_subst ...) ((subst-typ theta targ) ...))
   ------------------------------------------- "eval-targs"
   (eval-targs C (targ ...) (targ_subst ...))])
