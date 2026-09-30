;; spec-meta/al/5.5-eval-prem.watsup, included in 5-eval.rkt.

;;
;; Premise evaluation
;;

;; ctx |- prem : res<ctx>
(define-relation al
  #:mode (eval-prem I I O)
  #:contract (eval-prem ctx prem ctxres)

  ;;; Relation premises

  [(eval-exps C (exp ...) valsres)
   (eval-prem/relpr C id (exp_out ...) valsres ctxres)
   ---------------------------------------------------- "relpr"
   (eval-prem C (REL id (exp ...) (exp_out ...)) ctxres)]

  ;;; If premises

  [(eval-exp C exp valres)
   (eval-prem/ifpr C valres ctxres)
   ----------------------------- "ifpr"
   (eval-prem C (IF exp) ctxres)]

  ;;; If-hold and if-not-hold premises

  [(eval-exps C (exp ...) valsres)
   (eval-prem/ifholdpr C (IFHOLD id (exp ...)) valsres ctxres)
   ------------------------------------------ "ifholdpr/hold"
   (eval-prem C (IFHOLD id (exp ...)) ctxres)]
  [(eval-exps C (exp ...) valsres)
   (eval-prem/ifholdpr C (IFNOTHOLD id (exp ...)) valsres ctxres)
   --------------------------------------------- "ifholdpr/nothold"
   (eval-prem C (IFNOTHOLD id (exp ...)) ctxres)]

  ;;; Let premises

  [(eval-exp C exp_r valres_r)
   (eval-prem/letpr C exp_l valres_r ctxres)
   -------------------------------------- "letpr"
   (eval-prem C (LET exp_l exp_r) ctxres)]

  ;;; Iteration premises

  [(eval-prem/iterpr-opt C prem (vari_bound ...) (vari_bind ...) ctxres)
   ------------------------------------------------------------------------ "iterpr-opt"
   (eval-prem C (ITER prem (QUEST (vari_bound ...) (vari_bind ...))) ctxres)]
  [(eval-prem/iterpr-list C prem (vari_bound ...) (vari_bind ...) ctxres)
   ----------------------------------------------------------------------- "iterpr-list"
   (eval-prem C (ITER prem (STAR (vari_bound ...) (vari_bind ...))) ctxres)]

  ;;; Debug premises

  [(eval-exp C exp valres)
   (eval-prem/dbg C valres ctxres)
   -------------------------------- "dbg"
   (eval-prem C (DEBUG exp) ctxres)])

;;; Relation premises

;; rule Eval_prem/relpr, given the inputs
(define-relation al
  #:mode (eval-prem/relpr I I I I O)
  #:contract (eval-prem/relpr ctx id (exp ...) valsres ctxres)
  [(where (valsres_call ...)
          ,(judgment-holds (call-rel C id (val ...) valsres_out) valsres_out))
   (eval-prem/relpr-call C (exp_out ...) (valsres_call ...) ctxres)
   --------------------------------------------------------- "relpr"
   (eval-prem/relpr C id (exp_out ...) (OK (val ...)) ctxres)]
  [--------------------------------------------- "fail"
   (eval-prem/relpr C id (exp_out ...) FAIL FAIL)])

;; rule Eval_prem/relpr, given the outputs of Call_rel
(define-relation al
  #:mode (eval-prem/relpr-call I I I O)
  #:contract (eval-prem/relpr-call ctx (exp ...) (valsres ...) ctxres)
  [(where (C_1 ...)
          ,(judgment-holds (assign-exps C (exp_out ...) (val_out ...) C_out) C_out))
   (eval-prem/relpr-assign (C_1 ...) ctxres)
   -------------------------------------------------------------------- "relpr"
   (eval-prem/relpr-call C (exp_out ...) ((OK (val_out ...))) ctxres)]
  [(side-condition ,(member (term (valsres ...)) '(() (FAIL))))
   -------------------------------------------------------- "fail"
   (eval-prem/relpr-call C (exp_out ...) (valsres ...) FAIL)])

;; rule Eval_prem/relpr, given the outputs of Assign_exps
(define-relation al
  #:mode (eval-prem/relpr-assign I O)
  #:contract (eval-prem/relpr-assign (ctx ...) ctxres)
  [------------------------------------ "relpr"
   (eval-prem/relpr-assign (C) (OK C))]
  [------------------------------- "fail"
   (eval-prem/relpr-assign () FAIL)])

;;; If premises

;; rulegroup Eval_prem/ifpr, given the condition
(define-relation al
  #:mode (eval-prem/ifpr I I O)
  #:contract (eval-prem/ifpr ctx valres ctxres)
  [---------------------------------------- "true"
   (eval-prem/ifpr C (OK (BOOL #t)) (OK C))]
  [--------------------------------------- "false"
   (eval-prem/ifpr C (OK (BOOL #f)) FAIL)]
  [(side-condition ,(not (redex-match? al (OK (BOOL b)) (term valres))))
   ------------------------------ "fail"
   (eval-prem/ifpr C valres FAIL)])

;;; If-hold and if-not-hold premises

;; rulegroup Eval_prem/ifholdpr, given the inputs
(define-relation al
  #:mode (eval-prem/ifholdpr I I I O)
  #:contract (eval-prem/ifholdpr ctx prem valsres ctxres)
  [(where (valsres_call ...)
          ,(judgment-holds (call-rel C id (val_input ...) valsres_out) valsres_out))
   (eval-prem/ifholdpr-call C (IFHOLD id (exp ...)) (valsres_call ...) ctxres)
   ------------------------------------------------------------------------ "hold"
   (eval-prem/ifholdpr C (IFHOLD id (exp ...)) (OK (val_input ...)) ctxres)]
  [(where (valsres_call ...)
          ,(judgment-holds (call-rel C id (val_input ...) valsres_out) valsres_out))
   (eval-prem/ifholdpr-call C (IFNOTHOLD id (exp ...)) (valsres_call ...) ctxres)
   --------------------------------------------------------------------------- "nothold"
   (eval-prem/ifholdpr C (IFNOTHOLD id (exp ...)) (OK (val_input ...)) ctxres)]
  [------------------------------------ "fail"
   (eval-prem/ifholdpr C prem FAIL FAIL)])

;; rulegroup Eval_prem/ifholdpr, given the outputs of Call_rel
(define-relation al
  #:mode (eval-prem/ifholdpr-call I I I O)
  #:contract (eval-prem/ifholdpr-call ctx prem (valsres ...) ctxres)
  [----------------------------------------------------------------- "hold"
   (eval-prem/ifholdpr-call C (IFHOLD id (exp ...)) ((OK ())) (OK C))]
  [------------------------------------------------------------------ "nothold"
   (eval-prem/ifholdpr-call C (IFNOTHOLD id (exp ...)) (FAIL) (OK C))]
  [(side-condition ,(< (length (term (valsres ...))) 2))
   (side-condition
    ,(not (redex-match? al ((IFHOLD id (exp ...)) ((OK ()))) (term (prem (valsres ...))))))
   (side-condition
    ,(not (redex-match? al ((IFNOTHOLD id (exp ...)) (FAIL)) (term (prem (valsres ...))))))
   --------------------------------------------------- "fail"
   (eval-prem/ifholdpr-call C prem (valsres ...) FAIL)])

;;; Let premises

;; rule Eval_prem/letpr, given the right-hand side
(define-relation al
  #:mode (eval-prem/letpr I I I O)
  #:contract (eval-prem/letpr ctx exp valres ctxres)
  [(where (C_1 ...) ,(judgment-holds (assign-exp C exp_l val_r C_out) C_out))
   (eval-prem/letpr-assign (C_1 ...) ctxres)
   ------------------------------------------- "letpr"
   (eval-prem/letpr C exp_l (OK val_r) ctxres)]
  [----------------------------------- "fail"
   (eval-prem/letpr C exp_l FAIL FAIL)])

;; rule Eval_prem/letpr, given the outputs of Assign_exp
(define-relation al
  #:mode (eval-prem/letpr-assign I O)
  #:contract (eval-prem/letpr-assign (ctx ...) ctxres)
  [----------------------------------- "letpr"
   (eval-prem/letpr-assign (C) (OK C))]
  [------------------------------ "fail"
   (eval-prem/letpr-assign () FAIL)])

;;; Iteration premises - optional

;; rulegroup Eval_prem/iterpr-opt
(define-relation al
  #:mode (eval-prem/iterpr-opt I I I I O)
  #:contract (eval-prem/iterpr-opt ctx prem (vari ...) (vari ...) ctxres)
  [(where () (sub-opt C (vari_bound ...)))
   (where (vari_bind_iter ...) ((iter-vari vari_bind QUEST) ...))
   (where (val_bind ...) (repeat- (OPT ()) ,(length (term (vari_bind ...)))))
   (where C_res (add-varis C (vari_bind_iter ...) (val_bind ...)))
   ------------------------------------------------------------------------ "none"
   (eval-prem/iterpr-opt C prem (vari_bound ...) (vari_bind ...) (OK C_res))]
  [(where (C_sub) (sub-opt C (vari_bound ...)))
   (where (vari_bind_iter ...) ((iter-vari vari_bind QUEST) ...))
   (eval-prem C_sub prem ctxres_sub)
   (eval-prem/iterpr-opt-sub C (vari_bind ...) (vari_bind_iter ...) ctxres_sub ctxres)
   ------------------------------------------------------------------- "some"
   (eval-prem/iterpr-opt C prem (vari_bound ...) (vari_bind ...) ctxres)]
  [(where ⊥ (sub-opt C (vari_bound ...)))
   ------------------------------------------------------------------ "fail"
   (eval-prem/iterpr-opt C prem (vari_bound ...) (vari_bind ...) FAIL)])

;; rule Eval_prem/iterpr-opt/some, given prem under the sub-context
(define-relation al
  #:mode (eval-prem/iterpr-opt-sub I I I I O)
  #:contract (eval-prem/iterpr-opt-sub ctx (vari ...) (vari ...) ctxres ctxres)
  [(where ((val_bind) ...) ((find-vari C_sub_res vari_bind) ...))
   (where C_res (add-varis C (vari_bind_iter ...) ((OPT (val_bind)) ...)))
   ------------------------------------------------------------------------ "some"
   (eval-prem/iterpr-opt-sub C (vari_bind ...) (vari_bind_iter ...) (OK C_sub_res)
                             (OK C_res))]
  [(side-condition
    ,(not (redex-match? al ((val) ...) (term ((find-vari C_sub_res vari_bind) ...)))))
   ------------------------------------------------------------------ "some-fail"
   (eval-prem/iterpr-opt-sub C (vari_bind ...) (vari_bind_iter ...) (OK C_sub_res) FAIL)]
  [---------------------------------------------------------------- "fail"
   (eval-prem/iterpr-opt-sub C (vari_bind ...) (vari_bind_iter ...) FAIL FAIL)])

;;; Iteration premises - list

;; rulegroup Eval_prem/iterpr-list
(define-relation al
  #:mode (eval-prem/iterpr-list I I I I O)
  #:contract (eval-prem/iterpr-list ctx prem (vari ...) (vari ...) ctxres)
  [(where () (sub-list C (vari_bound ...)))
   (where (vari_bind_iter ...) ((iter-vari vari_bind STAR) ...))
   (where (val_bind ...) (repeat- (LIST ()) ,(length (term (vari_bind ...)))))
   (where C_res (add-varis C (vari_bind_iter ...) (val_bind ...)))
   ------------------------------------------------------------------------- "empty"
   (eval-prem/iterpr-list C prem (vari_bound ...) (vari_bind ...) (OK C_res))]
  [(where (C_sub ...) (sub-list C (vari_bound ...)))
   (side-condition ,(pair? (term (C_sub ...))))
   (eval-prem-subs (C_sub ...) prem ctxsres)
   (eval-prem/iterpr-list-subs C (vari_bind ...) ctxsres ctxres)
   -------------------------------------------------------------------- "list"
   (eval-prem/iterpr-list C prem (vari_bound ...) (vari_bind ...) ctxres)]
  [(where ⊥ (sub-list C (vari_bound ...)))
   ------------------------------------------------------------------- "fail"
   (eval-prem/iterpr-list C prem (vari_bound ...) (vari_bind ...) FAIL)])

;; rule Eval_prem/iterpr-list/list, given prem under each sub-context
(define-relation al
  #:mode (eval-prem/iterpr-list-subs I I I O)
  #:contract (eval-prem/iterpr-list-subs ctx (vari ...) ctxsres ctxres)
  [(where (((val_sub_bind ...)) ...) ((find-varis C_sub_res (vari_bind ...)) ...))
   (where ((val_bind ...) ...) (transpose- ((val_sub_bind ...) ...)))
   (where (val_res_bind ...) ((LIST (val_bind ...)) ...))
   (where (vari_bind_iter ...) ((iter-vari vari_bind STAR) ...))
   (where C_res (add-varis C (vari_bind_iter ...) (val_res_bind ...)))
   ------------------------------------------------------------------------------ "list"
   (eval-prem/iterpr-list-subs C (vari_bind ...) (OK (C_sub_res ...)) (OK C_res))]
  [(side-condition
    ,(not (redex-match? al (((val ...)) ...)
                       (term ((find-varis C_sub_res (vari_bind ...)) ...)))))
   ------------------------------------------------------------------------ "list-fail"
   (eval-prem/iterpr-list-subs C (vari_bind ...) (OK (C_sub_res ...)) FAIL)]
  [------------------------------------------------------ "fail"
   (eval-prem/iterpr-list-subs C (vari_bind ...) FAIL FAIL)])

;; (Eval_prem: C_sub |- prem : OK C_sub_res)*, which stops at the first FAIL
(define-relation al
  #:mode (eval-prem-subs I I O)
  #:contract (eval-prem-subs (ctx ...) prem ctxsres)
  [------------------------------- "nil"
   (eval-prem-subs () prem (OK ()))]
  [(eval-prem C_h prem ctxres_h)
   (eval-prem-subs/cons (C_t ...) prem ctxres_h ctxsres)
   ------------------------------------------------- "cons"
   (eval-prem-subs (C_h C_t ...) prem ctxsres)])

(define-relation al
  #:mode (eval-prem-subs/cons I I I O)
  #:contract (eval-prem-subs/cons (ctx ...) prem ctxres ctxsres)
  [(eval-prem-subs (C_t ...) prem ctxsres_t)
   (where ctxsres (cons-ctxsres C_res ctxsres_t))
   -------------------------------------------------------- "cons-succ"
   (eval-prem-subs/cons (C_t ...) prem (OK C_res) ctxsres)]
  [---------------------------------------------- "cons-fail"
   (eval-prem-subs/cons (C_t ...) prem FAIL FAIL)])

;;; Debug premises

;; rule Eval_prem/dbg, given the expression
(define-relation al
  #:mode (eval-prem/dbg I I O)
  #:contract (eval-prem/dbg ctx valres ctxres)
  [(where _ ,(debug (term val)))
   --------------------------------- "dbg"
   (eval-prem/dbg C (OK val) (OK C))]
  [--------------------------- "fail"
   (eval-prem/dbg C FAIL FAIL)])

;;
;; Premise sequence evaluation
;;

;; ctx |- prem* : res<ctx>
(define-relation al
  #:mode (eval-prems I I O)
  #:contract (eval-prems ctx (prem ...) ctxres)
  [------------------------ "empty"
   (eval-prems C () (OK C))]
  [(eval-prem C prem_h ctxres_h)
   (eval-prems/head C (prem_t ...) ctxres_h ctxres)
   ---------------------------------------------- "head"
   (eval-prems C (prem_h prem_t ...) ctxres)])

;; rules Eval_prems/head-fail and head-succ, given the head
(define-relation al
  #:mode (eval-prems/head I I I O)
  #:contract (eval-prems/head ctx (prem ...) ctxres ctxres)
  [----------------------------------------- "head-fail"
   (eval-prems/head C (prem_t ...) FAIL FAIL)]
  [(eval-prems C_1 (prem_t ...) ctxres)
   ------------------------------------------------ "head-succ"
   (eval-prems/head C (prem_t ...) (OK C_1) ctxres)])
