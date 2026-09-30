;; spec-meta/al/5.7-eval-call-rel.watsup, included in 5-eval.rkt.

;;
;; Rule path evaluation
;;

;; ctx |- rulmatch -> rulpath '@' `( val* `) : res<val*>
(define-relation al
  #:mode (eval-rul I I I I O)
  #:contract (eval-rul ctx rulmatch rulpath (val ...) valsres)
  [(where (id (exp_out ...) (prem_path ...)) rulpath)
   (where ((exp ...) (prem_match ...)) rulmatch)
   (where (C_1 ...) ,(judgment-holds (assign-exps C (exp ...) (val ...) C_out) C_out))
   (eval-rul/succ (C_1 ...) (prem_match ...) (prem_path ...) (exp_out ...) valsres)
   ------------------------------------------------ "succ"
   (eval-rul C rulmatch rulpath (val ...) valsres)])

;; rule Eval_rul/succ, given the outputs of Assign_exps
(define-relation al
  #:mode (eval-rul/succ I I I I O)
  #:contract (eval-rul/succ (ctx ...) (prem ...) (prem ...) (exp ...) valsres)
  [(where (prem ...) (prem_match ... prem_path ...))
   (eval-prems C_1 (prem ...) ctxres)
   (eval-rul/succ-prems (exp_out ...) ctxres valsres)
   ------------------------------------------------------------------------------ "succ"
   (eval-rul/succ (C_1) (prem_match ...) (prem_path ...) (exp_out ...) valsres)]
  [-------------------------------------------------------------------------- "fail"
   (eval-rul/succ () (prem_match ...) (prem_path ...) (exp_out ...) FAIL)])

;; rule Eval_rul/succ, given the premises
(define-relation al
  #:mode (eval-rul/succ-prems I I O)
  #:contract (eval-rul/succ-prems (exp ...) ctxres valsres)
  [(eval-exps C_2 (exp_out ...) valsres_out)
   (eval-rul/succ-exps valsres_out valsres)
   ------------------------------------------------------ "succ"
   (eval-rul/succ-prems (exp_out ...) (OK C_2) valsres)]
  [---------------------------------------------- "fail"
   (eval-rul/succ-prems (exp_out ...) FAIL FAIL)])

;; rule Eval_rul/succ, given the output expressions
(define-relation al
  #:mode (eval-rul/succ-exps I O)
  #:contract (eval-rul/succ-exps valsres valsres)
  [------------------------------------------------------------ "succ"
   (eval-rul/succ-exps (OK (val_out ...)) (OK (val_out ...)))]
  [------------------------------ "fail"
   (eval-rul/succ-exps FAIL FAIL)])

;;; Rule path sequence evaluation

;; ctx |- rulmatch -> rulpath* '@' `( val* `) : res<val*>
(define-relation al
  #:mode (eval-ruls I I I I O)
  #:contract (eval-ruls ctx rulmatch (rulpath ...) (val ...) valsres)
  [------------------------------------------ "nil"
   (eval-ruls C rulmatch () (val ...) FAIL)]
  [(eval-rul C rulmatch rulpath_h (val ...) valsres_h)
   (eval-ruls/cons C rulmatch (rulpath_t ...) (val ...) valsres_h valsres)
   --------------------------------------------------------------- "cons"
   (eval-ruls C rulmatch (rulpath_h rulpath_t ...) (val ...) valsres)])

;; rules Eval_ruls/cons-succ and cons-fail, given the head
(define-relation al
  #:mode (eval-ruls/cons I I I I I O)
  #:contract (eval-ruls/cons ctx rulmatch (rulpath ...) (val ...) valsres valsres)
  [---------------------------------------------------------------------- "cons-succ"
   (eval-ruls/cons C rulmatch (rulpath_t ...) (val ...) (OK (val_out ...))
                   (OK (val_out ...)))]
  [(eval-ruls C rulmatch (rulpath_t ...) (val ...) valsres)
   ----------------------------------------------------------------- "cons-fail"
   (eval-ruls/cons C rulmatch (rulpath_t ...) (val ...) FAIL valsres)])

;;; Rule group evaluation

;; ctx |- rulgroup '@' `( val* `) : res<val*>
(define-relation al
  #:mode (eval-rulgroup I I I O)
  #:contract (eval-rulgroup ctx rulgroup (val ...) valsres)
  [(where (id rulmatch (rulpath ...)) rulgroup)
   (eval-ruls C rulmatch (rulpath ...) (val ...) valsres_ruls)
   (eval-rulgroup/succ valsres_ruls valsres)
   ------------------------------------------- "succ"
   (eval-rulgroup C rulgroup (val ...) valsres)])

;; rule Eval_rulgroup/succ, given the rule paths
(define-relation al
  #:mode (eval-rulgroup/succ I O)
  #:contract (eval-rulgroup/succ valsres valsres)
  [------------------------------------------------------------ "succ"
   (eval-rulgroup/succ (OK (val_out ...)) (OK (val_out ...)))]
  [------------------------------ "fail"
   (eval-rulgroup/succ FAIL FAIL)])

;;; Rule group sequence evaluation

;; ctx |- rulgroup* '@' `( val* `) : res<val*>
(define-relation al
  #:mode (eval-rulgroups I I I O)
  #:contract (eval-rulgroups ctx (rulgroup ...) (val ...) valsres)
  [----------------------------------- "nil"
   (eval-rulgroups C () (val ...) FAIL)]
  [(eval-rulgroup C rulgroup_h (val ...) valsres_h)
   (eval-rulgroups/cons C (rulgroup_t ...) (val ...) valsres_h valsres)
   ------------------------------------------------------------ "cons"
   (eval-rulgroups C (rulgroup_h rulgroup_t ...) (val ...) valsres)])

;; rules Eval_rulgroups/cons-succ and cons-fail, given the head
(define-relation al
  #:mode (eval-rulgroups/cons I I I I O)
  #:contract (eval-rulgroups/cons ctx (rulgroup ...) (val ...) valsres valsres)
  [-------------------------------------------------------------------------- "cons-succ"
   (eval-rulgroups/cons C (rulgroup_t ...) (val ...) (OK (val_out ...)) (OK (val_out ...)))]
  [(eval-rulgroups C (rulgroup_t ...) (val ...) valsres)
   ------------------------------------------------------------ "cons-fail"
   (eval-rulgroups/cons C (rulgroup_t ...) (val ...) FAIL valsres)])

;;; Defined relations

(define-dec al
  elsgroup-as-rulgroup : elsgroup -> rulgroup
  [(elsgroup-as-rulgroup (id rulmatch rulpath)) (id rulmatch (rulpath))])

;; ctx |- definedRelDef '@' `( val* `) : res<val*>
(define-relation al
  #:mode (call-defined-rel I I I O)
  #:contract (call-defined-rel ctx definedRelDef (val ...) valsres)
  [(where (DEF (rulgroup ...) (elsgroup ...)) definedRelDef)
   (where (rulgroup_els ...) ((elsgroup-as-rulgroup elsgroup) ...))
   (where (rulgroup_opt ...) (opt-as-seq- (rulgroup_els ...)))
   (where (rulgroup_all ...) (rulgroup ... rulgroup_opt ...))
   (where {GLOBAL layer_g LOCAL _} C)
   (where layer_l (empty-layer))
   (where C_local {GLOBAL layer_g LOCAL layer_l})
   (eval-rulgroups C_local (rulgroup_all ...) (val ...) valsres)
   ---------------------------------------------------- "call-defined-rel"
   (call-defined-rel C definedRelDef (val ...) valsres)])

;;
;; Relation dispatch
;;

;; ctx |- reldef val* : res<val*>
(define-relation al
  #:mode (call-rel-dispatch I I I O)
  #:contract (call-rel-dispatch ctx reldef (val ...) valsres)
  [(call-defined-rel C definedRelDef (val ...) valsres)
   ----------------------------------------------------- "defined"
   (call-rel-dispatch C definedRelDef (val ...) valsres)]
  [(call-extern-rel id (val ...) valsres)
   ------------------------------------------------ "ext"
   (call-rel-dispatch C (EXT id) (val ...) valsres)])

;;
;; Relation call
;;

;; ctx |- id val* : res<val*>
(define-relation al
  #:mode (call-rel I I I O)
  #:contract (call-rel ctx id (val ...) valsres)
  [(where (reldef) (find-rel C id))
   (call-rel-dispatch C reldef (val ...) valsres)
   ------------------------------------ "call-rel"
   (call-rel C id (val ...) valsres)])
