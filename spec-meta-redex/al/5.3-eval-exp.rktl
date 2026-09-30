;; spec-meta/al/5.3-eval-exp.watsup, included in 5-eval.rkt.

;;
;; Expression evaluation
;;

;; ctx |- exp : res<val>
(define-relation al
  #:mode (eval-exp I I O)
  #:contract (eval-exp ctx exp valres)

  ;;; Boolean, number, and text evaluation rules

  [----------------------------------- "literal/boolean"
   (eval-exp C (BOOL b) (OK (BOOL b)))]
  [------------------------- "literal/number"
   (eval-exp C num (OK num))]
  [----------------------------------- "literal/string"
   (eval-exp C (TEXT t) (OK (TEXT t)))]

  ;;; Variable evaluation rules

  [(where (val ...) (find-varr C (id ())))
   (eval-exp/variable (val ...) valres)
   ---------------------------- "variable"
   (eval-exp C (VAR id) valres)]

  ;;; Unary, binary, and comparison evaluation rules

  [(eval-exp C exp valres_e)
   (eval-exp/unary unop valres_e valres)
   --------------------------------- "unary"
   (eval-exp C (UN unop exp) valres)]

  [(eval-exp C exp_l valres_l)
   (eval-exp/binary C binop valres_l exp_r valres)
   ------------------------------------------- "binary"
   (eval-exp C (BIN binop exp_l exp_r) valres)]

  [(eval-exp C exp_l valres_l)
   (eval-exp/compare C cmpop valres_l exp_r valres)
   -------------------------------------------- "compare"
   (eval-exp C (CMP cmpop exp_l exp_r) valres)]

  ;;; Upcasting, downcasting, and subtyping evaluation rules

  [(eval-exp C exp valres_e)
   (eval-exp/upcast C typ valres_e valres)
   ------------------------------------ "upcast"
   (eval-exp C (UPCAST typ exp) valres)]

  [(eval-exp C exp valres_e)
   (eval-exp/downcast C typ valres_e valres)
   -------------------------------------- "downcast"
   (eval-exp C (DOWNCAST typ exp) valres)]

  [(eval-exp C exp valres_e)
   (eval-exp/subtype C typ valres_e valres)
   --------------------------------- "subtype"
   (eval-exp C (SUB exp typ) valres)]

  ;;; Match evaluation rules

  [(eval-exp C exp valres_e)
   (eval-exp/match pattern valres_e valres)
   --------------------------------------- "match"
   (eval-exp C (MATCH exp pattern) valres)]

  ;;; Tuple, case, struct, option, and list evaluation rules

  [(eval-exps C (exp ...) valsres)
   (eval-exp/tuple valsres valres)
   ----------------------------------- "tuple"
   (eval-exp C (TUP (exp ...)) valres)]

  [(eval-exps C (exp ...) valsres)
   (eval-exp/case mixop valsres valres)
   ------------------------------------------- "case"
   (eval-exp C (INJ (mixop (exp ...))) valres)]

  [(where ((atom exp) ...) (expfield ...))
   (eval-exps C (exp ...) valsres)
   (eval-exp/struct (atom ...) valsres valres)
   ---------------------------------------- "struct"
   (eval-exp C (STR (expfield ...)) valres)]

  ;; The group's premise, as (Eval_exp: C |- exp : OK val)? over exp?
  [(eval-exps C (exp ...) valsres)
   (eval-exp/opt valsres valres)
   ----------------------------------- "opt"
   (eval-exp C (OPT (exp ...)) valres)]

  [(eval-exps C (exp ...) valsres)
   (eval-exp/list valsres valres)
   ------------------------------------ "list"
   (eval-exp C (LIST (exp ...)) valres)]

  ;;; Cons-list, concatenation, membership, and length evaluation rules

  ;; exp_t is evaluated only if exp_h succeeds, as eval-exps does.
  [(eval-exps C (exp_h exp_t) valsres)
   (eval-exp/cons valsres valres)
   -------------------------------------- "cons"
   (eval-exp C (CONS exp_h exp_t) valres)]

  [(eval-exp C exp_l valres_l)
   (eval-exp/concat C valres_l exp_r valres)
   ------------------------------------- "concat"
   (eval-exp C (CAT exp_l exp_r) valres)]

  [(eval-exps C (exp_e exp_s) valsres)
   (eval-exp/mem valsres valres)
   ------------------------------------- "mem"
   (eval-exp C (MEM exp_e exp_s) valres)]

  [(eval-exp C exp valres_e)
   (eval-exp/len valres_e valres)
   ---------------------------- "len"
   (eval-exp C (LEN exp) valres)]

  ;;; Dot, indexing, slicing, and update evaluation rules

  [(eval-exp C exp valres_e)
   (eval-exp/dot C atom valres_e valres)
   ---------------------------------- "dot"
   (eval-exp C (DOT exp atom) valres)]

  [(eval-exp C exp_b valres_b)
   (eval-exp/idx C exp_i valres_b valres)
   ------------------------------------- "idx"
   (eval-exp C (IDX exp_b exp_i) valres)]

  [(eval-exp C exp_b valres_b)
   (eval-exp/slice C exp_i exp_n valres_b valres)
   --------------------------------------------- "slice"
   (eval-exp C (SLICE exp_b exp_i exp_n) valres)]

  [(eval-exps C (exp_b exp_f) valsres)
   (eval-exp/upd C path valsres valres)
   ------------------------------------------ "upd"
   (eval-exp C (UPD exp_b path exp_f) valres)]

  ;;; Meta-function call evaluation rules

  [(where ((typ_input ...) ...)
          ,(judgment-holds (eval-targs C (targ ...) (typ_out ...)) (typ_out ...)))
   (eval-exp/call C id ((typ_input ...) ...) (arg ...) valres)
   -------------------------------------------------- "call"
   (eval-exp C (CALL id (targ ...) (arg ...)) valres)]

  ;;; Iterated expression evaluation rules

  [(where (varr ...) (is-iter-on-var (ITER exp iterexp)))
   (eval-exp/iter C exp iterexp (varr ...) valres)
   -------------------------------------- "iter"
   (eval-exp C (ITER exp iterexp) valres)])

;;; Variable

;; rule Eval_exp/variable, given the value of id
(define-relation al
  #:mode (eval-exp/variable I O)
  #:contract (eval-exp/variable (val ...) valres)
  [---------------------------------- "variable"
   (eval-exp/variable (val) (OK val))]
  [--------------------------- "fail"
   (eval-exp/variable () FAIL)])

;;; Unary, binary, and comparison

;; rulegroup Eval_exp/unary, given the operand
(define-relation al
  #:mode (eval-exp/unary I I O)
  #:contract (eval-exp/unary unop valres valres)
  [(where b_res ,(not (term b)))
   --------------------------------------------------- "boolean"
   (eval-exp/unary NOT (OK (BOOL b)) (OK (BOOL b_res)))]
  [(where val_res (unop-number numunop num))
   ---------------------------------------------- "number"
   (eval-exp/unary numunop (OK num) (OK val_res))]
  [(side-condition ,(not (redex-match? al (NOT (OK (BOOL b))) (term (unop valres)))))
   (side-condition ,(not (redex-match? al (numunop (OK num)) (term (unop valres)))))
   ---------------------------------- "fail"
   (eval-exp/unary unop valres FAIL)])

;; rulegroup Eval_exp/binary, given the left operand. The right one is
;; evaluated only if the left one fits a rule.
(define-relation al
  #:mode (eval-exp/binary I I I I O)
  #:contract (eval-exp/binary ctx binop valres exp valres)
  [(eval-exp C exp_r valres_r)
   (eval-exp/binary-right boolbinop (BOOL b_l) valres_r valres)
   ---------------------------------------------------------- "boolean"
   (eval-exp/binary C boolbinop (OK (BOOL b_l)) exp_r valres)]
  [(eval-exp C exp_r valres_r)
   (eval-exp/binary-right numbinop num_l valres_r valres)
   ---------------------------------------------------- "number"
   (eval-exp/binary C numbinop (OK num_l) exp_r valres)]
  [(side-condition ,(not (redex-match? al (boolbinop (OK (BOOL b))) (term (binop valres_l)))))
   (side-condition ,(not (redex-match? al (numbinop (OK num)) (term (binop valres_l)))))
   --------------------------------------------- "fail"
   (eval-exp/binary C binop valres_l exp_r FAIL)])

;; rulegroup Eval_exp/binary, given both operands
(define-relation al
  #:mode (eval-exp/binary-right I I I O)
  #:contract (eval-exp/binary-right binop val valres valres)
  [(where b_res (binop-bool boolbinop b_l b_r))
   ------------------------------------------------------------------------------ "boolean"
   (eval-exp/binary-right boolbinop (BOOL b_l) (OK (BOOL b_r)) (OK (BOOL b_res)))]
  [(where val_res (binop-number numbinop num_l num_r))
   -------------------------------------------------------------- "number"
   (eval-exp/binary-right numbinop num_l (OK num_r) (OK val_res))]
  [(where ⊥ (binop-number numbinop num_l num_r))
   ------------------------------------------------------- "number-fail"
   (eval-exp/binary-right numbinop num_l (OK num_r) FAIL)]
  [(side-condition
    ,(not (redex-match? al (boolbinop (BOOL b_1) (OK (BOOL b_2))) (term (binop val_l valres_r)))))
   (side-condition
    ,(not (redex-match? al (numbinop num_1 (OK num_2)) (term (binop val_l valres_r)))))
   ------------------------------------------------- "fail"
   (eval-exp/binary-right binop val_l valres_r FAIL)])

;; rulegroup Eval_exp/compare, given the left operand. The right one is
;; evaluated only if the left one fits a rule.
(define-relation al
  #:mode (eval-exp/compare I I I I O)
  #:contract (eval-exp/compare ctx cmpop valres exp valres)
  [(eval-exp C exp_r valres_r)
   (eval-exp/compare-right polycmpop val_l valres_r valres)
   ------------------------------------------------------ "poly"
   (eval-exp/compare C polycmpop (OK val_l) exp_r valres)]
  [(eval-exp C exp_r valres_r)
   (eval-exp/compare-right numcmpop num_l valres_r valres)
   ----------------------------------------------------- "number"
   (eval-exp/compare C numcmpop (OK num_l) exp_r valres)]
  [(side-condition ,(not (redex-match? al (polycmpop (OK val)) (term (cmpop valres_l)))))
   (side-condition ,(not (redex-match? al (numcmpop (OK num)) (term (cmpop valres_l)))))
   ---------------------------------------------- "fail"
   (eval-exp/compare C cmpop valres_l exp_r FAIL)])

;; rulegroup Eval_exp/compare, given both operands
(define-relation al
  #:mode (eval-exp/compare-right I I I O)
  #:contract (eval-exp/compare-right cmpop val valres valres)
  [(where b_res (cmpop-poly polycmpop val_l val_r))
   --------------------------------------------------------------------- "poly"
   (eval-exp/compare-right polycmpop val_l (OK val_r) (OK (BOOL b_res)))]
  [(where b_res (cmpop-number numcmpop num_l num_r))
   ------------------------------------------------------------------- "number"
   (eval-exp/compare-right numcmpop num_l (OK num_r) (OK (BOOL b_res)))]
  [(where ⊥ (cmpop-number numcmpop num_l num_r))
   ------------------------------------------------------- "number-fail"
   (eval-exp/compare-right numcmpop num_l (OK num_r) FAIL)]
  [(side-condition
    ,(not (redex-match? al (polycmpop val_1 (OK val_2)) (term (cmpop val_l valres_r)))))
   (side-condition
    ,(not (redex-match? al (numcmpop num_1 (OK num_2)) (term (cmpop val_l valres_r)))))
   -------------------------------------------------- "fail"
   (eval-exp/compare-right cmpop val_l valres_r FAIL)])

;;; Upcasting, downcasting, and subtyping

;; rule Eval_exp/upcast, given the operand
(define-relation al
  #:mode (eval-exp/upcast I I I O)
  #:contract (eval-exp/upcast ctx typ valres valres)
  [(where valres (upcast C typ val))
   --------------------------------------- "upcast"
   (eval-exp/upcast C typ (OK val) valres)]
  [--------------------------------- "fail"
   (eval-exp/upcast C typ FAIL FAIL)])

;; rule Eval_exp/downcast, given the operand
(define-relation al
  #:mode (eval-exp/downcast I I I O)
  #:contract (eval-exp/downcast ctx typ valres valres)
  [(where valres (downcast C typ val))
   ----------------------------------------- "downcast"
   (eval-exp/downcast C typ (OK val) valres)]
  [----------------------------------- "fail"
   (eval-exp/downcast C typ FAIL FAIL)])

;; rule Eval_exp/subtype, given the operand
(define-relation al
  #:mode (eval-exp/subtype I I I O)
  #:contract (eval-exp/subtype ctx typ valres valres)
  [(where {GLOBAL {TYP tdenv_g REL _ FUNC _ VAL _} LOCAL {TYP tdenv_l REL _ FUNC _ VAL _}} C)
   (where tdenv (extend-tdenv tdenv_g tdenv_l))
   (where b (subtyp tdenv typ val))
   ------------------------------------------------ "subtype"
   (eval-exp/subtype C typ (OK val) (OK (BOOL b)))]
  [---------------------------------- "fail"
   (eval-exp/subtype C typ FAIL FAIL)])

;;; Match

;; rulegroup Eval_exp/match, given the operand
(define-relation al
  #:mode (eval-exp/match I I O)
  #:contract (eval-exp/match pattern valres valres)
  [(where b ,(equal? (term mixop_p) (term mixop_v)))
   ---------------------------------------------------------------------------- "inj"
   (eval-exp/match (INJ mixop_p) (OK (INJ (mixop_v (val ...)))) (OK (BOOL b)))]
  [(where b ,(> (length (term (val ...))) 0))
   -------------------------------------------------------- "list-cons"
   (eval-exp/match CONS (OK (LIST (val ...))) (OK (BOOL b)))]
  [(where b ,(= (length (term (val ...))) (term n)))
   ------------------------------------------------------------- "list-fixed"
   (eval-exp/match (FIXED n) (OK (LIST (val ...))) (OK (BOOL b)))]
  [(where b ,(= (length (term (val ...))) 0))
   ------------------------------------------------------- "list-nil"
   (eval-exp/match NIL (OK (LIST (val ...))) (OK (BOOL b)))]
  [(where b ,(pair? (term (val ...))))
   ------------------------------------------------------- "opt-some"
   (eval-exp/match SOME (OK (OPT (val ...))) (OK (BOOL b)))]
  [(where b ,(null? (term (val ...))))
   ------------------------------------------------------- "opt-none"
   (eval-exp/match NONE (OK (OPT (val ...))) (OK (BOOL b)))]
  [(side-condition
    ,(not (redex-match? al ((INJ mixop) (OK (INJ valcase))) (term (pattern valres)))))
   (side-condition
    ,(not (redex-match? al (listpattern (OK (LIST (val ...)))) (term (pattern valres)))))
   (side-condition
    ,(not (redex-match? al (optpattern (OK (OPT (val ...)))) (term (pattern valres)))))
   ------------------------------------ "fail"
   (eval-exp/match pattern valres FAIL)])

;;; Tuple, case, struct, option, and list

;; rule Eval_exp/tuple, given the components
(define-relation al
  #:mode (eval-exp/tuple I O)
  #:contract (eval-exp/tuple valsres valres)
  [---------------------------------------------------- "tuple"
   (eval-exp/tuple (OK (val ...)) (OK (TUP (val ...))))]
  [-------------------------- "fail"
   (eval-exp/tuple FAIL FAIL)])

;; rule Eval_exp/case, given the arguments
(define-relation al
  #:mode (eval-exp/case I I O)
  #:contract (eval-exp/case mixop valsres valres)
  [------------------------------------------------------------------ "case"
   (eval-exp/case mixop (OK (val ...)) (OK (INJ (mixop (val ...)))))]
  [-------------------------------- "fail"
   (eval-exp/case mixop FAIL FAIL)])

;; rule Eval_exp/struct, given the fields' atoms and values
(define-relation al
  #:mode (eval-exp/struct I I O)
  #:contract (eval-exp/struct (atom ...) valsres valres)
  [(where (valfield ...) ((atom val) ...))
   ------------------------------------------------------------------------ "struct"
   (eval-exp/struct (atom ..._n) (OK (val ..._n)) (OK (STR (valfield ...))))]
  [(side-condition
    ,(not (redex-match? al ((atom ..._n) (OK (val ..._n))) (term ((atom ...) valsres)))))
   ---------------------------------------- "fail"
   (eval-exp/struct (atom ...) valsres FAIL)])

;; rulegroup Eval_exp/opt, given exp?
(define-relation al
  #:mode (eval-exp/opt I O)
  #:contract (eval-exp/opt valsres valres)
  [------------------------------------------ "some"
   (eval-exp/opt (OK (val)) (OK (OPT (val))))]
  [------------------------------------ "none"
   (eval-exp/opt (OK ()) (OK (OPT ())))]
  [------------------------ "fail"
   (eval-exp/opt FAIL FAIL)])

;; rule Eval_exp/list, given the elements
(define-relation al
  #:mode (eval-exp/list I O)
  #:contract (eval-exp/list valsres valres)
  [---------------------------------------------------- "list"
   (eval-exp/list (OK (val ...)) (OK (LIST (val ...))))]
  [------------------------- "fail"
   (eval-exp/list FAIL FAIL)])

;;; Cons-list, concatenation, membership, and length

;; rule Eval_exp/cons, given the head and the tail
(define-relation al
  #:mode (eval-exp/cons I O)
  #:contract (eval-exp/cons valsres valres)
  [----------------------------------------------------------------------------- "cons"
   (eval-exp/cons (OK (val_h (LIST (val_t ...)))) (OK (LIST (val_h val_t ...))))]
  [(side-condition ,(not (redex-match? al (OK (val (LIST (val_t ...)))) (term valsres))))
   ---------------------------- "fail"
   (eval-exp/cons valsres FAIL)])

;; rulegroup Eval_exp/concat, given the left operand. The right one is
;; evaluated only if the left one fits a rule.
(define-relation al
  #:mode (eval-exp/concat I I I O)
  #:contract (eval-exp/concat ctx valres exp valres)
  [(eval-exp C exp_r valres_r)
   (eval-exp/concat-right (TEXT text_l) valres_r valres)
   ---------------------------------------------------- "text"
   (eval-exp/concat C (OK (TEXT text_l)) exp_r valres)]
  [(eval-exp C exp_r valres_r)
   (eval-exp/concat-right (LIST (val_l ...)) valres_r valres)
   --------------------------------------------------------- "list"
   (eval-exp/concat C (OK (LIST (val_l ...))) exp_r valres)]
  [(side-condition ,(not (redex-match? al (OK (TEXT t)) (term valres_l))))
   (side-condition ,(not (redex-match? al (OK (LIST (val ...))) (term valres_l))))
   ---------------------------------------- "fail"
   (eval-exp/concat C valres_l exp_r FAIL)])

;; rulegroup Eval_exp/concat, given both operands
(define-relation al
  #:mode (eval-exp/concat-right I I O)
  #:contract (eval-exp/concat-right val valres valres)
  [(where text_res ,(string-append (term text_l) (term text_r)))
   ------------------------------------------------------------------------- "text"
   (eval-exp/concat-right (TEXT text_l) (OK (TEXT text_r)) (OK (TEXT text_res)))]
  [------------------------------------------------------------ "list"
   (eval-exp/concat-right (LIST (val_l ...)) (OK (LIST (val_r ...)))
                          (OK (LIST (val_l ... val_r ...))))]
  [(side-condition ,(not (redex-match? al ((TEXT t_1) (OK (TEXT t_2))) (term (val_l valres_r)))))
   (side-condition
    ,(not (redex-match? al ((LIST (val_1 ...)) (OK (LIST (val_2 ...)))) (term (val_l valres_r)))))
   ------------------------------------------ "fail"
   (eval-exp/concat-right val_l valres_r FAIL)])

;; rule Eval_exp/mem, given the element and the list
(define-relation al
  #:mode (eval-exp/mem I O)
  #:contract (eval-exp/mem valsres valres)
  [(where b (exists- ,(for/list ([v (in-list (term (val ...)))])
                        (equal? (term val_e) v))))
   ---------------------------------------------------------- "mem"
   (eval-exp/mem (OK (val_e (LIST (val ...)))) (OK (BOOL b)))]
  [(side-condition ,(not (redex-match? al (OK (val_1 (LIST (val_2 ...)))) (term valsres))))
   --------------------------- "fail"
   (eval-exp/mem valsres FAIL)])

;; rulegroup Eval_exp/len, given the operand
(define-relation al
  #:mode (eval-exp/len I O)
  #:contract (eval-exp/len valres valres)
  [(where n ,(text-length (term text)))
   ------------------------------------------- "text"
   (eval-exp/len (OK (TEXT text)) (OK (NAT n)))]
  [(where n ,(length (term (val ...))))
   -------------------------------------------------- "list"
   (eval-exp/len (OK (LIST (val ...))) (OK (NAT n)))]
  [(side-condition ,(not (redex-match? al (OK (TEXT t)) (term valres))))
   (side-condition ,(not (redex-match? al (OK (LIST (val ...))) (term valres))))
   ------------------------- "fail"
   (eval-exp/len valres FAIL)])

;;; Dot, indexing, slicing, and update

;; rule Eval_exp/dot, given the base
(define-relation al
  #:mode (eval-exp/dot I I I O)
  #:contract (eval-exp/dot ctx atom valres valres)
  [(eval-path C val (DOT ROOT atom) valres)
   ------------------------------------- "dot"
   (eval-exp/dot C atom (OK val) valres)]
  [------------------------------- "fail"
   (eval-exp/dot C atom FAIL FAIL)])

;; rule Eval_exp/idx, given the base
(define-relation al
  #:mode (eval-exp/idx I I I O)
  #:contract (eval-exp/idx ctx exp valres valres)
  [(eval-path C val_b (IDX ROOT exp_i) valres)
   ---------------------------------------- "idx"
   (eval-exp/idx C exp_i (OK val_b) valres)]
  [-------------------------------- "fail"
   (eval-exp/idx C exp_i FAIL FAIL)])

;; rule Eval_exp/slice, given the base
(define-relation al
  #:mode (eval-exp/slice I I I I O)
  #:contract (eval-exp/slice ctx exp exp valres valres)
  [(eval-path C val_b (SLICE ROOT exp_i exp_n) valres)
   ------------------------------------------------ "slice"
   (eval-exp/slice C exp_i exp_n (OK val_b) valres)]
  [---------------------------------------- "fail"
   (eval-exp/slice C exp_i exp_n FAIL FAIL)])

;; rule Eval_exp/upd, given the base and the new value
(define-relation al
  #:mode (eval-exp/upd I I I O)
  #:contract (eval-exp/upd ctx path valsres valres)
  [(eval-path-upd C val_b path val_f valres)
   ---------------------------------------------- "upd"
   (eval-exp/upd C path (OK (val_b val_f)) valres)]
  [(side-condition ,(not (redex-match? al (OK (val_1 val_2)) (term valsres))))
   ---------------------------------- "fail"
   (eval-exp/upd C path valsres FAIL)])

;;; Meta-function calls

;; rule Eval_exp/call, given the outputs of Eval_targs
(define-relation al
  #:mode (eval-exp/call I I I I O)
  #:contract (eval-exp/call ctx id ((typ ...) ...) (arg ...) valres)
  [(eval-args C (arg ...) valsres)
   (eval-exp/call-args C id (typ_input ...) valsres valres)
   ------------------------------------------------------- "call"
   (eval-exp/call C id ((typ_input ...)) (arg ...) valres)]
  [-------------------------------------- "fail"
   (eval-exp/call C id () (arg ...) FAIL)])

;; rule Eval_exp/call, given the arguments. Deviation: Call_func gets
;; typ_input*, where watsup passes targ* and leaves typ_input* unused.
(define-relation al
  #:mode (eval-exp/call-args I I I I O)
  #:contract (eval-exp/call-args ctx id (typ ...) valsres valres)
  [(where (valres_call ...)
          ,(judgment-holds (call-func C id (typ_input ...) (val_input ...) valres_out) valres_out))
   (eval-exp/call-func (valres_call ...) valres)
   --------------------------------------------------------------------- "call"
   (eval-exp/call-args C id (typ_input ...) (OK (val_input ...)) valres)]
  [-------------------------------------------------- "fail"
   (eval-exp/call-args C id (typ_input ...) FAIL FAIL)])

;; rule Eval_exp/call, given the outputs of Call_func
(define-relation al
  #:mode (eval-exp/call-func I O)
  #:contract (eval-exp/call-func (valres ...) valres)
  [----------------------------------- "call"
   (eval-exp/call-func (valres) valres)]
  [---------------------------- "fail"
   (eval-exp/call-func () FAIL)])

;;; Iterated expressions

;; rulegroup Eval_exp/iter, given the iterated variable, if any
(define-relation al
  #:mode (eval-exp/iter I I I I O)
  #:contract (eval-exp/iter ctx exp iterexp (varr ...) valres)
  [(where (val) (find-varr C varr))
   --------------------------------------------- "simple"
   (eval-exp/iter C exp iterexp (varr) (OK val))]
  [(where () (find-varr C varr))
   ----------------------------------------- "simple-fail"
   (eval-exp/iter C exp iterexp (varr) FAIL)]
  [(where (C_sub ...) (sub-opt C (vari ...)))
   (eval-exp-subs (C_sub ...) exp valsres)
   (eval-exp/iter-subs QUEST valsres valres)
   -------------------------------------------------- "opt"
   (eval-exp/iter C exp (QUEST (vari ...)) () valres)]
  [(where ⊥ (sub-opt C (vari ...)))
   ------------------------------------------------ "opt-fail"
   (eval-exp/iter C exp (QUEST (vari ...)) () FAIL)]
  [(where (C_sub ...) (sub-list C (vari ...)))
   (eval-exp-subs (C_sub ...) exp valsres)
   (eval-exp/iter-subs STAR valsres valres)
   ------------------------------------------------- "list"
   (eval-exp/iter C exp (STAR (vari ...)) () valres)]
  [(where ⊥ (sub-list C (vari ...)))
   ----------------------------------------------- "list-fail"
   (eval-exp/iter C exp (STAR (vari ...)) () FAIL)])

;; rulegroup Eval_exp/iter, given exp under each sub-context
(define-relation al
  #:mode (eval-exp/iter-subs I I O)
  #:contract (eval-exp/iter-subs iter valsres valres)
  [------------------------------------------------------------ "opt"
   (eval-exp/iter-subs QUEST (OK (val ...)) (OK (OPT (val ...))))]
  [------------------------------------------------------------ "list"
   (eval-exp/iter-subs STAR (OK (val ...)) (OK (LIST (val ...))))]
  [---------------------------------- "fail"
   (eval-exp/iter-subs iter FAIL FAIL)])

;;; Sequences, which stop at the first FAIL

;; (Eval_exp: C |- exp : OK val)*
(define-relation al
  #:mode (eval-exps I I O)
  #:contract (eval-exps ctx (exp ...) valsres)
  [------------------------ "nil"
   (eval-exps C () (OK ()))]
  [(eval-exp C exp_h valres_h)
   (eval-exps/cons C valres_h (exp_t ...) valsres)
   --------------------------------------------- "cons"
   (eval-exps C (exp_h exp_t ...) valsres)])

(define-relation al
  #:mode (eval-exps/cons I I I O)
  #:contract (eval-exps/cons ctx valres (exp ...) valsres)
  [(eval-exps C (exp_t ...) valsres_t)
   (where valsres (cons-valsres val_h valsres_t))
   ------------------------------------------------- "cons-succ"
   (eval-exps/cons C (OK val_h) (exp_t ...) valsres)]
  [----------------------------------------- "cons-fail"
   (eval-exps/cons C FAIL (exp_t ...) FAIL)])

;; (Eval_exp: C_sub |- exp : OK val)*
(define-relation al
  #:mode (eval-exp-subs I I O)
  #:contract (eval-exp-subs (ctx ...) exp valsres)
  [---------------------------- "nil"
   (eval-exp-subs () exp (OK ()))]
  [(eval-exp C_h exp valres_h)
   (eval-exp-subs/cons (C_t ...) exp valres_h valsres)
   ------------------------------------------------- "cons"
   (eval-exp-subs (C_h C_t ...) exp valsres)])

(define-relation al
  #:mode (eval-exp-subs/cons I I I O)
  #:contract (eval-exp-subs/cons (ctx ...) exp valres valsres)
  [(eval-exp-subs (C_t ...) exp valsres_t)
   (where valsres (cons-valsres val_h valsres_t))
   ----------------------------------------------------- "cons-succ"
   (eval-exp-subs/cons (C_t ...) exp (OK val_h) valsres)]
  [--------------------------------------------- "cons-fail"
   (eval-exp-subs/cons (C_t ...) exp FAIL FAIL)])

;;
;; Dot, indexing, and slicing
;;

;; ctx |- val path : res<val>
(define-relation al
  #:mode (eval-path I I I O)
  #:contract (eval-path ctx val path valres)
  [---------------------------------- "root"
   (eval-path C val_b ROOT (OK val_b))]
  [(eval-path C val_b path valres_p)
   (eval-path/dot atom valres_p valres)
   ------------------------------------------ "dot"
   (eval-path C val_b (DOT path atom) valres)]
  [(eval-path C val_b path valres_p)
   (eval-path/idx C exp_i valres_p valres)
   ------------------------------------------- "idx"
   (eval-path C val_b (IDX path exp_i) valres)]
  [(eval-path C val_b path valres_p)
   (eval-path/slice C exp_i exp_n valres_p valres)
   --------------------------------------------------- "slice"
   (eval-path C val_b (SLICE path exp_i exp_n) valres)])

;; rule Eval_path/dot, given the inner path
(define-relation al
  #:mode (eval-path/dot I I O)
  #:contract (eval-path/dot atom valres valres)
  [(where ((atom_field val_field) ...) (valfield ...))
   (where (val) (assoc- atom ((atom_field val_field) ...)))
   ------------------------------------------------------ "dot"
   (eval-path/dot atom (OK (STR (valfield ...))) (OK val))]
  [(where ((atom_field val_field) ...) (valfield ...))
   (where () (assoc- atom ((atom_field val_field) ...)))
   -------------------------------------------------- "dot-fail"
   (eval-path/dot atom (OK (STR (valfield ...))) FAIL)]
  [(side-condition ,(not (redex-match? al (OK (STR (valfield ...))) (term valres))))
   -------------------------------- "fail"
   (eval-path/dot atom valres FAIL)])

;; rulegroup Eval_path/idx, given the inner path
(define-relation al
  #:mode (eval-path/idx I I I O)
  #:contract (eval-path/idx ctx exp valres valres)
  [(eval-exp C exp_i valres_i)
   (eval-path/idx-index (TEXT t) valres_i valres)
   --------------------------------------------- "text"
   (eval-path/idx C exp_i (OK (TEXT t)) valres)]
  [(eval-exp C exp_i valres_i)
   (eval-path/idx-index (LIST (val ...)) valres_i valres)
   ----------------------------------------------------- "list"
   (eval-path/idx C exp_i (OK (LIST (val ...))) valres)]
  [(side-condition ,(not (redex-match? al (OK (TEXT t)) (term valres_p))))
   (side-condition ,(not (redex-match? al (OK (LIST (val ...))) (term valres_p))))
   ------------------------------------ "fail"
   (eval-path/idx C exp_i valres_p FAIL)])

;; rulegroup Eval_path/idx, given the index
(define-relation al
  #:mode (eval-path/idx-index I I O)
  #:contract (eval-path/idx-index val valres valres)
  [(side-condition ,(< (term n) (text-length (term t))))
   (where t_res ,(text-idx (term t) (term n)))
   ------------------------------------------------------------- "text"
   (eval-path/idx-index (TEXT t) (OK (NAT n)) (OK (TEXT t_res)))]
  [(side-condition ,(not (< (term n) (text-length (term t)))))
   ------------------------------------------------- "text-fail"
   (eval-path/idx-index (TEXT t) (OK (NAT n)) FAIL)]
  [(side-condition ,(< (term n) (length (term (val ...)))))
   (where val_res ,(list-idx (term (val ...)) (term n)))
   ------------------------------------------------------------ "list"
   (eval-path/idx-index (LIST (val ...)) (OK (NAT n)) (OK val_res))]
  [(side-condition ,(not (< (term n) (length (term (val ...))))))
   --------------------------------------------------------- "list-fail"
   (eval-path/idx-index (LIST (val ...)) (OK (NAT n)) FAIL)]
  [(side-condition ,(not (redex-match? al ((TEXT t) (OK (NAT n))) (term (val_b valres_i)))))
   (side-condition
    ,(not (redex-match? al ((LIST (val ...)) (OK (NAT n))) (term (val_b valres_i)))))
   ----------------------------------------- "fail"
   (eval-path/idx-index val_b valres_i FAIL)])

;; rulegroup Eval_path/slice, given the inner path
(define-relation al
  #:mode (eval-path/slice I I I I O)
  #:contract (eval-path/slice ctx exp exp valres valres)
  [(eval-exp C exp_i valres_i)
   (eval-path/slice-start C exp_n (TEXT t) valres_i valres)
   ----------------------------------------------------- "text"
   (eval-path/slice C exp_i exp_n (OK (TEXT t)) valres)]
  [(eval-exp C exp_i valres_i)
   (eval-path/slice-start C exp_n (LIST (val ...)) valres_i valres)
   ------------------------------------------------------------- "list"
   (eval-path/slice C exp_i exp_n (OK (LIST (val ...))) valres)]
  [(side-condition ,(not (redex-match? al (OK (TEXT t)) (term valres_p))))
   (side-condition ,(not (redex-match? al (OK (LIST (val ...))) (term valres_p))))
   -------------------------------------------- "fail"
   (eval-path/slice C exp_i exp_n valres_p FAIL)])

;; rulegroup Eval_path/slice, given the start
(define-relation al
  #:mode (eval-path/slice-start I I I I O)
  #:contract (eval-path/slice-start ctx exp val valres valres)
  [(eval-exp C exp_n valres_n)
   (eval-path/slice-length (TEXT t) n_i valres_n valres)
   ------------------------------------------------------------ "text"
   (eval-path/slice-start C exp_n (TEXT t) (OK (NAT n_i)) valres)]
  [(eval-exp C exp_n valres_n)
   (eval-path/slice-length (LIST (val ...)) n_i valres_n valres)
   -------------------------------------------------------------------- "list"
   (eval-path/slice-start C exp_n (LIST (val ...)) (OK (NAT n_i)) valres)]
  [(side-condition ,(not (redex-match? al ((TEXT t) (OK (NAT n))) (term (val_b valres_i)))))
   (side-condition
    ,(not (redex-match? al ((LIST (val ...)) (OK (NAT n))) (term (val_b valres_i)))))
   -------------------------------------------------- "fail"
   (eval-path/slice-start C exp_n val_b valres_i FAIL)])

;; rulegroup Eval_path/slice, given the length. No premise bounds the slice,
;; so one out of bounds raises an error.
(define-relation al
  #:mode (eval-path/slice-length I I I O)
  #:contract (eval-path/slice-length val nat valres valres)
  [(where t_res ,(text-slice (term t) (term n_i) (term n_n)))
   ------------------------------------------------------------------------ "text"
   (eval-path/slice-length (TEXT t) n_i (OK (NAT n_n)) (OK (TEXT t_res)))]
  [(where (val_res ...) ,(list-slice (term (val ...)) (term n_i) (term n_n)))
   ------------------------------------------------------------------------------------ "list"
   (eval-path/slice-length (LIST (val ...)) n_i (OK (NAT n_n)) (OK (LIST (val_res ...))))]
  [(side-condition ,(not (redex-match? al ((TEXT t) (OK (NAT n))) (term (val_b valres_n)))))
   (side-condition
    ,(not (redex-match? al ((LIST (val ...)) (OK (NAT n))) (term (val_b valres_n)))))
   ------------------------------------------------ "fail"
   (eval-path/slice-length val_b n_i valres_n FAIL)])

;;
;; Update
;;

;; ctx |- val path := val : res<val>
(define-relation al
  #:mode (eval-path-upd I I I I O)
  #:contract (eval-path-upd ctx val path val valres)
  [--------------------------------------------- "root"
   (eval-path-upd C val_b ROOT val_n (OK val_n))]
  [(eval-path C val_b path valres_p)
   (eval-path-upd/idx C val_b path exp_i val_n valres_p valres)
   ----------------------------------------------------- "idx"
   (eval-path-upd C val_b (IDX path exp_i) val_n valres)]
  [(eval-path-upd/slice C val_b path exp_i exp_n val_n valres)
   ------------------------------------------------------------- "slice"
   (eval-path-upd C val_b (SLICE path exp_i exp_n) val_n valres)]
  [(eval-path C val_b path valres_p)
   (eval-path-upd/dot C val_b path atom val_n valres_p valres)
   ----------------------------------------------------- "dot"
   (eval-path-upd C val_b (DOT path atom) val_n valres)])

;; rulegroup Eval_path_upd/idx, given the inner path. The text rule needs a
;; text as the new value, the list rule any value.
(define-relation al
  #:mode (eval-path-upd/idx I I I I I I O)
  #:contract (eval-path-upd/idx ctx val path exp val valres valres)
  [(eval-exp C exp_i valres_i)
   (eval-path-upd/idx-index C val_b path (TEXT t) (TEXT t_n) valres_i valres)
   --------------------------------------------------------------------- "text"
   (eval-path-upd/idx C val_b path exp_i (TEXT t_n) (OK (TEXT t)) valres)]
  [(eval-exp C exp_i valres_i)
   (eval-path-upd/idx-index C val_b path (LIST (val ...)) val_n valres_i valres)
   ------------------------------------------------------------------------ "list"
   (eval-path-upd/idx C val_b path exp_i val_n (OK (LIST (val ...))) valres)]
  [(side-condition ,(not (redex-match? al ((TEXT t_1) (OK (TEXT t_2))) (term (val_n valres_p)))))
   (side-condition ,(not (redex-match? al (OK (LIST (val ...))) (term valres_p))))
   -------------------------------------------------------- "fail"
   (eval-path-upd/idx C val_b path exp_i val_n valres_p FAIL)])

;; rulegroup Eval_path_upd/idx, given the index. No premise bounds the index,
;; so one out of bounds raises an error.
(define-relation al
  #:mode (eval-path-upd/idx-index I I I I I I O)
  #:contract (eval-path-upd/idx-index ctx val path val val valres valres)
  [(side-condition ,(= (text-length (term t_n)) 1))
   (where t_upd ,(text-upd (term t) (term n_target) (term t_n)))
   (eval-path-upd C val_b path (TEXT t_upd) valres)
   ----------------------------------------------------------------------------- "text"
   (eval-path-upd/idx-index C val_b path (TEXT t) (TEXT t_n) (OK (NAT n_target)) valres)]
  [(side-condition ,(not (= (text-length (term t_n)) 1)))
   --------------------------------------------------------------------------- "text-fail"
   (eval-path-upd/idx-index C val_b path (TEXT t) (TEXT t_n) (OK (NAT n_target)) FAIL)]
  [(where (val_upd ...) ,(list-upd (term (val ...)) (term n_target) (term val_n)))
   (eval-path-upd C val_b path (LIST (val_upd ...)) valres)
   -------------------------------------------------------------------------------- "list"
   (eval-path-upd/idx-index C val_b path (LIST (val ...)) val_n (OK (NAT n_target)) valres)]
  [(side-condition
    ,(not (redex-match? al ((TEXT t_1) (TEXT t_2) (OK (NAT n))) (term (val_c val_n valres_i)))))
   (side-condition
    ,(not (redex-match? al ((LIST (val ...)) val_2 (OK (NAT n))) (term (val_c val_n valres_i)))))
   --------------------------------------------------------------- "fail"
   (eval-path-upd/idx-index C val_b path val_c val_n valres_i FAIL)])

;; rulegroup Eval_path_upd/slice, given the new value, before any premise.
;; Deviation: the list rule takes a list, LIST val_n*, whose elements replace
;; the slice, where watsup takes a val_n and elaborates it into [val_n].
(define-relation al
  #:mode (eval-path-upd/slice I I I I I I O)
  #:contract (eval-path-upd/slice ctx val path exp exp val valres)
  [(eval-path C val_b path valres_p)
   (eval-path-upd/slice-base C val_b path exp_i exp_n (TEXT t_n) valres_p valres)
   ---------------------------------------------------------------- "text"
   (eval-path-upd/slice C val_b path exp_i exp_n (TEXT t_n) valres)]
  [(eval-path C val_b path valres_p)
   (eval-path-upd/slice-base C val_b path exp_i exp_n (LIST (val_n ...)) valres_p valres)
   ------------------------------------------------------------------------ "list"
   (eval-path-upd/slice C val_b path exp_i exp_n (LIST (val_n ...)) valres)]
  [(side-condition ,(not (redex-match? al (TEXT t) (term val_n))))
   (side-condition ,(not (redex-match? al (LIST (val ...)) (term val_n))))
   --------------------------------------------------------- "fail"
   (eval-path-upd/slice C val_b path exp_i exp_n val_n FAIL)])

;; rulegroup Eval_path_upd/slice, given the inner path
(define-relation al
  #:mode (eval-path-upd/slice-base I I I I I I I O)
  #:contract (eval-path-upd/slice-base ctx val path exp exp val valres valres)
  [(eval-exp C exp_i valres_i)
   (eval-path-upd/slice-start C val_b path exp_n (TEXT t) (TEXT t_n) valres_i valres)
   ---------------------------------------------------------------------------------- "text"
   (eval-path-upd/slice-base C val_b path exp_i exp_n (TEXT t_n) (OK (TEXT t)) valres)]
  [(eval-exp C exp_i valres_i)
   (eval-path-upd/slice-start C val_b path exp_n (LIST (val ...)) (LIST (val_n ...))
                              valres_i valres)
   ---------------------------------------------------------------------------- "list"
   (eval-path-upd/slice-base C val_b path exp_i exp_n (LIST (val_n ...))
                             (OK (LIST (val ...))) valres)]
  [(side-condition ,(not (redex-match? al ((TEXT t_1) (OK (TEXT t_2))) (term (val_n valres_p)))))
   (side-condition
    ,(not (redex-match? al ((LIST (val_1 ...)) (OK (LIST (val_2 ...)))) (term (val_n valres_p)))))
   ---------------------------------------------------------------------- "fail"
   (eval-path-upd/slice-base C val_b path exp_i exp_n val_n valres_p FAIL)])

;; rulegroup Eval_path_upd/slice, given the start
(define-relation al
  #:mode (eval-path-upd/slice-start I I I I I I I O)
  #:contract (eval-path-upd/slice-start ctx val path exp val val valres valres)
  [(eval-exp C exp_n valres_n)
   (eval-path-upd/slice-length C val_b path (TEXT t) (TEXT t_n) n_i valres_n valres)
   ---------------------------------------------------------------------------------- "text"
   (eval-path-upd/slice-start C val_b path exp_n (TEXT t) (TEXT t_n) (OK (NAT n_i)) valres)]
  [(eval-exp C exp_n valres_n)
   (eval-path-upd/slice-length C val_b path (LIST (val ...)) (LIST (val_n ...)) n_i
                               valres_n valres)
   ------------------------------------------------------------------------- "list"
   (eval-path-upd/slice-start C val_b path exp_n (LIST (val ...)) (LIST (val_n ...))
                              (OK (NAT n_i)) valres)]
  [(side-condition
    ,(not (redex-match? al ((TEXT t_1) (TEXT t_2) (OK (NAT n))) (term (val_c val_n valres_i)))))
   (side-condition
    ,(not (redex-match? al ((LIST (val_1 ...)) (LIST (val_2 ...)) (OK (NAT n)))
                       (term (val_c val_n valres_i)))))
   ------------------------------------------------------------------- "fail"
   (eval-path-upd/slice-start C val_b path exp_n val_c val_n valres_i FAIL)])

;; rulegroup Eval_path_upd/slice, given the length. No premise bounds the
;; slice, so one out of bounds raises an error, and so does a list of another
;; length than the slice.
(define-relation al
  #:mode (eval-path-upd/slice-length I I I I I I I O)
  #:contract (eval-path-upd/slice-length ctx val path val val nat valres valres)
  [(side-condition ,(= (text-length (term t_n)) (term n_n)))
   (where t_upd ,(text-upd-slice (term t) (term n_i) (term n_n) (term t_n)))
   (eval-path-upd C val_b path (TEXT t_upd) valres)
   ------------------------------------------------------------------------------------- "text"
   (eval-path-upd/slice-length C val_b path (TEXT t) (TEXT t_n) n_i (OK (NAT n_n)) valres)]
  [(side-condition ,(not (= (text-length (term t_n)) (term n_n))))
   ----------------------------------------------------------------------------------- "text-fail"
   (eval-path-upd/slice-length C val_b path (TEXT t) (TEXT t_n) n_i (OK (NAT n_n)) FAIL)]
  [(where (val_upd ...)
          ,(list-upd-slice (term (val ...)) (term n_i) (term n_n) (term (val_n ...))))
   (eval-path-upd C val_b path (LIST (val_upd ...)) valres)
   ------------------------------------------------------------------------- "list"
   (eval-path-upd/slice-length C val_b path (LIST (val ...)) (LIST (val_n ...)) n_i
                               (OK (NAT n_n)) valres)]
  [(side-condition
    ,(not (redex-match? al ((TEXT t_1) (TEXT t_2) (OK (NAT n))) (term (val_c val_n valres_n)))))
   (side-condition
    ,(not (redex-match? al ((LIST (val_1 ...)) (LIST (val_2 ...)) (OK (NAT n)))
                       (term (val_c val_n valres_n)))))
   ------------------------------------------------------------------------ "fail"
   (eval-path-upd/slice-length C val_b path val_c val_n n_i valres_n FAIL)])

;; rule Eval_path_upd/dot, given the inner path
(define-relation al
  #:mode (eval-path-upd/dot I I I I I I O)
  #:contract (eval-path-upd/dot ctx val path atom val valres valres)
  [(where ((atom_field val_field) ...) (valfield ...))
   (where (b_eq ...) ,(for/list ([a (in-list (term (atom_field ...)))])
                        (equal? (term atom) a)))
   (where (valfield_new ...) ((atom_field (ite b_eq val_n val_field)) ...))
   (eval-path-upd C val_b path (STR (valfield_new ...)) valres)
   --------------------------------------------------------------------------- "dot"
   (eval-path-upd/dot C val_b path atom val_n (OK (STR (valfield ...))) valres)]
  [(side-condition ,(not (redex-match? al (OK (STR (valfield ...))) (term valres_p))))
   -------------------------------------------------------- "fail"
   (eval-path-upd/dot C val_b path atom val_n valres_p FAIL)])
