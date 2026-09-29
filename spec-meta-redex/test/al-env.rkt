#lang racket/base

(require "../common/0.0-prelude.rkt"
         "../al/2-env.rkt")

;;
;; AL-base: the common languages and the AL syntax, merged
;;

(test-match AL-base script (term ((FUNC "main" () () INT () ()))))
(test-match AL-base valres (term (OK (NAT 1))))
(test-match AL-base tdenv (term (("t" PARAM))))
(test-equal (length (redex-match AL-base (exp ...) (term ((NAT 1) (VAR "x"))))) 1)

;;
;; Relation environment
;;

(define-term rulgroup-ex ("g" (((VAR "x")) ()) (("r" ((NAT 0)) ()))))
(define-term elsgroup-ex ("g" (((VAR "x")) ()) ("r" ((NAT 0)) ())))

(test-match AL-env externRelDef (term (EXT "R")))
(test-match AL-env definedRelDef (term (DEF () ())))
(test-match AL-env definedRelDef (term (DEF (rulgroup-ex rulgroup-ex) ())))
(test-match AL-env definedRelDef (term (DEF (rulgroup-ex) (elsgroup-ex))))
(test-no-match AL-env definedRelDef (term (DEF (rulgroup-ex) (elsgroup-ex elsgroup-ex))))
(test-no-match AL-env definedRelDef (term (DEF (rulgroup-ex))))

(test-match AL-env reldef (term (EXT "R")))
(test-match AL-env reldef (term (DEF () ())))
(test-no-match AL-env reldef (term (EXT R)))
(test-no-match AL-env reldef (term (BUILTIN "R" () ())))

(test-match AL-env renv (term ()))
(test-match AL-env renv (term (("R" (EXT "R")) ("S" (DEF (rulgroup-ex) ())))))
(test-no-match AL-env renv (term (("R" PARAM))))

;;
;; Function environment
;;

(define-term clause-ex (((EXP (VAR "n"))) (VAR "n") ()))

(test-match AL-env externFuncDef (term (EXT "f")))
(test-match AL-env builtinFuncDef (term (BUILTIN "rev_" ("X") ((EXP (ITER (VAR "X" ()) STAR))))))
(test-no-match AL-env builtinFuncDef (term (BUILTIN "rev_" ("X"))))
(test-match AL-env tableFuncDef (term (TABLE ((EXP NAT)) ())))
(test-match AL-env tableFuncDef (term (TABLE ((EXP NAT)) (clause-ex))))
(test-no-match AL-env tableFuncDef (term (TABLE ((EXP NAT)) NAT ())))
(test-match AL-env definedFuncDef (term (DEF () () ())))
(test-match AL-env definedFuncDef (term (DEF ("X") (clause-ex clause-ex) ())))
(test-match AL-env definedFuncDef (term (DEF () (clause-ex) (clause-ex))))
(test-no-match AL-env definedFuncDef (term (DEF () () (clause-ex clause-ex))))
(test-no-match AL-env definedFuncDef (term (DEF () ())))

(test-match AL-env funcdef (term (EXT "f")))
(test-match AL-env funcdef (term (BUILTIN "f" () ())))
(test-match AL-env funcdef (term (TABLE () ())))
(test-match AL-env funcdef (term (DEF () () ())))
(test-no-match AL-env funcdef (term (DEF () ((NAT 1)) ())))

(test-match AL-env fenv (term (("f" (EXT "f")) ("g" (DEF () (clause-ex) ())))))
(test-no-match AL-env fenv (term (("f" (EXT "R" "S")))))

(test-results)
