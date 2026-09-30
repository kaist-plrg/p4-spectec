#lang racket/base

(require "../common/0.0-prelude.rkt"
         "../al/2-env.rkt")

;;
;; al-base: the common languages and the AL syntax, merged
;;

(test-match al-base script (term ((FUNC "main" () () INT () ()))))
(test-match al-base valres (term (OK (NAT 1))))
(test-match al-base tdenv (term (("t" PARAM))))
(test-equal (length (redex-match al-base (exp ...) (term ((NAT 1) (VAR "x"))))) 1)

;;
;; Relation environment
;;

(define-term rulgroup-ex ("g" (((VAR "x")) ()) (("r" ((NAT 0)) ()))))
(define-term elsgroup-ex ("g" (((VAR "x")) ()) ("r" ((NAT 0)) ())))

(test-match al-env externRelDef (term (EXT "R")))
(test-match al-env definedRelDef (term (DEF () ())))
(test-match al-env definedRelDef (term (DEF (rulgroup-ex rulgroup-ex) ())))
(test-match al-env definedRelDef (term (DEF (rulgroup-ex) (elsgroup-ex))))
(test-no-match al-env definedRelDef (term (DEF (rulgroup-ex) (elsgroup-ex elsgroup-ex))))
(test-no-match al-env definedRelDef (term (DEF (rulgroup-ex))))

(test-match al-env reldef (term (EXT "R")))
(test-match al-env reldef (term (DEF () ())))
(test-no-match al-env reldef (term (EXT R)))
(test-no-match al-env reldef (term (BUILTIN "R" () ())))

(test-match al-env renv (term ()))
(test-match al-env renv (term (("R" (EXT "R")) ("S" (DEF (rulgroup-ex) ())))))
(test-no-match al-env renv (term (("R" PARAM))))

;;
;; Function environment
;;

(define-term clause-ex (((EXP (VAR "n"))) (VAR "n") ()))

(test-match al-env externFuncDef (term (EXT "f")))
(test-match al-env builtinFuncDef (term (BUILTIN "rev_" ("X") ((EXP (ITER (VAR "X" ()) STAR))))))
(test-no-match al-env builtinFuncDef (term (BUILTIN "rev_" ("X"))))
(test-match al-env tableFuncDef (term (TABLE ((EXP NAT)) ())))
(test-match al-env tableFuncDef (term (TABLE ((EXP NAT)) (clause-ex))))
(test-no-match al-env tableFuncDef (term (TABLE ((EXP NAT)) NAT ())))
(test-match al-env definedFuncDef (term (DEF () () ())))
(test-match al-env definedFuncDef (term (DEF ("X") (clause-ex clause-ex) ())))
(test-match al-env definedFuncDef (term (DEF () (clause-ex) (clause-ex))))
(test-no-match al-env definedFuncDef (term (DEF () () (clause-ex clause-ex))))
(test-no-match al-env definedFuncDef (term (DEF () ())))

(test-match al-env funcdef (term (EXT "f")))
(test-match al-env funcdef (term (BUILTIN "f" () ())))
(test-match al-env funcdef (term (TABLE () ())))
(test-match al-env funcdef (term (DEF () () ())))
(test-no-match al-env funcdef (term (DEF () ((NAT 1)) ())))

(test-match al-env fenv (term (("f" (EXT "f")) ("g" (DEF () (clause-ex) ())))))
(test-no-match al-env fenv (term (("f" (EXT "R" "S")))))

(test-results)
