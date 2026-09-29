#lang racket/base

(require (for-syntax racket/base)
         "../common/0.0-prelude.rkt"
         "../common/1-syntax.rkt"
         "../al/1-syntax.rkt")

;; (matches lang pat t ...) tests that each term t matches pat, and
;; (no-matches lang pat t ...) that none does. Failures report t's location.
(define-syntax (matches stx)
  (syntax-case stx ()
    [(_ lang pat t ...)
     #`(begin
         #,@(for/list ([t (in-list (syntax->list #'(t ...)))])
              (quasisyntax/loc t (test-match lang pat (term #,t)))))]))

(define-syntax (no-matches stx)
  (syntax-case stx ()
    [(_ lang pat t ...)
     #`(begin
         #,@(for/list ([t (in-list (syntax->list #'(t ...)))])
              (quasisyntax/loc t (test-no-match lang pat (term #,t)))))]))

;;
;; Common: metavariables and identifiers
;;

(matches Common bool #t #f)
(no-matches Common bool 1 true "true")

(matches Common int 0 42 -3)
(no-matches Common int 1.0 1/2 "1")

(matches Common nat 0 42)
(no-matches Common nat -1 1.0)

(matches Common text "" "x")
(no-matches Common text x #\x)

(matches Common id "x")
(matches Common atom "Some")
(no-matches Common id x)

(matches Common mixop
         ()
         (())
         (("Some") ())
         (("") ("->") ("")))
(no-matches Common mixop
            ("Some")
            (("Some" x))
            (("Some") "x"))

;;
;; Common: types
;;

(matches Common numtyp NAT INT)
(no-matches Common numtyp BOOL (NAT))

(matches Common optyp BOOL NAT INT TEXT)
(no-matches Common optyp FUNC (VAR "t" ()))

(matches Common typ
         BOOL NAT INT TEXT
         (VAR "t" ())
         (VAR "map" (TEXT (VAR "t" ())))
         (TUP ())
         (TUP (NAT BOOL))
         (ITER NAT QUEST)
         (ITER (ITER NAT STAR) STAR)
         FUNC)
(no-matches Common typ
            (VAR "t")
            (VAR t ())
            (TUP NAT)
            (ITER NAT PLUS)
            (ITER NAT (STAR))
            (FUNC)
            (NAT 1))

(matches Common deftyp
         (ALIAS NAT)
         (STRUCT ())
         (STRUCT (("x" NAT) ("y" BOOL)))
         (VARIANT ())
         (VARIANT (((("Some") ()) (NAT)) ((("None")) ()))))
(no-matches Common deftyp
            (ALIAS)
            (ALIAS NAT BOOL)
            (STRUCT ("x" NAT))
            (VARIANT ((("Some") ()) (NAT))))

(matches Common typfield ("x" NAT))
(no-matches Common typfield (x NAT) ("x" NAT BOOL))

(matches Common typcase ((("Some") ()) (NAT)) (() ()))
(no-matches Common typcase ((("Some") ()) NAT))

;;
;; Common: iterators and variables
;;

(matches Common iter QUEST STAR)
(no-matches Common iter PLUS (STAR))

(matches Common vari
         ("x" NAT ())
         ("xs" NAT (STAR))
         ("x" (VAR "t" ()) (QUEST STAR)))
(no-matches Common vari
            ("x" NAT STAR)
            ("x" NAT (PLUS))
            ("x" NAT))

;;
;; Common: numbers and operators
;;

(matches Common num (NAT 0) (INT -1) (INT 3))
(no-matches Common num 3 (NAT -1) (NAT 1.0) (INT 1/2) (NAT))

(matches Common boolunop NOT)
(matches Common boolbinop AND OR IMPL EQUIV)
(matches Common numunop PLUS MINUS)
(matches Common numbinop ADD SUB MUL DIV MOD POW)
(matches Common numcmpop LT GT LE GE)
(matches Common polycmpop EQ NE)

(matches Common unop NOT PLUS MINUS)
(no-matches Common unop ADD EQ)
(matches Common binop AND OR IMPL EQUIV ADD SUB MUL DIV MOD POW)
(no-matches Common binop NOT LT EQ)
(matches Common cmpop EQ NE LT GT LE GE)
(no-matches Common cmpop ADD NOT)

;;
;; Common: values
;;

(matches Common json
         1 -2 2.5 "x" #t #f null
         ()
         (1 "x" (null))
         #hasheq()
         #hasheq((a . 1) (|b c| . (#hasheq((d . null))))))
(no-matches Common json
            x
            (1 x)
            +nan.0
            1/2
            #hasheq(("a" . 1)))

(matches Common val
         (BOOL #t)
         (NAT 3)
         (INT -3)
         (TEXT "s")
         (STR ())
         (STR (("x" (NAT 1)) ("y" (BOOL #f))))
         (INJ ((("Some") ()) ((NAT 3))))
         (INJ ((("None")) ()))
         (TUP ())
         (TUP ((NAT 1) (BOOL #f)))
         (OPT ())
         (OPT ((NAT 3)))
         (LIST ())
         (LIST ((NAT 1) (NAT 2)))
         (FUNC "f")
         (EXT (1 "x"))
         (EXT #hasheq((a . 1))))
(no-matches Common val
            3
            (OPT ((NAT 1) (NAT 2)))
            (OPT (NAT 1))
            (LIST (3))
            (BOOL 1)
            (TEXT x)
            (FUNC)
            (EXT)
            (EXT (1 x))
            (VAR "x"))

(matches Common valfield ("x" (NAT 1)))
(no-matches Common valfield ("x" 1))

(matches Common valcase ((("Some") ()) ((NAT 3))))
(no-matches Common valcase ((("Some") ()) (NAT 3)))

;;
;; Common: expressions
;;

(matches Common exp
         (BOOL #f)
         (NAT 3)
         (INT -3)
         (TEXT "s")
         (VAR "x")
         (UN NOT (VAR "b"))
         (UN MINUS (VAR "i"))
         (BIN ADD (NAT 42) (NAT 77))
         (BIN AND (VAR "a") (VAR "b"))
         (CMP EQ (VAR "x") (VAR "y"))
         (CMP LT (VAR "x") (NAT 1))
         (UPCAST INT (NAT 1))
         (DOWNCAST NAT (VAR "i"))
         (SUB (VAR "v") (VAR "t" ()))
         (MATCH (VAR "xs") CONS)
         (MATCH (VAR "o") (INJ (("Some") ())))
         (TUP ())
         (TUP ((VAR "x") (NAT 1)))
         (INJ ((("Some") ()) ((VAR "x"))))
         (STR ())
         (STR (("x" (VAR "x"))))
         (OPT ())
         (OPT ((VAR "x")))
         (LIST ())
         (LIST ((NAT 1) (VAR "x")))
         (CONS (VAR "h") (VAR "t"))
         (CAT (VAR "xs") (VAR "ys"))
         (MEM (VAR "x") (VAR "xs"))
         (LEN (VAR "xs"))
         (DOT (VAR "s") "x")
         (IDX (VAR "xs") (NAT 0))
         (SLICE (VAR "xs") (NAT 0) (NAT 2))
         (UPD (VAR "s") (DOT ROOT "x") (NAT 1))
         (CALL "f" () ())
         (CALL "f" (NAT (VAR "t" ())) ((EXP (VAR "x")) (FUN "g")))
         (ITER (VAR "x") (STAR (("x" NAT ()))))
         (ITER (VAR "x") (QUEST ())))
(no-matches Common exp
            3
            (VAR x)
            (VAR "x" ())
            (UN NOT 3)
            (UN ADD (VAR "x"))
            (BIN ADD 1 2)
            (CMP ADD (VAR "x") (VAR "y"))
            (MATCH (VAR "x") (FIXED -1))
            (OPT ((NAT 1) (NAT 2)))
            (OPT (NAT 1))
            (STR ("x" (VAR "x")))
            (DOT (VAR "s") x)
            (UPD (VAR "s") (VAR "p") (NAT 1))
            (CALL "f" () ((VAR "x")))
            (CALL "f" ((VAR "t")) ())
            (ITER (VAR "x") (PLUS ()))
            (ITER (VAR "x") (STAR ("x" NAT ())))
            (FUNC "f"))

(matches Common expcase ((("Some") ()) ((VAR "x"))))
(matches Common expfield ("x" (VAR "x")))
(matches Common iterexp
         (STAR ())
         (QUEST (("x" NAT ()) ("ys" INT (STAR)))))
(no-matches Common iterexp (STAR) (PLUS ()))

;;
;; Common: patterns and paths
;;

(matches Common listpattern CONS (FIXED 0) (FIXED 2) NIL)
(no-matches Common listpattern FIXED (FIXED -1) (CONS))

(matches Common optpattern SOME NONE)
(no-matches Common optpattern (SOME))

(matches Common pattern
         (INJ (("Some") ()))
         (INJ ())
         CONS (FIXED 1) NIL
         SOME NONE)
(no-matches Common pattern INJ (INJ "Some") (INJ (("Some") ()) ()))

(matches Common path
         ROOT
         (IDX ROOT (NAT 0))
         (SLICE ROOT (NAT 0) (NAT 1))
         (DOT ROOT "x")
         (DOT (IDX ROOT (VAR "i")) "x"))
(no-matches Common path
            (ROOT)
            (VAR "x")
            (DOT ROOT x)
            (IDX (VAR "xs") (NAT 0)))

;;
;; Common: arguments and type parameters
;;

(matches Common targ NAT (VAR "t" ()))
(no-matches Common targ (VAR "t"))

(matches Common arg (EXP (VAR "x")) (FUN "f"))
(no-matches Common arg (EXP NAT) (FUN f) (FUN "f" () () NAT) (VAR "x"))

(matches Common tparam "X")
(no-matches Common tparam X)

;;
;; AL-syntax: parameters and premises
;;

(matches AL-syntax param
         (EXP NAT)
         (EXP (VAR "t" ()))
         (FUN "f" () () NAT)
         (FUN "f" ("X") ((EXP (VAR "X" ())) (FUN "g" () () BOOL)) BOOL))
(no-matches AL-syntax param
            (EXP (VAR "x"))
            (FUN "f")
            (FUN "f" () NAT))

(matches AL-syntax iterprem
         (QUEST () ())
         (STAR (("x" NAT ())) (("xs" NAT (STAR)))))
(no-matches AL-syntax iterprem
            (STAR (("x" NAT ())))
            (PLUS () ()))

(matches AL-syntax prem
         (REL "Sub" ((VAR "t1") (VAR "t2")) ())
         (REL "Eval" ((VAR "e")) ((VAR "v")))
         (IF (BOOL #t))
         (IFHOLD "Sub" ((VAR "t")))
         (IFNOTHOLD "Sub" ())
         (LET (VAR "x") (NAT 1))
         (ITER (IF (VAR "b")) (STAR (("b" BOOL ())) ()))
         (ITER (ITER (IF (VAR "b")) (STAR () ())) (QUEST () ()))
         (DEBUG (TEXT "msg")))
(no-matches AL-syntax prem
            (REL "R" (VAR "x") ())
            (REL "R" ((VAR "x")))
            (IF (VAR "x") (VAR "y"))
            (IFHOLD "R" (VAR "x"))
            (LET (VAR "x"))
            (ITER (IF (VAR "b")) (STAR (("b" BOOL ()))))
            (ITER (VAR "b") (STAR () ()))
            (DEBUG))

;;
;; AL-syntax: definitions
;;

(matches AL-syntax rulmatch
         (() ())
         (((VAR "x")) ((IF (VAR "b")))))
(no-matches AL-syntax rulmatch ((VAR "x") ()))

(matches AL-syntax rulpath ("base" ((NAT 0)) ()))
(no-matches AL-syntax rulpath ("base" (NAT 0) ()) (((NAT 0)) ()))

(matches AL-syntax rulgroup
         ("g" (((VAR "x")) ()) ())
         ("g" (((VAR "x")) ())
              (("r1" ((NAT 0)) ())
               ("r2" ((NAT 1)) ((IF (VAR "b")))))))
(no-matches AL-syntax rulgroup ("g" (((VAR "x")) ()) ("r" ((NAT 0)) ())))

(matches AL-syntax elsgroup ("g" (((VAR "x")) ()) ("r" ((NAT 0)) ())))
(no-matches AL-syntax elsgroup
            ("g" (((VAR "x")) ()) ())
            ("g" (((VAR "x")) ()) (("r" ((NAT 0)) ()))))

(matches AL-syntax clause
         (() (NAT 0) ())
         (((EXP (VAR "n")) (FUN "f")) (VAR "n") ((IF (VAR "b")))))
(no-matches AL-syntax clause ((EXP (VAR "n")) (VAR "n") ()) (() (NAT 0)))

(matches AL-syntax elsclause (() (NAT 0) ()))

(matches AL-syntax tblrow (((EXP (NAT 0))) (NAT 1) ()))
(no-matches AL-syntax tblrow ((EXP (NAT 0)) (NAT 1) ()))

(define-term rulgroup-ex ("g" (((VAR "x")) ()) (("r" ((NAT 0)) ()))))
(define-term elsgroup-ex ("g" (((VAR "x")) ()) ("r" ((NAT 0)) ())))
(define-term clause-ex (((EXP (VAR "n"))) (VAR "n") ()))

(matches AL-syntax defn
         (EXTTYP "json")
         (TYP "t" () (ALIAS NAT))
         (TYP "list" ("X") (VARIANT (((("Nil")) ()))))
         (EXTREL "Ext" (NAT) (BOOL))
         (REL "R" (NAT) () () ())
         (REL "R" (NAT) (NAT) (rulgroup-ex rulgroup-ex) ())
         (REL "R" (NAT) (NAT) (rulgroup-ex) (elsgroup-ex))
         (EXTFUNC "f" () () NAT)
         (BUILTINFUNC "rev_" ("X") ((EXP (ITER (VAR "X" ()) STAR)))
                      (ITER (VAR "X" ()) STAR))
         (TABLEFUNC "tbl" ((EXP NAT)) NAT ())
         (TABLEFUNC "tbl" ((EXP NAT)) NAT (clause-ex))
         (FUNC "f" () ((EXP NAT)) NAT () ())
         (FUNC "f" () ((EXP NAT)) NAT (clause-ex clause-ex) ())
         (FUNC "f" () ((EXP NAT)) NAT (clause-ex) (clause-ex)))
(no-matches AL-syntax defn
            (EXTTYP json)
            (TYP "t" () NAT)
            (REL "R" (NAT) (NAT) (rulgroup-ex))
            (REL "R" (NAT) (NAT) () (elsgroup-ex elsgroup-ex))
            (REL "R" (NAT) (NAT) () rulgroup-ex)
            (EXTFUNC "f" () NAT)
            (TABLEFUNC "tbl" () () NAT ())
            (FUNC "f" () () NAT ())
            (FUNC "f" () () NAT () (clause-ex clause-ex))
            (FUNC "f" () () NAT () clause-ex))

;;
;; AL-syntax: scripts
;;

(matches AL-syntax script ())
(no-matches AL-syntax script (EXTTYP "json"))

;; examples/add.watsup, as `spectec-boot kast` boots it
(define-term add-script
  ((FUNC "main" () () INT
         ((() (VAR "i")
              ((DEBUG (TEXT "Add"))
               (LET (VAR "i") (UPCAST INT (BIN ADD (NAT 42) (NAT 77)))))))
         ())))

(matches AL-syntax script add-script)

(test-results)
