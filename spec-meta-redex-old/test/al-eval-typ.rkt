#lang racket/base

(require racket/port
         rackunit
         "../common/0.0-prelude.rkt"
         "../al/5.1-eval-typ.rkt")

(define-term tdenv-g
  (("int" (DEF () (ALIAS INT)))
   ("nat" (DEF () (ALIAS NAT)))
   ("pair" (DEF ("X" "Y") (ALIAS (TUP ((VAR "X" ()) (VAR "Y" ()))))))
   ;; Substituting X fails.
   ("app" (DEF ("X") (ALIAS (VAR "X" (NAT)))))
   ("shadow" (DEF () (ALIAS INT)))
   ("json" EXT)
   ("param" PARAM)
   ("option" (DEF ("X") (VARIANT (((("Some") ()) ((VAR "X" ()))) ((("None")) ())))))
   ("wrap" (DEF ("X") (VARIANT (((("Wrap") ()) ((VAR "X" (NAT))))))))
   ("record" (DEF ("X") (STRUCT (("a" (VAR "X" ())) ("b" BOOL)))))
   ("box" (DEF ("X") (STRUCT (("a" (VAR "X" (NAT)))))))))

;; The local layer shadows "shadow" with a PARAM.
(define-term C-typs
  {GLOBAL {TYP tdenv-g REL () FUNC () VAL ()}
   LOCAL {TYP (("lint" (DEF () (ALIAS INT))) ("shadow" PARAM)) REL () FUNC () VAL ()}})

;; The number of calls to metafunction f while running thunk, counted in its trace
(define (count-calls f thunk)
  (define trace
    (parameterize ([current-traced-metafunctions (list f)])
      (with-output-to-string thunk)))
  (length (regexp-match* (pregexp (format "\\(~a\\s" f)) trace)))

;; Tuples nested n deep, each with a sibling of type typ_s whose cast fails
(define (nested-tup n typ val typ_s)
  (for/fold ([typ typ] [val val] #:result (list typ val)) ([_ (in-range n)])
    (values `(TUP (,typ ,typ_s)) `(TUP (,val (BOOL #t))))))

;;
;; $upcast
;;

(test-equal (term (upcast C-typs INT (INT -1))) '(OK (INT -1)))
(test-equal (term (upcast C-typs INT (NAT 2))) '(OK (INT 2)))
(test-equal (term (upcast C-typs INT (BOOL #t))) 'FAIL)

;; VAR: an alias found in either layer
(test-equal (term (upcast C-typs (VAR "int" ()) (NAT 2))) '(OK (INT 2)))
(test-equal (term (upcast C-typs (VAR "lint" ()) (NAT 2))) '(OK (INT 2)))
(test-equal (term (upcast C-typs (VAR "pair" (INT NAT)) (TUP ((NAT 1) (NAT 2)))))
            '(OK (TUP ((INT 1) (NAT 2)))))
(test-equal (term (upcast C-typs (VAR "int" ()) (TEXT "x"))) 'FAIL)
;; otherwise: not found, not an alias, other arity, or a failing substitution
(test-equal (term (upcast C-typs (VAR "none" ()) (NAT 2))) '(OK (NAT 2)))
(test-equal (term (upcast C-typs (VAR "shadow" ()) (NAT 2))) '(OK (NAT 2)))
(test-equal (term (upcast C-typs (VAR "json" ()) (NAT 2))) '(OK (NAT 2)))
(test-equal (term (upcast C-typs (VAR "option" (INT)) (NAT 2))) '(OK (NAT 2)))
(test-equal (term (upcast C-typs (VAR "int" (INT)) (NAT 2))) '(OK (NAT 2)))
(test-equal (term (upcast C-typs (VAR "pair" (INT)) (TUP ((NAT 1) (NAT 2)))))
            '(OK (TUP ((NAT 1) (NAT 2)))))
(test-equal (term (upcast C-typs (VAR "app" (INT)) (NAT 2))) '(OK (NAT 2)))

;; TUP
(test-equal (term (upcast C-typs (TUP (INT BOOL)) (TUP ((NAT 1) (BOOL #t)))))
            '(OK (TUP ((INT 1) (BOOL #t)))))
(test-equal (term (upcast C-typs (TUP ()) (TUP ()))) '(OK (TUP ())))
(test-equal (term (upcast C-typs (TUP (INT)) (NAT 1))) 'FAIL)
;; otherwise: a component upcast fails, or the lengths differ
(test-equal (term (upcast C-typs (TUP (INT INT)) (TUP ((NAT 1) (BOOL #t)))))
            '(OK (TUP ((NAT 1) (BOOL #t)))))
(test-equal (term (upcast C-typs (TUP (INT)) (TUP ((NAT 1) (NAT 2)))))
            '(OK (TUP ((NAT 1) (NAT 2)))))

;; OPT
(test-equal (term (upcast C-typs (ITER INT QUEST) (OPT ((NAT 1))))) '(OK (OPT ((INT 1)))))
(test-equal (term (upcast C-typs (ITER INT QUEST) (OPT ()))) '(OK (OPT ())))
;; otherwise: the component upcast fails, or not an option
(test-equal (term (upcast C-typs (ITER INT QUEST) (OPT ((BOOL #t))))) '(OK (OPT ((BOOL #t)))))
(test-equal (term (upcast C-typs (ITER INT QUEST) (NAT 1))) '(OK (NAT 1)))
(test-equal (term (upcast C-typs (ITER INT QUEST) (LIST ((NAT 1))))) '(OK (LIST ((NAT 1)))))

;; LIST
(test-equal (term (upcast C-typs (ITER INT STAR) (LIST ((NAT 1) (INT -2)))))
            '(OK (LIST ((INT 1) (INT -2)))))
(test-equal (term (upcast C-typs (ITER INT STAR) (LIST ()))) '(OK (LIST ())))
;; otherwise: an element upcast fails, or not a list
(test-equal (term (upcast C-typs (ITER INT STAR) (LIST ((NAT 1) (TEXT "x")))))
            '(OK (LIST ((NAT 1) (TEXT "x")))))
(test-equal (term (upcast C-typs (ITER INT STAR) (OPT ((NAT 1))))) '(OK (OPT ((NAT 1)))))

;; otherwise: types that are not cast
(test-equal (term (upcast C-typs BOOL (NAT 1))) '(OK (NAT 1)))
(test-equal (term (upcast C-typs NAT (INT -1))) '(OK (INT -1)))
(test-equal (term (upcast C-typs TEXT (TEXT "x"))) '(OK (TEXT "x")))
(test-equal (term (upcast C-typs FUNC (FUNC "f"))) '(OK (FUNC "f")))

;; Nested
(test-equal (term (upcast C-typs (ITER (TUP ((VAR "int" ()) (ITER INT QUEST))) STAR)
                          (LIST ((TUP ((NAT 1) (OPT ((NAT 2))))) (TUP ((INT 3) (OPT ())))))))
            '(OK (LIST ((TUP ((INT 1) (OPT ((INT 2))))) (TUP ((INT 3) (OPT ())))))))

;; Each component is upcast once: two calls per level and one for the leaf.
(let ([tv (nested-tup 8 'INT '(NAT 1) 'INT)])
  (define (run) (term (upcast C-typs ,(car tv) ,(cadr tv))))
  (test-equal (run) `(OK ,(cadr tv)))
  (check-equal? (count-calls 'upcast run) 17))

;;
;; $downcast
;;

(test-equal (term (downcast C-typs NAT (NAT 1))) '(OK (NAT 1)))
(test-equal (term (downcast C-typs NAT (INT 2))) '(OK (NAT 2)))
(test-equal (term (downcast C-typs NAT (INT 0))) '(OK (NAT 0)))
(test-equal (term (downcast C-typs NAT (TEXT "x"))) 'FAIL)
;; otherwise: a negative INT
(test-equal (term (downcast C-typs NAT (INT -1))) '(OK (INT -1)))

;; VAR: an alias found in either layer
(test-equal (term (downcast C-typs (VAR "nat" ()) (INT 2))) '(OK (NAT 2)))
(test-equal (term (downcast C-typs (VAR "pair" (NAT INT)) (TUP ((INT 1) (INT 2)))))
            '(OK (TUP ((NAT 1) (INT 2)))))
(test-equal (term (downcast C-typs (VAR "nat" ()) (BOOL #t))) 'FAIL)
;; otherwise: not found, not an alias, other arity, or a failing substitution
(test-equal (term (downcast C-typs (VAR "none" ()) (INT 2))) '(OK (INT 2)))
(test-equal (term (downcast C-typs (VAR "param" ()) (INT 2))) '(OK (INT 2)))
(test-equal (term (downcast C-typs (VAR "record" (NAT)) (INT 2))) '(OK (INT 2)))
(test-equal (term (downcast C-typs (VAR "nat" (NAT)) (INT 2))) '(OK (INT 2)))
(test-equal (term (downcast C-typs (VAR "app" (NAT)) (INT 2))) '(OK (INT 2)))

;; TUP
(test-equal (term (downcast C-typs (TUP (NAT BOOL)) (TUP ((INT 1) (BOOL #t)))))
            '(OK (TUP ((NAT 1) (BOOL #t)))))
(test-equal (term (downcast C-typs (TUP ()) (TUP ()))) '(OK (TUP ())))
(test-equal (term (downcast C-typs (TUP (NAT)) (INT 1))) 'FAIL)
;; otherwise: a component downcast fails, or the lengths differ
(test-equal (term (downcast C-typs (TUP (NAT NAT)) (TUP ((INT 1) (BOOL #t)))))
            '(OK (TUP ((INT 1) (BOOL #t)))))
(test-equal (term (downcast C-typs (TUP (NAT)) (TUP ((INT 1) (INT 2)))))
            '(OK (TUP ((INT 1) (INT 2)))))

;; OPT
(test-equal (term (downcast C-typs (ITER NAT QUEST) (OPT ((INT 1))))) '(OK (OPT ((NAT 1)))))
(test-equal (term (downcast C-typs (ITER NAT QUEST) (OPT ()))) '(OK (OPT ())))
;; otherwise: the component downcast fails, or not an option
(test-equal (term (downcast C-typs (ITER NAT QUEST) (OPT ((BOOL #t))))) '(OK (OPT ((BOOL #t)))))
(test-equal (term (downcast C-typs (ITER NAT QUEST) (INT 1))) '(OK (INT 1)))

;; LIST
(test-equal (term (downcast C-typs (ITER NAT STAR) (LIST ((INT 1) (NAT 2)))))
            '(OK (LIST ((NAT 1) (NAT 2)))))
(test-equal (term (downcast C-typs (ITER NAT STAR) (LIST ()))) '(OK (LIST ())))
;; otherwise: an element downcast fails, or not a list
(test-equal (term (downcast C-typs (ITER NAT STAR) (LIST ((INT 1) (TEXT "x")))))
            '(OK (LIST ((INT 1) (TEXT "x")))))
(test-equal (term (downcast C-typs (ITER NAT STAR) (TUP ((INT 1))))) '(OK (TUP ((INT 1)))))

;; otherwise: types that are not cast
(test-equal (term (downcast C-typs BOOL (NAT 1))) '(OK (NAT 1)))
(test-equal (term (downcast C-typs INT (NAT 1))) '(OK (NAT 1)))
(test-equal (term (downcast C-typs TEXT (TEXT "x"))) '(OK (TEXT "x")))
(test-equal (term (downcast C-typs FUNC (FUNC "f"))) '(OK (FUNC "f")))

;; Each component is downcast once.
(let ([tv (nested-tup 8 'NAT '(INT 1) 'NAT)])
  (define (run) (term (downcast C-typs ,(car tv) ,(cadr tv))))
  (test-equal (run) `(OK ,(cadr tv)))
  (check-equal? (count-calls 'downcast run) 17))

;;
;; $subtyp
;;

(test-equal (term (subtyp tdenv-g BOOL (BOOL #f))) #t)
(test-equal (term (subtyp tdenv-g NAT (NAT 0))) #t)
(test-equal (term (subtyp tdenv-g NAT (INT 0))) #t)
(test-equal (term (subtyp tdenv-g NAT (INT -1))) #f)
(test-equal (term (subtyp tdenv-g INT (NAT 1))) #t)
(test-equal (term (subtyp tdenv-g INT (INT -1))) #t)
(test-equal (term (subtyp tdenv-g TEXT (TEXT ""))) #t)
;; otherwise: a value of another shape
(test-equal (term (subtyp tdenv-g BOOL (NAT 1))) #f)
(test-equal (term (subtyp tdenv-g NAT (BOOL #t))) #f)
(test-equal (term (subtyp tdenv-g INT (TEXT "1"))) #f)
(test-equal (term (subtyp tdenv-g TEXT (BOOL #t))) #f)
(test-equal (term (subtyp tdenv-g FUNC (FUNC "f"))) #f)

;; VAR: EXT
(test-equal (term (subtyp tdenv-g (VAR "json" ()) (EXT (1 "a")))) #t)
;; otherwise: not an EXT value
(test-equal (term (subtyp tdenv-g (VAR "json" ()) (NAT 1))) #f)

;; VAR: ALIAS
(test-equal (term (subtyp tdenv-g (VAR "pair" (NAT BOOL)) (TUP ((NAT 1) (BOOL #t))))) #t)
(test-equal (term (subtyp tdenv-g (VAR "pair" (NAT BOOL)) (TUP ((NAT 1) (NAT 2))))) #f)
;; otherwise: other arity, or a failing substitution
(test-equal (term (subtyp tdenv-g (VAR "pair" (NAT)) (TUP ((NAT 1) (BOOL #t))))) #f)
(test-equal (term (subtyp tdenv-g (VAR "app" (NAT)) (NAT 1))) #f)

;; VAR, otherwise: not found, or a PARAM
(test-equal (term (subtyp tdenv-g (VAR "none" ()) (NAT 1))) #f)
(test-equal (term (subtyp tdenv-g (VAR "param" ()) (NAT 1))) #f)

;; VAR: VARIANT
(test-equal (term (subtyp tdenv-g (VAR "option" (NAT)) (INJ ((("Some") ()) ((NAT 1)))))) #t)
(test-equal (term (subtyp tdenv-g (VAR "option" (NAT)) (INJ ((("None")) ())))) #t)
(test-equal (term (subtyp tdenv-g (VAR "option" (NAT)) (INJ ((("Some") ()) ((INT -1)))))) #f)
;; otherwise: not a case, as many values as the case's types, a failing
;; substitution, not an INJ, or other arity
(test-equal (term (subtyp tdenv-g (VAR "option" (NAT)) (INJ ((("Other") ()) ((NAT 1)))))) #f)
(test-equal (term (subtyp tdenv-g (VAR "option" (NAT)) (INJ ((("Some") ()) ((NAT 1) (NAT 2))))))
            #f)
(test-equal (term (subtyp tdenv-g (VAR "option" (NAT)) (INJ ((("None")) ((NAT 1)))))) #f)
(test-equal (term (subtyp tdenv-g (VAR "wrap" (NAT)) (INJ ((("Wrap") ()) ((NAT 1)))))) #f)
(test-equal (term (subtyp tdenv-g (VAR "option" (NAT)) (NAT 1))) #f)
(test-equal (term (subtyp tdenv-g (VAR "option" ()) (INJ ((("None")) ())))) #f)

;; VAR: STRUCT
(test-equal (term (subtyp tdenv-g (VAR "record" (NAT)) (STR (("a" (NAT 1)) ("b" (BOOL #t))))))
            #t)
(test-equal (term (subtyp tdenv-g (VAR "record" (NAT)) (STR (("a" (INT -1)) ("b" (BOOL #t))))))
            #f)
;; otherwise: other fields, a failing substitution, not a STR, or other arity
(test-equal (term (subtyp tdenv-g (VAR "record" (NAT)) (STR (("b" (BOOL #t)) ("a" (NAT 1))))))
            #f)
(test-equal (term (subtyp tdenv-g (VAR "record" (NAT)) (STR (("a" (NAT 1)))))) #f)
(test-equal (term (subtyp tdenv-g (VAR "box" (NAT)) (STR (("a" (NAT 1)))))) #f)
(test-equal (term (subtyp tdenv-g (VAR "record" (NAT)) (NAT 1))) #f)
(test-equal (term (subtyp tdenv-g (VAR "record" ()) (STR (("a" (NAT 1)) ("b" (BOOL #t)))))) #f)

;; TUP
(test-equal (term (subtyp tdenv-g (TUP (NAT BOOL)) (TUP ((NAT 1) (BOOL #t))))) #t)
(test-equal (term (subtyp tdenv-g (TUP (NAT BOOL)) (TUP ((NAT 1) (NAT 2))))) #f)
(test-equal (term (subtyp tdenv-g (TUP ()) (TUP ()))) #t)
;; otherwise: the lengths differ, or not a tuple
(test-equal (term (subtyp tdenv-g (TUP (NAT)) (TUP ((NAT 1) (NAT 2))))) #f)
(test-equal (term (subtyp tdenv-g (TUP (NAT)) (NAT 1))) #f)

;; OPT
(test-equal (term (subtyp tdenv-g (ITER NAT QUEST) (OPT ((NAT 1))))) #t)
(test-equal (term (subtyp tdenv-g (ITER NAT QUEST) (OPT ((INT -1))))) #f)
(test-equal (term (subtyp tdenv-g (ITER NAT QUEST) (OPT ()))) #t)
;; otherwise: not an option
(test-equal (term (subtyp tdenv-g (ITER NAT QUEST) (NAT 1))) #f)
(test-equal (term (subtyp tdenv-g (ITER NAT QUEST) (LIST ()))) #f)

;; LIST
(test-equal (term (subtyp tdenv-g (ITER NAT STAR) (LIST ((NAT 1) (INT 2))))) #t)
(test-equal (term (subtyp tdenv-g (ITER NAT STAR) (LIST ((NAT 1) (INT -2))))) #f)
(test-equal (term (subtyp tdenv-g (ITER NAT STAR) (LIST ()))) #t)
;; otherwise: not a list
(test-equal (term (subtyp tdenv-g (ITER NAT STAR) (OPT ()))) #f)

;; Nested, through type arguments
(test-equal (term (subtyp tdenv-g (ITER (VAR "option" ((VAR "pair" (NAT (VAR "json" ()))))) STAR)
                          (LIST ((INJ ((("Some") ()) ((TUP ((NAT 1) (EXT "x"))))))
                                 (INJ ((("None")) ()))))))
            #t)

;;
;; $subtyps
;;

(test-equal (term (subtyps tdenv-g (NAT BOOL) ((NAT 1) (BOOL #t)))) #t)
(test-equal (term (subtyps tdenv-g (NAT BOOL) ((NAT 1) (NAT 2)))) #f)
(test-equal (term (subtyps tdenv-g () ())) #t)
;; otherwise: the lengths differ
(test-equal (term (subtyps tdenv-g (NAT) ((NAT 1) (NAT 2)))) #f)
(test-equal (term (subtyps tdenv-g (NAT BOOL) ())) #f)

(test-results)
