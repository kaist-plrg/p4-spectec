#lang racket/base

(require racket/list
         rackunit
         "../common/0.0-prelude.rkt"
         "../al/5.3-eval-exp.rkt"
         "machine.rkt")

(define coverage (start-coverage ->redex/eval-exp ->ctx/eval-exp))

;; Raises when evaluated, where a premise must not be evaluated
(define DIV0 (term (BIN DIV (NAT 1) (NAT 0))))

;; G binds a value too, which no variable finds, and types that L partly
;; shadows. Its functions add their arguments to 1, or to each other.
(define-term G-vals
  {TYP (("T" (DEF () (ALIAS INT)))
        ("S" (DEF () (ALIAS INT))))
   REL ()
   FUNC (("inc" (DEF () ((((EXP (VAR "n"))) (BIN ADD (VAR "n") (NAT 1)) ())) ()))
         ("add" (DEF () ((((EXP (VAR "a")) (EXP (VAR "b"))) (BIN ADD (VAR "a") (VAR "b")) ()))
                     ())))
   VAL ((("g" ()) (NAT 0)))})

(define-term L-vals
  {TYP (("S" (DEF () (ALIAS BOOL)))
        ("U" (DEF () (ALIAS NAT))))
   REL () FUNC ()
   VAL ((("x" ()) (NAT 1))
        (("b" ()) (BOOL #t))
        (("i" ()) (INT -2))
        (("f" ()) (FUNC "f"))
        (("l" ()) (LIST ((NAT 2) (NAT 3))))
        (("s" ()) (STR (("A" (NAT 1)) ("B" (TEXT "ab")))))
        (("xs" (STAR)) (LIST ((NAT 2))))
        (("ns" (STAR)) (LIST ((NAT 1) (NAT 2) (NAT 3))))
        (("ms" (STAR)) (LIST ((NAT 10) (NAT 20) (NAT 30))))
        (("ks" (STAR)) (LIST ((NAT 7))))
        (("ws" (STAR)) (NAT 0))
        (("o" (QUEST)) (OPT ((NAT 7))))
        (("p" (QUEST)) (OPT ((NAT 8))))
        (("n" (QUEST)) (OPT ()))
        (("m" (QUEST)) (OPT ())))})

(define (eval-exp e)
  (eval-in (term G-vals) (term L-vals) e))

(define (trace-exp e)
  (trace-in (term G-vals) (term L-vals) e))

(define (raises-on e rx)
  (check-exn rx (λ () (eval-exp e))))

;;
;; Literals
;;

(test-equal (eval-exp (term (BOOL #t))) (term (OK (BOOL #t))))
(test-equal (eval-exp (term (BOOL #f))) (term (OK (BOOL #f))))
(test-equal (eval-exp (term (NAT 3))) (term (OK (NAT 3))))
(test-equal (eval-exp (term (INT -3))) (term (OK (INT -3))))
(test-equal (eval-exp (term (TEXT "a"))) (term (OK (TEXT "a"))))
(test-equal (trace-exp (term (NAT 3))) '("eval-exp/literal/number"))

;;
;; Variables
;;

(test-equal (eval-exp (term (VAR "x"))) (term (OK (NAT 1))))
(test-equal (eval-exp (term (VAR "f"))) (term (OK (FUNC "f"))))
;; otherwise: unbound, bound only with iterators, or bound only in G
(test-equal (eval-exp (term (VAR "y"))) 'FAIL)
(test-equal (eval-exp (term (VAR "xs"))) 'FAIL)
(test-equal (eval-exp (term (VAR "g"))) 'FAIL)
(test-equal (trace-exp (term (VAR "y"))) '("eval-exp/variable/fail"))

;;
;; Unary operators
;;

(test-equal (eval-exp (term (UN NOT (BOOL #t)))) (term (OK (BOOL #f))))
(test-equal (eval-exp (term (UN NOT (VAR "b")))) (term (OK (BOOL #f))))
(test-equal (eval-exp (term (UN PLUS (NAT 1)))) (term (OK (NAT 1))))
(test-equal (eval-exp (term (UN PLUS (VAR "i")))) (term (OK (INT -2))))
(test-equal (eval-exp (term (UN MINUS (VAR "x")))) (term (OK (INT -1))))
(test-equal (eval-exp (term (UN MINUS (INT -2)))) (term (OK (INT 2))))
(test-equal (eval-exp (term (UN NOT (UN NOT (BOOL #f))))) (term (OK (BOOL #f))))
(test-equal (eval-exp (term (UN MINUS (UN MINUS (NAT 2))))) (term (OK (INT 2))))
(test-equal (trace-exp (term (UN MINUS (VAR "x"))))
            '("eval-exp/variable" "eval-exp/unary/number"))
;; otherwise: an operand of the wrong kind
(test-equal (eval-exp (term (UN NOT (NAT 1)))) 'FAIL)
(test-equal (eval-exp (term (UN NOT (TUP ())))) 'FAIL)
(test-equal (eval-exp (term (UN PLUS (BOOL #t)))) 'FAIL)
(test-equal (eval-exp (term (UN MINUS (TEXT "1")))) 'FAIL)
(test-equal (eval-exp (term (UN MINUS (VAR "f")))) 'FAIL)
(test-equal (trace-exp (term (UN NOT (NAT 1))))
            '("eval-exp/literal/number" "eval-exp/unary/fail"))
;; A failing operand fails the operator.
(test-equal (eval-exp (term (UN NOT (VAR "y")))) 'FAIL)
(test-equal (trace-exp (term (UN NOT (VAR "y"))))
            '("eval-exp/variable/fail" "frame/fail"))

;;
;; Binary operators
;;

(test-equal (eval-exp (term (BIN AND (BOOL #t) (VAR "b")))) (term (OK (BOOL #t))))
(test-equal (eval-exp (term (BIN OR (BOOL #f) (BOOL #f)))) (term (OK (BOOL #f))))
(test-equal (eval-exp (term (BIN IMPL (BOOL #f) (BOOL #f)))) (term (OK (BOOL #t))))
(test-equal (eval-exp (term (BIN EQUIV (BOOL #f) (BOOL #t)))) (term (OK (BOOL #f))))
(test-equal (eval-exp (term (BIN ADD (VAR "x") (NAT 2)))) (term (OK (NAT 3))))
(test-equal (eval-exp (term (BIN MUL (VAR "i") (INT 3)))) (term (OK (INT -6))))
(test-equal (eval-exp (term (BIN SUB (NAT 1) (NAT 3)))) (term (OK (INT -2))))
(test-equal (trace-exp (term (BIN ADD (VAR "x") (NAT 2))))
            '("eval-exp/variable" "eval-exp/literal/number" "eval-exp/binary/number"))
;; Both operands are evaluated, even when the left decides an AND.
(raises-on (term (BIN AND (BOOL #f) ,DIV0)) #rx"quotient: undefined for 0")
;; A nat and an int
(test-equal (eval-exp (term (BIN ADD (VAR "x") (INT 2)))) 'FAIL)
(test-equal (trace-exp (term (BIN ADD (VAR "x") (INT 2))))
            '("eval-exp/variable" "eval-exp/literal/number" "eval-exp/binary/number/fail"))
;; The left operand fits no rule, and the right one is not evaluated.
(test-equal (eval-exp (term (BIN AND (NAT 1) ,DIV0))) 'FAIL)
(test-equal (eval-exp (term (BIN ADD (BOOL #t) ,DIV0))) 'FAIL)
(test-equal (trace-exp (term (BIN ADD (TEXT "1") ,DIV0)))
            '("eval-exp/literal/string" "eval-exp/binary/fail-left"))
;; The right operand fits no rule.
(test-equal (eval-exp (term (BIN AND (BOOL #t) (NAT 1)))) 'FAIL)
(test-equal (eval-exp (term (BIN ADD (NAT 1) (BOOL #t)))) 'FAIL)
(test-equal (trace-exp (term (BIN OR (BOOL #t) (TUP ()))))
            '("eval-exp/literal/boolean" "eval-exp/tuple" "eval-exp/binary/fail"))
;; A failing operand
(test-equal (eval-exp (term (BIN ADD (VAR "y") ,DIV0))) 'FAIL)
(test-equal (eval-exp (term (BIN ADD (NAT 1) (VAR "y")))) 'FAIL)
;; Runtime errors are not FAIL.
(raises-on DIV0 #rx"quotient: undefined for 0")
(raises-on (term (BIN POW (INT 2) (INT -1))) #rx"negative exponent")

;;
;; Comparison operators
;;

(test-equal (eval-exp (term (CMP EQ (VAR "l") (LIST ((NAT 2) (NAT 3)))))) (term (OK (BOOL #t))))
(test-equal (eval-exp (term (CMP NE (VAR "x") (INT 1)))) (term (OK (BOOL #t))))
(test-equal (eval-exp (term (CMP LT (VAR "x") (NAT 2)))) (term (OK (BOOL #t))))
(test-equal (eval-exp (term (CMP GE (VAR "i") (INT -1)))) (term (OK (BOOL #f))))
(test-equal (trace-exp (term (CMP EQ (BOOL #t) (NAT 1))))
            '("eval-exp/literal/boolean" "eval-exp/literal/number" "eval-exp/compare/poly"))
;; A polymorphic comparison evaluates the right operand of any left one.
(raises-on (term (CMP EQ (BOOL #t) ,DIV0)) #rx"quotient: undefined for 0")
;; A nat and an int
(test-equal (eval-exp (term (CMP LT (VAR "x") (INT 2)))) 'FAIL)
(test-equal (trace-exp (term (CMP LT (VAR "x") (INT 2))))
            '("eval-exp/variable" "eval-exp/literal/number" "eval-exp/compare/number/fail"))
;; The left operand is not a number, and the right one is not evaluated.
(test-equal (eval-exp (term (CMP LT (BOOL #t) ,DIV0))) 'FAIL)
(test-equal (trace-exp (term (CMP LT (TEXT "a") ,DIV0)))
            '("eval-exp/literal/string" "eval-exp/compare/fail-left"))
;; The right operand is not a number.
(test-equal (trace-exp (term (CMP GT (NAT 1) (TEXT "a"))))
            '("eval-exp/literal/number" "eval-exp/literal/string" "eval-exp/compare/fail"))
(test-equal (eval-exp (term (CMP EQ (VAR "y") ,DIV0))) 'FAIL)

;;
;; Upcasting, downcasting, and subtyping
;;

(test-equal (eval-exp (term (UPCAST INT (VAR "x")))) (term (OK (INT 1))))
;; Types are found in L, and then in G.
(test-equal (eval-exp (term (UPCAST (VAR "T" ()) (NAT 3)))) (term (OK (INT 3))))
(test-equal (eval-exp (term (UPCAST (VAR "S" ()) (NAT 3)))) (term (OK (NAT 3))))
(test-equal (eval-exp (term (UPCAST INT (BOOL #t)))) 'FAIL)
(test-equal (trace-exp (term (UPCAST INT (BOOL #t))))
            '("eval-exp/literal/boolean" "eval-exp/upcast"))
(test-equal (eval-exp (term (UPCAST INT (VAR "y")))) 'FAIL)

(test-equal (eval-exp (term (DOWNCAST NAT (INT 2)))) (term (OK (NAT 2))))
(test-equal (eval-exp (term (DOWNCAST (VAR "U" ()) (INT 2)))) (term (OK (NAT 2))))
(test-equal (eval-exp (term (DOWNCAST NAT (INT -2)))) (term (OK (INT -2))))
(test-equal (eval-exp (term (DOWNCAST NAT (TEXT "a")))) 'FAIL)
(test-equal (trace-exp (term (DOWNCAST NAT (INT 2))))
            '("eval-exp/literal/number" "eval-exp/downcast"))
(test-equal (eval-exp (term (DOWNCAST NAT (VAR "y")))) 'FAIL)

(test-equal (eval-exp (term (SUB (VAR "x") NAT))) (term (OK (BOOL #t))))
(test-equal (eval-exp (term (SUB (VAR "i") NAT))) (term (OK (BOOL #f))))
(test-equal (eval-exp (term (SUB (INT -2) (VAR "T" ())))) (term (OK (BOOL #t))))
(test-equal (eval-exp (term (SUB (INT -2) (VAR "U" ())))) (term (OK (BOOL #f))))
;; L's definition of S shadows G's.
(test-equal (eval-exp (term (SUB (BOOL #t) (VAR "S" ())))) (term (OK (BOOL #t))))
(test-equal (eval-exp (term (SUB (INT 1) (VAR "S" ())))) (term (OK (BOOL #f))))
(test-equal (trace-exp (term (SUB (TEXT "a") TEXT)))
            '("eval-exp/literal/string" "eval-exp/subtype"))
(test-equal (eval-exp (term (SUB (VAR "y") NAT))) 'FAIL)

;;
;; Matches
;;

(define-term some-1 (INJ ((("Some") ()) ((NAT 1)))))

(test-equal (eval-exp (term (MATCH some-1 (INJ (("Some") ()))))) (term (OK (BOOL #t))))
(test-equal (eval-exp (term (MATCH some-1 (INJ (("None")))))) (term (OK (BOOL #f))))
(test-equal (trace-exp (term (MATCH some-1 (INJ (("Some") ())))))
            '("eval-exp/literal/number" "eval-exp/case" "eval-exp/match/inj"))
(test-equal (eval-exp (term (MATCH (VAR "l") CONS))) (term (OK (BOOL #t))))
(test-equal (eval-exp (term (MATCH (LIST ()) CONS))) (term (OK (BOOL #f))))
(test-equal (eval-exp (term (MATCH (VAR "l") (FIXED 2)))) (term (OK (BOOL #t))))
(test-equal (eval-exp (term (MATCH (VAR "l") (FIXED 3)))) (term (OK (BOOL #f))))
(test-equal (eval-exp (term (MATCH (LIST ()) NIL))) (term (OK (BOOL #t))))
(test-equal (eval-exp (term (MATCH (VAR "l") NIL))) (term (OK (BOOL #f))))
(test-equal (eval-exp (term (MATCH (OPT ((NAT 1))) SOME))) (term (OK (BOOL #t))))
(test-equal (eval-exp (term (MATCH (OPT ()) SOME))) (term (OK (BOOL #f))))
(test-equal (eval-exp (term (MATCH (OPT ()) NONE))) (term (OK (BOOL #t))))
(test-equal (eval-exp (term (MATCH (OPT ((NAT 1))) NONE))) (term (OK (BOOL #f))))
(test-equal (map (λ (e) (car (reverse (trace-exp e))))
                 (list (term (MATCH (VAR "l") CONS)) (term (MATCH (VAR "l") (FIXED 2)))
                       (term (MATCH (VAR "l") NIL)) (term (MATCH (OPT ()) SOME))
                       (term (MATCH (OPT ()) NONE))))
            '("eval-exp/match/list-cons" "eval-exp/match/list-fixed" "eval-exp/match/list-nil"
              "eval-exp/match/opt-some" "eval-exp/match/opt-none"))
;; otherwise: the value does not have the pattern's kind
(test-equal (eval-exp (term (MATCH (VAR "l") (INJ (("Some") ()))))) 'FAIL)
(test-equal (eval-exp (term (MATCH (OPT ()) CONS))) 'FAIL)
(test-equal (eval-exp (term (MATCH some-1 (FIXED 1)))) 'FAIL)
(test-equal (eval-exp (term (MATCH (VAR "l") SOME))) 'FAIL)
(test-equal (trace-exp (term (MATCH (NAT 1) NONE)))
            '("eval-exp/literal/number" "eval-exp/match/fail"))
(test-equal (eval-exp (term (MATCH (VAR "y") NIL))) 'FAIL)

;;
;; Tuples
;;

(test-equal (eval-exp (term (TUP ()))) (term (OK (TUP ()))))
(test-equal (eval-exp (term (TUP ((NAT 1) (VAR "x") (UN NOT (BOOL #f))))))
            (term (OK (TUP ((NAT 1) (NAT 1) (BOOL #t))))))
(test-equal (eval-exp (term (TUP ((TUP ()) (TUP ((TEXT "a") (VAR "b")))))))
            (term (OK (TUP ((TUP ()) (TUP ((TEXT "a") (BOOL #t))))))))
;; Left to right
(test-equal (trace-exp (term (TUP ((VAR "x") (NAT 1) (UN MINUS (NAT 2))))))
            '("eval-exp/variable" "eval-exp/literal/number"
              "eval-exp/literal/number" "eval-exp/unary/number" "eval-exp/tuple"))
;; The first failing element fails the tuple, and no later one is evaluated.
(test-equal (eval-exp (term (TUP ((NAT 1) (VAR "y") ,DIV0)))) 'FAIL)
(test-equal (trace-exp (term (TUP ((NAT 1) (VAR "y") ,DIV0))))
            '("eval-exp/literal/number" "eval-exp/variable/fail" "frame/fail"))
(test-equal (eval-exp (term (TUP ((TUP ((UN NOT (NAT 0)))) ,DIV0)))) 'FAIL)

;;
;; Cases, structs, options, and lists
;;

(test-equal (eval-exp (term (INJ ((("Some") ()) ((VAR "x")))))) (term (OK some-1)))
(test-equal (eval-exp (term (INJ ((("None")) ())))) (term (OK (INJ ((("None")) ())))))
(test-equal (trace-exp (term (INJ ((() ("->") ()) ((VAR "x") (VAR "b"))))))
            '("eval-exp/variable" "eval-exp/variable" "eval-exp/case"))
(test-equal (trace-exp (term (INJ ((() ("->") ()) ((VAR "y") ,DIV0)))))
            '("eval-exp/variable/fail" "frame/fail"))

(test-equal (eval-exp (term (STR (("A" (NAT 1)) ("B" (VAR "b"))))))
            (term (OK (STR (("A" (NAT 1)) ("B" (BOOL #t)))))))
(test-equal (eval-exp (term (STR ()))) (term (OK (STR ()))))
(test-equal (trace-exp (term (STR (("A" (VAR "x")) ("B" (NAT 2))))))
            '("eval-exp/variable" "eval-exp/literal/number" "eval-exp/struct"))
(test-equal (trace-exp (term (STR (("A" (VAR "y")) ("B" ,DIV0)))))
            '("eval-exp/variable/fail" "frame/fail"))

(test-equal (eval-exp (term (OPT ((VAR "x"))))) (term (OK (OPT ((NAT 1))))))
(test-equal (eval-exp (term (OPT ()))) (term (OK (OPT ()))))
(test-equal (trace-exp (term (OPT ()))) '("eval-exp/opt"))
(test-equal (trace-exp (term (OPT ((VAR "y"))))) '("eval-exp/variable/fail" "frame/fail"))

(test-equal (eval-exp (term (LIST ()))) (term (OK (LIST ()))))
(test-equal (eval-exp (term (LIST ((VAR "x") (NAT 2))))) (term (OK (LIST ((NAT 1) (NAT 2))))))
(test-equal (trace-exp (term (LIST ((VAR "x") (NAT 2)))))
            '("eval-exp/variable" "eval-exp/literal/number" "eval-exp/list"))
(test-equal (trace-exp (term (LIST ((VAR "y") ,DIV0)))) '("eval-exp/variable/fail" "frame/fail"))

;;
;; Cons-lists, concatenation, membership, and length
;;

(test-equal (eval-exp (term (CONS (NAT 1) (VAR "l")))) (term (OK (LIST ((NAT 1) (NAT 2) (NAT 3))))))
(test-equal (eval-exp (term (CONS (NAT 1) (LIST ())))) (term (OK (LIST ((NAT 1))))))
(test-equal (trace-exp (term (CONS (NAT 1) (LIST ()))))
            '("eval-exp/literal/number" "eval-exp/list" "eval-exp/cons"))
;; The head is evaluated first, and the tail after it, whatever it is.
(test-equal (trace-exp (term (CONS (VAR "y") ,DIV0))) '("eval-exp/variable/fail" "frame/fail"))
(raises-on (term (CONS (TUP ()) ,DIV0)) #rx"quotient: undefined for 0")
;; The tail is not a list.
(test-equal (eval-exp (term (CONS (NAT 1) (OPT ())))) 'FAIL)
(test-equal (trace-exp (term (CONS (NAT 1) (NAT 2))))
            '("eval-exp/literal/number" "eval-exp/literal/number" "eval-exp/cons/fail"))

(test-equal (eval-exp (term (CAT (TEXT "ab") (TEXT "cé")))) (term (OK (TEXT "abcé"))))
(test-equal (eval-exp (term (CAT (VAR "l") (LIST ((NAT 4)))))) (term (OK (LIST ((NAT 2) (NAT 3) (NAT 4))))))
(test-equal (eval-exp (term (CAT (LIST ()) (LIST ())))) (term (OK (LIST ()))))
(test-equal (trace-exp (term (CAT (TEXT "a") (TEXT "b"))))
            '("eval-exp/literal/string" "eval-exp/literal/string" "eval-exp/concat/text"))
(test-equal (trace-exp (term (CAT (LIST ()) (LIST ()))))
            '("eval-exp/list" "eval-exp/list" "eval-exp/concat/list"))
;; The left operand is neither a text nor a list, and the right one is not
;; evaluated.
(test-equal (eval-exp (term (CAT (NAT 1) ,DIV0))) 'FAIL)
(test-equal (trace-exp (term (CAT (OPT ()) ,DIV0))) '("eval-exp/opt" "eval-exp/concat/fail-left"))
;; Operands of different kinds
(test-equal (eval-exp (term (CAT (TEXT "a") (LIST ())))) 'FAIL)
(test-equal (trace-exp (term (CAT (LIST ()) (TEXT "a"))))
            '("eval-exp/list" "eval-exp/literal/string" "eval-exp/concat/fail"))
(test-equal (eval-exp (term (CAT (TEXT "a") (VAR "y")))) 'FAIL)

(test-equal (eval-exp (term (MEM (NAT 3) (VAR "l")))) (term (OK (BOOL #t))))
(test-equal (eval-exp (term (MEM (NAT 4) (VAR "l")))) (term (OK (BOOL #f))))
(test-equal (eval-exp (term (MEM (NAT 4) (LIST ())))) (term (OK (BOOL #f))))
;; Equality is structural.
(test-equal (eval-exp (term (MEM (INT 2) (VAR "l")))) (term (OK (BOOL #f))))
(test-equal (trace-exp (term (MEM (NAT 4) (LIST ()))))
            '("eval-exp/literal/number" "eval-exp/list" "eval-exp/mem"))
;; The element is evaluated first.
(test-equal (trace-exp (term (MEM (VAR "y") ,DIV0))) '("eval-exp/variable/fail" "frame/fail"))
;; Not a list
(test-equal (eval-exp (term (MEM (NAT 1) (OPT ((NAT 1)))))) 'FAIL)
(test-equal (trace-exp (term (MEM (NAT 1) (TEXT "1"))))
            '("eval-exp/literal/number" "eval-exp/literal/string" "eval-exp/mem/fail"))

;; Texts are measured in UTF-8 bytes.
(test-equal (eval-exp (term (LEN (TEXT "abc")))) (term (OK (NAT 3))))
(test-equal (eval-exp (term (LEN (TEXT "é")))) (term (OK (NAT 2))))
(test-equal (eval-exp (term (LEN (VAR "l")))) (term (OK (NAT 2))))
(test-equal (eval-exp (term (LEN (LIST ())))) (term (OK (NAT 0))))
(test-equal (map (λ (e) (car (reverse (trace-exp e))))
                 (list (term (LEN (TEXT ""))) (term (LEN (LIST ()))) (term (LEN (TUP ())))))
            '("eval-exp/len/text" "eval-exp/len/list" "eval-exp/len/fail"))
(test-equal (eval-exp (term (LEN (NAT 1)))) 'FAIL)
(test-equal (eval-exp (term (LEN (VAR "y")))) 'FAIL)

;;
;; Dot, indexing, and slicing
;;

(test-equal (eval-exp (term (DOT (VAR "s") "B"))) (term (OK (TEXT "ab"))))
(test-equal (trace-exp (term (DOT (VAR "s") "A")))
            '("eval-exp/variable" "eval-exp/dot" "eval-path/dot" "eval-path/root"
              "eval-path/dot/field"))
;; The first field with the atom
(test-equal (eval-exp (term (DOT (STR (("A" (NAT 1)) ("A" (NAT 2)))) "A"))) (term (OK (NAT 1))))
;; No such field, or not a struct
(test-equal (eval-exp (term (DOT (VAR "s") "C"))) 'FAIL)
(test-equal (car (reverse (trace-exp (term (DOT (VAR "s") "C"))))) "eval-path/dot/field/fail")
(test-equal (eval-exp (term (DOT (VAR "l") "A"))) 'FAIL)
(test-equal (car (reverse (trace-exp (term (DOT (VAR "l") "A"))))) "eval-path/dot/fail")
(test-equal (trace-exp (term (DOT (VAR "y") "A"))) '("eval-exp/variable/fail" "frame/fail"))

(test-equal (eval-exp (term (IDX (VAR "l") (NAT 1)))) (term (OK (NAT 3))))
(test-equal (eval-exp (term (IDX (TEXT "abc") (VAR "x")))) (term (OK (TEXT "b"))))
(test-equal (trace-exp (term (IDX (VAR "l") (NAT 0))))
            '("eval-exp/variable" "eval-exp/idx" "eval-path/idx" "eval-path/root"
              "eval-exp/literal/number" "eval-path/idx/list"))
(test-equal (car (reverse (trace-exp (term (IDX (TEXT "a") (NAT 0)))))) "eval-path/idx/text")
;; Texts are indexed by UTF-8 bytes, and a byte that is not a character raises.
(test-equal (eval-exp (term (IDX (TEXT "éa") (NAT 2)))) (term (OK (TEXT "a"))))
(raises-on (term (IDX (TEXT "é") (NAT 0))) #rx"text-idx: the result splits a UTF-8 character")
;; An index out of bounds fails, as the elaborated rules check it.
(test-equal (eval-exp (term (IDX (VAR "l") (NAT 2)))) 'FAIL)
(test-equal (eval-exp (term (IDX (TEXT "é") (NAT 2)))) 'FAIL)
(test-equal (car (reverse (trace-exp (term (IDX (TEXT "") (NAT 0)))))) "eval-path/idx/fail")
;; The index is not a nat.
(test-equal (eval-exp (term (IDX (VAR "l") (INT 0)))) 'FAIL)
(test-equal (eval-exp (term (IDX (VAR "l") (VAR "y")))) 'FAIL)
;; Neither a text nor a list, and the index is not evaluated
(test-equal (eval-exp (term (IDX (NAT 1) ,DIV0))) 'FAIL)
(test-equal (trace-exp (term (IDX (OPT ()) ,DIV0)))
            '("eval-exp/opt" "eval-exp/idx" "eval-path/idx" "eval-path/root"
              "eval-path/idx/fail-base"))

;; A slice is a start and a length.
(test-equal (eval-exp (term (SLICE (TEXT "abcd") (NAT 1) (NAT 2)))) (term (OK (TEXT "bc"))))
(test-equal (eval-exp (term (SLICE (TEXT "aéb") (NAT 1) (NAT 2)))) (term (OK (TEXT "é"))))
(test-equal (eval-exp (term (SLICE (VAR "l") (NAT 0) (NAT 2)))) (term (OK (LIST ((NAT 2) (NAT 3))))))
(test-equal (eval-exp (term (SLICE (VAR "l") (NAT 2) (NAT 0)))) (term (OK (LIST ()))))
(test-equal (trace-exp (term (SLICE (VAR "l") (NAT 1) (VAR "x"))))
            '("eval-exp/variable" "eval-exp/slice" "eval-path/slice" "eval-path/root"
              "eval-exp/literal/number" "eval-exp/variable" "eval-path/slice/list"))
(test-equal (car (reverse (trace-exp (term (SLICE (TEXT "a") (NAT 0) (NAT 1))))))
            "eval-path/slice/text")
;; Nothing bounds slices, so a slice out of bounds raises, as does one that
;; splits a character.
(raises-on (term (SLICE (TEXT "ab") (NAT 1) (NAT 2))) #rx"slice \\[1, 3\\) out of bounds \\[0, 2\\)")
(raises-on (term (SLICE (VAR "l") (NAT 3) (NAT 0))) #rx"slice \\[3, 3\\) out of bounds \\[0, 2\\)")
(raises-on (term (SLICE (TEXT "é") (NAT 0) (NAT 1))) #rx"splits a UTF-8 character")
;; Neither a text nor a list, and no index is evaluated
(test-equal (car (reverse (trace-exp (term (SLICE (NAT 1) ,DIV0 ,DIV0)))))
            "eval-path/slice/fail-base")
;; The start is not a nat, and the length is not evaluated.
(test-equal (car (reverse (trace-exp (term (SLICE (TEXT "ab") (BOOL #t) ,DIV0)))))
            "eval-path/slice/fail-index")
(test-equal (eval-exp (term (SLICE (TEXT "ab") (VAR "y") ,DIV0))) 'FAIL)
;; The length is not a nat.
(test-equal (car (reverse (trace-exp (term (SLICE (VAR "l") (NAT 0) (INT 1))))))
            "eval-path/slice/fail")
(test-equal (eval-exp (term (SLICE (VAR "l") (NAT 0) (VAR "y")))) 'FAIL)

;; Eval_path recurses on the path. Eval_exp only builds paths on ROOT, and
;; Eval_path_upd builds the others.
(define (eval-path val path)
  (eval-exp `(eval-path ,val ,path)))

(define-term s-nested (STR (("A" (LIST ((TEXT "xy") (TEXT "zw")))))))

(test-equal (eval-path (term s-nested) (term (IDX (DOT ROOT "A") (NAT 1)))) (term (OK (TEXT "zw"))))
(test-equal (eval-path (term s-nested) (term (SLICE (IDX (DOT ROOT "A") (NAT 1)) (NAT 1) (NAT 1))))
            (term (OK (TEXT "w"))))
(test-equal (eval-path (term s-nested) (term (DOT (IDX (DOT ROOT "A") (NAT 1)) "B"))) 'FAIL)
(test-equal (eval-path (term s-nested) (term (IDX (DOT ROOT "B") ,DIV0))) 'FAIL)

;;
;; Updates
;;

(test-equal (eval-exp (term (UPD (VAR "x") ROOT (NAT 5)))) (term (OK (NAT 5))))
(test-equal (trace-exp (term (UPD (VAR "x") ROOT (NAT 5))))
            '("eval-exp/variable" "eval-exp/literal/number" "eval-exp/upd" "eval-path-upd/root"))
;; The base is evaluated first, and the new value after it, whatever it is.
(test-equal (trace-exp (term (UPD (VAR "y") ROOT ,DIV0))) '("eval-exp/variable/fail" "frame/fail"))
(test-equal (eval-exp (term (UPD (NAT 1) ROOT (VAR "y")))) 'FAIL)

;;; Eval_path_upd/idx

(test-equal (eval-exp (term (UPD (VAR "l") (IDX ROOT (NAT 1)) (TEXT "a"))))
            (term (OK (LIST ((NAT 2) (TEXT "a"))))))
(test-equal (trace-exp (term (UPD (VAR "l") (IDX ROOT (NAT 1)) (NAT 4))))
            '("eval-exp/variable" "eval-exp/literal/number" "eval-exp/upd"
              "eval-path-upd/idx" "eval-path/root" "eval-exp/literal/number"
              "eval-path-upd/idx/list" "eval-path-upd/root"))
(test-equal (eval-exp (term (UPD (TEXT "abc") (IDX ROOT (NAT 1)) (TEXT "z"))))
            (term (OK (TEXT "azc"))))
(test-equal (car (reverse (trace-exp (term (UPD (TEXT "abc") (IDX ROOT (NAT 1)) (TEXT "z"))))))
            "eval-path-upd/root")
(test-equal (car (cdr (reverse (trace-exp (term (UPD (TEXT "a") (IDX ROOT (NAT 0)) (TEXT "z")))))))
            "eval-path-upd/idx/text")
;; Texts are updated by UTF-8 bytes.
(test-equal (eval-exp (term (UPD (TEXT "aé") (IDX ROOT (NAT 0)) (TEXT "x")))) (term (OK (TEXT "xé"))))
(raises-on (term (UPD (TEXT "é") (IDX ROOT (NAT 0)) (TEXT "x"))) #rx"splits a UTF-8 character")
;; Nothing bounds updates, so an index out of bounds raises.
(raises-on (term (UPD (VAR "l") (IDX ROOT (NAT 2)) (NAT 0))) #rx"index 2 out of bounds \\[0, 2\\)")
(raises-on (term (UPD (TEXT "a") (IDX ROOT (NAT 1)) (TEXT "b"))) #rx"index 1 out of bounds \\[0, 1\\)")
;; Neither a text updated by a text nor a list, and the index is not evaluated
(test-equal (eval-exp (term (UPD (TEXT "ab") (IDX ROOT ,DIV0) (NAT 2)))) 'FAIL)
(test-equal (car (reverse (trace-exp (term (UPD (NAT 1) (IDX ROOT ,DIV0) (NAT 2))))))
            "eval-path-upd/idx/fail-base")
;; The index is not a nat, or the text is not of length 1.
(test-equal (eval-exp (term (UPD (VAR "l") (IDX ROOT (INT 0)) (NAT 2)))) 'FAIL)
(test-equal (eval-exp (term (UPD (TEXT "ab") (IDX ROOT (NAT 0)) (TEXT "xy")))) 'FAIL)
(test-equal (car (reverse (trace-exp (term (UPD (TEXT "ab") (IDX ROOT (NAT 0)) (TEXT "é"))))))
            "eval-path-upd/idx/fail")
(test-equal (eval-exp (term (UPD (VAR "l") (IDX ROOT (VAR "y")) (NAT 2)))) 'FAIL)

;;; Eval_path_upd/slice

(test-equal (eval-exp (term (UPD (TEXT "abcd") (SLICE ROOT (NAT 1) (NAT 2)) (TEXT "xy"))))
            (term (OK (TEXT "axyd"))))
(test-equal (eval-exp (term (UPD (TEXT "ab") (SLICE ROOT (NAT 1) (NAT 0)) (TEXT ""))))
            (term (OK (TEXT "ab"))))
(test-equal (car (cdr (reverse (trace-exp (term (UPD (TEXT "a") (SLICE ROOT (NAT 0) (NAT 1))
                                                     (TEXT "b")))))))
            "eval-path-upd/slice/text")
;; The elements of the new list replace the slice.
(test-equal (eval-exp (term (UPD (LIST ((NAT 1) (NAT 2) (NAT 3))) (SLICE ROOT (NAT 1) (NAT 2))
                                 (LIST ((NAT 8) (NAT 9))))))
            (term (OK (LIST ((NAT 1) (NAT 8) (NAT 9))))))
(test-equal (trace-exp (term (UPD (VAR "l") (SLICE ROOT (NAT 0) (NAT 0)) (LIST ()))))
            '("eval-exp/variable" "eval-exp/list" "eval-exp/upd"
              "eval-path-upd/slice" "eval-path/root" "eval-exp/literal/number"
              "eval-exp/literal/number" "eval-path-upd/slice/list" "eval-path-upd/root"))
;; A new list of another length, or a slice out of bounds, raises.
(raises-on (term (UPD (VAR "l") (SLICE ROOT (NAT 0) (NAT 2)) (LIST ((NAT 1)))))
           #rx"list-upd-slice: the replacement has length 1 instead of 2")
(raises-on (term (UPD (VAR "l") (SLICE ROOT (NAT 1) (NAT 2)) (LIST ((NAT 1) (NAT 2)))))
           #rx"slice \\[1, 3\\) out of bounds \\[0, 2\\)")
(raises-on (term (UPD (TEXT "ab") (SLICE ROOT (NAT 2) (NAT 1)) (TEXT "c")))
           #rx"slice \\[2, 3\\) out of bounds \\[0, 2\\)")
(raises-on (term (UPD (TEXT "é") (SLICE ROOT (NAT 0) (NAT 1)) (TEXT "x")))
           #rx"splits a UTF-8 character")
;; The new value is neither a text nor a list, and nothing is evaluated.
(test-equal (trace-exp (term (UPD (VAR "l") (SLICE (IDX ROOT ,DIV0) ,DIV0 ,DIV0) (NAT 5))))
            '("eval-exp/variable" "eval-exp/literal/number" "eval-exp/upd"
              "eval-path-upd/slice/fail-value"))
;; The base is not of the new value's kind, and no index is evaluated.
(test-equal (car (reverse (trace-exp (term (UPD (TEXT "ab") (SLICE ROOT ,DIV0 ,DIV0) (LIST ()))))))
            "eval-path-upd/slice/fail-base")
(test-equal (eval-exp (term (UPD (VAR "l") (SLICE ROOT ,DIV0 ,DIV0) (TEXT "")))) 'FAIL)
;; The start is not a nat, and the length is not evaluated.
(test-equal (car (reverse (trace-exp (term (UPD (TEXT "ab") (SLICE ROOT (BOOL #t) ,DIV0) (TEXT ""))))))
            "eval-path-upd/slice/fail-index")
;; The length is not a nat, or not the new text's length.
(test-equal (eval-exp (term (UPD (VAR "l") (SLICE ROOT (NAT 0) (INT 0)) (LIST ())))) 'FAIL)
(test-equal (car (reverse (trace-exp (term (UPD (TEXT "ab") (SLICE ROOT (NAT 0) (NAT 1))
                                                (TEXT "xy"))))))
            "eval-path-upd/slice/fail")

;;; Eval_path_upd/dot

(test-equal (eval-exp (term (UPD (VAR "s") (DOT ROOT "A") (NAT 9))))
            (term (OK (STR (("A" (NAT 9)) ("B" (TEXT "ab")))))))
(test-equal (trace-exp (term (UPD (VAR "s") (DOT ROOT "A") (NAT 9))))
            '("eval-exp/variable" "eval-exp/literal/number" "eval-exp/upd"
              "eval-path-upd/dot" "eval-path/root" "eval-path-upd/dot/field" "eval-path-upd/root"))
;; Every field with the atom is updated, and none if there is none.
(test-equal (eval-exp (term (UPD (STR (("A" (NAT 1)) ("A" (NAT 2)))) (DOT ROOT "A") (NAT 9))))
            (term (OK (STR (("A" (NAT 9)) ("A" (NAT 9)))))))
(test-equal (eval-exp (term (UPD (VAR "s") (DOT ROOT "C") (NAT 9))))
            (term (OK (STR (("A" (NAT 1)) ("B" (TEXT "ab")))))))
;; Not a struct
(test-equal (car (reverse (trace-exp (term (UPD (VAR "l") (DOT ROOT "A") (NAT 9))))))
            "eval-path-upd/dot/fail")

;;; Nested paths: the inner path is read with Eval_path, then written back.

(test-equal (eval-exp (term (UPD (VAR "s") (IDX (DOT ROOT "B") (NAT 0)) (TEXT "z"))))
            (term (OK (STR (("A" (NAT 1)) ("B" (TEXT "zb")))))))
(test-equal (eval-exp (term (UPD s-nested (SLICE (IDX (DOT ROOT "A") (NAT 1)) (NAT 0) (NAT 2))
                                 (TEXT "ZW"))))
            (term (OK (STR (("A" (LIST ((TEXT "xy") (TEXT "ZW")))))))))
(test-equal (eval-exp (term (UPD (LIST ((VAR "s"))) (DOT (IDX ROOT (NAT 0)) "A") (NAT 0))))
            (term (OK (LIST ((STR (("A" (NAT 0)) ("B" (TEXT "ab")))))))))
(test-equal (trace-exp (term (UPD (LIST ((VAR "s"))) (DOT (IDX ROOT (NAT 0)) "A") (NAT 0))))
            '("eval-exp/variable" "eval-exp/list" "eval-exp/literal/number" "eval-exp/upd"
              "eval-path-upd/dot" "eval-path/idx" "eval-path/root" "eval-exp/literal/number"
              "eval-path/idx/list" "eval-path-upd/dot/field"
              "eval-path-upd/idx" "eval-path/root" "eval-exp/literal/number"
              "eval-path-upd/idx/list" "eval-path-upd/root"))
;; The inner path fails.
(test-equal (eval-exp (term (UPD (VAR "l") (DOT (IDX ROOT (NAT 5)) "A") (NAT 0)))) 'FAIL)
(test-equal (eval-exp (term (UPD (VAR "s") (IDX (DOT ROOT "C") (NAT 0)) (NAT 0)))) 'FAIL)
;;
;; Calls
;;

;; The type arguments, then the arguments left to right, and then the call
(test-equal (eval-exp (term (CALL "inc" () ((EXP (VAR "x")))))) (term (OK (NAT 2))))
(test-equal (eval-exp (term (CALL "add" () ((EXP (VAR "x")) (EXP (NAT 2)))))) (term (OK (NAT 3))))
(test-equal (take (trace-exp (term (CALL "add" () ((EXP (VAR "x")) (EXP (NAT 2)))))) 7)
            '("eval-exp/call" "eval-targs" "eval-exp/variable" "eval-arg/exp"
              "eval-exp/literal/number" "eval-arg/exp" "eval-exp/call/func"))
;; The type arguments fail, and no argument is evaluated.
(test-equal (trace-exp (term (CALL "inc" ((VAR "S" (BOOL))) ((EXP ,DIV0)))))
            '("eval-exp/call" "eval-targs/fail-subst" "frame/fail"))
;; An argument fails, and no later one is evaluated.
(test-equal (trace-exp (term (CALL "add" () ((EXP (VAR "y")) (EXP ,DIV0)))))
            '("eval-exp/call" "eval-targs" "eval-exp/variable/fail" "frame/fail" "frame/fail"))
;; No such function: a variable bound to a function value is not one.
(test-equal (trace-exp (term (CALL "f" () ((FUN "inc")))))
            '("eval-exp/call" "eval-targs" "eval-arg/fun" "eval-exp/call/func" "call-func/fail"))

;;
;; Iterated expressions
;;

;;; Eval_exp/iter/simple: an iteration on a variable looks it up.

(test-equal (eval-exp (term (ITER (VAR "xs") (STAR (("xs" NAT ()))))))
            (term (OK (LIST ((NAT 2))))))
(test-equal (eval-exp (term (ITER (VAR "ws") (STAR (("ws" NAT ())))))) (term (OK (NAT 0))))
(test-equal (trace-exp (term (ITER (VAR "o") (QUEST (("o" NAT ()))))))
            '("eval-exp/iter/simple"))
(test-equal (eval-exp (term (ITER (VAR "zs") (STAR (("zs" NAT ())))))) 'FAIL)
(test-equal (trace-exp (term (ITER (VAR "zs") (STAR (("zs" NAT ()))))))
            '("eval-exp/iter/simple/fail"))

;;; Eval_exp/iter/opt

(test-equal (eval-exp (term (ITER (BIN ADD (VAR "o") (VAR "p")) (QUEST (("o" NAT ()) ("p" NAT ()))))))
            (term (OK (OPT ((NAT 15))))))
(test-equal (trace-exp (term (ITER (UN MINUS (VAR "o")) (QUEST (("o" NAT ()))))))
            '("eval-exp/iter/opt" "eval-exp/variable" "eval-exp/unary/number" "in/ok"
              "eval-exp/iter/opt/collect"))
;; No sub-context, and nothing is evaluated
(test-equal (trace-exp (term (ITER (TUP ((VAR "n") (VAR "m") ,DIV0)) (QUEST (("n" NAT ()) ("m" NAT ()))))))
            '("eval-exp/iter/opt" "eval-exp/iter/opt/collect"))
(test-equal (eval-exp (term (ITER (UN MINUS (VAR "n")) (QUEST (("n" NAT ()))))))
            (term (OK (OPT ()))))
;; No iterated variable: one sub-context
(test-equal (eval-exp (term (ITER (VAR "x") (QUEST ())))) (term (OK (OPT ((NAT 1))))))
;; A mix of OPT val and OPT eps, an unbound variable, or a value that is not an
;; option: $sub_opt has no result.
(test-equal (eval-exp (term (ITER (TUP ((VAR "o") (VAR "n"))) (QUEST (("o" NAT ()) ("n" NAT ()))))))
            'FAIL)
(test-equal (trace-exp (term (ITER (UN MINUS (VAR "q")) (QUEST (("q" NAT ()))))))
            '("eval-exp/iter/opt/fail"))
(test-equal (eval-exp (term (ITER (UN MINUS (VAR "xs")) (QUEST (("xs" NAT ())))))) 'FAIL)
;; The element fails.
(test-equal (trace-exp (term (ITER (UN NOT (VAR "o")) (QUEST (("o" NAT ()))))))
            '("eval-exp/iter/opt" "eval-exp/variable" "eval-exp/unary/fail" "in/fail" "frame/fail"))

;;; Eval_exp/iter/list

(test-equal (eval-exp (term (ITER (BIN MUL (VAR "ns") (NAT 2)) (STAR (("ns" NAT ()))))))
            (term (OK (LIST ((NAT 2) (NAT 4) (NAT 6))))))
(test-equal (eval-exp (term (ITER (TUP ((VAR "ns") (VAR "ms"))) (STAR (("ns" NAT ()) ("ms" NAT ()))))))
            (term (OK (LIST ((TUP ((NAT 1) (NAT 10))) (TUP ((NAT 2) (NAT 20)))
                             (TUP ((NAT 3) (NAT 30))))))))
;; The sub-contexts see the enclosing layer's values, and leave it unchanged.
(test-equal (run-in (term G-vals) (term L-vals)
                    (term (ITER (BIN ADD (VAR "ns") (VAR "x")) (STAR (("ns" NAT ()))))))
            (term ((OK (LIST ((NAT 2) (NAT 3) (NAT 4)))) L-vals)))
(test-equal (trace-exp (term (ITER (VAR "x") (STAR (("ks" NAT ()))))))
            '("eval-exp/iter/list" "eval-exp/variable" "in/ok" "eval-exp/iter/list/collect"))
;; No iterated variable: no sub-context
(test-equal (trace-exp (term (ITER ,DIV0 (STAR ()))))
            '("eval-exp/iter/list" "eval-exp/iter/list/collect"))
(test-equal (eval-exp (term (ITER (TUP ()) (STAR ())))) (term (OK (LIST ()))))
;; An unbound variable, or a value that is not a list: $sub_list has no result.
(test-equal (trace-exp (term (ITER (UN MINUS (VAR "q")) (STAR (("q" NAT ()))))))
            '("eval-exp/iter/list/fail"))
(test-equal (eval-exp (term (ITER (UN MINUS (VAR "ws")) (STAR (("ws" NAT ())))))) 'FAIL)
;; Lists of different lengths raise, as $transpose_ does.
(raises-on (term (ITER (TUP ((VAR "ns") (VAR "ks"))) (STAR (("ns" NAT ()) ("ks" NAT ())))))
           #rx"cannot transpose")
;; The first failing element fails the iteration, and no later one is
;; evaluated.
(test-equal (trace-exp (term (ITER (BIN DIV (NAT 6) (BIN SUB (VAR "ns") (NAT 1)))
                                   (STAR (("ns" NAT ()))))))
            '("eval-exp/iter/list" "eval-exp/literal/number" "eval-exp/variable"
              "eval-exp/literal/number" "eval-exp/binary/number" "eval-exp/binary/number/fail"
              "in/fail" "frame/fail"))

(check-coverage coverage)
