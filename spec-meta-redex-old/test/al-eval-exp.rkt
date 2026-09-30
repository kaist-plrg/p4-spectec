#lang racket/base

(require rackunit
         "../common/0.0-prelude.rkt"
         "../al/5-eval.rkt"
         "judgment.rkt")

;; Globally: aliases, and the functions `id` and `g`, where $g<Y>(x) = x <: Y.
;; Locally: the type X, a shadowing alias, and values.
(define C-exp
  (ctx-of '((("nat" (DEF () (ALIAS NAT))) ("shadow" (DEF () (ALIAS NAT))))
            ()
            (("id" (DEF () ((((EXP (VAR "x"))) (VAR "x") ())) ()))
             ("g" (DEF ("Y") ((((EXP (VAR "x"))) (SUB (VAR "x") (VAR "Y" ())) ())) ())))
            ())
          '((("X" (DEF () (ALIAS NAT))) ("shadow" (DEF () (ALIAS BOOL))))
            ()
            ()
            ((("x" ()) (NAT 1))
             (("t" ()) (TEXT "hello"))
             (("s" ()) (STR (("a" (NAT 1)) ("b" (BOOL #f)))))
             (("l" ()) (LIST ((NAT 10) (NAT 20) (NAT 30))))
             (("ls" ()) (LIST ((STR (("a" (NAT 1)))) (STR (("a" (NAT 2)))))))
             (("a" (STAR)) (LIST ((NAT 1) (NAT 2))))
             (("u" (STAR)) (LIST ((NAT 0))))
             (("o" (QUEST)) (OPT ((NAT 5))))
             (("n" (QUEST)) (OPT ()))
             (("w" (STAR)) (NAT 0))))))

;; The results of the derivations of exp under C
(define (ev exp [C C-exp])
  (outputs (eval-exp ,C ,exp any)))

;; An expression that raises an error if it is evaluated, and one that fails
(define boom '(BIN DIV (NAT 1) (NAT 0)))
(define none '(VAR "none"))

;;
;; eval-exp
;;

;;; Literals

(test-equal (ev '(BOOL #t)) '((OK (BOOL #t))))
(test-equal (ev '(NAT 3)) '((OK (NAT 3))))
(test-equal (ev '(INT -3)) '((OK (INT -3))))
(test-equal (ev '(TEXT "x")) '((OK (TEXT "x"))))

;;; Variables

(test-equal (ev '(VAR "x")) '((OK (NAT 1))))
;; fail: unbound, or bound only with iterators
(test-equal (ev none) '(FAIL))
(test-equal (ev '(VAR "a")) '(FAIL))

;;; Unary

(test-equal (ev '(UN NOT (BOOL #t))) '((OK (BOOL #f))))
(test-equal (ev '(UN MINUS (NAT 1))) '((OK (INT -1))))
(test-equal (ev '(UN PLUS (INT -2))) '((OK (INT -2))))
;; fail: an operand of the wrong kind, or a failing one
(test-equal (ev '(UN NOT (NAT 1))) '(FAIL))
(test-equal (ev '(UN MINUS (BOOL #t))) '(FAIL))
(test-equal (ev `(UN NOT ,none)) '(FAIL))

;;; Binary

(test-equal (ev '(BIN AND (BOOL #t) (BOOL #f))) '((OK (BOOL #f))))
(test-equal (ev '(BIN OR (BOOL #f) (BOOL #t))) '((OK (BOOL #t))))
(test-equal (ev '(BIN IMPL (BOOL #f) (BOOL #f))) '((OK (BOOL #t))))
(test-equal (ev '(BIN EQUIV (BOOL #f) (BOOL #f))) '((OK (BOOL #t))))
(test-equal (ev '(BIN ADD (NAT 1) (VAR "x"))) '((OK (NAT 2))))
(test-equal (ev '(BIN SUB (NAT 1) (NAT 3))) '((OK (INT -2))))
(test-equal (ev '(BIN DIV (INT -7) (INT 2))) '((OK (INT -3))))
(test-equal (ev '(BIN MOD (INT -7) (INT 2))) '((OK (INT -1))))
(check-exn exn:fail:contract:divide-by-zero? (λ () (ev boom)))
;; number-fail: $binop_number has no clause for NAT and INT
(test-equal (ev '(BIN ADD (NAT 1) (INT 1))) '(FAIL))
;; fail: the right operand fails or has the wrong kind
(test-equal (ev `(BIN AND (BOOL #t) ,none)) '(FAIL))
(test-equal (ev '(BIN AND (BOOL #t) (NAT 1))) '(FAIL))
(test-equal (ev '(BIN ADD (NAT 1) (BOOL #t))) '(FAIL))
;; fail: the left operand fails or has the wrong kind, and the right one is
;; not evaluated
(test-equal (ev `(BIN AND ,none ,boom)) '(FAIL))
(test-equal (ev `(BIN AND (NAT 1) ,boom)) '(FAIL))
(test-equal (ev `(BIN ADD (BOOL #t) ,boom)) '(FAIL))

;;; Compare

(test-equal (ev '(CMP EQ (VAR "s") (STR (("a" (NAT 1)) ("b" (BOOL #f)))))) '((OK (BOOL #t))))
(test-equal (ev '(CMP NE (NAT 1) (INT 1))) '((OK (BOOL #t))))
(test-equal (ev '(CMP LT (NAT 1) (NAT 2))) '((OK (BOOL #t))))
(test-equal (ev '(CMP GE (INT -1) (INT 2))) '((OK (BOOL #f))))
;; number-fail: $cmpop_number has no clause for NAT and INT
(test-equal (ev '(CMP LT (NAT 1) (INT 2))) '(FAIL))
;; fail: the right operand fails or has the wrong kind
(test-equal (ev `(CMP EQ (NAT 1) ,none)) '(FAIL))
(test-equal (ev '(CMP LT (NAT 1) (TEXT "x"))) '(FAIL))
;; fail: the left operand fails or has the wrong kind, and the right one is
;; not evaluated
(test-equal (ev `(CMP EQ ,none ,boom)) '(FAIL))
(test-equal (ev `(CMP LT (BOOL #t) ,boom)) '(FAIL))

;;; Casts and subtyping

(test-equal (ev '(UPCAST INT (NAT 3))) '((OK (INT 3))))
(test-equal (ev '(UPCAST (VAR "nat" ()) (NAT 3))) '((OK (NAT 3))))
;; $upcast's own FAIL, and a failing operand
(test-equal (ev '(UPCAST INT (BOOL #t))) '(FAIL))
(test-equal (ev `(UPCAST INT ,none)) '(FAIL))

(test-equal (ev '(DOWNCAST NAT (INT 3))) '((OK (NAT 3))))
(test-equal (ev '(DOWNCAST NAT (BOOL #t))) '(FAIL))
(test-equal (ev `(DOWNCAST NAT ,none)) '(FAIL))

(test-equal (ev '(SUB (INT 3) NAT)) '((OK (BOOL #t))))
(test-equal (ev '(SUB (INT -3) NAT)) '((OK (BOOL #f))))
(test-equal (ev '(SUB (INT 3) (VAR "nat" ()))) '((OK (BOOL #t))))
;; The local type definitions extend the global ones.
(test-equal (ev '(SUB (BOOL #t) (VAR "shadow" ()))) '((OK (BOOL #t))))
(test-equal (ev '(SUB (NAT 1) (VAR "shadow" ()))) '((OK (BOOL #f))))
(test-equal (ev '(SUB (NAT 1) (VAR "X" ()))) '((OK (BOOL #t))))
(test-equal (ev `(SUB ,none NAT)) '(FAIL))

;;; Match

(define some-1 '(INJ ((("Some") ()) ((NAT 1)))))

(test-equal (ev `(MATCH ,some-1 (INJ (("Some") ())))) '((OK (BOOL #t))))
(test-equal (ev `(MATCH ,some-1 (INJ (("None"))))) '((OK (BOOL #f))))
(test-equal (ev '(MATCH (VAR "l") CONS)) '((OK (BOOL #t))))
(test-equal (ev '(MATCH (LIST ()) CONS)) '((OK (BOOL #f))))
(test-equal (ev '(MATCH (VAR "l") (FIXED 3))) '((OK (BOOL #t))))
(test-equal (ev '(MATCH (VAR "l") (FIXED 2))) '((OK (BOOL #f))))
(test-equal (ev '(MATCH (LIST ()) NIL)) '((OK (BOOL #t))))
(test-equal (ev '(MATCH (VAR "l") NIL)) '((OK (BOOL #f))))
(test-equal (ev '(MATCH (OPT ((NAT 1))) SOME)) '((OK (BOOL #t))))
(test-equal (ev '(MATCH (OPT ()) SOME)) '((OK (BOOL #f))))
(test-equal (ev '(MATCH (OPT ()) NONE)) '((OK (BOOL #t))))
(test-equal (ev '(MATCH (OPT ((NAT 1))) NONE)) '((OK (BOOL #f))))
;; fail: a value of another kind than the pattern's, or a failing operand
(test-equal (ev '(MATCH (VAR "l") (INJ (("Some") ())))) '(FAIL))
(test-equal (ev `(MATCH ,some-1 NIL)) '(FAIL))
(test-equal (ev '(MATCH (VAR "l") SOME)) '(FAIL))
(test-equal (ev `(MATCH ,none CONS)) '(FAIL))

;;; Tuples, cases, structs, options, and lists

(test-equal (ev '(TUP ((VAR "x") (BOOL #t)))) '((OK (TUP ((NAT 1) (BOOL #t))))))
(test-equal (ev '(TUP ())) '((OK (TUP ()))))
;; fail: a component fails, and the later ones are not evaluated
(test-equal (ev `(TUP ((VAR "x") ,none ,boom))) '(FAIL))

(test-equal (ev '(INJ ((("Some") ()) ((VAR "x"))))) `((OK ,some-1)))
(test-equal (ev `(INJ ((("Some") ()) (,none)))) '(FAIL))

(test-equal (ev '(STR (("a" (VAR "x")) ("b" (BOOL #t)))))
            '((OK (STR (("a" (NAT 1)) ("b" (BOOL #t)))))))
(test-equal (ev '(STR ())) '((OK (STR ()))))
(test-equal (ev `(STR (("a" ,none)))) '(FAIL))

(test-equal (ev '(OPT ((VAR "x")))) '((OK (OPT ((NAT 1))))))
(test-equal (ev '(OPT ())) '((OK (OPT ()))))
(test-equal (ev `(OPT (,none))) '(FAIL))

(test-equal (ev '(LIST ((VAR "x") (NAT 2)))) '((OK (LIST ((NAT 1) (NAT 2))))))
(test-equal (ev '(LIST ())) '((OK (LIST ()))))
(test-equal (ev `(LIST (,none))) '(FAIL))

;;; Cons-lists, concatenation, membership, and length

(test-equal (ev '(CONS (VAR "x") (VAR "l"))) '((OK (LIST ((NAT 1) (NAT 10) (NAT 20) (NAT 30))))))
(test-equal (ev '(CONS (VAR "x") (LIST ()))) '((OK (LIST ((NAT 1))))))
;; fail: the tail is not a list, or fails; the head fails, and the tail is not
;; evaluated
(test-equal (ev '(CONS (VAR "x") (VAR "x"))) '(FAIL))
(test-equal (ev `(CONS (VAR "x") ,none)) '(FAIL))
(test-equal (ev `(CONS ,none ,boom)) '(FAIL))

(test-equal (ev '(CAT (VAR "t") (TEXT "!"))) '((OK (TEXT "hello!"))))
(test-equal (ev '(CAT (VAR "l") (LIST ((NAT 40)))))
            '((OK (LIST ((NAT 10) (NAT 20) (NAT 30) (NAT 40))))))
;; fail: the right operand has another kind than the left one, or fails
(test-equal (ev '(CAT (VAR "t") (LIST ()))) '(FAIL))
(test-equal (ev '(CAT (LIST ()) (TEXT ""))) '(FAIL))
(test-equal (ev `(CAT (VAR "t") ,none)) '(FAIL))
;; fail: the left operand fails or is neither a text nor a list, and the
;; right one is not evaluated
(test-equal (ev `(CAT ,none ,boom)) '(FAIL))
(test-equal (ev `(CAT (VAR "x") ,boom)) '(FAIL))

(test-equal (ev '(MEM (NAT 20) (VAR "l"))) '((OK (BOOL #t))))
(test-equal (ev '(MEM (INT 20) (VAR "l"))) '((OK (BOOL #f))))
(test-equal (ev '(MEM (NAT 1) (LIST ()))) '((OK (BOOL #f))))
;; fail: not a list, a failing list; a failing element, and the list is not
;; evaluated
(test-equal (ev '(MEM (NAT 1) (VAR "x"))) '(FAIL))
(test-equal (ev `(MEM (NAT 1) ,none)) '(FAIL))
(test-equal (ev `(MEM ,none ,boom)) '(FAIL))

;; A text's length is in bytes.
(test-equal (ev '(LEN (VAR "t"))) '((OK (NAT 5))))
(test-equal (ev '(LEN (TEXT "é"))) '((OK (NAT 2))))
(test-equal (ev '(LEN (VAR "l"))) '((OK (NAT 3))))
(test-equal (ev '(LEN (VAR "x"))) '(FAIL))
(test-equal (ev `(LEN ,none)) '(FAIL))

;;; Dot, indexing, and slicing, through eval-path

(test-equal (ev '(DOT (VAR "s") "b")) '((OK (BOOL #f))))
;; dot-fail: no such field; fail: not a struct, or a failing base
(test-equal (ev '(DOT (VAR "s") "c")) '(FAIL))
(test-equal (ev '(DOT (VAR "l") "a")) '(FAIL))
(test-equal (ev `(DOT ,none "a")) '(FAIL))

(test-equal (ev '(IDX (VAR "l") (NAT 2))) '((OK (NAT 30))))
(test-equal (ev '(IDX (VAR "t") (NAT 1))) '((OK (TEXT "e"))))
;; text-fail and list-fail: the index is out of bounds
(test-equal (ev '(IDX (VAR "t") (NAT 5))) '(FAIL))
(test-equal (ev '(IDX (VAR "l") (NAT 3))) '(FAIL))
;; fail: an index that is not a NAT, or fails
(test-equal (ev '(IDX (VAR "l") (INT 0))) '(FAIL))
(test-equal (ev `(IDX (VAR "t") ,none)) '(FAIL))
;; fail: a base that is neither a text nor a list, or fails, and the index is
;; not evaluated
(test-equal (ev `(IDX (VAR "x") ,boom)) '(FAIL))
(test-equal (ev `(IDX ,none ,boom)) '(FAIL))

(test-equal (ev '(SLICE (VAR "l") (NAT 1) (NAT 2))) '((OK (LIST ((NAT 20) (NAT 30))))))
(test-equal (ev '(SLICE (VAR "l") (NAT 3) (NAT 0))) '((OK (LIST ()))))
(test-equal (ev '(SLICE (VAR "t") (NAT 1) (NAT 3))) '((OK (TEXT "ell"))))
;; No premise bounds a slice, so one out of bounds raises an error.
(check-exn #rx"out of bounds" (λ () (ev '(SLICE (VAR "l") (NAT 2) (NAT 2)))))
(check-exn #rx"out of bounds" (λ () (ev '(SLICE (VAR "t") (NAT 5) (NAT 1)))))
;; fail: a length that is not a NAT, or fails
(test-equal (ev '(SLICE (VAR "l") (NAT 1) (INT 1))) '(FAIL))
(test-equal (ev `(SLICE (VAR "t") (NAT 1) ,none)) '(FAIL))
;; fail: a start that is not a NAT, or fails, and the length is not evaluated
(test-equal (ev `(SLICE (VAR "l") (BOOL #t) ,boom)) '(FAIL))
(test-equal (ev `(SLICE (VAR "t") ,none ,boom)) '(FAIL))
;; fail: a base that is neither a text nor a list, or fails, and the start
;; is not evaluated
(test-equal (ev `(SLICE (VAR "x") ,boom (NAT 1))) '(FAIL))
(test-equal (ev `(SLICE ,none ,boom (NAT 1))) '(FAIL))

;; Nested paths, straight through eval-path
(define ls-val '(LIST ((STR (("a" (NAT 1)))) (STR (("a" (NAT 2)))))))
(test-equal (outputs (eval-path ,C-exp ,ls-val (DOT (IDX ROOT (NAT 1)) "a") any)) '((OK (NAT 2))))
(test-equal (outputs (eval-path ,C-exp ,ls-val (SLICE (IDX ROOT (NAT 1)) (NAT 0) (NAT 1)) any))
            '(FAIL))
(test-equal (outputs (eval-path ,C-exp ,ls-val ROOT any)) `((OK ,ls-val)))

;;; Update, through eval-path-upd

(test-equal (ev '(UPD (VAR "x") ROOT (NAT 2))) '((OK (NAT 2))))

(test-equal (ev '(UPD (VAR "l") (IDX ROOT (NAT 1)) (NAT 99)))
            '((OK (LIST ((NAT 10) (NAT 99) (NAT 30))))))
;; A list element may be a text.
(test-equal (ev '(UPD (VAR "l") (IDX ROOT (NAT 1)) (TEXT "x")))
            '((OK (LIST ((NAT 10) (TEXT "x") (NAT 30))))))
(test-equal (ev '(UPD (VAR "t") (IDX ROOT (NAT 0)) (TEXT "j"))) '((OK (TEXT "jello"))))
;; text-fail: a new text of another length than 1
(test-equal (ev '(UPD (VAR "t") (IDX ROOT (NAT 0)) (TEXT "jj"))) '(FAIL))
;; No premise bounds the index, so one out of bounds raises an error.
(check-exn #rx"out of bounds" (λ () (ev '(UPD (VAR "l") (IDX ROOT (NAT 3)) (NAT 99)))))
(check-exn #rx"out of bounds" (λ () (ev '(UPD (VAR "t") (IDX ROOT (NAT 5)) (TEXT "j")))))
;; fail: an index that is not a NAT; a text updated with a non-text; a base
;; that is neither, and the index is not evaluated
(test-equal (ev '(UPD (VAR "l") (IDX ROOT (INT 1)) (NAT 99))) '(FAIL))
(test-equal (ev `(UPD (VAR "t") (IDX ROOT ,boom) (NAT 1))) '(FAIL))
(test-equal (ev `(UPD (VAR "x") (IDX ROOT ,boom) (NAT 1))) '(FAIL))

;; Deviation: a list slice is replaced by the elements of a list.
(test-equal (ev '(UPD (VAR "l") (SLICE ROOT (NAT 1) (NAT 2)) (LIST ((NAT 7) (NAT 8)))))
            '((OK (LIST ((NAT 10) (NAT 7) (NAT 8))))))
(test-equal (ev '(UPD (VAR "l") (SLICE ROOT (NAT 3) (NAT 0)) (LIST ())))
            '((OK (LIST ((NAT 10) (NAT 20) (NAT 30))))))
(test-equal (ev '(UPD (VAR "t") (SLICE ROOT (NAT 1) (NAT 3)) (TEXT "ELL"))) '((OK (TEXT "hELLo"))))
;; text-fail: a new text of another length than the slice
(test-equal (ev '(UPD (VAR "t") (SLICE ROOT (NAT 1) (NAT 3)) (TEXT "EL"))) '(FAIL))
;; No premise checks a list's length, nor bounds the slice.
(check-exn #rx"length 2 updated with one of length 1"
           (λ () (ev '(UPD (VAR "l") (SLICE ROOT (NAT 1) (NAT 2)) (LIST ((NAT 7)))))))
(check-exn #rx"out of bounds"
           (λ () (ev '(UPD (VAR "l") (SLICE ROOT (NAT 2) (NAT 2)) (LIST ((NAT 7) (NAT 8)))))))
;; fail: a new value that is neither a text nor a list, before any premise
(test-equal (ev `(UPD (VAR "l") (SLICE (IDX ROOT ,boom) ,boom ,boom) (NAT 5))) '(FAIL))
;; fail: a base of another kind than the new value, and the start is not
;; evaluated
(test-equal (ev `(UPD (VAR "t") (SLICE ROOT ,boom (NAT 1)) (LIST ()))) '(FAIL))
(test-equal (ev `(UPD (VAR "l") (SLICE ROOT ,boom (NAT 1)) (TEXT ""))) '(FAIL))
;; fail: a start that is not a NAT, and the length is not evaluated
(test-equal (ev `(UPD (VAR "l") (SLICE ROOT (INT 0) ,boom) (LIST ()))) '(FAIL))
;; fail: a length that is not a NAT
(test-equal (ev '(UPD (VAR "t") (SLICE ROOT (NAT 0) (INT 0)) (TEXT ""))) '(FAIL))

;; Every field with the atom is updated; none, if there is none.
(test-equal (ev '(UPD (VAR "s") (DOT ROOT "a") (NAT 9)))
            '((OK (STR (("a" (NAT 9)) ("b" (BOOL #f)))))))
(test-equal (ev '(UPD (STR (("a" (NAT 1)) ("a" (NAT 2)))) (DOT ROOT "a") (NAT 9)))
            '((OK (STR (("a" (NAT 9)) ("a" (NAT 9)))))))
(test-equal (ev '(UPD (VAR "s") (DOT ROOT "c") (NAT 9)))
            '((OK (STR (("a" (NAT 1)) ("b" (BOOL #f)))))))
(test-equal (ev '(UPD (VAR "l") (DOT ROOT "a") (NAT 9))) '(FAIL))

;; Nested paths rebuild the outer values.
(test-equal (ev '(UPD (VAR "ls") (DOT (IDX ROOT (NAT 1)) "a") (NAT 9)))
            '((OK (LIST ((STR (("a" (NAT 1)))) (STR (("a" (NAT 9)))))))))
(test-equal (ev '(UPD (VAR "ls") (DOT (IDX ROOT (NAT 2)) "a") (NAT 9))) '(FAIL))

;; fail: the base or the new value fails, and the new value is evaluated only
;; if the base succeeds
(test-equal (ev `(UPD ,none ROOT ,boom)) '(FAIL))
(test-equal (ev `(UPD (VAR "x") ROOT ,none)) '(FAIL))

;;; Calls

(test-equal (ev '(CALL "id" () ((EXP (VAR "x"))))) '((OK (NAT 1))))
;; Deviation: the callee gets the type arguments substituted by the caller's
;; local types, so Y is NAT in g.
(test-equal (ev '(CALL "g" ((VAR "X" ())) ((EXP (INT 3))))) '((OK (BOOL #t))))
(test-equal (ev '(CALL "g" ((VAR "X" ())) ((EXP (INT -3))))) '((OK (BOOL #f))))
;; The callee's FAIL: no clause of id applies to no argument.
(test-equal (ev '(CALL "id" () ())) '(FAIL))
;; fail: Eval_targs has no derivation, since X takes no type arguments
(test-equal (ev '(CALL "id" ((VAR "X" (NAT))) ((EXP (VAR "x"))))) '(FAIL))
;; fail: an argument fails
(test-equal (ev `(CALL "id" () ((EXP ,none)))) '(FAIL))
;; fail: Call_func has no derivation, since there is no such function
(test-equal (ev '(CALL "none" () ())) '(FAIL))

;;; Iteration

;; simple: an iterated variable
(test-equal (ev '(ITER (VAR "a") (STAR (("a" NAT ()))))) '((OK (LIST ((NAT 1) (NAT 2))))))
(test-equal (ev '(ITER (VAR "w") (STAR (("w" NAT ()))))) '((OK (NAT 0))))
;; simple-fail: not bound
(test-equal (ev '(ITER (VAR "none") (STAR (("none" NAT ()))))) '(FAIL))

;; opt
(test-equal (ev '(ITER (BIN ADD (VAR "o") (NAT 1)) (QUEST (("o" NAT ()))))) '((OK (OPT ((NAT 6))))))
(test-equal (ev '(ITER (BIN ADD (VAR "n") (NAT 1)) (QUEST (("n" NAT ()))))) '((OK (OPT ()))))
;; opt-fail: $sub_opt has no clause for a mix of some and none
(test-equal (ev '(ITER (TUP ((VAR "o") (VAR "n"))) (QUEST (("o" NAT ()) ("n" NAT ()))))) '(FAIL))

;; list
(test-equal (ev '(ITER (BIN ADD (VAR "a") (NAT 1)) (STAR (("a" NAT ())))))
            '((OK (LIST ((NAT 2) (NAT 3))))))
;; Lists of different lengths do not transpose.
(check-exn #rx"cannot transpose"
           (λ () (ev '(ITER (TUP ((VAR "a") (VAR "u"))) (STAR (("a" NAT ()) ("u" NAT ())))))))
;; list-fail: $sub_list has no clause for a non-list
(test-equal (ev '(ITER (BIN ADD (VAR "w") (NAT 1)) (STAR (("w" NAT ()))))) '(FAIL))

;; fail: the expression fails under a sub-context
(test-equal (ev '(ITER (BIN ADD (VAR "a") (BOOL #t)) (STAR (("a" NAT ()))))) '(FAIL))
(test-equal (ev '(ITER (BIN ADD (VAR "o") (BOOL #t)) (QUEST (("o" NAT ()))))) '(FAIL))

;;
;; Nesting: each shared premise is evaluated once, so the number of calls grows
;; polynomially with depth. Evaluating one twice would double it at each level.
;;

;; A nesting named *-fail fails; the others succeed, so they evaluate every level.
(define (check-not-exponential name nest)
  (define (nested depth)
    (for/fold ([exp (nest #f)]) ([_ (in-range depth)])
      (nest exp)))
  (define (calls depth)
    (count-calls 'eval-exp (λ () (ev (nested depth)))))
  (define-values (c4 c10) (values (calls 4) (calls 10)))
  (check-true (< c10 (* 10 c4))
              (format "~a: ~a calls at depth 4, ~a at depth 10" name c4 c10))
  (define res (car (ev (nested 4))))
  (check-equal? (if (pair? res) (car res) res)
                (if (regexp-match? #rx"-fail$" (symbol->string name)) 'FAIL 'OK)
                (format "~a at depth 4" name)))

;; (nest #f) is the innermost expression, (nest exp) wraps exp.
(for ([name+nest
       (in-list
        `([unary ,(λ (e) (if e `(UN NOT ,e) '(BOOL #t)))]
          [binary-left ,(λ (e) (if e `(BIN AND ,e (BOOL #t)) '(BOOL #t)))]
          [binary-right ,(λ (e) (if e `(BIN ADD (NAT 1) ,e) '(NAT 1)))]
          [binary-fail ,(λ (e) (if e `(BIN AND ,e ,none) '(BOOL #t)))]
          [compare ,(λ (e) (if e `(CMP EQ ,e (BOOL #t)) '(BOOL #t)))]
          [compare-right ,(λ (e) (if e `(CMP LT (NAT 1) (LEN (LIST (,e)))) '(NAT 1)))]
          [upcast ,(λ (e) (if e `(UPCAST INT ,e) '(NAT 1)))]
          [downcast ,(λ (e) (if e `(DOWNCAST NAT ,e) '(INT 1)))]
          [subtype ,(λ (e) (if e `(SUB ,e BOOL) '(BOOL #t)))]
          [match ,(λ (e) (if e `(MATCH (LIST (,e)) CONS) '(BOOL #t)))]
          [tuple ,(λ (e) (if e `(TUP (,e (BOOL #t))) '(BOOL #t)))]
          [case ,(λ (e) (if e `(INJ ((("C") ()) (,e))) '(BOOL #t)))]
          [struct ,(λ (e) (if e `(STR (("a" ,e))) '(BOOL #t)))]
          [opt ,(λ (e) (if e `(OPT (,e)) '(BOOL #t)))]
          [list ,(λ (e) (if e `(LIST (,e)) '(BOOL #t)))]
          [cons ,(λ (e) (if e `(CONS ,e (LIST ())) '(BOOL #t)))]
          [concat ,(λ (e) (if e `(CAT ,e (LIST ())) '(LIST ())))]
          [concat-right ,(λ (e) (if e `(CAT (LIST ()) ,e) '(LIST ())))]
          [mem ,(λ (e) (if e `(MEM ,e (LIST ((BOOL #t)))) '(BOOL #t)))]
          [len ,(λ (e) (if e `(LEN (LIST (,e))) '(NAT 0)))]
          [dot ,(λ (e) (if e `(DOT (STR (("a" ,e))) "a") '(NAT 0)))]
          [idx ,(λ (e) (if e `(IDX (LIST (,e)) (NAT 0)) '(NAT 0)))]
          [idx-index ,(λ (e) (if e `(IDX (LIST ((NAT 0))) ,e) '(NAT 0)))]
          [slice ,(λ (e) (if e `(IDX (SLICE (LIST (,e)) (NAT 0) (NAT 1)) (NAT 0)) '(NAT 0)))]
          [slice-start ,(λ (e) (if e `(LEN (SLICE (LIST ((NAT 0))) ,e (NAT 0))) '(NAT 0)))]
          [slice-length ,(λ (e) (if e `(LEN (SLICE (LIST ((NAT 0))) (NAT 0) ,e)) '(NAT 1)))]
          [upd ,(λ (e) (if e `(IDX (UPD (LIST ((NAT 0))) (IDX ROOT (NAT 0)) ,e) (NAT 0)) '(NAT 0)))]
          [upd-index ,(λ (e) (if e `(IDX (UPD (LIST ((NAT 0))) (IDX ROOT ,e) (NAT 0)) (NAT 0))
                                 '(NAT 0)))]
          [upd-base ,(λ (e) (if e `(UPD ,e (IDX ROOT (NAT 0)) (NAT 0)) '(LIST ((NAT 0)))))]
          [upd-slice ,(λ (e) (if e `(LEN (UPD (LIST ((NAT 0))) (SLICE ROOT (NAT 0) (NAT 1))
                                              (LIST (,e))))
                                 '(NAT 0)))]
          [upd-slice-base ,(λ (e) (if e `(UPD ,e (SLICE ROOT (NAT 0) (NAT 0)) (LIST ())) '(LIST ())))]
          [upd-slice-start ,(λ (e) (if e `(LEN (UPD (LIST ()) (SLICE ROOT ,e (NAT 0)) (LIST ())))
                                       '(NAT 0)))]
          [upd-slice-length ,(λ (e) (if e `(LEN (UPD (LIST ()) (SLICE ROOT (NAT 0) ,e) (LIST ())))
                                        '(NAT 0)))]
          [upd-dot ,(λ (e) (if e `(DOT (UPD (STR (("a" (NAT 0)))) (DOT ROOT "a") ,e) "a")
                               '(NAT 0)))]
          [call ,(λ (e) (if e `(CALL "id" () ((EXP ,e))) '(NAT 0)))]
          [iter ,(λ (e) (if e `(LEN (ITER (TUP ((VAR "u") ,e)) (STAR (("u" NAT ())))))
                            '(NAT 0)))]))])
  (check-not-exponential (car name+nest) (cadr name+nest)))

;; Every rule appears in some derivation above.
(check-rules-used eval-exp)
(check-rules-used eval-exp/variable)
(check-rules-used eval-exp/unary)
(check-rules-used eval-exp/binary)
(check-rules-used eval-exp/binary-right)
(check-rules-used eval-exp/compare)
(check-rules-used eval-exp/compare-right)
(check-rules-used eval-exp/upcast)
(check-rules-used eval-exp/downcast)
(check-rules-used eval-exp/subtype)
(check-rules-used eval-exp/match)
(check-rules-used eval-exp/tuple)
(check-rules-used eval-exp/case)
(check-rules-used eval-exp/struct)
(check-rules-used eval-exp/opt)
(check-rules-used eval-exp/list)
(check-rules-used eval-exp/cons)
(check-rules-used eval-exp/concat)
(check-rules-used eval-exp/concat-right)
(check-rules-used eval-exp/mem)
(check-rules-used eval-exp/len)
(check-rules-used eval-exp/dot)
(check-rules-used eval-exp/idx)
(check-rules-used eval-exp/slice)
(check-rules-used eval-exp/upd)
(check-rules-used eval-exp/call)
(check-rules-used eval-exp/call-args)
(check-rules-used eval-exp/call-func)
(check-rules-used eval-exp/iter)
(check-rules-used eval-exp/iter-subs)
(check-rules-used eval-exps)
(check-rules-used eval-exps/cons)
(check-rules-used eval-exp-subs)
(check-rules-used eval-exp-subs/cons)
(check-rules-used eval-path)
(check-rules-used eval-path/dot)
(check-rules-used eval-path/idx)
(check-rules-used eval-path/idx-index)
(check-rules-used eval-path/slice)
(check-rules-used eval-path/slice-start)
(check-rules-used eval-path/slice-length)
(check-rules-used eval-path-upd)
(check-rules-used eval-path-upd/idx)
(check-rules-used eval-path-upd/idx-index)
(check-rules-used eval-path-upd/slice)
(check-rules-used eval-path-upd/slice-base)
(check-rules-used eval-path-upd/slice-start)
(check-rules-used eval-path-upd/slice-length)
(check-rules-used eval-path-upd/dot)

(test-results)
