#lang racket/base

(require racket/list
         "../common/0.0-prelude.rkt"
         "../al/3-context.rkt"
         "../al/5.2-eval-assign.rkt"
         "judgment.rkt")

;; A context with the given global and local layers, each a list of the
;; TYP, REL, FUNC, and VAL maps.
(define (ctx-of global local)
  (define (layer maps) (append-map list '(TYP REL FUNC VAL) maps))
  (list 'GLOBAL (layer global) 'LOCAL (layer local)))

(define (local-funcs C) (list-ref (list-ref C 3) 5))
(define (local-vals C) (list-ref (list-ref C 3) 7))

(define C-empty (term (empty_ctx)))
(define C-xy (ctx-of '(() () () ()) '(() () () ((("x" ()) (NAT 0)) (("y" ()) (NAT 1))))))
(define C-z (ctx-of '(() () () ()) '(() () () ((("z" ()) (NAT 9))))))

;; The LOCAL VAL maps of the contexts derived by assigning val to exp under C
(define (assign exp val [C C-empty])
  (map local-vals (outputs (Assign_exp ,C ,exp ,val any))))

;;
;; Assign_exp
;;

;; variable: any value, replacing an earlier binding where it stands
(test-equal (assign '(VAR "x") '(NAT 1)) '(((("x" ()) (NAT 1)))))
(test-equal (assign '(VAR "x") '(FUNC "f")) '(((("x" ()) (FUNC "f")))))
(test-equal (assign '(VAR "x") '(NAT 2) C-xy) '(((("x" ()) (NAT 2)) (("y" ()) (NAT 1)))))

;; tup
(test-equal (assign '(TUP ((VAR "x") (VAR "y"))) '(TUP ((NAT 1) (NAT 2))))
            '(((("x" ()) (NAT 1)) (("y" ()) (NAT 2)))))
(test-equal (assign '(TUP ()) '(TUP ())) '(()))
;; A repeated variable takes the last value.
(test-equal (assign '(TUP ((VAR "x") (VAR "x"))) '(TUP ((NAT 1) (NAT 2))))
            '(((("x" ()) (NAT 2)))))
;; No rule: other lengths, or not a tuple
(test-equal (assign '(TUP ((VAR "x"))) '(TUP ((NAT 1) (NAT 2)))) '())
(test-equal (assign '(TUP ((VAR "x"))) '(LIST ((NAT 1)))) '())

;; inj
(test-equal (assign '(INJ ((("Some") ()) ((VAR "x")))) '(INJ ((("Some") ()) ((NAT 1)))))
            '(((("x" ()) (NAT 1)))))
;; No rule: another mixop, or other arity
(test-equal (assign '(INJ ((("Some") ()) ((VAR "x")))) '(INJ ((("Other") ()) ((NAT 1))))) '())
(test-equal (assign '(INJ ((("Some") ()) ((VAR "x")))) '(INJ ((("Some") ()) ((NAT 1) (NAT 2)))))
            '())

;; str
(test-equal (assign '(STR (("a" (VAR "x")) ("b" (VAR "y")))) '(STR (("a" (NAT 1)) ("b" (NAT 2)))))
            '(((("x" ()) (NAT 1)) (("y" ()) (NAT 2)))))
(test-equal (assign '(STR ()) '(STR ())) '(()))
;; No rule: other atoms, in another order, or another number of fields
(test-equal (assign '(STR (("a" (VAR "x")))) '(STR (("b" (NAT 1))))) '())
(test-equal (assign '(STR (("a" (VAR "x")) ("b" (VAR "y")))) '(STR (("b" (NAT 2)) ("a" (NAT 1)))))
            '())
(test-equal (assign '(STR (("a" (VAR "x")))) '(STR (("a" (NAT 1)) ("b" (NAT 2))))) '())

;; opt
(test-equal (assign '(OPT ((VAR "x"))) '(OPT ((NAT 1)))) '(((("x" ()) (NAT 1)))))
(test-equal (assign '(OPT ()) '(OPT ())) '(()))
;; No rule: one side empty
(test-equal (assign '(OPT ((VAR "x"))) '(OPT ())) '())
(test-equal (assign '(OPT ()) '(OPT ((NAT 1)))) '())

;; list
(test-equal (assign '(LIST ((VAR "x") (VAR "y"))) '(LIST ((NAT 1) (NAT 2))))
            '(((("x" ()) (NAT 1)) (("y" ()) (NAT 2)))))
(test-equal (assign '(LIST ()) '(LIST ())) '(()))
;; No rule: other lengths, or not a list
(test-equal (assign '(LIST ((VAR "x"))) '(LIST ())) '())
(test-equal (assign '(LIST ((VAR "x"))) '(TUP ((NAT 1)))) '())

;; cons
(test-equal (assign '(CONS (VAR "h") (VAR "t")) '(LIST ((NAT 1) (NAT 2))))
            '(((("h" ()) (NAT 1)) (("t" ()) (LIST ((NAT 2)))))))
(test-equal (assign '(CONS (VAR "h") (VAR "t")) '(LIST ((NAT 1))))
            '(((("h" ()) (NAT 1)) (("t" ()) (LIST ())))))
(test-equal (assign '(CONS (VAR "a") (CONS (VAR "b") (VAR "t"))) '(LIST ((NAT 1) (NAT 2) (NAT 3))))
            '(((("a" ()) (NAT 1)) (("b" ()) (NAT 2)) (("t" ()) (LIST ((NAT 3)))))))
;; No rule: an empty list, not a list, or a tail that does not assign
(test-equal (assign '(CONS (VAR "h") (VAR "t")) '(LIST ())) '())
(test-equal (assign '(CONS (VAR "h") (VAR "t")) '(TUP ((NAT 1)))) '())
(test-equal (assign '(CONS (VAR "h") (LIST ())) '(LIST ((NAT 1) (NAT 2)))) '())

;; No rule for any other expression, even with a value of its own shape
(for ([exp+val
       (in-list '([(BOOL #t) (BOOL #t)]
                  [(NAT 1) (NAT 1)]
                  [(INT -1) (INT -1)]
                  [(TEXT "x") (TEXT "x")]
                  [(UN NOT (VAR "x")) (BOOL #f)]
                  [(BIN ADD (VAR "x") (VAR "y")) (NAT 1)]
                  [(CMP EQ (VAR "x") (VAR "y")) (BOOL #t)]
                  [(UPCAST INT (VAR "x")) (INT 1)]
                  [(DOWNCAST NAT (VAR "x")) (NAT 1)]
                  [(SUB (VAR "x") NAT) (BOOL #t)]
                  [(MATCH (VAR "x") NIL) (BOOL #t)]
                  [(CAT (VAR "x") (VAR "y")) (LIST ())]
                  [(MEM (VAR "x") (VAR "y")) (BOOL #t)]
                  [(LEN (VAR "x")) (NAT 0)]
                  [(DOT (VAR "x") "a") (NAT 1)]
                  [(IDX (VAR "x") (NAT 0)) (NAT 1)]
                  [(SLICE (VAR "x") (NAT 0) (NAT 1)) (LIST ())]
                  [(UPD (VAR "x") ROOT (NAT 1)) (NAT 1)]
                  [(CALL "f" () ()) (NAT 1)]))])
  (test-equal (assign (car exp+val) (cadr exp+val)) '()))

;;; iter

(define ab? '(ITER (TUP ((VAR "a") (VAR "b"))) (QUEST (("a" NAT ()) ("b" BOOL ())))))
(define ab* '(ITER (TUP ((VAR "a") (VAR "b"))) (STAR (("a" NAT ()) ("b" BOOL ())))))
(define w? '(ITER (INJ ((("W") ()) ((VAR "a")))) (QUEST (("a" NAT ())))))

;; iter/simple: an iterated variable takes any value.
(test-equal (assign '(ITER (VAR "x") (STAR (("x" NAT ())))) '(LIST ((NAT 1) (NAT 2))))
            '(((("x" (STAR)) (LIST ((NAT 1) (NAT 2)))))))
(test-equal (assign '(ITER (VAR "x") (QUEST (("x" NAT ())))) '(NAT 5))
            '(((("x" (QUEST)) (NAT 5)))))
(test-equal (assign '(ITER (ITER (VAR "x") (QUEST (("x" NAT ())))) (STAR (("x" NAT (QUEST)))))
                    '(LIST ((OPT ()))))
            '(((("x" (QUEST STAR)) (LIST ((OPT ())))))))

;; iter/opt-none: every variable is bound to OPT eps.
(test-equal (assign ab? '(OPT ())) '(((("a" (QUEST)) (OPT ())) (("b" (QUEST)) (OPT ())))))
(test-equal (assign '(ITER (TUP ()) (QUEST ())) '(OPT ())) '(()))

;; iter/opt-some: every variable is bound to OPT of its inner value, which
;; stays bound too.
(test-equal (assign w? '(OPT ((INJ ((("W") ()) ((NAT 3)))))))
            '(((("a" ()) (NAT 3)) (("a" (QUEST)) (OPT ((NAT 3)))))))
(test-equal (assign ab? '(OPT ((TUP ((NAT 1) (BOOL #t))))))
            '(((("a" ()) (NAT 1)) (("b" ()) (BOOL #t))
               (("a" (QUEST)) (OPT ((NAT 1)))) (("b" (QUEST)) (OPT ((BOOL #t)))))))
(test-equal (assign '(ITER (TUP ()) (QUEST ())) '(OPT ((TUP ())))) '(()))
;; No rule: the inner assignment fails, or does not bind a variable
(test-equal (assign w? '(OPT ((NAT 3)))) '())
(test-equal (assign '(ITER (INJ ((("W") ()) ((VAR "a")))) (QUEST (("z" NAT ()))))
                    '(OPT ((INJ ((("W") ()) ((NAT 3)))))))
            '())

;; iter/list: the elements' bindings, transposed
(test-equal (assign ab* '(LIST ((TUP ((NAT 1) (BOOL #t))) (TUP ((NAT 2) (BOOL #f))))))
            '(((("a" (STAR)) (LIST ((NAT 1) (NAT 2))))
                (("b" (STAR)) (LIST ((BOOL #t) (BOOL #f)))))))
(test-equal (assign ab* '(LIST ())) '(((("a" (STAR)) (LIST ())) (("b" (STAR)) (LIST ())))))
(test-equal (assign '(ITER (ITER (TUP ((VAR "a"))) (STAR (("a" NAT ())))) (STAR (("a" NAT (STAR)))))
                    '(LIST ((LIST ((TUP ((NAT 1))) (TUP ((NAT 2))))) (LIST ()))))
            '(((("a" (STAR STAR)) (LIST ((LIST ((NAT 1) (NAT 2))) (LIST ())))))))
;; Elements are assigned without the local values: their bindings stay out of
;; the result, and a variable bound only outside is not found.
(test-equal (assign '(ITER (TUP ((VAR "a"))) (STAR (("a" NAT ())))) '(LIST ((TUP ((NAT 1))))) C-z)
            '(((("z" ()) (NAT 9)) (("a" (STAR)) (LIST ((NAT 1)))))))
(test-equal (assign '(ITER (TUP ((VAR "a"))) (STAR (("a" NAT ()) ("z" NAT ()))))
                    '(LIST ((TUP ((NAT 1)))))
                    C-z)
            '())
;; No rule: an element that does not assign
(test-equal (assign ab* '(LIST ((TUP ((NAT 1) (BOOL #t))) (NAT 2)))) '())

;; No iter rule: the iterator and the value disagree, or the value is neither
;; an option nor a list
(test-equal (assign ab? '(LIST ())) '())
(test-equal (assign ab* '(OPT ())) '())
(test-equal (assign ab* '(TUP ())) '())

;;
;; Assign_exps
;;

(define (assign-exps exps vals)
  (map local-vals (outputs (Assign_exps ,C-empty ,exps ,vals any))))

(test-equal (assign-exps '() '()) '(()))
(test-equal (assign-exps '((VAR "x") (VAR "y")) '((NAT 1) (NAT 2)))
            '(((("x" ()) (NAT 1)) (("y" ()) (NAT 2)))))
;; No rule: other lengths, or an element that does not assign
(test-equal (assign-exps '((VAR "x")) '()) '())
(test-equal (assign-exps '() '((NAT 1))) '())
(test-equal (assign-exps '((VAR "x") (TUP ())) '((NAT 1) (NAT 2))) '())

;;
;; Assign_arg and Assign_args
;;

;; f is in the caller's local layer, g in its global one.
(define C-caller
  (ctx-of '(() () (("g" (EXT "g"))) ())
          '(() () (("f" (DEF () () ()))) ())))

(define (assign-arg arg val)
  (outputs (Assign_arg ,C-empty ,C-caller ,arg ,val any)))

(test-equal (map local-vals (assign-arg '(EXP (VAR "x")) '(NAT 1))) '(((("x" ()) (NAT 1)))))
(test-equal (map local-vals (assign-arg '(EXP (VAR "x")) '(FUNC "f"))) '(((("x" ()) (FUNC "f")))))
(test-equal (map local-funcs (assign-arg '(FUN "h") '(FUNC "f"))) '((("h" (DEF () () ())))))
(test-equal (map local-funcs (assign-arg '(FUN "h") '(FUNC "g"))) '((("h" (EXT "g")))))
;; No rule: an unknown function, a value that is not a function, or an
;; expression that does not assign
(test-equal (assign-arg '(FUN "h") '(FUNC "none")) '())
(test-equal (assign-arg '(FUN "h") '(NAT 1)) '())
(test-equal (assign-arg '(EXP (NAT 1)) '(NAT 1)) '())

(define (assign-args args vals)
  (outputs (Assign_args ,C-empty ,C-caller ,args ,vals any)))

(test-equal (assign-args '() '()) (list C-empty))
(test-equal (assign-args '((EXP (VAR "x")) (FUN "h")) '((NAT 1) (FUNC "f")))
            (list (ctx-of '(() () () ()) '(() () (("h" (DEF () () ()))) ((("x" ()) (NAT 1)))))))
;; No rule: other lengths, or an argument that does not assign
(test-equal (assign-args '((EXP (VAR "x"))) '()) '())
(test-equal (assign-args '() '((NAT 1))) '())
(test-equal (assign-args '((EXP (VAR "x")) (FUN "h")) '((NAT 1) (NAT 2))) '())

;; Every rule appears in some derivation above.
(check-rules-used Assign_exp)
(check-rules-used Assign_exps)
(check-rules-used Assign_arg)
(check-rules-used Assign_args)

(test-results)
