#lang racket/base

(require racket/list
         racket/port
         rackunit
         "../common/0.0-prelude.rkt"
         "../al/5-eval.rkt"
         "judgment.rkt")

;; A relation with one rule group of one rule path, from input exp_in to
;; outputs exp_out* under prem*
(define (rel-of exp_in exp_outs prems)
  `(DEF (("" ((,exp_in) ()) (("" ,exp_outs ,prems)))) ()))

;; Double(n) = n + n, IsZero(n) holds for 0, and Down(i) holds for i >= 0 by
;; recursion.
(define double (rel-of '(VAR "n") '((VAR "m")) '((LET (VAR "m") (BIN ADD (VAR "n") (VAR "n"))))))
(define is-zero (rel-of '(VAR "n") '() '((IF (CMP EQ (VAR "n") (NAT 0))))))
(define down
  '(DEF (("zero" (((VAR "i")) ()) (("" () ((IF (CMP EQ (VAR "i") (INT 0)))))))
         ("succ" (((VAR "i")) ())
          (("" () ((IF (CMP GT (VAR "i") (INT 0)))
                   (LET (VAR "j") (BIN SUB (VAR "i") (INT 1)))
                   (IFHOLD "Down" ((VAR "j"))))))))
        ()))

(define vals
  '((("x" ()) (NAT 2))
    (("a" (STAR)) (LIST ((NAT 1) (NAT 2))))
    (("e" (STAR)) (LIST ()))
    (("i" (STAR)) (LIST ((INT 1) (INT 0))))
    (("u" (STAR)) (LIST ((NAT 0))))
    (("o" (QUEST)) (OPT ((NAT 5))))
    (("n" (QUEST)) (OPT ()))
    (("w" (STAR)) (NAT 0))))

;; $f(i) = 0 for i = 0, and otherwise recurses once through a premise made by
;; recur, given the recursive call
(define (recursive-def f recur)
  (define (clause-of exp prems) `(((EXP (VAR "i"))) ,exp ,prems))
  `(DEF ()
        (,(clause-of '(INT 0) '((IF (CMP EQ (VAR "i") (INT 0))))))
        (,(apply clause-of (recur `(CALL ,f () ((EXP (BIN SUB (VAR "i") (INT 1))))))))))

(define funcs
  `(("rif" ,(recursive-def "rif" (λ (call) `((INT 0) ((IF (CMP EQ ,call (INT 0))))))))
    ("rlet" ,(recursive-def "rlet" (λ (call) `((VAR "j") ((LET (VAR "j") ,call))))))
    ("rdbg" ,(recursive-def "rdbg" (λ (call) `((INT 0) ((DEBUG ,call))))))))

(define C-prem
  (ctx-of `(() (("Double" ,double) ("IsZero" ,is-zero) ("Down" ,down)) ,funcs ())
          `(() () () ,vals)))

(define boom '(BIN DIV (NAT 1) (NAT 0)))
(define none '(VAR "none"))

;; The local values the result adds to C-prem's, or FAIL
(define (added results)
  (for/list ([res (in-list results)])
    (if (eq? res 'FAIL)
        'FAIL
        (drop (list-ref (list-ref (cadr res) 3) 7) (length vals)))))

(define (ep prem) (added (outputs (eval-prem ,C-prem ,prem any))))
(define (eps prems) (added (outputs (eval-prems ,C-prem ,prems any))))

;;
;; eval-prem
;;

;;; Relation premises

(test-equal (ep '(REL "Double" ((VAR "x")) ((VAR "y")))) '(((("y" ()) (NAT 4)))))
(test-equal (ep '(REL "IsZero" ((NAT 0)) ())) '(()))
;; fail: an input fails
(test-equal (ep `(REL "Double" (,none) ((VAR "y")))) '(FAIL))
;; fail: Call_rel is FAIL, or has no derivation since there is no such
;; relation
(test-equal (ep '(REL "IsZero" ((NAT 1)) ())) '(FAIL))
(test-equal (ep '(REL "None" () ())) '(FAIL))
;; fail: Assign_exps has no derivation
(test-equal (ep '(REL "Double" ((VAR "x")) ((TUP ())))) '(FAIL))
(test-equal (ep '(REL "Double" ((VAR "x")) ((VAR "y") (VAR "z")))) '(FAIL))

;;; If premises

(test-equal (ep '(IF (CMP EQ (VAR "x") (NAT 2)))) '(()))
(test-equal (ep '(IF (BOOL #f))) '(FAIL))
;; fail: not a boolean, or a failing condition
(test-equal (ep '(IF (VAR "x"))) '(FAIL))
(test-equal (ep `(IF ,none)) '(FAIL))

;;; If-hold and if-not-hold premises

(test-equal (ep '(IFHOLD "IsZero" ((NAT 0)))) '(()))
(test-equal (ep '(IFNOTHOLD "IsZero" ((NAT 1)))) '(()))
;; fail: the relation is FAIL for IFHOLD, has outputs, or has no derivation
(test-equal (ep '(IFHOLD "IsZero" ((NAT 1)))) '(FAIL))
(test-equal (ep '(IFHOLD "Double" ((NAT 1)))) '(FAIL))
(test-equal (ep '(IFHOLD "None" ())) '(FAIL))
;; fail: the relation holds for IFNOTHOLD, or has no derivation
(test-equal (ep '(IFNOTHOLD "IsZero" ((NAT 0)))) '(FAIL))
(test-equal (ep '(IFNOTHOLD "Double" ((NAT 1)))) '(FAIL))
(test-equal (ep '(IFNOTHOLD "None" ())) '(FAIL))
;; fail: an input fails
(test-equal (ep `(IFHOLD "IsZero" (,none))) '(FAIL))
(test-equal (ep `(IFNOTHOLD "IsZero" (,none))) '(FAIL))

;;; Let premises

(test-equal (ep '(LET (VAR "y") (VAR "x"))) '(((("y" ()) (NAT 2)))))
(test-equal (ep '(LET (TUP ((VAR "y") (VAR "z"))) (TUP ((NAT 1) (NAT 2)))))
            '(((("y" ()) (NAT 1)) (("z" ()) (NAT 2)))))
;; fail: Assign_exp has no derivation, or the right-hand side fails
(test-equal (ep '(LET (TUP ()) (VAR "x"))) '(FAIL))
(test-equal (ep `(LET (VAR "y") ,none)) '(FAIL))

;;; Iteration premises, optional

;; none: every binding variable is bound to OPT eps.
(test-equal (ep '(ITER (LET (VAR "y") (VAR "n")) (QUEST (("n" NAT ())) (("y" NAT ())))))
            '(((("y" (QUEST)) (OPT ())))))
;; some: every binding variable is bound to OPT of its value under the
;; sub-context, whose other bindings are dropped.
(test-equal (ep '(ITER (LET (VAR "y") (BIN ADD (VAR "o") (NAT 1)))
                       (QUEST (("o" NAT ())) (("y" NAT ())))))
            '(((("y" (QUEST)) (OPT ((NAT 6)))))))
;; With no bound variable, $sub_opt gives the context itself.
(test-equal (ep '(ITER (LET (VAR "y") (NAT 1)) (QUEST () (("y" NAT ())))))
            '(((("y" (QUEST)) (OPT ((NAT 1)))))))
;; fail: $sub_opt has no clause for a mix of some and none
(test-equal (ep '(ITER (IF (BOOL #t)) (QUEST (("o" NAT ()) ("n" NAT ())) ()))) '(FAIL))
;; fail: the premise fails under the sub-context
(test-equal (ep '(ITER (IF (CMP EQ (VAR "o") (NAT 0))) (QUEST (("o" NAT ())) ()))) '(FAIL))
;; some-fail: a binding variable is not bound under the sub-context
(test-equal (ep '(ITER (IF (BOOL #t)) (QUEST (("o" NAT ())) (("y" NAT ()))))) '(FAIL))

;;; Iteration premises, list

;; empty: every binding variable is bound to LIST eps.
(test-equal (ep '(ITER (LET (VAR "y") (VAR "e")) (STAR (("e" NAT ())) (("y" NAT ())))))
            '(((("y" (STAR)) (LIST ())))))
;; list: the binding variables' values under each sub-context, transposed
(test-equal (ep '(ITER (LET (VAR "y") (BIN ADD (VAR "a") (NAT 1)))
                       (STAR (("a" NAT ())) (("y" NAT ())))))
            '(((("y" (STAR)) (LIST ((NAT 2) (NAT 3)))))))
(test-equal (ep '(ITER (LET (TUP ((VAR "y") (VAR "z"))) (TUP ((VAR "a") (BOOL #t))))
                       (STAR (("a" NAT ())) (("y" NAT ()) ("z" BOOL ())))))
            '(((("y" (STAR)) (LIST ((NAT 1) (NAT 2))))
               (("z" (STAR)) (LIST ((BOOL #t) (BOOL #t)))))))
(test-equal (ep '(ITER (IF (CMP LT (VAR "a") (NAT 3))) (STAR (("a" NAT ())) ()))) '(()))
;; fail: $sub_list has no clause for a non-list
(test-equal (ep '(ITER (IF (BOOL #t)) (STAR (("w" NAT ())) ()))) '(FAIL))
;; fail: the premise fails under a sub-context, and the later ones are not
;; evaluated
(test-equal (ep '(ITER (IF (CMP LT (VAR "a") (NAT 2))) (STAR (("a" NAT ())) ()))) '(FAIL))
(test-equal (ep '(ITER (IF (CMP EQ (BIN DIV (INT 1) (VAR "i")) (INT 0))) (STAR (("i" INT ())) ())))
            '(FAIL))
;; list-fail: a binding variable is not bound under a sub-context
(test-equal (ep '(ITER (IF (BOOL #t)) (STAR (("a" NAT ())) (("y" NAT ()))))) '(FAIL))

;;; Debug premises

(define (debug-output prem)
  (define err (open-output-string))
  (define results (parameterize ([current-error-port err]) (ep prem)))
  (list results (get-output-string err)))

(test-equal (debug-output '(DEBUG (VAR "x"))) '((()) "(NAT 2)\n"))
(test-equal (debug-output `(DEBUG ,none)) '((FAIL) ""))

;;
;; eval-prems
;;

(test-equal (eps '()) '(()))
;; Each premise sees the bindings of the ones before it.
(test-equal (eps '((LET (VAR "y") (NAT 1))
                    (IF (CMP EQ (VAR "y") (NAT 1)))
                    (LET (VAR "z") (VAR "y"))))
            '(((("y" ()) (NAT 1)) (("z" ()) (NAT 1)))))
;; It stops at the first FAIL.
(test-equal (eps `((IF (BOOL #f)) (IF ,boom))) '(FAIL))
(test-equal (eps `((LET (VAR "y") (NAT 1)) (IF (BOOL #f)) (IF ,boom))) '(FAIL))

;;
;; Nesting: each shared premise is evaluated once.
;;

;; Each nesting succeeds, so it evaluates every level.
(define (check-not-exponential name J prem-of)
  (define (calls depth)
    (count-calls J (λ () (ep (prem-of depth)))))
  (define-values (c4 c10) (values (calls 4) (calls 10)))
  (check-true (< c10 (* 10 c4))
              (format "~a: ~a calls at depth 4, ~a at depth 10" name c4 c10))
  (check-equal? (ep (prem-of 4)) '(()) (format "~a at depth 4" name)))

(define (nest-prem wrap depth)
  (for/fold ([prem '(IF (BOOL #t))]) ([_ (in-range depth)])
    (wrap prem)))

(check-not-exponential 'iterpr-opt 'eval-prem
                       (λ (depth) (nest-prem (λ (p) `(ITER ,p (QUEST () ()))) depth)))
(check-not-exponential 'iterpr-list 'eval-prem
                       (λ (depth) (nest-prem (λ (p) `(ITER ,p (STAR (("e" NAT ())) ()))) depth)))
(check-not-exponential 'iterpr-list-nonempty 'eval-prem
                       (λ (depth) (nest-prem (λ (p) `(ITER ,p (STAR (("u" NAT ())) ()))) depth)))
;; Down recurses through IFHOLD, rule groups, and rule paths.
(check-not-exponential 'ifholdpr 'eval-prem (λ (depth) `(IFHOLD "Down" ((INT ,depth)))))
(check-not-exponential 'relpr 'eval-prem (λ (depth) `(REL "Down" ((INT ,depth)) ())))
;; Functions that recurse through IF, LET, and DEBUG
(for ([f (in-list '("rif" "rlet" "rdbg"))])
  (parameterize ([current-error-port (open-output-nowhere)])
    (check-not-exponential (string->symbol f) 'eval-prem
                           (λ (depth) `(IF (CMP EQ (CALL ,f () ((EXP (INT ,depth)))) (INT 0)))))))
(test-equal (ep '(IFHOLD "Down" ((INT 3)))) '(()))
(test-equal (ep '(IFHOLD "Down" ((INT -1)))) '(FAIL))

;; Every rule appears in some derivation above.
(check-rules-used eval-prem)
(check-rules-used eval-prem/relpr)
(check-rules-used eval-prem/relpr-call)
(check-rules-used eval-prem/relpr-assign)
(check-rules-used eval-prem/ifpr)
(check-rules-used eval-prem/ifholdpr)
(check-rules-used eval-prem/ifholdpr-call)
(check-rules-used eval-prem/letpr)
(check-rules-used eval-prem/letpr-assign)
(check-rules-used eval-prem/iterpr-opt)
(check-rules-used eval-prem/iterpr-opt-sub)
(check-rules-used eval-prem/iterpr-list)
(check-rules-used eval-prem/iterpr-list-subs)
(check-rules-used eval-prem-subs)
(check-rules-used eval-prem-subs/cons)
(check-rules-used eval-prem/dbg)
(check-rules-used eval-prems)
(check-rules-used eval-prems/head)

(test-results)
