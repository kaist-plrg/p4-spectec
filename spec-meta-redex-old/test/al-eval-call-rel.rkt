#lang racket/base

(require rackunit
         "../common/0.0-prelude.rkt"
         "../al/5-eval.rkt"
         "judgment.rkt")

;; A rule path, id = exp_out* -- prem*, and a rule match, exp* -- prem*
(define (rulpath-of id exp_outs prems) (list id exp_outs prems))
(define (rulmatch-of exps prems) (list exps prems))

;; Double(n) = n + n
(define double-group
  `("" ,(rulmatch-of '((VAR "n")) '())
       (,(rulpath-of "" '((VAR "m")) '((LET (VAR "m") (BIN ADD (VAR "n") (VAR "n"))))))))
;; Classify(n) is "zero", "small even" or "small odd" below 10, or else "big".
;; The group "small" shares its premise between its paths.
(define classify-def
  `(DEF (("zero" ,(rulmatch-of '((VAR "n")) '())
                 (,(rulpath-of "" '((TEXT "zero")) '((IF (CMP EQ (VAR "n") (NAT 0)))))))
         ("small" ,(rulmatch-of '((VAR "n")) '((IF (CMP LT (VAR "n") (NAT 10)))))
                  (,(rulpath-of "even" '((TEXT "small even"))
                                '((IF (CMP EQ (BIN MOD (VAR "n") (NAT 2)) (NAT 0)))))
                   ,(rulpath-of "odd" '((TEXT "small odd")) '()))))
        (("big" ,(rulmatch-of '((VAR "n")) '()) ,(rulpath-of "" '((TEXT "big")) '())))))
;; Sum(i) = 0 + 1 + ... + i, by recursion
(define sum-def
  `(DEF (("zero" ,(rulmatch-of '((VAR "i")) '())
                 (,(rulpath-of "" '((INT 0)) '((IF (CMP EQ (VAR "i") (INT 0)))))))
         ("succ" ,(rulmatch-of '((VAR "i")) '((IF (CMP GT (VAR "i") (INT 0)))))
                 (,(rulpath-of "" '((BIN ADD (VAR "s") (VAR "i")))
                               '((REL "Sum" ((BIN SUB (VAR "i") (INT 1))) ((VAR "s"))))))))
        ()))
;; Relations that fail: on reading x, which only the caller binds; on a
;; non-tuple input; and on an output that fails
(define peek-def `(DEF (("" ,(rulmatch-of '() '()) (,(rulpath-of "" '((VAR "x")) '())))) ()))
(define pair-def
  `(DEF (("" ,(rulmatch-of '((TUP ((VAR "a") (VAR "b")))) '())
             (,(rulpath-of "" '((VAR "b") (VAR "a")) '()))))
        ()))
(define bad-def `(DEF (("" ,(rulmatch-of '() '()) (,(rulpath-of "" '((VAR "none")) '())))) ()))

(define rels
  `(("Double" (DEF (,double-group) ())) ("Classify" ,classify-def) ("Sum" ,sum-def)
    ("Peek" ,peek-def) ("Pair" ,pair-def) ("Bad" ,bad-def) ("Ext" (EXT "Ext"))))

(define C-rel (ctx-of `(() ,rels () ()) '(() () () ((("x" ()) (NAT 7))))))

(define (call id vals) (outputs (call-rel ,C-rel ,id ,vals any)))

;;
;; call-rel, through the whole chain
;;

(test-equal (call "Double" '((NAT 2))) '((OK ((NAT 4)))))
;; Rule groups are tried in order, then the else group, and rule paths in
;; order within a group.
(test-equal (call "Classify" '((NAT 0))) '((OK ((TEXT "zero")))))
(test-equal (call "Classify" '((NAT 4))) '((OK ((TEXT "small even")))))
(test-equal (call "Classify" '((NAT 3))) '((OK ((TEXT "small odd")))))
(test-equal (call "Classify" '((NAT 12))) '((OK ((TEXT "big")))))
(test-equal (call "Sum" '((INT 4))) '((OK ((INT 10)))))
(test-equal (call "Pair" '((TUP ((NAT 1) (NAT 2))))) '((OK ((NAT 2) (NAT 1)))))
;; FAIL: no rule applies, a rule does not see the caller's local values, the
;; input does not assign, or an output fails
(test-equal (call "Sum" '((INT -1))) '(FAIL))
(test-equal (call "Peek" '()) '(FAIL))
(test-equal (call "Pair" '((NAT 1))) '(FAIL))
(test-equal (call "Bad" '()) '(FAIL))
;; No derivation: no such relation
(test-equal (call "None" '()) '())
;; Extern relations are not implemented until Step 9.
(check-exn #rx"call-extern-rel: externs are not implemented" (λ () (call "Ext" '())))

;;
;; The relations below call-rel, one at a time
;;

(define C-local (ctx-of `(() ,rels () ()) '(() () () ())))

(define (run-eval-rul rulmatch rulpath vals)
  (outputs (eval-rul ,C-local ,rulmatch ,rulpath ,vals any)))

(test-equal (run-eval-rul (rulmatch-of '((VAR "n")) '()) (rulpath-of "" '((VAR "n") (VAR "n")) '())
                          '((NAT 1)))
            '((OK ((NAT 1) (NAT 1)))))
;; The rule match's premises come before the rule path's.
(test-equal (run-eval-rul (rulmatch-of '() '((LET (VAR "y") (NAT 1))))
                          (rulpath-of "" '((VAR "y")) '((IF (CMP EQ (VAR "y") (NAT 1)))))
                          '())
            '((OK ((NAT 1)))))
;; fail: the inputs do not assign, a premise fails, or an output fails
(test-equal (run-eval-rul (rulmatch-of '((VAR "n")) '()) (rulpath-of "" '() '()) '()) '(FAIL))
(test-equal (run-eval-rul (rulmatch-of '() '((IF (BOOL #f)))) (rulpath-of "" '() '()) '()) '(FAIL))
(test-equal (run-eval-rul (rulmatch-of '() '()) (rulpath-of "" '((VAR "none")) '()) '()) '(FAIL))

(test-equal (outputs (eval-ruls ,C-local ,(rulmatch-of '() '()) () () any)) '(FAIL))
(test-equal (outputs (eval-rulgroup ,C-local ,double-group ((NAT 1)) any)) '((OK ((NAT 2)))))
(test-equal (outputs (eval-rulgroup ,C-local ,double-group () any)) '(FAIL))
(test-equal (outputs (eval-rulgroups ,C-local () () any)) '(FAIL))
(test-equal (outputs (call-defined-rel ,C-rel ,classify-def ((NAT 12)) any)) '((OK ((TEXT "big")))))
(test-equal (outputs (call-rel-dispatch ,C-rel ,sum-def ((INT 1)) any)) '((OK ((INT 1)))))

;; An else group becomes a rule group with one rule path.
(test-equal (term (elsgroup-as-rulgroup ("big" (((VAR "n")) ()) ("" ((TEXT "big")) ()))))
            '("big" (((VAR "n")) ()) (("" ((TEXT "big")) ()))))

;;
;; Nesting: each shared premise is evaluated once.
;;

;; Each nesting succeeds, so it evaluates every level.
(define (check-not-exponential name J vals-of)
  (define (calls depth)
    (count-calls J (λ () (call "Sum" (vals-of depth)))))
  (define-values (c4 c10) (values (calls 4) (calls 10)))
  (check-true (< c10 (* 10 c4))
              (format "~a: ~a calls at depth 4, ~a at depth 10" name c4 c10))
  (check-equal? (caar (call "Sum" (vals-of 4))) 'OK (format "~a at depth 4" name)))

;; Sum tries its group "zero", which fails until the bottom, then "succ".
(check-not-exponential 'rulgroups 'eval-rulgroup (λ (depth) `((INT ,depth))))
(check-not-exponential 'ruls 'eval-rul (λ (depth) `((INT ,depth))))

;; Every rule appears in some derivation above, except the extern one.
(check-rules-used eval-rul)
(check-rules-used eval-rul/succ)
(check-rules-used eval-rul/succ-prems)
(check-rules-used eval-rul/succ-exps)
(check-rules-used eval-ruls)
(check-rules-used eval-ruls/cons)
(check-rules-used eval-rulgroup)
(check-rules-used eval-rulgroup/succ)
(check-rules-used eval-rulgroups)
(check-rules-used eval-rulgroups/cons)
(check-rules-used call-defined-rel)
(check-rules-used call-rel-dispatch #:except ("ext"))
(check-rules-used call-rel)

(test-results)
