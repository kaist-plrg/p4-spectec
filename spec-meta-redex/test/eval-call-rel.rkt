#lang racket/base

(require racket/list
         racket/match
         rackunit
         "../common/0.0-prelude.rkt"
         "../common/0.1-stdlib.rkt"
         "../al/5.7-eval-call-rel.rkt"
         "machine.rkt")

(define coverage
  (remove* '("call-extern-rel")
           (start-coverage ->redex/eval-call-rel ->ctx/eval-call-rel)))

;; Raises when evaluated, where a premise must not be evaluated
(define DIV0 (term (BIN DIV (NAT 1) (NAT 0))))

(define script
  (boot-text #<<EOF
var n : nat

syntax tm = NUM nat | NEG tm | ADD tm tm

;; Size counts the nodes of a term, except for NEG nodes. The rulegroup
;; Size/num has two rule paths, and Size/else becomes the else group.
relation Size: |- tm ':' nat
  hint(input %0)

rulegroup Size/num {
  rule Size/small:
    |- (NUM n) ':' 1
    -- if $(n < 10)
  rule Size/big:
    |- (NUM n) ':' 2
    -- if $(n >= 10)
}

rule Size/add:
  |- (ADD tm_l tm_r) ':' $(n_l + n_r + 1)
  -- Size: |- tm_l ':' n_l
  -- Size: |- tm_r ':' n_r

rule Size/else:
  |- tm ':' 0
  -- otherwise

relation Halves: |- nat ':' nat '+' nat
  hint(input %0)

rule Halves:
  |- n ':' n_q '+' n_r
  -- if n_q = $(n / 2)
  -- if n_r = $(n \ 2)

extern relation Ext: |- nat ':' nat
  hint(input %0)
EOF
             ))

;; G has the script's relations, and these, which are written by hand:
;; - Leak has a path that binds y and then fails, and one that reads y;
;; - First has a path that succeeds, and one that fails if evaluated;
;; - Groups has a group whose match fails, one that succeeds, and one that
;;   fails if evaluated;
;; - Outs has two outputs, of which the first fails;
;; - Pattern has a group that matches only an empty tuple, and an else group;
;; - Env has no inputs, and reads x.
(define G-rels
  (match (global-of script)
    [(list 'TYP tdenv 'REL renv 'FUNC fenv 'VAL venv)
     (define renv_1
       (for/fold ([renv renv])
                 ([id+reldef
                   (in-list
                    (term
                     (("Leak" (DEF (("g" (((VAR "a")) ())
                                         (("p1" ((VAR "a")) ((LET (VAR "y") (NAT 1)) (IF (BOOL #f))))
                                          ("p2" ((VAR "y")) ()))))
                                   ()))
                      ("First" (DEF (("g" (((VAR "a")) ()) (("p1" ((VAR "a")) ()) ("p2" (,DIV0) ())))) ()))
                      ("Groups" (DEF (("g1" (((VAR "a")) ((IF (BOOL #f)))) (("p" ((NAT 1)) ())))
                                      ("g2" (((VAR "a")) ()) (("p" ((NAT 2)) ())))
                                      ("g3" (((VAR "a")) ((IF ,DIV0))) (("p" ((NAT 3)) ()))))
                                     ()))
                      ("Outs" (DEF (("g" (((VAR "a")) ()) (("p" ((VAR "z") ,DIV0) ())))) ()))
                      ("Pattern" (DEF (("g" (((TUP ())) ()) (("p" ((NAT 1)) ()))))
                                      (("e" (((VAR "a")) ()) ("e" ((NAT 0)) ())))))
                      ("Env" (DEF (("g" (() ()) (("p" ((VAR "x")) ())))) ()))))
                    )])
         (match-define (list id reldef) id+reldef)
         (term (add-map ,renv ,id ,reldef))))
     `(TYP ,tdenv REL ,renv_1 FUNC ,fenv VAL ,venv)]))

(define-term L-caller {TYP () REL () FUNC () VAL ((("x" ()) (NAT 1)))})

(define (call-rel id vals)
  (eval-in G-rels (term L-caller) `(call-rel ,id ,vals)))

(define (trace-rel id vals)
  (trace-in G-rels (term L-caller) `(call-rel ,id ,vals)))

(define (num n) (term (INJ ((("NUM") ()) ((NAT ,n))))))
(define (neg tm) (term (INJ ((("NEG") ()) (,tm)))))
(define (add tm_l tm_r) (term (INJ ((("ADD") () ()) (,tm_l ,tm_r)))))

;;
;; Rule paths and groups
;;

;; Eval_rul/succ assigns the inputs, evaluates the group's premises and then
;; the path's, and then the outputs.
(test-equal (call-rel "Size" (list (num 3))) (term (OK ((NAT 1)))))
(test-equal (call-rel "Halves" (term ((NAT 7)))) (term (OK ((NAT 3) (NAT 1)))))
(test-equal (take (trace-rel "Halves" (term ((NAT 7)))) 8)
            '("call-rel" "call-rel-dispatch/defined" "call-defined-rel" "eval-rulgroups/cons"
              "eval-rulgroup" "eval-ruls/cons" "eval-rul/succ" "assign-exps/cons"))
;; Eval_rul/fail: the inputs do not match, a premise fails, or an output does,
;; and no later output is evaluated.
(test-equal (call-rel "Pattern" (term ((TUP ())))) (term (OK ((NAT 1)))))
(test-equal (call-rel "Pattern" (term ((NAT 1))))  (term (OK ((NAT 0)))))
(test-equal (call-rel "Size" (list (num 30))) (term (OK ((NAT 2)))))
(test-equal (call-rel "Outs" (term ((NAT 1)))) 'FAIL)
(test-equal (take-right (trace-rel "Outs" (term ((NAT 1)))) 8)
            '("eval-exp/variable/fail" "frame/fail" "in/fail" "eval-ruls/cons-fail" "eval-ruls/nil"
              "eval-rulgroups/cons-fail" "eval-rulgroups/nil" "in/fail"))

;; Eval_ruls tries the paths in order, each in a copy of the relation's layer.
(test-equal (filter (λ (rule) (regexp-match? #rx"^eval-rul" rule))
                    (trace-rel "Size" (list (num 30))))
            '("eval-rulgroups/cons" "eval-rulgroup"
              "eval-ruls/cons" "eval-rul/succ" "eval-ruls/cons-fail"
              "eval-ruls/cons" "eval-rul/succ" "eval-rul/succ/output" "eval-ruls/cons-succ"
              "eval-rulgroups/cons-succ"))
;; A failed path leaves no bindings for the next one.
(test-equal (call-rel "Leak" (term ((NAT 5)))) 'FAIL)
;; The first path that succeeds stops the search.
(test-equal (call-rel "First" (term ((NAT 5)))) (term (OK ((NAT 5)))))

;; Eval_rulgroups tries the groups in order, and the else group last.
(test-equal (call-rel "Groups" (term ((NAT 5)))) (term (OK ((NAT 2)))))
(test-equal (call-rel "Size" (list (add (num 3) (num 30)))) (term (OK ((NAT 4)))))
(test-equal (call-rel "Size" (list (neg (num 3)))) (term (OK ((NAT 0)))))
(test-equal (call-rel "Size" (list (add (neg (num 3)) (add (num 1) (num 2)))))
            (term (OK ((NAT 4)))))
;; Every path of a group checks the group's premises.
(test-equal (filter (λ (rule) (regexp-match? #rx"^eval-rul" rule))
                    (trace-rel "Size" (list (neg (num 3)))))
            '("eval-rulgroups/cons" "eval-rulgroup"
              "eval-ruls/cons" "eval-rul/succ" "eval-ruls/cons-fail"
              "eval-ruls/cons" "eval-rul/succ" "eval-ruls/cons-fail" "eval-ruls/nil"
              "eval-rulgroups/cons-fail" "eval-rulgroups/cons" "eval-rulgroup"
              "eval-ruls/cons" "eval-rul/succ" "eval-ruls/cons-fail" "eval-ruls/nil"
              "eval-rulgroups/cons-fail" "eval-rulgroups/cons" "eval-rulgroup"
              "eval-ruls/cons" "eval-rul/succ" "eval-rul/succ/output" "eval-ruls/cons-succ"
              "eval-rulgroups/cons-succ"))

;; $elsgroup_as_rulgroup
(test-equal (term (elsgroup-as-rulgroup ("e" (((VAR "a")) ()) ("e" ((NAT 0)) ()))))
            (term ("e" (((VAR "a")) ()) (("e" ((NAT 0)) ())))))

;;
;; Calls
;;

;; Call_defined_rel runs the relation in an empty layer, and leaves the
;; caller's unchanged.
(test-equal (call-rel "Env" '()) 'FAIL)
(test-equal (run-in G-rels (term L-caller) (term (call-rel "Halves" ((NAT 4)))))
            (term ((OK ((NAT 2) (NAT 0))) L-caller)))
;; Call_rel: no such relation
(test-equal (trace-rel "Nope" '()) '("call-rel/fail"))
;; An extern relation reaches the host, which is not reachable yet.
(check-exn #rx"host-call-extern-rel: the host is not reachable yet"
           (λ () (call-rel "Ext" (term ((NAT 1))))))

(check-coverage coverage)
