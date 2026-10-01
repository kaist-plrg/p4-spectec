#lang racket/base

(require racket/list
         racket/match
         rackunit
         "../common/0.0-prelude.rkt"
         "../common/0.3-extern-ffi.rkt"
         "../al/0-boot.rkt"
         "../al/3-context.rkt"
         "../al/5.6-eval-call-func.rkt"
         "machine.rkt")

(define coverage (start-coverage ->redex/eval-call-func ->ctx/eval-call-func))

;; Raises when evaluated, where a premise must not be evaluated
(define DIV0 (term (BIN DIV (NAT 1) (NAT 0))))

(define script-path
  (text-file #<<EOF
var i : int
var n : nat

syntax warm = RED | ORANGE
syntax cool = BLUE
syntax green = GREEN nat
syntax color = warm | cool | green

;; The clauses are tried in order, and the otherwise clause last.
dec $sign(int) : int
def $sign(i) = $(-1)
  -- if $(i < 0)
def $sign(i) = 0
  -- if i = 0
def $sign(i) = 1
  -- otherwise

;; No clause applies to a negative number.
dec $half(int) : int
def $half(i) = $(i / 2)
  -- if $(i >= 0)

tbl dec $is_warm(color) : bool
tbl def $is_warm =
  | warm => true
  | GREEN n => $(n < 3)
  | _ => false

dec $inc(int) : int
def $inc(i) = $(i + 1)

;; Function arguments, passed on from one call to the next
dec $twice(def $k(int) : int, int) : int
def $twice(def $k, i) = $k($k(i))

dec $thrice(def $k(int) : int, int) : int
def $thrice(def $k, i) = $k($twice(def $k, i))

dec $sum(int) : int
def $sum(i) = 0
  -- if $(i <= 0)
def $sum(i) = $(i + $sum($(i - 1)))
  -- otherwise

extern dec $ext(nat) : nat
builtin dec $rev_<X>(X*) : X*
EOF
             ))

(define script (boot-script script-path))

;; The host builds its runner from the script.
(host-spec script-path)

;; G has the script's functions, and these, which are written by hand:
;; - fits<X>(v) is whether v has type X, and outer<Y>(v) calls fits<Y>(v);
;; - leak(v) has a clause that binds y and then fails, and one that reads y;
;; - poly<X>(v) fails if evaluated;
;; - small has a row for nats below 3 only, and env one that reads x;
;; - apply1 has a row whose function argument it calls;
;; - ext-inc is the host's $inc, and missing a builtin the host lacks.
(define G-funcs
  (for/fold ([G (global-of script)])
            ([id+funcdef
              (in-list
               (term
                (("fits" (DEF ("X") ((((EXP (VAR "v"))) (SUB (VAR "v") (VAR "X" ())) ())) ()))
                 ("outer" (DEF ("Y") ((((EXP (VAR "v"))) (CALL "fits" ((VAR "Y" ())) ((EXP (VAR "v")))) ()))
                               ()))
                 ("leak" (DEF ()
                              ((((EXP (VAR "a"))) (VAR "a") ((LET (VAR "y") (NAT 1)) (IF (BOOL #f))))
                               (((EXP (VAR "b"))) (VAR "y") ()))
                              ()))
                 ("poly" (DEF ("X") ((((EXP (VAR "v"))) ,DIV0 ())) ()))
                 ("caller-x" (DEF () ((() (VAR "x") ())) ()))
                 ("small" (TABLE ((EXP NAT))
                                 ((((EXP (VAR "n"))) (BOOL #t) ((IF (CMP LT (VAR "n") (NAT 3))))))))
                 ("env" (TABLE ((EXP NAT)) ((((EXP (VAR "n"))) (VAR "x") ()))))
                 ("apply1" (TABLE ((FUN "k" () ((EXP NAT)) BOOL))
                                  ((((FUN "k")) (CALL "k" () ((EXP (NAT 1)))) ()))))
                 ("ext-inc" (EXT "inc"))
                 ("missing" (BUILTIN "missing" () ())))))])
    (match-define (list id funcdef) id+funcdef)
    (term (add-func ,G ,id ,funcdef))))

;; The caller binds x, and a function of its own, loc, which shadows none of
;; G's. L-shadow binds inc to another function.
(define-term L-caller
  {TYP () REL ()
   FUNC (("loc" (DEF () ((((EXP (VAR "n"))) (CMP EQ (VAR "n") (NAT 1)) ())) ())))
   VAL ((("x" ()) (NAT 1)))})
(define-term L-shadow
  {TYP () REL () FUNC (("inc" (DEF () ((((EXP (VAR "i"))) (INT 100) ())) ()))) VAL ()})
(define-term L-empty (empty-layer))

(define (eval-call e)
  (eval-in G-funcs (term L-caller) e))

(define (trace-call e)
  (trace-in G-funcs (term L-caller) e))

;; Without the cross-check, which would call the host twice
(define (eval-host e)
  (eval-in G-funcs (term L-caller) e #:cross-check? #f))

(define (int i) (term (EXP (INT ,i))))

(define (layer-funcs L) (list-ref L 5))
(define (layer-vals L) (list-ref L 7))

;;
;; Clauses
;;

;; Eval_clause/succ assigns the arguments, then evaluates the premises, then
;; the output, all in the layer it runs in.
(define-term clause-pair
  (((EXP (VAR "a")) (FUN "k")) (TUP ((VAR "a") (VAR "y"))) ((LET (VAR "y") (CALL "k" () ((EXP (VAR "a"))))))))

(match (run-in G-funcs (term L-empty) (term (eval-clause L-caller clause-pair ((NAT 1) (FUNC "loc")))))
  [(list res L_1)
   (test-equal res (term (OK (TUP ((NAT 1) (BOOL #t))))))
   ;; The function argument is found in the caller's layer.
   (test-equal (layer-funcs L_1) (term (("k" (DEF () ((((EXP (VAR "n"))) (CMP EQ (VAR "n") (NAT 1)) ())) ())))))
   (test-equal (map car (layer-vals L_1)) '(("a" ()) ("y" ())))])
(test-equal (take (trace-in G-funcs (term L-empty)
                            (term (eval-clause L-caller (((EXP (VAR "a"))) (VAR "a") ((IF (BOOL #t))))
                                               ((NAT 1)))))
                  4)
            '("eval-clause/succ" "assign-args/cons" "assign-arg/exp" "assign-exp/variable"))
;; Eval_clause/fail: the arguments do not match, and the premises are not
;; evaluated; or a premise fails, and the output is not evaluated.
(test-equal (trace-in G-funcs (term L-empty)
                      (term (eval-clause L-caller (((EXP (TUP ()))) (NAT 0) ((IF ,DIV0))) ((NAT 1)))))
            '("eval-clause/succ" "assign-args/cons" "assign-arg/exp" "assign-exp/fail"
              "frame/fail" "frame/fail"))
(test-equal (eval-in G-funcs (term L-empty)
                     (term (eval-clause L-caller (((EXP (VAR "a"))) ,DIV0 ((IF (BOOL #f)))) ((NAT 1)))))
            'FAIL)
(test-equal (eval-in G-funcs (term L-empty)
                     (term (eval-clause L-caller (((EXP (VAR "a"))) (VAR "b") ()) ((NAT 1)))))
            'FAIL)

;; Eval_clauses tries the clauses in order, each in a copy of the callee's
;; layer.
(test-equal (eval-call (term (CALL "sign" () (,(int -5))))) (term (OK (INT -1))))
(test-equal (eval-call (term (CALL "sign" () (,(int 0))))) (term (OK (INT 0))))
(test-equal (eval-call (term (CALL "sign" () (,(int 5))))) (term (OK (INT 1))))
(test-equal (filter (λ (rule) (regexp-match? #rx"^eval-clauses/" rule))
                    (trace-call (term (CALL "sign" () (,(int 5))))))
            '("eval-clauses/cons" "eval-clauses/cons-fail" "eval-clauses/cons" "eval-clauses/cons-fail"
              "eval-clauses/cons" "eval-clauses/cons-succ"))
;; A failed clause leaves no bindings for the next one.
(test-equal (eval-call (term (CALL "leak" () (,(int 5))))) 'FAIL)
;; Nor does a clause that succeeds, in the callee's layer.
(test-equal (run-in G-funcs (term L-empty)
                    (term (eval-clauses L-caller ((((EXP (VAR "a"))) (VAR "a") ((LET (VAR "z") (NAT 2)))))
                                        ((NAT 1)))))
            (term ((OK (NAT 1)) L-empty)))
;; The first clause that succeeds stops the search.
(test-equal (eval-in G-funcs (term L-empty)
                     (term (eval-clauses L-caller ((((EXP (VAR "a"))) (VAR "a") ())
                                                   (((EXP (VAR "a"))) ,DIV0 ()))
                                         ((NAT 1)))))
            (term (OK (NAT 1))))
;; Eval_clauses/nil: no clause applies.
(test-equal (eval-call (term (CALL "half" () (,(int 4))))) (term (OK (INT 2))))
(test-equal (eval-call (term (CALL "half" () (,(int -4))))) 'FAIL)
(test-equal (take-right (trace-call (term (CALL "half" () (,(int -4))))) 4)
            '("in/fail" "eval-clauses/cons-fail" "eval-clauses/nil" "in/fail"))

;;
;; Table functions
;;

(define (color c) (term (EXP (INJ (((,c)) ())))))
(define (green n) (term (EXP (INJ ((("GREEN") ()) ((NAT ,n)))))))

;; Eval_tblrows tries the rows in order.
(test-equal (eval-call (term (CALL "is_warm" () (,(color "RED"))))) (term (OK (BOOL #t))))
(test-equal (eval-call (term (CALL "is_warm" () (,(green 2))))) (term (OK (BOOL #t))))
(test-equal (eval-call (term (CALL "is_warm" () (,(green 7))))) (term (OK (BOOL #f))))
(test-equal (eval-call (term (CALL "is_warm" () (,(color "BLUE"))))) (term (OK (BOOL #f))))
(test-equal (filter (λ (rule) (regexp-match? #rx"^eval-tblrows?/|^call-table-func" rule))
                    (trace-call (term (CALL "is_warm" () (,(green 7))))))
            '("call-table-func" "eval-tblrows/cons" "eval-tblrow/succ" "eval-tblrows/cons-fail"
              "eval-tblrows/cons" "eval-tblrow/succ" "eval-tblrow/succ/output" "eval-tblrows/cons-succ"))
;; Eval_tblrows/nil: no row applies.
(test-equal (eval-call (term (CALL "small" () ((EXP (NAT 1)))))) (term (OK (BOOL #t))))
(test-equal (eval-call (term (CALL "small" () ((EXP (NAT 5)))))) 'FAIL)
(test-equal (take-right (trace-call (term (CALL "small" () ((EXP (NAT 5)))))) 3)
            '("in/fail" "eval-tblrows/cons-fail" "eval-tblrows/nil"))
;; A row runs in an empty layer, and its arguments are assigned with the
;; caller's layer.
(test-equal (eval-call (term (CALL "env" () ((EXP (NAT 1)))))) 'FAIL)
(test-equal (eval-call (term (CALL "apply1" () ((FUN "loc"))))) (term (OK (BOOL #t))))
;; The first row that succeeds stops the search.
(test-equal (eval-in G-funcs (term L-empty)
                     (term (eval-tblrows ((((EXP (VAR "a"))) (VAR "a") ())
                                          (((EXP (VAR "a"))) ,DIV0 ()))
                                         ((NAT 1)))))
            (term (OK (NAT 1))))

;;
;; Defined functions
;;

;; Call_defined_func binds the type parameters in the callee's layer.
(test-equal (eval-call (term (CALL "fits" (NAT) ((EXP (NAT 1)))))) (term (OK (BOOL #t))))
(test-equal (eval-call (term (CALL "fits" (NAT) (,(int -1))))) (term (OK (BOOL #f))))
;; A polymorphic call from a context where Y is NAT passes NAT on, where
;; spec-meta passes (VAR Y).
(test-equal (eval-call (term (CALL "outer" (NAT) ((EXP (NAT 1)))))) (term (OK (BOOL #t))))
(test-equal (eval-call (term (CALL "outer" (NAT) (,(int -1))))) (term (OK (BOOL #f))))
(test-equal (eval-call (term (CALL "outer" ((TUP (NAT BOOL))) ((EXP (TUP ((NAT 1) (BOOL #t))))))))
            (term (OK (BOOL #t))))
;; The callee's layer has none of the caller's values.
(test-equal (eval-call (term (CALL "caller-x" () ()))) 'FAIL)
;; otherwise: as many types as type parameters, and nothing is evaluated
(test-equal (eval-call (term (CALL "poly" () ((EXP (NAT 1)))))) 'FAIL)
(test-equal (take-right (trace-call (term (CALL "poly" (NAT BOOL) ((EXP (NAT 1)))))) 2)
            '("call-func-dispatch/defined" "call-defined-func/fail"))

;;
;; Dispatch
;;

;; A table function takes no types.
(test-equal (eval-call (term (CALL "small" (NAT) ((EXP (NAT 1)))))) 'FAIL)
(test-equal (take-right (trace-call (term (CALL "small" (NAT) ((EXP (NAT 1)))))) 2)
            '("call-func" "call-func-dispatch/table/fail"))
;; Extern and builtin functions go to the host. An extern function the host
;; lacks fails, and a builtin it lacks raises.
(test-equal (eval-host (term (CALL "ext-inc" () (,(int 1))))) (term (OK (INT 2))))
(test-equal (eval-host (term (CALL "ext" () ((EXP (NAT 1)))))) 'FAIL)
(test-equal (eval-host (term (CALL "rev_" (NAT) ((EXP (LIST ((NAT 1) (NAT 2))))))))
            (term (OK (LIST ((NAT 2) (NAT 1))))))
(check-exn #rx"host: Builtin error: implementation for builtin missing is missing"
           (λ () (eval-host (term (CALL "missing" () ())))))

;;
;; Calls
;;

;; Call_func finds the function in the caller's layer first.
(test-equal (eval-call (term (CALL "inc" () (,(int 1))))) (term (OK (INT 2))))
(test-equal (eval-in G-funcs (term L-shadow) (term (CALL "inc" () (,(int 1))))) (term (OK (INT 100))))
(test-equal (eval-call (term (CALL "loc" () ((EXP (NAT 1)))))) (term (OK (BOOL #t))))
;; otherwise: no such function
(test-equal (eval-call (term (CALL "nope" () ()))) 'FAIL)
(test-equal (eval-in G-funcs (term L-empty) (term (CALL "loc" () ((EXP (NAT 1)))))) 'FAIL)

;; Function arguments, found in each caller's layer
(test-equal (eval-call (term (CALL "twice" () ((FUN "inc") ,(int 3))))) (term (OK (INT 5))))
(test-equal (eval-call (term (CALL "thrice" () ((FUN "inc") ,(int 3))))) (term (OK (INT 6))))
(test-equal (eval-in G-funcs (term L-shadow) (term (CALL "thrice" () ((FUN "inc") ,(int 3)))))
            (term (OK (INT 100))))
(test-equal (eval-call (term (CALL "twice" () ((FUN "nope") ,(int 3))))) 'FAIL)

;; Deep recursion
(test-equal (eval-call (term (CALL "sum" () (,(int 3))))) (term (OK (INT 6))))
(test-equal (eval-in G-funcs (term L-caller) (term (CALL "sum" () (,(int 30)))) #:cross-check? #f)
            (term (OK (INT 465))))

(check-coverage coverage)
