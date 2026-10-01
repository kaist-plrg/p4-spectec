#lang racket/base

(require racket/match
         racket/runtime-path
         racket/string
         rackunit
         "../common/0.0-prelude.rkt"
         "../common/0.2-extern-json.rkt"
         "../al/0-boot.rkt"
         "../al/5-eval.rkt"
         "../al/6-entry.rkt"
         "machine.rkt")

(define-runtime-path examples "../../examples")

(define (example name)
  (boot-script (build-path examples (string-append name ".watsup"))))

;; Gives the result of running thunk, every configuration checked against
;; conf, and what it wrote to stderr
(define (with-stderr thunk #:cross-check? cross-check-on?)
  (define err (open-output-string))
  (define result
    (parameterize ([cross-check? cross-check-on?]
                   [check-conf? #t]
                   [current-error-port err])
      (thunk)))
  (list result (get-output-string err)))

;; Entry's result on script, the line 6-entry.rkt prints for it, and what
;; Entry wrote to stderr
(define (entry/output script #:cross-check? [cross-check-on? #t])
  (match (with-stderr (λ () (entry script)) #:cross-check? cross-check-on?)
    [(list res err) (list res (result->output res) err)]))

;; What Entry writes to stderr, with lines from $main and Entry's last debug
(define (entry-stderr . lines)
  (string-append* (for/list ([line (in-list (list* "(TEXT \"entry-al\")"
                                                   "(TEXT \"load complete\")"
                                                   "(TEXT \"into call\")"
                                                   lines))])
                    (string-append line "\n"))))

;;
;; The examples without builtins
;;

;; The line 6-entry.rkt prints is the last line k-run.sh prints.
(define (test-example name val output #:cross-check? [cross-check-on? #t])
  (match-define (list res line _) (entry/output (example name) #:cross-check? cross-check-on?))
  (test-equal res (term (OK ,val)))
  (test-equal line output))

;; Each takes under 5 s with the cross-check.
(test-example "add" (term (INT 119)) "[\"intN\",\"119\"]")
(test-example "iter-nontrivial" (term (INT -42)) "[\"intN\",\"-42\"]")
(test-example "relation-typing" (term (INT 110)) "[\"intN\",\"110\"]")
(test-example "variant-tree" (term (INT 6)) "[\"intN\",\"6\"]")
;; 8 s with the cross-check, and 1 s without
(test-example "iter-sequence" (term (INT 1085)) "[\"intN\",\"1085\"]" #:cross-check? #f)
;; fibo and mutual-recursion take 7 and 5 s, and 62 and 44 s with the
;; cross-check, so they are left out.

;; Entry's debug premises, and $main's, write to stderr.
(test-equal (entry/output (example "add") #:cross-check? #f)
            (list (term (OK (INT 119)))
                  "[\"intN\",\"119\"]"
                  (entry-stderr "(TEXT \"Add\")" "(INT 119)")))

;;
;; Failing scripts
;;

(define failing
  (boot-text #<<EOF
var n : nat

dec $main() : nat
def $main() = n
  -- if n = 1
  -- if $(n > 2)
EOF
             ))

;; Entry has no derivation, and so its last debug writes nothing.
(test-equal (entry/output failing) (list 'FAIL "fail" (entry-stderr)))
(test-equal (entry/output (boot-text "dec $other() : nat\ndef $other() = 1\n"))
            (list 'FAIL "fail" (entry-stderr)))

;;
;; The JSON encoding of values
;;

;; As k-run.sh prints it
(test-equal
 (entry/output (boot-text #<<EOF
syntax point = { X nat, Y int }

syntax shape =
  | CIRCLE nat
  | RECT nat nat
  | DOT

dec $main() : (bool, nat, int, text, point, shape, shape, nat?, nat?, nat*, (nat, text)*)
def $main() = (true, 3, -2, "q r", {X 1, Y (-1)}, RECT 2 3, DOT, eps, 4, [5, 6], [])
EOF
                          ))
 (list (term (OK (TUP ((BOOL #t) (NAT 3) (INT -2) (TEXT "q r")
                       (STR (("X" (NAT 1)) ("Y" (INT -1))))
                       (INJ ((("RECT") () ()) ((NAT 2) (NAT 3))))
                       (INJ ((("DOT")) ()))
                       (OPT ()) (OPT ((NAT 4)))
                       (LIST ((NAT 5) (NAT 6))) (LIST ())))))
       (string-append "[\"tupV\",[[\"boolV\",true],[\"natN\",\"3\"],[\"intN\",\"-2\"],"
                      "[\"textV\",\"q r\"],"
                      "[\"strV\",[[\"X\",[\"natN\",\"1\"]],[\"Y\",[\"intN\",\"-1\"]]]],"
                      "[\"injV\",[[\"RECT\"],[],[]],[[\"natN\",\"2\"],[\"natN\",\"3\"]]],"
                      "[\"injV\",[[\"DOT\"]],[]],"
                      "[\"optV\",null],[\"optV\",[\"natN\",\"4\"]],"
                      "[\"listV\",[[\"natN\",\"5\"],[\"natN\",\"6\"]]],[\"listV\",[]]]]")
       (entry-stderr "(TUP ((BOOL #t) (NAT 3) (INT -2) (TEXT \"q r\") (STR ((\"X\" (NAT 1)) (\"Y\" (INT -1)))) (INJ (((\"RECT\") () ()) ((NAT 2) (NAT 3)))) (INJ (((\"DOT\")) ())) (OPT ()) (OPT ((NAT 4))) (LIST ((NAT 5) (NAT 6))) (LIST ())))")))

;; A text is written as UTF-8, and only control characters are escaped.
(test-equal (result->output (term (OK (TEXT "é\"\n")))) "[\"textV\",\"é\\\"\\n\"]")
(test-equal (val->jsexpr (term (FUNC "f"))) '("funcV" "f"))
(test-equal (result->output (term (OK (EXT #hasheq((a . (1 null)))))))
            "[\"extV\",{\"a\":[1,null]}]")

;;
;; Program_ok on a P4 program
;;

(define program-ok
  (boot-text #<<EOF
var n : nat

syntax program = PROGRAM nat

relation Program_ok:
  |- program : nat
  hint(input %0)

rule Program_ok:
  |- PROGRAM n : n
  -- if $(n > 0)
EOF
             ))

(define (entry-p4/stderr script val_p4)
  (with-stderr (λ () (entry-p4 script val_p4)) #:cross-check? #t))

(test-equal (entry-p4/stderr program-ok (term (INJ ((("PROGRAM") ()) ((NAT 3))))))
            (list (term (OK ((NAT 3)))) (entry-stderr)))
(test-equal (entry-p4/stderr program-ok (term (INJ ((("PROGRAM") ()) ((NAT 0))))))
            (list 'FAIL (entry-stderr)))
;; No relation Program_ok
(test-equal (entry-p4/stderr failing (term (INJ ((("PROGRAM") ()) ((NAT 3))))))
            (list 'FAIL (entry-stderr)))
