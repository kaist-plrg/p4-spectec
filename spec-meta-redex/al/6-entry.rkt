#lang racket/base
;; spec-meta/al/6-entry.watsup: load a script, then run $main() through the
;; driver. entry-p4 runs Program_ok on a P4 program instead, as K's afterLoad
;; does, under a context that load-script gives, so one load can serve many
;; programs.
;;
;; As a program, it runs $main() of a SpecTec script through Entry, and
;; prints its value in k-run.sh's JSON format, or `fail`. With --p4, it runs
;; Program_ok of the spec on a P4 program, and prints `passed` or `fail`, as
;; k-run-p4.sh does. -i adds a P4 include directory, and --p4 needs at least
;; one. Entry's debug messages go to stderr.
;;
;;   racket spec-meta-redex/al/6-entry.rkt FILE.watsup
;;   racket spec-meta-redex/al/6-entry.rkt --p4 PROGRAM.p4 -i DIR [-i DIR]... spec

(require json
         racket/cmdline
         racket/match
         "../common/0.0-prelude.rkt"
         "../common/0.1-stdlib.rkt"
         "../common/0.2-extern-json.rkt"
         "../common/0.3-extern-ffi.rkt"
         "0-boot.rkt"
         "3-context.rkt"
         "5-eval.rkt")
(provide entry
         entry-p4
         load-script
         result->output)

;; rule Entry:
;;   |- script : val
;;   -- debug "entry-al"
;;   -- if C = $load($empty_ctx, script)
;;   -- debug "load complete"
;;   -- debug "into call"
;;   -- Eval_exp: C |- CALL "main" eps eps : OK val
;;   -- debug val
;; Gives (OK val), or FAIL where Entry has no derivation.
(define (entry script)
  (match (run-loaded (load-script script) (term (CALL "main" () ())))
    [(and res (list 'OK val))
     (debug val)
     res]
    ['FAIL 'FAIL]))

;; The result of Program_ok on the P4 program val_p4, under the context C that
;; the spec loads into: (OK (val ...)), or FAIL
(define (entry-p4 C val_p4)
  (run-loaded C (term (call-rel "Program_ok" (,val_p4)))))

;; The context that script loads into, (GLOBAL G LOCAL L)
(define (load-script script)
  (debug (term (TEXT "entry-al")))
  (begin0 (term (load (empty-ctx) ,script))
    (debug (term (TEXT "load complete")))))

;; The result of e under the loaded context C
(define (run-loaded C e)
  (match-define (list 'GLOBAL G 'LOCAL L) C)
  (debug (term (TEXT "into call")))
  (match (run (list G (list 'IN L e)))
    [(list _ (list 'IN _ res)) res]))

;; The line printed for Entry's result
(define (result->output res)
  (match res
    [(list 'OK val) (jsexpr->string (val->jsexpr val))]
    ['FAIL "fail"]))

;; The line printed for Program_ok's result
(define (result-p4->output res)
  (match res
    [(list 'OK _) "passed"]
    ['FAIL "fail"]))

(module+ main
  (define p4 #f)
  (define includes '())
  (define path
    (command-line
     #:program "6-entry.rkt"
     #:once-each
     [("--p4") program "Run Program_ok of the spec FILE on a P4 program"
               (set! p4 program)]
     #:multi
     [("-i") dir "Add a P4 include directory (required with --p4)"
             (set! includes (append includes (list dir)))]
     #:args (file) file))
  (when (and p4 (null? includes))
    (raise-user-error '6-entry.rkt "-i is required with --p4"))
  (parameterize ([host-spec path])
    (displayln
     (if p4
         (let ([val_p4 (boot-p4 p4 #:includes includes)])
           (result-p4->output (entry-p4 (load-script (boot-script path)) val_p4)))
         (result->output (entry (boot-script path)))))))
