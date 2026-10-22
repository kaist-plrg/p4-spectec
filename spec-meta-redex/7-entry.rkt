#lang racket/base
;; spec-meta/6-entry.watsup: load a script, then run $main() through the
;; driver. entry-p4 runs Program_ok on a P4 program instead, as K's afterLoad
;; does, under a context that load-script gives, so one load can serve many
;; programs. Both need the host started on the spec first, with host-init!.
;;
;; As a program, it runs $main() of a SpecTec script through Entry, and
;; prints its value in k-run.sh's JSON format, or `fail`. With --p4, it runs
;; Program_ok of the spec on a P4 program, and prints `passed` or `fail`, as
;; k-run-p4.sh does. -i adds a P4 include directory, and --p4 needs at least
;; one. Entry's debug messages go to stderr.
;;
;;   racket spec-meta-redex/7-entry.rkt FILE.watsup
;;   racket spec-meta-redex/7-entry.rkt --p4 PROGRAM.p4 -i DIR [-i DIR]... spec

(require json
         racket/cmdline
         racket/match
         redex/reduction-semantics
         "0.0-stdlib.rkt"
         "0.1-extern-json.rkt"
         "0.2-extern-ffi.rkt"
         "0.3-boot.rkt"
         "3-context.rkt"
         "6-driver.rkt")
(provide entry
         entry-p4
         load-script)

;; rule Entry:
;;   |- script : val
;;   -- debug "entry-al"
;;   -- if C = $load($empty_ctx, script)
;;   -- debug "load complete"
;;   -- debug "into call"
;;   -- Eval_exp: C |- CALL "main" eps eps : OK val
;;   -- debug val
;; Gives val, or FAIL where Entry has no derivation.
(define (entry script)
  (match (run-loaded (load-script script) (term (CALL "main" () ())))
    ['FAIL 'FAIL]
    [val
     (debug val)
     val]))

;; The result of Program_ok on the P4 program val_p4, under loaded, the
;; context that load-script gives for the spec: (OK (val ...)), or FAIL
(define (entry-p4 loaded val_p4)
  (run-loaded loaded (term (call-rel "Program_ok" (,val_p4)))))

;; The context that script loads into, as (G L): the handle of its global
;; layer, now registered, and its local layer
(define (load-script script)
  (debug (term (TEXT "entry-al")))
  (match-define (list 'GLOBAL layer_global 'LOCAL L) (term (load (empty-ctx) ,script)))
  (debug (term (TEXT "load complete")))
  (list (register-global! layer_global) L))

;; The result of e under the loaded context (G L)
(define (run-loaded loaded e)
  (match-define (list G L) loaded)
  (debug (term (TEXT "into call")))
  (match (run (list G (list 'IN L e)))
    [(list _ (list 'IN _ res)) res]))

;; The line printed for Entry's result
(define (result->output res)
  (match res
    ['FAIL "fail"]
    [val (jsexpr->string (val->jsexpr val))]))

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
     #:program "7-entry.rkt"
     #:once-each
     [("--p4") program "Run Program_ok of the spec FILE on a P4 program"
               (set! p4 program)]
     #:multi
     [("-i") dir "Add a P4 include directory (required with --p4)"
             (set! includes (append includes (list dir)))]
     #:args (file) file))
  (when (and p4 (null? includes))
    (raise-user-error '7-entry.rkt "-i is required with --p4"))
  (host-init! path)
  (displayln
   (if p4
       (let ([val_p4 (boot-p4 p4 #:includes includes)])
         (result-p4->output (entry-p4 (load-script (boot-script path)) val_p4)))
       (result->output (entry (boot-script path))))))
