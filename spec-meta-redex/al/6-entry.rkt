#lang racket/base
;; spec-meta/al/6-entry.watsup: load a script, then run $main() through the
;; driver. entry-p4 runs Program_ok on a P4 program instead, as K's afterLoad
;; does.
;;
;; As a program, it runs $main() of a SpecTec script through Entry, and
;; prints its value in k-run.sh's JSON format, or `fail`. Entry's debug
;; messages go to stderr.
;;
;;   racket spec-meta-redex/al/6-entry.rkt FILE.watsup

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
  (match (run-loaded script (term (CALL "main" () ())))
    [(and res (list 'OK val))
     (debug val)
     res]
    ['FAIL 'FAIL]))

;; The result of Program_ok on the P4 program val_p4: (OK (val ...)), or FAIL
(define (entry-p4 script val_p4)
  (run-loaded script (term (call-rel "Program_ok" (,val_p4)))))

;; The result of e under the context that script loads into
(define (run-loaded script e)
  (debug (term (TEXT "entry-al")))
  (match-define (list 'GLOBAL G 'LOCAL L) (term (load (empty-ctx) ,script)))
  (debug (term (TEXT "load complete")))
  (debug (term (TEXT "into call")))
  (match (run (list G (list 'IN L e)))
    [(list _ (list 'IN _ res)) res]))

;; The line printed for Entry's result
(define (result->output res)
  (match res
    [(list 'OK val) (jsexpr->string (val->jsexpr val))]
    ['FAIL "fail"]))

(module+ main
  (define path
    (command-line
     #:program "6-entry.rkt"
     #:args (file) file))
  (parameterize ([host-spec path])
    (displayln (result->output (entry (boot-script path))))))
