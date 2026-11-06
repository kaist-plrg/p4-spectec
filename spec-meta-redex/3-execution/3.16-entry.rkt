#lang racket/base
;; Serves the calls the host makes on a loaded spec ($load_ctx, Call_rel,
;; Call_func). The entries of spec-meta/5-entry.watsup are two of them in a
;; row: Eval_AL_rel is `load` then `call-rel`, and Eval_AL_func is `load` then
;; `call-func` with no type arguments.
;;
;; As a program, it says it is ready, then answers each call of the host by
;; reducing it with ->al. Calls and answers are s-expressions that `read`
;; reads and `write` prints (3.04-extern-host.rkt), so the answer is the
;; result itself: OK with its values, UNMATCH, or ERROR for a stuck term. A
;; call that raises a Racket error is answered with `raise` and its message.
;; `exit`, or the end of stdin, ends it. Debug values go to the host, and
;; other output to stderr.
;;
;; call   ::= (load spec)                   $load_ctx($empty_ctx, spec)
;;          | (call-rel id (val ...))
;;          | (call-func id (typ ...) (val ...))
;;          | (exit)
;;
;; answer ::= READY                         before the first call
;;          | (OK ())                       load
;;          | valsres                       call-rel
;;          | valres                        call-func
;;
;;   racket spec-meta-redex/3-execution/3.16-entry.rkt
;;
;; p4spectec::metacirc::redex starts it, once per tower.

(require racket/match
         redex/reduction-semantics
         "../0-aux/0.0-stdlib.rkt"
         "3.04-extern-host.rkt"
         "3.02-context.rkt"
         "3.03-machine.rkt"
         "3.14-step.rkt")
(provide load-script
         run-loaded
         serve)

;; The context that spec loads into, as (G L): its global layer and its
;; local layer
(define (load-script spec)
  (match-define (list 'GLOBAL G 'LOCAL L) (term (load (empty-ctx) ,spec)))
  (list G L))

;; The term that e reduces to by ->al: final, or stuck. The rules are
;; deterministic, so a term has at most one successor.
(define (run e)
  (match (apply-reduction-relation ->al e)
    ['() e]
    [(list e_1) (run e_1)]
    [es (error 'run "~a successors of ~e" (length es) e)]))

;; The result of e under the loaded context (G L), or ERROR if it gets stuck
(define (run-loaded loaded e)
  (match-define (list G L) loaded)
  (define e_final (run (term (IN ,G ,L ,e))))
  (if (redex-match? al res e_final) e_final 'ERROR))

;; The context of the spec loaded last, which calls run under
(define loaded #f)

;; Answers the host's calls until it says exit.
(define (serve)
  (parameterize ([current-call-handler answer!])
    (let loop ()
      (match (receive)
        [(? eof-object?) (void)]
        [(list 'exit) (void)]
        [call
         (answer! call)
         (loop)]))))

;; Emits the answer to a call, or the Racket error it raised.
(define (answer! call)
  (with-handlers ([exn:fail? (λ (e) (emit! "raise" (exn-message e)))])
    (emit! "serve" (answer call))))

(define (answer call)
  (match call
    [(list 'load spec)
     (set! loaded (load-script spec))
     '(OK ())]
    [(list 'call-rel id vals)
     (run-loaded (loaded-ctx) (term (call-rel ,id ,vals)))]
    [(list 'call-func id typs vals)
     (run-loaded (loaded-ctx) (term (call-func ,id ,typs ,vals)))]
    [_ (error 'serve "not a call: ~s" call)]))

(define (loaded-ctx)
  (or loaded (error 'serve "no spec is loaded")))

(module+ main
  (parameterize ([current-output-port (current-error-port)]
                 [current-debug (λ (val) (emit! "debug" val))])
    (emit! "serve" 'READY)
    (serve)))
