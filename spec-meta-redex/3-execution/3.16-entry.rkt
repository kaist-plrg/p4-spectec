#lang racket/base
;; Mirrors spec-meta/5-entry.watsup for Eval_AL, and serves the calls the host
;; makes on a loaded spec as well ($load_ctx, Call_rel, Call_func).
;;
;; As a program, it says it is ready, then answers each call of the host
;; (3.04.1-extern-json.rkt) by running it through the driver: OK with its
;; values, UNMATCH as a fail, ERROR as an error. A call that raises a Racket
;; error is answered with `raise` and its message. `exit`, or the end of
;; stdin, ends it. Debug values go to the host, and other output to stderr.
;;
;;   racket spec-meta-redex/3-execution/3.16-entry.rkt
;;
;; p4spectec::metacirc::engine::redex starts it, once per tower.

(require json
         racket/match
         redex/reduction-semantics
         "../0-aux/0.0-stdlib.rkt"
         "3.04.1-extern-json.rkt"
         "3.04.2-extern-host.rkt"
         "3.02-context.rkt"
         "3.15-driver.rkt")
(provide load-script
         run-loaded
         serve)

;; The context that spec loads into, as (G L): the handle of its global
;; layer, now registered, and its local layer
(define (load-script spec)
  (match-define (list 'GLOBAL layer_global 'LOCAL L) (term (load (empty-ctx) ,spec)))
  (list (register-global! layer_global) L))

;; The result of e under the loaded context (G L)
(define (run-loaded loaded e)
  (match-define (list G L) loaded)
  (match (run (list G (list 'IN L e)))
    [(list _ (list 'IN _ res)) res]))

;; The spec that the s-expression text sexp holds
(define (read-spec sexp)
  (define in (open-input-string sexp))
  (define spec (read in))
  (unless (and (not (eof-object? spec)) (eof-object? (read in)))
    (error 'serve "a spec is not one s-expression"))
  spec)

;; The context of the spec loaded last, which calls run under
(define loaded #f)

;; Answers the host's calls until it says exit.
(define (serve)
  (parameterize ([current-call-handler answer!])
    (let loop ()
      (match (receive)
        [(? eof-object?) (void)]
        [(hash-table ('exit _)) (void)]
        [call
         (answer! call)
         (loop)]))))

;; Emits the answer to a call, or the Racket error it raised.
(define (answer! call)
  (with-handlers ([exn:fail? (λ (e) (emit! "raise" (exn-message e)))])
    (emit! "serve" (answer call))))

(define (answer call)
  (match call
    [(hash-table ('load (? string? sexp)))
     (set! loaded (load-script (read-spec sexp)))
     (hasheq 'ok '())]
    ;; rule Entry:
    ;;   |- spec : val
    ;;   -- debug "entry-al"
    ;;   -- if C = $load_ctx($empty_ctx, spec)
    ;;   -- debug "load complete"
    ;;   -- debug "into call"
    ;;   -- Eval_exp: C |- CALL "main" eps eps : OK val
    ;;   -- debug val
    [(hash-table ('eval-al (? string? sexp)))
     (debug (term (TEXT "entry-al")))
     (define loaded_main (load-script (read-spec sexp)))
     (debug (term (TEXT "load complete")))
     (debug (term (TEXT "into call")))
     (match (run-loaded loaded_main (term (CALL "main" () ())))
       [(and failure (or 'UNMATCH 'ERROR)) (failure->answer failure)]
       [val
        (debug val)
        (hasheq 'ok (val->jsexpr val))])]
    [(hash-table ('call-rel (? string? id)) ('args (? list? args)))
     (match (run-loaded (loaded-ctx) (term (call-rel ,id ,(map jsexpr->val args))))
       [(list 'OK vals) (hasheq 'ok (map val->jsexpr vals))]
       [failure (failure->answer failure)])]
    [(hash-table ('call-func (? string? id)) ('targs (? list? targs)) ('args (? list? args)))
     (match (run-loaded (loaded-ctx)
                        (term (call-func ,id ,(map jsexpr->typ targs) ,(map jsexpr->val args))))
       [(and failure (or 'UNMATCH 'ERROR)) (failure->answer failure)]
       [val (hasheq 'ok (val->jsexpr val))])]
    [_ (error 'serve "not a call: ~a" (jsexpr->string call))]))

(define (loaded-ctx)
  (or loaded (error 'serve "no spec is loaded")))

(define (failure->answer failure)
  (match failure
    ['UNMATCH (hasheq 'fail (json-null))]
    ['ERROR (hasheq 'error "the specification evaluated to ERROR")]))

(module+ main
  (parameterize ([current-output-port (current-error-port)]
                 [current-debug (λ (val) (emit! "debug" (val->jsexpr val)))])
    (emit! "serve" (hasheq 'ready (json-null)))
    (serve)))
