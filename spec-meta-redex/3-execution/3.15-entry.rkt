#lang racket/base
;; Serves the host's calls, reducing each with ->al.
;;
;;   racket spec-meta-redex/3-execution/3.15-entry.rkt
;;
;; call   ::= (load spec)
;;          | (call-rel id (val ...))
;;          | (call-func id (typ ...) (val ...))
;;          | (exit)
;;
;; answer ::= READY       before the first call
;;          | (OK ())     load
;;          | valsres     call-rel
;;          | valres      call-func

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

;; The context that spec loads into
(define (load-script spec)
  (match-define (list 'GLOBAL G 'LOCAL L) (term (load (empty-ctx) ,spec)))
  (list G L))

;; The configuration that conf reduces to by ->al
(define (run conf)
  (match (apply-reduction-relation ->al conf)
    ['() conf]
    [(list conf_1) (run conf_1)]
    [confs (error 'run "~a successors of ~e" (length confs) (car conf))]))

;; The result of e under (G L)
(define (run-loaded loaded e)
  (match-define (list G L) loaded)
  (match (run (term (,e ,G ,L ())))
    [(list (? res? res) _ _ '()) res]
    [(list e_stuck _ _ _) (error 'run "stuck: no rule applies to ~e" e_stuck)]))

(define res? (redex-match? al res))
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
