#lang racket/base
;; Transport for the host's boundary: Racket <-> Rust, as lines over this
;; process's stdin and stdout, and the host procedures of the three extern
;; relations.
;;
;;   racket 3.16-entry.rkt  <--pipe-->  p4spectec::metacirc::engine::redex
;;
;; Rust starts this process and calls into it (3.16-entry.rkt). Each line to
;; Rust starts with a word saying what it is: `serve <answer>`,
;; `eval <request>`, `debug <val>`, or `raise <text>`; each line from Rust is
;; one JSON call or response (3.04.1-extern-json.rkt). A request waits for its
;; response; a call that arrives first is nested in the request, and is
;; answered in place. The host procedures are impure, so only a reduction rule
;; may call them, never a metafunction.

(require json
         "3.04.1-extern-json.rkt")
(provide receive
         emit!
         current-call-handler
         host-call-extern-func
         host-call-builtin-func
         host-call-extern-rel)

;; Rust's ends: stdin and stdout as they are when this module is
;; instantiated, so the entry can send other output elsewhere
(define from-host (current-input-port))
(define to-host (current-output-port))

;; The next call or response, or eof
(define (receive)
  (read-json from-host))

;; Sends js as one line after word; js is printed whole before any of it goes.
(define (emit! word js)
  (define line (string-append word " " (jsexpr->string js) "\n"))
  (write-string line to-host)
  (flush-output to-host))

;; Answers a call of the host, emitting the answer; the entry sets it.
(define current-call-handler
  (make-parameter (λ (call) (error 'extern-host "no call is served: ~a" (jsexpr->string call)))))

(define (call? js)
  (and (hash? js)
       (for/or ([key (in-list '(load eval-al call-rel call-func))])
         (hash-has-key? js key))))

;; Sends a request to the host, and gives its response as a jsexpr.
(define (host-request request)
  (emit! "eval" request)
  (let loop ()
    (define message (receive))
    (cond
      [(eof-object? message) (error 'extern-host "the host closed the pipe")]
      [(call? message)
       ((current-call-handler) message)
       (loop)]
      [else message])))

;;
;; The host procedures of the extern relations
;;

;;; Extern meta-function invocation

;; |- id `< typ* `> `( val* `) : funcRes, as a valres
(define (host-call-extern-func id typs vals)
  (response->valres (host-request (extern-func-request id typs vals))))

;;; Builtin meta-function invocation

;; |- id `< typ* `> `( val* `) : funcRes, as a valres
(define (host-call-builtin-func id typs vals)
  (response->valres (host-request (builtin-request id typs vals))))

;;; Extern relations

;; |- id `( val* `) : relRes, as a valsres
(define (host-call-extern-rel id vals)
  (response->valsres (host-request (extern-rel-request id vals))))
