#lang racket/base
;; Transport for the host's boundary: Racket <-> Rust, as lines over this
;; process's stdin and stdout, and the host procedures of the three extern
;; relations.
;;
;;   racket 3.15-entry.rkt  <--pipe-->  p4spectec::metacirc::redex
;;
;; Rust starts this process and calls into it (3.15-entry.rkt). Each line to
;; Rust starts with a word saying what it is: `serve <answer>`,
;; `eval <request>`, `debug <val>`, or `raise <text>`; each line from Rust is
;; one call or response. Both are s-expressions, which `write` prints and
;; `read` reads: values and types are the val and typ terms themselves, and
;; answers and responses are results (p4spectec/src/metacirc/redex/serve.rs).
;; A request waits for its response; a call that arrives first is nested in
;; the request, and is answered in place. The host procedures are impure, so
;; only a reduction rule may call them, never a metafunction.
;;
;; request  ::= (builtin id (typ ...) (val ...))
;;            | (extern-func id (typ ...) (val ...))
;;            | (extern-rel id (val ...))
;;
;; response ::= valres          builtin and extern-func: val, UNMATCH, or ERROR
;;            | valsres         extern-rel: (OK (val ...)), UNMATCH, or ERROR
;;
;; ERROR says the host could not answer, and the host reports why. An extern
;; value (EXT json) holds its JSON text as a string, which only Rust reads.

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
  (read from-host))

;; Sends datum as one line after word; it is printed whole before any of it goes.
(define (emit! word datum)
  (define out (open-output-string))
  (write-string word out)
  (write-char #\space out)
  (write datum out)
  (newline out)
  (write-string (get-output-string out) to-host)
  (flush-output to-host))

;; Answers a call of the host, emitting the answer; the entry sets it.
(define current-call-handler
  (make-parameter (λ (call) (error 'extern-host "no call is served: ~s" call))))

(define (call? message)
  (and (pair? message) (memq (car message) '(load call-rel call-func)) #t))

;; Sends a request to the host, and gives its response.
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
  (host-request (list 'extern-func id typs vals)))

;;; Builtin meta-function invocation

;; |- id `< typ* `> `( val* `) : funcRes, as a valres
(define (host-call-builtin-func id typs vals)
  (host-request (list 'builtin id typs vals)))

;;; Extern relations

;; |- id `( val* `) : relRes, as a valsres
(define (host-call-extern-rel id vals)
  (host-request (list 'extern-rel id vals)))
