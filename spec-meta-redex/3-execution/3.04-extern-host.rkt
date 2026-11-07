#lang racket/base
;; Transport to the host over stdin/stdout, one s-expression per line.
;;
;;   racket 3.15-entry.rkt  <--pipe-->  p4spectec::metacirc::redex
;;
;; To the host:
;;   serve <answer>    the answer to a call
;;   eval <request>    a request
;;   debug <val>       a debug value
;;   raise <text>      a Racket error raised by a call
;;
;; From the host: a call, or a response.
;;
;; request  ::= (builtin id (typ ...) (val ...))
;;            | (extern-func id (typ ...) (val ...))
;;            | (extern-rel id (val ...))
;;
;; response ::= valres          builtin and extern-func
;;            | valsres         extern-rel

(provide receive
         emit!
         current-call-handler
         host-call-extern-func
         host-call-builtin-func
         host-call-extern-rel)

;; Rust's ends: stdin and stdout at instantiation
(define from-host (current-input-port))
(define to-host (current-output-port))

;; The next call or response, or eof
(define (receive)
  (read from-host))

;; Sends datum as one line after word
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
