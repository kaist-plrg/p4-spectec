#lang racket/base
;; Transport for the extern wire: Racket -> Rust, over ffi2, and the host
;; procedures of the three extern relations.
;;
;;   host-eval  --ffi2-->  bin/libp4spectec.so  (p4spectec::metacross::ffi)
;;
;; The host lives in this process, and only this place may call it. The host
;; procedures are impure, so only a reduction rule may call them, never a
;; metafunction. `make redex` builds the library.

(require ffi2
         json
         racket/promise
         racket/runtime-path
         "3.04.1-extern-json.rkt")
(provide host-init!
         host-call-extern-func
         host-call-builtin-func
         host-call-extern-rel)

(define-runtime-path lib-path "../../bin/libp4spectec.so")

;; host_init and host_eval, loaded on first use
(define host
  (delay
    (unless (file-exists? lib-path)
      (error 'extern-ffi "~a is missing; run `make redex`" (simplify-path lib-path)))
    (define lib (ffi2-lib lib-path))
    (cons (ffi2-procedure (ffi2-lib-ref lib "host_init") (-> string_t int64_t))
          (ffi2-procedure (ffi2-lib-ref lib "host_eval") (-> string_t string_t)))))

;; Starts the host, and builds its runner from spec: the spec being run, or
;; spec/ for a P4 program, as K's <specdir>. Call it once, before anything
;; reaches the host. The host resolves a relative path against the process's
;; directory, so the path goes complete.
(define (host-init! spec)
  (define path (path->string (simplify-path (path->complete-path spec))))
  (unless (= ((car (force host)) path) 1)
    (error 'host-init! "the host cannot build a runner for ~a" path)))

;; JSON request text -> JSON reply text
(define (host-eval request)
  ((cdr (force host)) request))

;;
;; The host procedures of the extern relations
;;

;; Sends a request to the host, and gives its response as a jsexpr.
(define (host-request request)
  (string->jsexpr (host-eval (jsexpr->string request))))

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
