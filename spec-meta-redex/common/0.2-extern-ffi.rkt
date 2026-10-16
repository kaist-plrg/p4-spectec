#lang racket/base
;; Transport for the extern wire: Racket -> Rust, over ffi2.
;;
;;   host-eval  --ffi2-->  bin/libp4spectec.so  (p4spectec::metacross::ffi)
;;
;; The host lives in this process, and only this place may call it.
;; `make ffi` builds the library.

(require ffi2
         racket/promise
         racket/runtime-path)
(provide host-init!
         host-eval)

(define-runtime-path lib-path "../../bin/libp4spectec.so")

;; host_init and host_eval, loaded on first use
(define host
  (delay
    (unless (file-exists? lib-path)
      (error 'extern-ffi "~a is missing; run `make ffi`" (simplify-path lib-path)))
    (define lib (ffi2-lib lib-path))
    (cons (ffi2-procedure (ffi2-lib-ref lib "host_init") (-> string_t int64_t))
          (ffi2-procedure (ffi2-lib-ref lib "host_eval") (-> string_t string_t)))))

;; Starts the host, and builds its runner from spec: the script being run, or
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
