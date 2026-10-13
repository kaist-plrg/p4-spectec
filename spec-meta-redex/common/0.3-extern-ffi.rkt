#lang racket/base
;; Transport for the extern wire: Racket -> C shim -> OCaml, over ffi2.
;;
;;   host-eval  --ffi2-->  spec-meta-redex/ffi/shim.c  --caml_callback-->
;;   p4spec/bin/ffi.ml
;;
;; The OCaml runtime lives in this process, and only this place may call it.
;; `make redex` builds the shim.

(require ffi2
         racket/promise
         racket/runtime-path)
(provide host-init!
         host-eval)

(define-runtime-path shim-path "../ffi/shim.so")

;; host_init and host_eval, loaded on first use
(define shim
  (delay
    (unless (file-exists? shim-path)
      (error 'extern-ffi "~a is missing; run `make redex`" (simplify-path shim-path)))
    (define lib (ffi2-lib shim-path))
    (cons (ffi2-procedure (ffi2-lib-ref lib "host_init") (-> string_t int64_t))
          (ffi2-procedure (ffi2-lib-ref lib "host_eval") (-> string_t string_t)))))

;; Starts the host, and builds its runner from spec: the script being run, or
;; spec/ for a P4 program, as K's <specdir>. Call it once, before anything
;; reaches the host. OCaml resolves a relative path against the process's
;; directory, so the path goes complete.
(define (host-init! spec)
  (define path (path->string (simplify-path (path->complete-path spec))))
  (case ((car (force shim)) path)
    [(1) (void)]
    [(0) (error 'host-init! "the host cannot build a runner for ~a" path)]
    [else (error 'host-init! "ffi.ml's callbacks are missing from the host")]))

;; JSON request text -> JSON reply text
(define (host-eval request)
  ((cdr (force shim)) request))
