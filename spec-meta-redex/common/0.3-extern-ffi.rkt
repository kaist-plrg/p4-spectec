#lang racket/base
;; Transport for the extern wire: Racket -> C shim -> OCaml, over ffi2.
;;
;;   host-eval  --ffi2-->  spec-meta-redex/ffi/shim.c  --caml_callback-->
;;   p4spec/bin/ffi.ml
;;
;; The OCaml runtime lives in this process, and only this place may call it.
;; `make redex-ffi` builds the shim.

(require ffi2
         racket/promise
         racket/runtime-path)
(provide host-spec
         host-eval)

(define-runtime-path shim-path "../ffi/shim.so")

;; The spec the host's runner is built from: the script being run, or spec/
;; for a P4 program, as K's <specdir>
(define host-spec (make-parameter #f))

;; host_init and host_eval, loaded on the first call
(define shim
  (delay
    (unless (file-exists? shim-path)
      (error 'host-eval "~a is missing; run `make redex-ffi`" (simplify-path shim-path)))
    (define lib (ffi2-lib shim-path))
    (cons (ffi2-procedure (ffi2-lib-ref lib "host_init") (-> string_t int64_t))
          (ffi2-procedure (ffi2-lib-ref lib "host_eval") (-> string_t string_t)))))

;; The spec of the current runner, as a complete path string
(define current-spec #f)

(define (init! spec)
  (case ((car (force shim)) spec)
    [(1) (set! current-spec spec)]
    [(0) (error 'host-eval "the host cannot build a runner for ~a" spec)]
    [else (error 'host-eval "ffi.ml's callbacks are missing from the host")]))

;; JSON request text -> JSON reply text, under the runner for host-spec
(define (host-eval request)
  (unless (host-spec)
    (error 'host-eval "host-spec is not set"))
  (define spec (path->string (simplify-path (path->complete-path (host-spec)))))
  (unless (equal? spec current-spec)
    (init! spec))
  ((cdr (force shim)) request))
