#lang racket/base
;; Booting SpecTec scripts and P4 programs into Redex terms, through
;; `spectec-boot sexp` and `spectec-boot sexp-p4`.

(require json
         racket/port
         racket/runtime-path
         racket/string)
(provide boot-script
         boot-p4
         decode-ext)

(define-runtime-path spectec-boot-default "../../spectec-boot")

;; SPECTEC_BOOT overrides the binary, as in spec-meta-k/scripts.
(define (spectec-boot)
  (define env (getenv "SPECTEC_BOOT"))
  (define exe (if env
                  (or (find-executable-path env) env)
                  spectec-boot-default))
  (unless (file-exists? exe)
    (error 'spectec-boot "~a not found; run `make boot`" exe))
  exe)

;; Runs spectec-boot with args, and reads the one datum it prints.
(define (read-spectec-boot . args)
  (define cmd (string-join (cons "spectec-boot" args)))
  (define-values (proc stdout stdin stderr)
    (apply subprocess #f #f #f (spectec-boot) args))
  (close-output-port stdin)
  ;; Drain stderr on its own thread, so a full pipe cannot block the process.
  (define stderr-text #f)
  (define stderr-thread
    (thread (λ () (set! stderr-text (port->string stderr)))))
  (define stdout-text (port->string stdout))
  (subprocess-wait proc)
  (thread-wait stderr-thread)
  (close-input-port stdout)
  (close-input-port stderr)
  (unless (zero? (subprocess-status proc))
    (error 'spectec-boot "`~a` failed:\n~a" cmd stderr-text))
  (define in (open-input-string stdout-text))
  (define datum (read in))
  (unless (and (not (eof-object? datum)) (eof-object? (read in)))
    (error 'spectec-boot "`~a` did not print one datum" cmd))
  datum)

(define (path-arg path)
  (if (path? path) (path->string path) path))

;; A .watsup file, or a directory of them, as a `script`.
(define (boot-script path)
  (read-spectec-boot "sexp" (path-arg path)))

;; A P4 program as a `val`. includes are the P4 include directories.
(define (boot-p4 path #:includes [includes '()])
  (decode-ext
   (apply read-spectec-boot "sexp-p4" "-p" (path-arg path)
          (for*/list ([include (in-list includes)]
                      [arg (in-list (list "-i" (path-arg include)))])
            arg))))

;; `sexp-p4` prints the JSON of `(EXT json)` as text; this decodes it into a
;; jsexpr in a `val`.
(define (decode-ext val)
  (cond
    [(and (pair? val) (eq? (car val) 'EXT))
     (list 'EXT (string->jsexpr (cadr val)))]
    [(pair? val) (map decode-ext val)]
    [else val]))
