#lang racket/base
;; Booting SpecTec scripts and P4 programs into Redex terms, through
;; `spectec-boot sexp` and `spectec-boot sexp-p4`.

(require json
         racket/list
         racket/runtime-path
         racket/string
         racket/system)
(provide boot-script
         boot-p4)

(define-runtime-path spectec-boot-path "../../spectec-boot")

(define (spectec-boot)
  (unless (file-exists? spectec-boot-path)
    (error 'spectec-boot "~a not found; run `make boot`" (simplify-path spectec-boot-path)))
  spectec-boot-path)

;; Runs spectec-boot with args, and reads the one datum it prints.
(define (read-spectec-boot . args)
  (define cmd (string-join (cons "spectec-boot" args)))
  (define stdout (open-output-string))
  (define stderr (open-output-string))
  (define ok?
    (parameterize ([current-input-port (open-input-string "")]
                   [current-output-port stdout]
                   [current-error-port stderr])
      (apply system* (spectec-boot) args)))
  (unless ok?
    (error 'spectec-boot "`~a` failed:\n~a" cmd (get-output-string stderr)))
  (define in (open-input-string (get-output-string stdout)))
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
(define (boot-p4 path #:includes includes)
  (decode-ext
   (apply read-spectec-boot "sexp-p4" "-p" (path-arg path)
          (append-map (λ (include) (list "-i" (path-arg include))) includes))))

;; `sexp-p4` prints the JSON of `(EXT json)` as text; this decodes it into a
;; jsexpr in a `val`.
(define (decode-ext val)
  (cond
    [(and (pair? val) (eq? (car val) 'EXT))
     (list 'EXT (string->jsexpr (cadr val)))]
    [(pair? val) (map decode-ext val)]
    [else val]))
