#lang racket/base
;; Booting SpecTec scripts and P4 programs into Redex terms, through
;; `metacross sexp` and `metacross sexp-p4`.

(require json
         racket/list
         racket/runtime-path
         racket/string
         racket/system)
(provide boot-script
         boot-p4)

(define-runtime-path metacross-path "../bin/metacross")

(define (metacross)
  (unless (file-exists? metacross-path)
    (error 'metacross "~a not found; run `make metacross`" (simplify-path metacross-path)))
  metacross-path)

;; Runs metacross with args, and reads the one datum it prints.
(define (read-metacross . args)
  (define cmd (string-join (cons "metacross" args)))
  (define stdout (open-output-string))
  (define stderr (open-output-string))
  (define ok?
    (parameterize ([current-input-port (open-input-string "")]
                   [current-output-port stdout]
                   [current-error-port stderr])
      (apply system* (metacross) args)))
  (unless ok?
    (error 'metacross "`~a` failed:\n~a" cmd (get-output-string stderr)))
  (define in (open-input-string (get-output-string stdout)))
  (define datum (read in))
  (unless (and (not (eof-object? datum)) (eof-object? (read in)))
    (error 'metacross "`~a` did not print one datum" cmd))
  datum)

(define (path-arg path)
  (if (path? path) (path->string path) path))

;; A .watsup file, or a directory of them, as a `script`.
(define (boot-script path)
  (read-metacross "sexp" (path-arg path)))

;; A P4 program as a `val`. includes are the P4 include directories.
(define (boot-p4 path #:includes includes)
  (decode-ext
   (apply read-metacross "sexp-p4" "-p" (path-arg path)
          (append-map (λ (include) (list "-i" (path-arg include))) includes))))

;; `sexp-p4` prints the JSON of `(EXT json)` as text; this decodes it into a
;; jsexpr in a `val`.
(define (decode-ext val)
  (cond
    [(and (pair? val) (eq? (car val) 'EXT))
     (list 'EXT (string->jsexpr (cadr val)))]
    [(pair? val) (map decode-ext val)]
    [else val]))
