#lang racket/base

;; Prints statistics of a Redex specification.
;;
;; Usage: racket scripts/stat/spec-meta-redex.rkt PATH...
;;
;; Each PATH is a .rkt file or a directory searched recursively for them.
;; Files are only read as s-expressions, never expanded or run.

(require racket/file
         racket/list
         racket/path
         racket/set
         racket/string)

(define (collect-files paths)
  (append*
   (for/list ([path (in-list paths)])
     (cond
       [(directory-exists? path)
        (sort (for/list ([file (in-directory path (λ (dir) (not (equal? (file-name-from-path dir)
                                                                         (string->path "compiled")))))]
                         #:when (path-has-extension? file #".rkt"))
                file)
              path<?)]
       [(file-exists? path) (list (string->path path))]
       [else (eprintf "cannot read ~a\n" path) (exit 1)]))))

;; Counts every line, including comments and blank lines
(define (count-lines text)
  (+ (for/sum ([c (in-string text)]) (if (char=? c #\newline) 1 0))
     (if (and (positive? (string-length text)) (not (string-suffix? text "\n"))) 1 0)))

;; Reads a `#lang` module as one `(module ...)` datum
(define (read-module file)
  (parameterize ([read-accept-reader #t]
                 [read-accept-lang #t])
    (call-with-input-file file read)))

(define nonterminals (mutable-set))
(define metafunctions 0)
(define rules 0)

;; Names the nonterminal of a `(nt ::= ...)` or `((nt nt-alias ...) ::= ...)` clause
(define (clause-nonterminal clause)
  (and (pair? clause)
       (let ([names (car clause)])
         (cond
           [(symbol? names) names]
           [(and (pair? names) (symbol? (car names))) (car names)]
           [else #f]))))

(define (visit! datum)
  (when (pair? datum)
    (case (car datum)
      [(define-language)
       (for ([clause (in-list (cddr datum))])
         (define nt (clause-nonterminal clause))
         (when nt (set-add! nonterminals nt)))]
      [(define-extended-language)
       (for ([clause (in-list (cdddr datum))])
         (define nt (clause-nonterminal clause))
         (when nt (set-add! nonterminals nt)))]
      [(define-metafunction)
       (set! metafunctions (add1 metafunctions))]
      [(reduction-relation)
       (for ([clause (in-list (cdr datum))]
             #:when (and (pair? clause) (eq? (car clause) '-->)))
         (set! rules (add1 rules)))]
      [else (void)])
    (let loop ([datum datum])
      (cond
        [(pair? datum) (visit! (car datum)) (loop (cdr datum))]
        [else (void)]))))

(module+ main
  (define paths (vector->list (current-command-line-arguments)))
  (when (null? paths)
    (eprintf "usage: racket scripts/stat/spec-meta-redex.rkt PATH...\n")
    (exit 1))
  (define files (collect-files paths))
  (define lines
    (for/sum ([file (in-list files)])
      (define text (file->string file))
      (visit! (read-module file))
      (count-lines text)))
  (for ([row (in-list (list (cons "files" (length files))
                            (cons "LoC" lines)
                            (cons "nonterminal" (set-count nonterminals))
                            (cons "metafunction" metafunctions)
                            (cons "rule" rules)))])
    (printf "~a~a\n"
            (pad-right (car row) 14)
            (pad-left (number->string (cdr row)) 8))))

(define (pad-right s n) (string-append s (make-string (max 0 (- n (string-length s))) #\space)))
(define (pad-left s n) (string-append (make-string (max 0 (- n (string-length s))) #\space) s))
