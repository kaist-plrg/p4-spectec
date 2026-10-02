#lang racket/base
;; Type-checks the P4 programs under the --p4-dir directories with SPEC, as
;; spec-meta-k/scripts/run-k-typecheck.py does for K. SPEC is booted and
;; loaded once, and each program runs Program_ok under that context in this
;; process. Programs named in the .exclude files under the -e directories are
;; skipped, as are files under an include/ directory (#include fragments, not
;; programs).
;;
;; Each program passes or fails; one that raises fails. A program is expected
;; to pass, or with --neg to fail. Progress, the summary, and the programs
;; without the expected result go to stdout and a result file. The result file
;; also gets the stderr and error message of each program that raises.
;; raco test runs only the empty test submodule; run this with
;; `make redex-test`, or:
;;
;;   raco make spec-meta-redex/test/p4-typecheck.rkt
;;   racket spec-meta-redex/test/p4-typecheck.rkt --p4-dir DIR -e DIR -i DIR [--neg] SPEC

(require racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/runtime-path
         racket/set
         racket/string
         json
         "../common/0.2-extern-json.rkt"
         "../common/0.3-extern-ffi.rkt"
         "../al/0-boot.rkt"
         "../al/6-entry.rkt")

;; The repository root, which .exclude entries are relative to, as in K's and
;; OCaml's tests
(define-runtime-path root-path "../..")
(define root (simplify-path root-path))
(define (default-result neg?)
  (build-path root "spec-meta-redex"
              (if neg? "p4-typecheck-neg.result" "p4-typecheck-pos.result")))

(define (normalize path)
  (path->string (simplify-path path #f)))

;; The programs that the .exclude files under exclude-dirs name, as complete
;; paths
(define (load-excludes exclude-dirs)
  (for*/set ([dir (in-list exclude-dirs)]
             [file (in-directory dir)]
             #:when (path-has-extension? file #".exclude")
             [line (in-list (file->lines file))]
             #:do [(define entry (string-trim line))]
             #:unless (or (string=? entry "") (string-prefix? entry "#")))
    (simple-form-path (path->complete-path entry root))))

;; Compares paths by their elements, as Python's sorted() on paths does, so
;; the order is K's.
(define (path<? a b)
  (let loop ([as (explode-path a)] [bs (explode-path b)])
    (cond
      [(null? bs) #f]
      [(null? as) #t]
      [else
       (define a_1 (path->string (car as)))
       (define b_1 (path->string (car bs)))
       (if (string=? a_1 b_1)
           (loop (cdr as) (cdr bs))
           (string<? a_1 b_1))])))

;; The programs under p4-dirs, minus the excluded and those under an include/
;; directory below a p4-dir
(define (collect-programs p4-dirs exclude-dirs)
  (define excluded (load-excludes exclude-dirs))
  (sort
   (for*/list ([dir (in-list p4-dirs)]
               [path (in-directory dir)]
               #:when (and (path-has-extension? path #".p4") (file-exists? path))
              ;  #:when (string-prefix? (path->string (file-name-from-path path)) "action-bind")
               #:do [(define rel (find-relative-path (simple-form-path dir) (simple-form-path path)))]
               #:unless (member "include" (map path->string (drop-right (explode-path rel) 1)))
               #:unless (set-member? excluded (simple-form-path path)))
     (normalize path))
   path<?
   #:key string->path))

(define (seconds-since started)
  (/ (- (current-inexact-monotonic-milliseconds) started) 1000))

;; The status of the program under the loaded context C, 'pass or 'fail, the
;; seconds it took, and, if it raised, what it wrote to stderr and the error's
;; message
(define (typecheck C program includes)
  (define started (current-inexact-monotonic-milliseconds))
  (define err (open-output-string))
  (define-values (status reason)
    (with-handlers ([exn:fail? (λ (e) (values 'fail (exn-message e)))])
      (parameterize ([current-error-port err])
        (define val_p4 (boot-p4 program #:includes includes))
        (match (entry-p4 C val_p4)
          [(list 'OK _) (values 'pass #f)]
          ['FAIL (values 'fail #f)]))))
  (values status
          (seconds-since started)
          (and reason (string-append (get-output-string err) reason))))

;; Checks the programs with spec, reporting to stdout and result-path; gives
;; whether each of them passes, or with neg? fails
(define (check-all spec includes programs result-path neg?)
  (call-with-output-file* result-path #:exists 'truncate/replace
    (λ (result)
      (define (report [line ""])
        (for ([out (list result (current-output-port))])
          (displayln line out)
          (flush-output out)))
      (define expected (if neg? 'fail 'pass))
      (define total (length programs))
      (define started (current-inexact-monotonic-milliseconds))
      (parameterize ([host-spec spec])
        (define C (load-script (boot-script spec)))
        ;; Starts the host, so the first program's time leaves out host_init.
        (host-eval (jsexpr->string (builtin-request "rev_" '(NAT) '((LIST ())))))
        (report (format "loaded spec in ~as" (~r (seconds-since started) #:precision '(= 2))))
        (define passed 0)
        (define failing
          (for/fold ([failing '()] #:result (reverse failing))
                    ([program (in-list programs)]
                     [i (in-naturals 1)])
            (define-values (status elapsed reason) (typecheck C program includes))
            (when (eq? status 'pass)
              (set! passed (add1 passed)))
            (report (format "[~a/~a] ~a ~as  ~a" i total (~a status #:min-width 7)
                            (~r elapsed #:precision '(= 2) #:min-width 7) program))
            (when reason
              (for ([line (in-list (string-split reason "\n"))])
                (fprintf result "    ~a\n" line))
              (flush-output result))
            (if (eq? status expected) failing (cons program failing))))
        (report)
        (report (make-string 60 #\=))
        (report (format "pass ~a  fail ~a  of ~a in ~a min"
                        passed (- total passed)
                        total (~r (/ (seconds-since started) 60) #:precision '(= 1))))
        (report (format "result: ~a" result-path))
        (unless (null? failing)
          (report (format "\nfailing (~a):" (length failing)))
          (for ([program (in-list failing)])
            (report (string-append "  " program))))
        (null? failing)))))

;; raco test runs this, not the sample run.
(module test racket/base)

(module+ main
  (define p4-dirs '())
  (define exclude-dirs '())
  (define includes '())
  (define result-path #f)
  (define dry-run? #f)
  (define neg? #f)
  (define spec
    (command-line
     #:program "p4-typecheck.rkt"
     #:multi
     [("--p4-dir") dir "Directory of P4 programs to check (required)"
                   (set! p4-dirs (append p4-dirs (list dir)))]
     [("-e") dir "Directory of .exclude files (required)"
             (set! exclude-dirs (append exclude-dirs (list dir)))]
     [("-i") dir "P4 include directory (required)"
             (set! includes (append includes (list dir)))]
     #:once-each
     [("-n" "--neg") "Expect each program to fail, for negative tests"
                     (set! neg? #t)]
     [("-o" "--output") file "Result file (default: spec-meta-redex/p4-typecheck-pos.result, or -neg.result with --neg)"
                        (set! result-path (path->complete-path file))]
     [("-d" "--dry-run") "List the programs that would be checked, then exit"
                         (set! dry-run? #t)]
     #:args (spec) spec))
  (for ([flag (in-list '("--p4-dir" "-e" "-i"))]
        [dirs (in-list (list p4-dirs exclude-dirs includes))])
    (when (null? dirs)
      (raise-user-error 'p4-typecheck "~a is required" flag))
    (for ([dir (in-list dirs)] #:unless (directory-exists? dir))
      (raise-user-error 'p4-typecheck "~a: no such directory: ~a" flag dir)))
  (define programs (collect-programs p4-dirs exclude-dirs))
  (cond
    [dry-run?
     (for-each displayln programs)
     (eprintf "\n~a program(s)\n" (length programs))]
    [(null? programs) (displayln "nothing to do")]
    [else
     (define path (or result-path (default-result neg?)))
     (exit (if (check-all spec includes programs path neg?) 0 1))]))
