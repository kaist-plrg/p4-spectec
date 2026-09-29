#lang racket/base

(require json
         racket/path
         racket/runtime-path
         rackunit
         "../common/0.0-prelude.rkt"
         "../al/1-syntax.rkt"
         "../al/0-boot.rkt")

(define-runtime-path repo "../..")

(define (repo-path . parts) (simplify-path (apply build-path repo parts)))

;; Each definition is matched on its own, so a failure shows only that one.
(define (test-script path)
  (define script (boot-script path))
  (test-equal (list? script) #t)
  (for ([defn (in-list script)])
    (test-match AL-syntax defn defn))
  (test-match AL-syntax script script))

;;
;; Scripts
;;

(define examples
  (sort (for/list ([file (in-directory (repo-path "examples"))]
                   #:when (path-has-extension? file #".watsup"))
          file)
        path<?))

(test-equal (length examples) 11)
(for-each test-script examples)

(test-script (repo-path "spec"))
(test-script (repo-path "spec-meta" "al"))

;; The encoding of examples/add.watsup in test/syntax.rkt
(test-equal
 (boot-script (repo-path "examples" "add.watsup"))
 '((FUNC "main" () () INT
         ((() (VAR "i")
              ((DEBUG (TEXT "Add"))
               (LET (VAR "i") (UPCAST INT (BIN ADD (NAT 42) (NAT 77)))))))
         ())))

;;
;; P4 programs
;;

(define p4include (repo-path "p4c" "p4include"))

(for ([program (in-list '(("p4_16_samples" "action-bind.p4")
                          ("p4_16_samples" "checksum-l4-bmv2.p4")
                          ("p4_16_samples" "dash" "dash-pipeline-v1model-bmv2.p4")
                          ("p4_16_errors" "action-bind.p4")))])
  (define path (apply repo-path "p4c" "testdata" program))
  (test-match AL-syntax val (boot-p4 path #:includes (list p4include))))

;; `sexp-p4` prints `(EXT json)` with its JSON as text, as sexp.ml writes it.
;; The parser never produces EXT, so this checks the decoding on its own.
(define ext-printed
  "(INJ (((\"Ext\")) ((EXT \"{\\\"a\\\":[1,2.5,null,\\\"q\\\\\\\"\\\"],\\\"b|c\\\":true}\"))))")
(define ext-val (decode-ext (read (open-input-string ext-printed))))
(test-equal ext-val
            `(INJ ((("Ext")) ((EXT ,(hasheq 'a '(1 2.5 null "q\"")
                                            (string->symbol "b|c") #t))))))
(test-match AL-syntax val ext-val)
(test-equal (decode-ext '(LIST ((NAT 1) (TEXT "EXT")))) '(LIST ((NAT 1) (TEXT "EXT"))))

;;
;; Errors
;;

(check-exn #rx"spectec-boot sexp .*No such file"
           (λ () (boot-script (repo-path "examples" "missing.watsup"))))
(check-exn #rx"spectec-boot sexp-p4 .*"
           (λ () (boot-p4 (repo-path "examples" "add.watsup"))))
(check-exn exn:fail:read? (λ () (decode-ext '(EXT "{\"a\":"))))

(test-results)
