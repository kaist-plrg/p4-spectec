#lang racket/base
;; The extern wire's codec, and the transport to the OCaml host. The host
;; writes its diagnostics to file descriptor 2, so they show in the output.

(require json
         racket/list
         racket/match
         racket/runtime-path
         rackunit
         "../common/0.0-prelude.rkt"
         "../common/0.2-extern-json.rkt"
         "../common/0.3-extern-ffi.rkt"
         "../common/4-relation.rkt"
         "../al/0-boot.rkt"
         "../al/6-entry.rkt"
         "machine.rkt")

(define-runtime-path repo "../..")

;; v, after a trip through the codec and JSON text
(define (round-trip v)
  (jsexpr->val (string->jsexpr (jsexpr->string (val->jsexpr v)))))

;;
;; The codec
;;

(define every-kind
  (term (TUP ((BOOL #t) (BOOL #f) (NAT 0) (NAT 12345678901234567890) (INT -3) (TEXT "é\"\n")
              (STR (("X" (NAT 1)) ("Y" (LIST ()))))
              (INJ ((("RECT") () ()) ((NAT 2) (NAT 3)))) (INJ ((("DOT")) ()))
              (OPT ()) (OPT ((OPT ()))) (LIST ((INT 1) (INT 2)))
              (FUNC "f") (EXT #hasheq((a . (1 null)) (b . "c")))))))

(test-equal (round-trip every-kind) every-kind)
(test-equal (val->jsexpr (term (OPT ((OPT ()))))) (list "optV" (list "optV" 'null)))

(test-equal (map typ->jsexpr
                 (term (NAT INT BOOL TEXT FUNC (VAR "map" (TEXT (ITER NAT STAR)))
                            (TUP ((ITER BOOL QUEST) (TUP ()))))))
            '(("natT") ("intT") ("boolT") ("textT") ("funcT")
              ("varT" "map" (("textT") ("iterT" ("natT") "*")))
              ("tupT" (("iterT" ("boolT") "?") ("tupT" ())))))

(test-equal (builtin-request "rev_" (term (NAT)) (term ((LIST ((NAT 1))))))
            (hasheq 'builtin "rev_" 'targs '(("natT")) 'args '(("listV" (("natN" "1"))))))
(test-equal (extern-func-request "f" '() (term ((BOOL #t))))
            (hasheq 'extern-func "f" 'targs '() 'args '(("boolV" #t))))
(test-equal (extern-rel-request "R" (term ((TEXT "a"))))
            (hasheq 'extern-rel "R" 'args '(("textV" "a"))))

(test-equal (response->valres (hasheq 'ok '("natN" "3"))) (term (OK (NAT 3))))
(test-equal (response->valres (hasheq 'fail 'null)) 'FAIL)
(test-equal (response->valsres (hasheq 'ok '(("natN" "3") ("intN" "-3"))))
            (term (OK ((NAT 3) (INT -3)))))
(test-equal (response->valsres (hasheq 'ok '())) (term (OK ())))
(test-equal (response->valsres (hasheq 'fail 'null)) 'FAIL)
;; A runtime error is not FAIL.
(check-exn #rx"^host: Builtin error: arity mismatch$"
           (λ () (response->valres (hasheq 'error "Builtin error: arity mismatch"))))
(check-exn #rx"^host: ~a$" (λ () (response->valsres (hasheq 'error "~a"))))

;; Malformed values and responses raise.
(for ([js (in-list '(("natN" "-1") ("natN" "1.0") ("intN" "#x10") ("intN" 3) ("boolV" "true")
                     ("injV" (("A" 1)) ()) ("strV" (("X"))) ("optV") ("textV" "a" "b") ("nope")))])
  (check-exn #rx"^extern-json: expected" (λ () (jsexpr->val js))))
(for ([js (in-list (list (hasheq 'ok '("natN" "1") 'fail 'null) (hasheq 'fail #f) (hasheq)
                         (hasheq 'error 3) '("natN" "1")))])
  (check-exn #rx"^extern-json: expected a response" (λ () (response->valres js))))
(check-exn #rx"^extern-json: expected a response"
           (λ () (response->valsres (hasheq 'ok "natN"))))

;; Values from sexp-p4 survive a round trip.
(define p4-programs
  (for/list ([program (in-list '(("p4_16_samples" "action-bind.p4")
                                 ("p4_16_samples" "checksum-l4-bmv2.p4")
                                 ("p4_16_samples" "dash" "dash-pipeline-v1model-bmv2.p4")
                                 ("p4_16_errors" "action-bind.p4")))])
    (boot-p4 (apply build-path repo "p4c" "testdata" program)
             #:includes (list (build-path repo "p4c" "p4include")))))

(for ([v (in-list p4-programs)])
  (test-equal (round-trip v) v))

;;
;; The transport
;;

(check-exn #rx"^host-eval: host-spec is not set"
           (λ () (host-eval "{}")))
;; The first runner the host builds fails, and the next one does not.
(check-exn #rx"^host-eval: the host cannot build a runner for .*no-such-file.watsup$"
           (λ () (parameterize ([host-spec (build-path repo "no-such-file.watsup")])
                   (host-eval "{}"))))

;; Every call makes a fresh id, including calls that only a defined function
;; makes, and caching is on.
(define fresh-text #<<EOF
builtin dec $fresh_typeId() : text

dec $fresh() : text
def $fresh() = $fresh_typeId()

dec $main() : (text, text, text, text)
def $main() = ($fresh_typeId(), $fresh_typeId(), $fresh(), $fresh())
EOF
  )
(define fresh-path (text-file fresh-text))

;; $main's four ids, under spec as host-spec
(define (fresh-ids spec)
  (parameterize ([host-spec spec]
                 [current-error-port (open-output-string)])
    (match (entry (boot-script fresh-path))
      [(list 'OK (list 'TUP (list (list 'TEXT ids) ...))) ids])))

(check-true (caching-enabled?))
(define ids-1 (fresh-ids fresh-path))
(test-equal (length (remove-duplicates ids-1)) 4)
;; Another host-spec builds another runner, and the counter keeps counting.
(define ids-2 (fresh-ids (text-file fresh-text)))
(test-equal (length (remove-duplicates (append ids-1 ids-2))) 8)

;; A malformed request gives OCaml's message.
(parameterize ([host-spec fresh-path])
  (test-equal (string->jsexpr (host-eval "{\"bogus\": 1}"))
              (hasheq 'error "Extern error: request has none of the fields builtin, extern-func, extern-rel"))
  (check-exn #rx"^host: JSON error: .*Invalid token 'not json'"
             (λ () (response->valres (string->jsexpr (host-eval "not json")))))
  ;; Values from sexp-p4 survive a trip through the host, as $rev_'s argument.
  (for ([v (in-list p4-programs)])
    (test-equal (host-call-builtin-func "rev_" (term ((VAR "X" ()))) (list (term (LIST (,v)))))
                (term (OK (LIST (,v)))))))
