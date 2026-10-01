#lang racket/base
;; The JSON encoding of the extern wire, documented in
;; p4spec/lib/interface/spectec/ali/extern_json.ml, as jsexprs. k-run.sh prints
;; a result in the same encoding.

(require json
         racket/match
         (for-syntax racket/base))
(provide val->jsexpr
         jsexpr->val
         typ->jsexpr
         builtin-request
         extern-func-request
         extern-rel-request
         response->valres
         response->valsres)

;; val ::= ["boolV", <bool>]
;;       | ["natN", "<decimal>"] | ["intN", "<decimal>"]
;;       | ["textV", <string>]
;;       | ["strV", [[<atom>, val], ...]]
;;       | ["injV", mixop, [val, ...]]
;;       | ["tupV", [val, ...]]
;;       | ["optV", null] | ["optV", val]
;;       | ["listV", [val, ...]]
;;       | ["funcV", <id>]
;;       | ["extV", <json>]
(define (val->jsexpr val)
  (match val
    [(list 'BOOL b) (list "boolV" b)]
    [(list 'NAT n) (list "natN" (number->string n))]
    [(list 'INT i) (list "intN" (number->string i))]
    [(list 'TEXT t) (list "textV" t)]
    [(list 'STR (list (list atoms vals) ...))
     (list "strV" (for/list ([atom (in-list atoms)] [val (in-list vals)])
                    (list atom (val->jsexpr val))))]
    [(list 'INJ (list mixop vals)) (list "injV" (mixop->jsexpr mixop) (map val->jsexpr vals))]
    [(list 'TUP vals) (list "tupV" (map val->jsexpr vals))]
    [(list 'OPT '()) (list "optV" (json-null))]
    [(list 'OPT (list val)) (list "optV" (val->jsexpr val))]
    [(list 'LIST vals) (list "listV" (map val->jsexpr vals))]
    [(list 'FUNC id) (list "funcV" id)]
    [(list 'EXT json) (list "extV" json)]))

(define (jsexpr->val js)
  (match js
    [(list "boolV" (? boolean? b)) (list 'BOOL b)]
    [(list "natN" (? string? s)) (list 'NAT (decimal->integer s #:nat? #t))]
    [(list "intN" (? string? s)) (list 'INT (decimal->integer s))]
    [(list "textV" (? string? t)) (list 'TEXT t)]
    [(list "strV" (list (list (? string? atoms) jss) ...))
     (list 'STR (for/list ([atom (in-list atoms)] [js (in-list jss)])
                  (list atom (jsexpr->val js))))]
    [(list "injV" mixop (? list? jss)) (list 'INJ (list (jsexpr->mixop mixop) (map jsexpr->val jss)))]
    [(list "tupV" (? list? jss)) (list 'TUP (map jsexpr->val jss))]
    [(list "optV" (== (json-null))) (list 'OPT '())]
    [(list "optV" js) (list 'OPT (list (jsexpr->val js)))]
    [(list "listV" (? list? jss)) (list 'LIST (map jsexpr->val jss))]
    [(list "funcV" (? string? id)) (list 'FUNC id)]
    [(list "extV" json) (list 'EXT json)]
    [_ (malformed "a value" js)]))

;; Bigint.to_string's output: an optional minus sign, then digits
(define (decimal->integer s #:nat? [nat? #f])
  (unless (regexp-match? (if nat? #px"^[0-9]+$" #px"^-?[0-9]+$") s)
    (malformed (if nat? "a nat" "an int") s))
  (string->number s 10))

;; mixop ::= [[<atom>, ...], ...], which is the term's own shape
(define (mixop->jsexpr mixop)
  mixop)

(define (jsexpr->mixop js)
  (match js
    [(list (list (? string?) ...) ...) js]
    [_ (malformed "a mixop" js)]))

;; typ ::= ["natT"] | ["intT"] | ["boolT"] | ["textT"]
;;       | ["varT", <id>, [typ, ...]] | ["tupT", [typ, ...]]
;;       | ["iterT", typ, "?"|"*"] | ["funcT"]
(define (typ->jsexpr typ)
  (match typ
    ['NAT (list "natT")]
    ['INT (list "intT")]
    ['BOOL (list "boolT")]
    ['TEXT (list "textT")]
    [(list 'VAR id targs) (list "varT" id (map typ->jsexpr targs))]
    [(list 'TUP typs) (list "tupT" (map typ->jsexpr typs))]
    [(list 'ITER typ 'QUEST) (list "iterT" (typ->jsexpr typ) "?")]
    [(list 'ITER typ 'STAR) (list "iterT" (typ->jsexpr typ) "*")]
    ['FUNC (list "funcT")]))

;; request ::= {"builtin":     <id>, "targs": [typ, ...], "args": [val, ...]}
;;           | {"extern-func": <id>, "targs": [typ, ...], "args": [val, ...]}
;;           | {"extern-rel":  <id>, "args": [val, ...]}
(define (builtin-request id typs vals)
  (hasheq 'builtin id 'targs (map typ->jsexpr typs) 'args (map val->jsexpr vals)))

(define (extern-func-request id typs vals)
  (hasheq 'extern-func id 'targs (map typ->jsexpr typs) 'args (map val->jsexpr vals)))

(define (extern-rel-request id vals)
  (hasheq 'extern-rel id 'args (map val->jsexpr vals)))

;; response ::= {"ok": val}         builtin and extern-func
;;            | {"ok": [val, ...]}  extern-rel
;;            | {"fail": null}
;;            | {"error": <text>}   ffi.ml, on an OCaml exception

;; An object with key as its only field
(define-match-expander only-field
  (syntax-rules ()
    [(_ key pat) (and (hash-table (key pat)) (app hash-count 1))]))

;; A builtin or extern-func response, as a valres; an error raises.
(define (response->valres js)
  (match js
    [(only-field 'ok val) (list 'OK (jsexpr->val val))]
    [_ (response->fail js)]))

;; An extern-rel response, as a valsres; an error raises.
(define (response->valsres js)
  (match js
    [(only-field 'ok (? list? vals)) (list 'OK (map jsexpr->val vals))]
    [_ (response->fail js)]))

(define (response->fail js)
  (match js
    [(only-field 'fail (== (json-null))) 'FAIL]
    [(only-field 'error (? string? msg)) (error 'host "~a" msg)]
    [_ (malformed "a response" js)]))

(define (malformed what js)
  (error 'extern-json "expected ~a, but got ~a" what (jsexpr->string js)))
