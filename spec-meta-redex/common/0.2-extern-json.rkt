#lang racket/base
;; The JSON encoding of the extern wire, documented in
;; p4spec/lib/interface/spectec/ali/extern_json.ml, as jsexprs. k-run.sh prints
;; a result in the same encoding.

(require json
         racket/match)
(provide val->jsexpr)

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

;; mixop ::= [[<atom>, ...], ...], which is the term's own shape
(define (mixop->jsexpr mixop)
  mixop)
