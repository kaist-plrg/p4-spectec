#lang racket/base
;; spec-meta/2-runtime/2.0-value.watsup.

(require json
         redex/reduction-semantics
         "../1-syntax/1.0-syntax.rkt")
(provide value)

(define-extended-language value syntax
  ;; Values
  (json ::= (side-condition any_json (jsexpr? (term any_json))))
  (val ::=
       (BOOL bool)
       num
       (TEXT text)
       (STR (valField ...))
       (INJ valCase)
       (TUP (val ...))
       (OPT ()) (OPT (val))
       (LIST (val ...))
       (FUNC id)
       (EXT json))
  (valField ::= (atom val))
  (valCase ::= (mixop (val ...))))
