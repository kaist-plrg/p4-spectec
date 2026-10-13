#lang racket/base
;; The machine: the IN and FAIL rules, the notion of reduction (->redex and
;; ->ctx), and its closure ->al.

(require redex/reduction-semantics
         "4-relation.rkt"
         "5.2-eval-assign.rkt"
         "5.3-eval-exp.rkt"
         "5.4-eval-arg.rkt"
         "5.5-eval-prem.rkt"
         "5.6-eval-call-func.rkt"
         "5.7-eval-call-rel.rkt")
(provide ->redex/eval
         ->redex
         ->ctx
         ->al)

;;
;; The IN and FAIL rules
;;

(define ->redex/eval
  (reduction-relation
   al
   ;; Leaving a local context
   (--> (IN L (OK any)) (OK any)
        "in/ok")
   (--> (IN L FAIL) FAIL
        "in/fail")
   ;; FAIL passes up through every frame that does not catch it
   (--> (in-hole Fr-pass FAIL) FAIL
        "frame/fail")))

;;
;; The notion of reduction
;;

;; The rules that neither read nor write the context, on the redex r
(define ->redex
  (union-reduction-relations ->redex/eval
                             ->redex/eval-assign
                             ->redex/eval-exp
                             ->redex/eval-arg
                             ->redex/eval-prem
                             ->redex/eval-call-func
                             ->redex/eval-call-rel))

;; The rules that read G or L, or write L, on the focus triple (r G L)
(define ->ctx
  (union-reduction-relations ->ctx/eval-assign
                             ->ctx/eval-exp
                             ->ctx/eval-arg
                             ->ctx/eval-prem
                             ->ctx/eval-call-func
                             ->ctx/eval-call-rel))

;;
;; The closure, on configurations
;;

;; A step of ->redex at a redex, or of ->ctx at a redex under the layer L of
;; its innermost IN, with the name of the rule applied
(define ->al
  (union-reduction-relations
   (context-closure ->redex al (G (in-hole E (IN L F))))
   (reduction-relation
    al
    (--> (G (in-hole E (IN L (in-hole F e_r))))
         (G (in-hole E (IN L_1 (in-hole F e_1))))
         (where (_ ... (any_name (e_1 G L_1)) _ ...)
                ,(apply-reduction-relation/tag-with-names ->ctx (term (e_r G L))))
         (computed-name (term any_name))))))
