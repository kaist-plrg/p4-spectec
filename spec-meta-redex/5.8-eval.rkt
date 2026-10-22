#lang racket/base
;; The machine: the IN, seq, and FAIL rules, the notion of reduction (->term and
;; ->term+ctx), and its closure ->al.

(require redex/reduction-semantics
         "4-relation.rkt"
         "5.2-eval-assign.rkt"
         "5.3-eval-exp.rkt"
         "5.4-eval-arg.rkt"
         "5.5-eval-prem.rkt"
         "5.6-eval-call-func.rkt"
         "5.7-eval-call-rel.rkt")
(provide ->term
         ->term+ctx
         ->al)

;;
;; The IN, seq, and FAIL rules
;;

(define ->term/eval
  (reduction-relation
   al
   ;; Leaving a local context
   (--> (IN L val) val
        "in/val")
   (--> (IN L (OK (val ...))) (OK (val ...))
        "in/vals")
   (--> (IN L FAIL) FAIL
        "in/fail")
   ;; A statement sequence: once every statement before the last holds, the
   ;; last one's result
   (--> (seq (OK ...)) OK
        "seq/unit")
   (--> (seq (OK ... val)) val
        "seq/val")
   (--> (seq (OK ... (OK (val ...)))) (OK (val ...))
        "seq/vals")
   ;; FAIL passes up through every frame that does not catch it
   (--> (in-hole Fr-pass FAIL) FAIL
        "frame/fail")))

;;
;; The notion of reduction
;;

;; The rules on a term r, which neither read nor write the context
(define ->term
  (union-reduction-relations ->term/eval
                             ->term/eval-assign
                             ->term/eval-exp
                             ->term/eval-arg
                             ->term/eval-prem
                             ->term/eval-call-func
                             ->term/eval-call-rel))

;; The rules on a term and its context (r G L), which read G or L, or write L
(define ->term+ctx
  (union-reduction-relations ->term+ctx/eval-assign
                             ->term+ctx/eval-exp
                             ->term+ctx/eval-prem
                             ->term+ctx/eval-call-func
                             ->term+ctx/eval-call-rel))

;;
;; The closure, on configurations
;;

;; A step of ->term at a redex, or of ->term+ctx at a redex under the layer L
;; of its innermost IN, with the name of the rule applied
(define ->al
  (union-reduction-relations
   (context-closure ->term al (G (in-hole E (IN L F))))
   (reduction-relation
    al
    (--> (G (in-hole E (IN L (in-hole F e_r))))
         (G (in-hole E (IN L_1 (in-hole F e_1))))
         (where (_ ... (any_name (e_1 G L_1)) _ ...)
                ,(apply-reduction-relation/tag-with-names ->term+ctx (term (e_r G L))))
         (computed-name (term any_name))))))
