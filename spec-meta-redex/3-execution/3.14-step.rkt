#lang racket/base
;; The machine: the IN, seq, and unmatch rules, the notion of reduction
;; (->term and ->term+ctx), and its closure ->al.

(require redex/reduction-semantics
         "3.03-machine.rkt"
         "3.07-eval-assign.rkt"
         "3.08-eval-exp.rkt"
         "3.09-eval-path.rkt"
         "3.10-eval-arg.rkt"
         "3.11-eval-prem.rkt"
         "3.12-eval-call-func.rkt"
         "3.13-eval-call-rel.rkt")
(provide ->term
         ->term+ctx
         ->al)

;;
;; The IN, seq, and unmatch rules
;;

(define ->term/eval
  (reduction-relation
   al
   ;; Leaving a local context
   (--> (IN G L val) val
        "in/val")
   (--> (IN G L (OK (val ...))) (OK (val ...))
        "in/vals")
   (--> (IN G L UNMATCH) UNMATCH
        "in/unmatch")
   ;; A statement sequence: once every statement before the last holds, the
   ;; last one's result
   (--> (seq (OK ...)) OK
        "seq/unit")
   (--> (seq (OK ... val)) val
        "seq/val")
   (--> (seq (OK ... (OK (val ...)))) (OK (val ...))
        "seq/vals")
   ;; UNMATCH passes up through every frame that does not catch it
   (--> (in-hole Fr-pass UNMATCH) UNMATCH
        "frame/unmatch")))

;;
;; The notion of reduction
;;

;; The rules on a term r, which neither read nor write the context
(define ->term
  (union-reduction-relations ->term/eval
                             ->term/eval-assign
                             ->term/eval-exp
                             ->term/eval-path
                             ->term/eval-arg
                             ->term/eval-prem
                             ->term/eval-call-func
                             ->term/eval-call-rel))

;; The rules on a term under its innermost IN, (IN G L (in-hole F r)), which
;; read G or L, or write L. F never crosses an IN, so the IN is the innermost.
(define ->term+ctx
  (union-reduction-relations ->term+ctx/eval-assign
                             ->term+ctx/eval-exp
                             ->term+ctx/eval-arg
                             ->term+ctx/eval-prem
                             ->term+ctx/eval-call-func
                             ->term+ctx/eval-call-rel))

;;
;; The closure
;;

;; A step of either at its place in the term
(define ->al
  (context-closure (union-reduction-relations ->term ->term+ctx) al E))
