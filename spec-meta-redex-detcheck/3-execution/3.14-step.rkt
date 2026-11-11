#lang racket/base
;; The machine step ->al on configurations.

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
;; The IN, seq, and failure rules
;;

(define ->term/eval
  (reduction-relation
   al
   ;; Leaving a local context
   (--> (IN L val) val
        "in/val")
   (--> (IN L (OK (val ...))) (OK (val ...))
        "in/vals")
   (--> (IN L failure) failure
        "in/failure")
   ;; A statement sequence
   (--> (seq (OK ...)) OK
        "seq/unit")
   (--> (seq (OK ... val)) val
        "seq/val")
   (--> (seq (OK ... (OK (val ...)))) (OK (val ...))
        "seq/vals")
   ;; A failure passes up through every frame that does not catch it
   (--> (in-hole Fr-pass failure) failure
        "frame/failure")))

;;
;; The notion of reduction
;;

;; The rules on a term r
(define ->term
  (union-reduction-relations ->term/eval
                             ->term/eval-assign
                             ->term/eval-exp
                             ->term/eval-path
                             ->term/eval-arg
                             ->term/eval-prem
                             ->term/eval-call-func
                             ->term/eval-call-rel))

;; The rules on a term and its context, (r G L K)
(define ->term+ctx
  (union-reduction-relations ->term+ctx/eval-assign
                             ->term+ctx/eval-exp
                             ->term+ctx/eval-arg
                             ->term+ctx/eval-prem
                             ->term+ctx/eval-call-func
                             ->term+ctx/eval-call-rel))

;;
;; Refocusing
;;

(define ->focus
  (reduction-relation
   al
   ;; Into the pending position of a frame
   (--> ((in-hole Fr any_sub) G L K) (any_sub G L (Fr K))
        (side-condition (not (done? (term any_sub))))
        "focus/down")
   ;; Back into the frame, once the position is done
   (--> (done G L (Fr K)) ((in-hole Fr done) G L K)
        "focus/up")
   ;; Into an IN, whose layer is the context until the focus leaves it
   (--> ((IN L_in any_body) G L K) (any_body G L_in ((RET L) K))
        (side-condition (not (done? (term any_body))))
        "focus/in")
   ;; Out of the IN, once its body is done
   (--> (done G L_in ((RET L) K)) ((IN L_in done) G L K)
        "focus/out")))

(define done? (redex-match? al done))

;; A step: a rule on the term in focus, or a move of the focus
(define ->al
  (union-reduction-relations (context-closure ->term al (hole _ _ _))
                             ->term+ctx
                             ->focus))
