#lang racket/base
;; Helpers that run machine terms through the driver, and the driver's tests.

(require racket/match
         rackunit
         "../common/0.0-prelude.rkt"
         "../al/5-eval.rkt")
(provide run-in
         eval-in
         trace-in
         start-coverage
         check-coverage)

;; Runs e under G and L to (G (IN L_1 res)), with the cross-check and the
;; grammar check on, and returns (list res L_1).
(define (run-in G L e)
  (match (checked (λ () (run (list G (list 'IN L e)))))
    [(list _ (list 'IN L_1 res)) (list res L_1)]))

;; The result of e under G and L
(define (eval-in G L e)
  (car (run-in G L e)))

;; The names of the rules that running e under G and L applies, in order
(define (trace-in G L e)
  (define-values (_final rules)
    (checked (λ () (run/trace (list G (list 'IN L e))))))
  rules)

(define (checked thunk)
  (parameterize ([cross-check? #t]
                 [check-conf? #t])
    (thunk)))

;; Records from now on how often each rule of rels is used by the driver.
(define (start-coverage . rels)
  (define coverage (for/list ([rel (in-list rels)]) (make-coverage rel)))
  (relation-coverage (append coverage (relation-coverage)))
  coverage)

;; Checks that every rule recorded in coverage was used.
(define (check-coverage coverage)
  (check-equal? (for*/list ([cov (in-list coverage)]
                            [case (in-list (covered-cases cov))]
                            #:when (zero? (cdr case)))
                  (car case))
                '()
                "rules never used"))

(module+ test
  (require "../al/3-context.rkt"
           "../al/4-relation.rkt")

  (define coverage (start-coverage ->redex/eval))

  (define-term G-vals {TYP () REL () FUNC () VAL ((("g" ()) (NAT 0)))})
  (define-term L-x {TYP () REL () FUNC () VAL ((("x" ()) (NAT 1)))})
  (define-term L-y {TYP () REL () FUNC () VAL ((("y" ()) (NAT 2)))})
  (define-term L-empty (empty-layer))

  (define (conf-of L e) (list (term G-vals) (list 'IN L e)))

  ;;
  ;; Final configurations
  ;;

  (check-true (final? (conf-of (term L-x) (term (OK (NAT 1))))))
  (check-true (final? (conf-of (term L-x) (term FAIL))))
  (check-true (final? (conf-of (term L-x) (term OK))))
  (check-false (final? (conf-of (term L-x) (term (VAR "x")))))
  (check-false (final? (conf-of (term L-x) (term (IN L-y OK)))))
  (check-false (step (conf-of (term L-x) (term (OK (NAT 1))))))
  (check-false (step (conf-of (term L-x) (term OK))))

  ;; (IN L OK) stays put: no rule applies to it, and its parent is the redex.
  (test-equal (apply-reduction-relation ->redex (term (IN L-y OK))) '())
  (test-equal (apply-reduction-relation ->al (conf-of (term L-x) (term (IN L-y OK)))) '())
  (check-exn #rx"stuck: no rule applies to '\\(IN .* OK\\)"
             (λ () (step (conf-of (term L-x) (term (IN L-y OK))))))
  (check-exn #rx"stuck: no rule applies to '\\(UN NOT \\(IN .* OK\\)\\)"
             (λ () (step (conf-of (term L-x) (term (UN NOT (IN L-y OK)))))))

  ;;
  ;; Local contexts
  ;;

  ;; A variable is looked up in the innermost IN's layer only.
  (test-equal (run-in (term G-vals) (term L-x) (term (UN MINUS (IN L-y (VAR "y")))))
              (term ((OK (INT -2)) L-x)))
  (test-equal (eval-in (term G-vals) (term L-y) (term (UN MINUS (IN L-x (VAR "y")))))
              'FAIL)
  (test-equal (eval-in (term G-vals) (term L-x) (term (VAR "g"))) 'FAIL)
  (test-equal (trace-in (term G-vals) (term L-x)
                        (term (TUP ((IN L-y (IN L-empty (NAT 1))) (VAR "x")))))
              '("eval-exp/literal/number" "in/ok" "in/ok" "eval-exp/variable" "eval-exp/tuple"))

  ;;
  ;; FAIL propagation
  ;;

  ;; Through two frames of each kind and two IN nodes, and no further operand
  ;; of the outer tuple is evaluated.
  (define-term e-fail
    (TUP ((UN NOT (IN L-y (UN MINUS (IN L-x (TUP ((NAT 1) (VAR "y")))))))
          (BIN DIV (NAT 1) (NAT 0)))))

  (test-equal (run-in (term G-vals) (term L-empty) (term e-fail)) (term (FAIL L-empty)))
  (test-equal (trace-in (term G-vals) (term L-empty) (term e-fail))
              '("eval-exp/literal/number" "eval-exp/variable/fail"
                "frame/fail" "in/fail" "frame/fail" "in/fail" "frame/fail" "frame/fail"))

  ;;
  ;; The driver's checks
  ;;

  (define (machine-with #:frames [frames (machine-frames al-machine)]
                        #:redex [->redex ->redex]
                        #:ctx [->ctx ->ctx]
                        #:al [->al #f])
    (machine frames ->redex ->ctx ->al))

  ;; Two rules apply to one redex.
  (define overlapping
    (machine-with
     #:redex (union-reduction-relations
              ->redex
              (reduction-relation
               al
               (--> (UN NOT (OK (BOOL b))) (OK (BOOL b)) "test/overlap")))))

  (check-exn #rx"rules .* all apply to '\\(UN NOT \\(OK \\(BOOL #t\\)\\)\\)"
             (λ () (run (conf-of (term L-x) (term (UN NOT (BOOL #t)))) #:machine overlapping)))
  (check-exn #rx"eval-exp/unary/boolean"
             (λ () (run (conf-of (term L-x) (term (UN NOT (BOOL #t)))) #:machine overlapping)))
  (check-exn #rx"test/overlap"
             (λ () (run (conf-of (term L-x) (term (UN NOT (BOOL #t)))) #:machine overlapping)))

  ;; Two positions of one node are pending.
  (define-extended-language al-pair al
    (e ::= .... (PAIR e e))
    (Fr ::= .... (PAIR hole e) (PAIR e hole)))

  (define pairs (machine-with #:frames (frames-of al-pair)))

  (check-exn #rx"2 pending positions, with subterms '\\(\\(NAT 1\\) \\(NAT 2\\)\\)"
             (λ () (run (conf-of (term L-x) (term (PAIR (NAT 1) (NAT 2)))) #:machine pairs)))
  ;; One pending position is evaluated, and then the node is the redex.
  (check-exn #rx"stuck: no rule applies to '\\(PAIR \\(OK \\(NAT 1\\)\\) \\(OK \\(BOOL #t\\)\\)\\)"
             (λ () (run (conf-of (term L-x) (term (PAIR (OK (NAT 1)) (UN NOT (BOOL #f)))))
                        #:machine pairs)))

  ;; A stuck term reports itself.
  (check-exn #rx"stuck: no rule applies to '\\(UN NOT \\(IN .* OK\\)\\)"
             (λ () (run (conf-of (term L-x) (term (TUP ((UN NOT (IN L-y OK)) (NAT 2))))))))

  ;; A step that ->al does not take fails the cross-check.
  (define-extended-language al-wrap al
    (Fr ::= .... (WRAP hole)))

  (define wraps (machine-with #:frames (frames-of al-wrap) #:al ->al))

  (check-equal? (step (conf-of (term L-x) (term (WRAP (NAT 1)))) #:machine wraps)
                (conf-of (term L-x) (term (WRAP (OK (NAT 1))))))
  (check-exn #rx"the driver's step differs from ->al's"
             (λ ()
               (parameterize ([cross-check? #t])
                 (step (conf-of (term L-x) (term (WRAP (NAT 1)))) #:machine wraps))))

  ;; A rule that writes L updates the innermost IN's layer.
  (define binding
    (machine-with
     #:ctx (reduction-relation
            al
            (--> ((VAR id) G L) ((OK (NAT 0)) G L_1)
                 (where L_1 (add-varr L (id ()) (NAT 0)))
                 "test/bind"))))

  (test-equal (run (conf-of (term L-empty) (term (TUP ((IN L-y (VAR "b")) (VAR "a")))))
                   #:machine binding)
              (conf-of (term {TYP () REL () FUNC () VAL ((("a" ()) (NAT 0)))})
                       (term (OK (TUP ((NAT 0) (NAT 0)))))))

  ;; A rule that writes G fails.
  (define writing-G
    (machine-with
     #:ctx (reduction-relation
            al
            (--> ((VAR id) G L) ((OK (NAT 0)) L L)
                 "test/write-g"))))

  (check-exn #rx"rule test/write-g gives no triple \\(r G L\\) with the same G"
             (λ () (run (conf-of (term L-x) (term (VAR "x"))) #:machine writing-G)))

  ;; Malformed configurations
  (check-exn #rx"not a configuration \\(G \\(IN L e\\)\\)"
             (λ () (step (list (term G-vals) (term (VAR "x"))))))
  (check-exn #rx"not a configuration \\(G e\\)"
             (λ ()
               (parameterize ([check-conf? #t])
                 (step (conf-of (term L-x) (term (FOO)))))))
  (check-exn #rx"stuck: no rule applies to '\\(FOO\\)"
             (λ () (step (conf-of (term L-x) (term (FOO))))))

  (check-coverage coverage))
