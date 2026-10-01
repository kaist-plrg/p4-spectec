#lang racket/base
;; The machine: the IN and FAIL rules, the notion of reduction (->redex and
;; ->ctx), its closure ->al, and a driver that computes ->al's steps.

(require racket/list
         racket/match
         "../common/0.0-prelude.rkt"
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
         ->al
         focus-steps
         (struct-out machine)
         frames-of
         al-machine
         cross-check?
         check-conf?
         final?
         step
         step/rule
         run
         run/trace)

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

;; Every (r_1 G L_1) that ->redex (with L_1 = L) or ->ctx gives for (r G L)
(define (focus-steps triple)
  (map cadr (focus-steps/names ->redex ->ctx triple)))

;; The same, each with the name of its rule
(define (focus-steps/names ->redex ->ctx triple)
  (match-define (list r G L) triple)
  (append
   (for/list ([name+r_1 (in-list (apply-reduction-relation/tag-with-names ->redex r))])
     (list (car name+r_1) (list (cadr name+r_1) G L)))
   (apply-reduction-relation/tag-with-names ->ctx triple)))

;;
;; The closure, on configurations
;;

(define ->al
  (reduction-relation
   al
   (--> (G (in-hole E (IN L (in-hole F e_r))))
        (G (in-hole E (IN L_1 (in-hole F e_1))))
        (where (_ ... (e_1 G L_1) _ ...) ,(focus-steps (term (e_r G L))))
        "closure")))

;;
;; The driver
;;
;; On (G (IN L e)), it descends to the redex through the one pending
;; evaluation position of each node, applies the rules to it once, and plugs
;; the contractum and the new layer back. It fails on a node with two pending
;; positions, on a redex that two rules apply to, and when it is stuck.

;; frames maps a node to its one-level decompositions (in-hole Fr any), as
;; (Fr . subterm) pairs. The driver's steps are checked against ->al, if any.
(struct machine (frames ->redex ->ctx ->al))

(define-syntax-rule (frames-of lang)
  (let ([decompose (redex-match lang (in-hole Fr any_sub))])
    (λ (t)
      (for/list ([m (in-list (or (decompose t) '()))])
        (cons (binding m 'Fr) (binding m 'any_sub))))))

(define (binding m name)
  (for/first ([b (in-list (match-bindings m))] #:when (eq? (bind-name b) name))
    (bind-exp b)))

(define al-machine (machine (frames-of al) ->redex ->ctx ->al))

;; Whether to check every step against ->al. That applies the rules a second
;; time, so their side effects happen twice.
(define cross-check? (make-parameter #f))

;; Whether to check every configuration against conf
(define check-conf? (make-parameter #f))

(define conf? (redex-match? al conf))

;; res and done, by their outer shape. Matching the nonterminals goes through
;; Redex's memo, where the subterms along a deep path collide, each lookup
;; then costing a deep equal?. check-conf? checks them precisely.
(define (res? t)
  (match t
    [(or 'OK 'FAIL (list 'OK _)) #t]
    [_ #f]))

(define (done? t)
  (match t
    [(list 'IN _ 'OK) #t]
    [_ (res? t)]))

;; (G (IN L res))
(define (final? conf)
  (match conf
    [(list _ (list 'IN _ body)) (res? body)]
    [_ #f]))

;; The configuration after conf and the name of the rule applied, or #f and #f
;; if conf is final
(define (step/rule conf #:machine [m al-machine])
  (when (and (check-conf?) (not (conf? conf)))
    (error 'step "not a configuration (G e), with e: ~e" (conf-term conf)))
  (define-values (next rule)
    (match conf
      [(list _ (list 'IN _ (? res?))) (values #f #f)]
      [(list G (list 'IN L body))
       (define-values (r L_r plug-r) (descend m body L))
       (define-values (r_1 L_1 rule) (reduce-redex m r G L_r))
       (define-values (body_1 L_body) (plug-r r_1 L_1))
       (values (list G (list 'IN L_body body_1)) rule)]
      [_ (error 'step "not a configuration (G (IN L e)), with (IN L e): ~e"
                (conf-term conf))]))
  (when (and (cross-check?) (machine-->al m))
    (cross-check m conf next))
  (values next rule))

;; The machine term of a configuration, for error messages that leave out G
(define (conf-term conf)
  (if (and (list? conf) (= (length conf) 2)) (cadr conf) conf))

;; The redex in t, which is not done and has the layer L as its innermost IN's,
;; the redex's own layer, and (plug r_1 L_1). plug puts the contractum r_1 in
;; place of the redex, and L_1 in place of the redex's layer, and returns t's
;; new term and L's replacement.
(define (descend m t L)
  (define (here) (values t L values))
  (match t
    [(list 'IN L_t body)
     (cond
       [(done? body) (here)]
       [else
        (define-values (r L_r plug-r) (descend m body L_t))
        (values r L_r
                (λ (r_1 L_1)
                  (define-values (body_1 L_t1) (plug-r r_1 L_1))
                  (values (list 'IN L_t1 body_1) L)))])]
    [_
     (match (pending m t)
       ['() (here)]
       [(list (cons Fr sub))
        (define-values (r L_r plug-r) (descend m sub L))
        (values r L_r
                (λ (r_1 L_1)
                  (define-values (sub_1 L_2) (plug-r r_1 L_1))
                  (values (plug Fr sub_1) L_2)))]
       [positions
        (error 'step "~a pending positions, with subterms ~e, in ~e"
               (length positions) (map cdr positions) t)])]))

;; The evaluation positions of node t whose subterms are not done
(define (pending m t)
  (remove-duplicates
   (for/list ([Fr+sub (in-list ((machine-frames m) t))]
              #:unless (done? (cdr Fr+sub)))
     Fr+sub)))

;; The contractum of redex r under G and L, the new layer, and the rule's name
(define (reduce-redex m r G L)
  (match (focus-steps/names (machine-->redex m) (machine-->ctx m) (list r G L))
    [(list (list rule triple))
     (match triple
       [(list r_1 G_1 L_1)
        #:when (equal? G_1 G)
        (values r_1 L_1 rule)]
       [_ (error 'step "rule ~a gives no triple (r G L) with the same G, on ~e" rule r)])]
    ['() (error 'step "stuck: no rule applies to ~e" r)]
    [results (error 'step "rules ~a all apply to ~e" (map car results) r)]))

(define (cross-check m conf next)
  (define expected (if next (list next) '()))
  (define actual
    (parameterize ([relation-coverage '()])
      (map cadr (apply-reduction-relation/tag-with-names (machine-->al m) conf))))
  (unless (equal? actual expected)
    (error 'step "the driver's step differs from ->al's on ~e:\n driver: ~e\n ->al: ~e"
           (conf-term conf) (map conf-term expected) (map conf-term actual))))

;; The configuration after conf, or #f if conf is final
(define (step conf #:machine [m al-machine])
  (define-values (next _rule) (step/rule conf #:machine m))
  next)

;; The final configuration that conf reduces to
(define (run conf #:machine [m al-machine])
  (define-values (next _rule) (step/rule conf #:machine m))
  (if next (run next #:machine m) conf))

;; The final configuration, and the names of the rules applied, in order
(define (run/trace conf #:machine [m al-machine])
  (let loop ([conf conf] [rules '()])
    (define-values (next rule) (step/rule conf #:machine m))
    (if next
        (loop next (cons rule rules))
        (values conf (reverse rules)))))
