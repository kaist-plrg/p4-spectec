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
         run/trace
         conf->cursor
         cursor->conf
         cursor-step)

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
;; It holds (G (IN L e)) as a cursor at the last contractum. A step moves the
;; cursor to the redex, applies the rules to it once, and puts the contractum
;; and the new layer at the cursor. To reach the redex, the cursor climbs past
;; the nodes whose subterm is done, and then descends through the one pending
;; evaluation position of each node. So a step matches only the nodes it
;; enters, not the whole path from the root. It fails on a node with two
;; pending positions, on a redex that two rules apply to, and when it is stuck.

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
  (define-values (next rule) (cursor-step (conf->cursor conf) #:machine m))
  (values (and next (cursor->conf next)) rule))

;; The machine term of a configuration, for error messages that leave out G
(define (conf-term conf)
  (if (and (list? conf) (= (length conf) 2)) (cadr conf) conf))

;; (G (IN L_root body)) with the subterm t of body in focus. L is the layer of
;; t's innermost IN, and path holds t's ancestors in body, innermost first.
(struct cursor (G path t L))
;; The ancestor (in-hole Fr t)
(struct path-fr (Fr))
;; The ancestor (IN L t), whose own innermost IN has the layer L_outer
(struct path-in (L_outer))

(define (conf->cursor conf)
  (match conf
    [(list G (list 'IN L body)) (cursor G '() body L)]
    [_ (error 'step "not a configuration (G (IN L e)), with (IN L e): ~e"
              (conf-term conf))]))

(define (cursor->conf c)
  (match-define (cursor G path t L) c)
  (let loop ([path path] [t t] [L L])
    (match path
      ['() (list G (list 'IN L t))]
      [(cons (path-fr Fr) path_1) (loop path_1 (plug Fr t) L)]
      [(cons (path-in L_outer) path_1) (loop path_1 (list 'IN L t) L_outer)])))

;; The cursor after the step of c's configuration and the name of the rule
;; applied, or #f and #f if the configuration is final
(define (cursor-step c #:machine [m al-machine])
  (define conf (and (or (check-conf?) (cross-check?)) (cursor->conf c)))
  (when (and (check-conf?) (not (conf? conf)))
    (error 'step "not a configuration (G e), with e: ~e" (conf-term conf)))
  (define-values (next rule)
    (match (settle m c)
      [#f (values #f #f)]
      [(cursor G path r L)
       (define-values (r_1 L_1 rule) (reduce-redex m r G L))
       (values (cursor G path r_1 L_1) rule)]))
  (when (and (cross-check?) (machine-->al m))
    (cross-check m conf (and next (cursor->conf next))))
  (values next rule))

;; c moved to the redex, or #f if c's configuration is final. A node whose
;; pending subterm is not done keeps that position, so the cursor climbs only
;; past done subterms.
(define (settle m c)
  (match-define (cursor G path t L) c)
  (let climb ([path path] [t t] [L L])
    (cond
      [(and (null? path) (res? t)) #f]
      [(or (null? path) (not (done? t))) (descend m G path t L)]
      [else
       (match path
         [(cons (path-fr Fr) path_1) (climb path_1 (plug Fr t) L)]
         [(cons (path-in L_outer) path_1) (climb path_1 (list 'IN L t) L_outer)])])))

;; The cursor at the redex in t, which is not done unless it is the root's body
(define (descend m G path t L)
  (match t
    [(list 'IN L_t body)
     (if (done? body)
         (cursor G path t L)
         (descend m G (cons (path-in L) path) body L_t))]
    [_
     (match (pending m t)
       ['() (cursor G path t L)]
       [(list (cons Fr sub)) (descend m G (cons (path-fr Fr) path) sub L)]
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
  (let loop ([c (conf->cursor conf)])
    (define-values (next _rule) (cursor-step c #:machine m))
    (if next (loop next) (cursor->conf c))))

;; The final configuration, and the names of the rules applied, in order
(define (run/trace conf #:machine [m al-machine])
  (let loop ([c (conf->cursor conf)] [rules '()])
    (define-values (next rule) (cursor-step c #:machine m))
    (if next
        (loop next (cons rule rules))
        (values (cursor->conf c) (reverse rules)))))
