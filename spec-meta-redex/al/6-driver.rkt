#lang racket/base
;; A driver that computes ->al's steps.
;;
;; It holds (G (IN L e)) as a cursor at the last contractum. A step moves the
;; cursor to the redex, applies ->redex to it and ->ctx to its focus triple,
;; and puts the contractum and the new layer at the cursor. To reach the redex,
;; the cursor climbs past the nodes whose subterm is done, and then descends
;; through the one pending evaluation position of each node. So a step matches
;; only the nodes it enters, not the whole path from the root. It fails on a
;; node with two pending positions, and on a redex to which no rule, or more
;; than one, applies.

(require racket/list
         racket/match
         redex/reduction-semantics
         "4-relation.rkt"
         "5-eval.rkt")
(provide run
         run/trace
         cross-check?
         check-conf?)

;; Whether to check every step against ->al. That applies the rules a second
;; time, so their side effects happen twice.
(define cross-check? (make-parameter #f))

;; Whether to check every configuration against conf
(define check-conf? (make-parameter #f))

(define conf? (redex-match? al conf))
(define res? (redex-match? al res))
(define done? (redex-match? al done))

;; The one-level decompositions (in-hole Fr t_sub) of a node t
(define decompose (redex-match al (in-hole Fr any_sub)))

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
(define (cursor-step c)
  (define conf (and (or (check-conf?) (cross-check?)) (cursor->conf c)))
  (when (and (check-conf?) (not (conf? conf)))
    (error 'step "not a configuration (G e), with e: ~e" (conf-term conf)))
  (define-values (next rule)
    (match (settle c)
      [#f (values #f #f)]
      [(cursor G path r L)
       (define-values (r_1 L_1 rule) (reduce-redex r G L))
       (values (cursor G path r_1 L_1) rule)]))
  (when (cross-check?)
    (cross-check conf rule (and next (cursor->conf next))))
  (values next rule))

;; c moved to the redex, or #f if c's configuration is final. A node whose
;; pending subterm is not done keeps that position, so the cursor climbs only
;; past done subterms.
(define (settle c)
  (match-define (cursor G path t L) c)
  (let climb ([path path] [t t] [L L])
    (cond
      [(and (null? path) (res? t)) #f]
      [(or (null? path) (not (done? t))) (descend G path t L)]
      [else
       (match path
         [(cons (path-fr Fr) path_1) (climb path_1 (plug Fr t) L)]
         [(cons (path-in L_outer) path_1) (climb path_1 (list 'IN L t) L_outer)])])))

;; The cursor at the redex in t, which is not done unless it is the root's body
(define (descend G path t L)
  (match t
    [(list 'IN L_t body)
     (if (done? body)
         (cursor G path t L)
         (descend G (cons (path-in L) path) body L_t))]
    [_
     (match (pending t)
       ['() (cursor G path t L)]
       [(list (cons Fr sub)) (descend G (cons (path-fr Fr) path) sub L)]
       [positions
        (error 'step "~a pending positions, with subterms ~e, in ~e"
               (length positions) (map cdr positions) t)])]))

;; The evaluation positions of node t whose subterms are not done, as
;; (Fr . subterm) pairs
(define (pending t)
  (remove-duplicates
   (for/list ([m (in-list (or (decompose t) '()))]
              #:unless (done? (binding m 'any_sub)))
     (cons (binding m 'Fr) (binding m 'any_sub)))))

(define (binding m name)
  (for/first ([b (in-list (match-bindings m))] #:when (eq? (bind-name b) name))
    (bind-exp b)))

;; The contractum of redex r under G and L, the new layer, and the rule's name.
;; G stays as it is.
(define (reduce-redex r G L)
  (define steps
    (remove-duplicates
     (append (for/list ([step (in-list (apply-reduction-relation/tag-with-names ->redex r))])
               (match-define (list rule r_1) step)
               (list rule r_1 L))
             (for/list ([step (in-list (apply-reduction-relation/tag-with-names
                                        ->ctx (list r G L)))])
               (match-define (list rule (list r_1 _ L_1)) step)
               (list rule r_1 L_1)))))
  (match steps
    [(list (list rule r_1 L_1)) (values r_1 L_1 rule)]
    ['() (error 'step "stuck: no rule applies to ~e" r)]
    [_ (error 'step "~a rules apply to ~e: ~a" (length steps) r (map car steps))]))

;; Checks that the step from conf to next by rule is the one step of ->al
(define (cross-check conf rule next)
  (define expected (if next (list (list rule next)) '()))
  (define actual
    (remove-duplicates
     (parameterize ([relation-coverage '()])
       (apply-reduction-relation/tag-with-names ->al conf))))
  (unless (equal? actual expected)
    (error 'step "the driver's step differs from ->al's on ~e:\n driver: ~e\n ->al: ~e"
           (conf-term conf)
           (for/list ([step expected]) (list (car step) (conf-term (cadr step))))
           (for/list ([step actual]) (list (car step) (conf-term (cadr step)))))))

;; The final configuration that conf reduces to
(define (run conf)
  (let loop ([c (conf->cursor conf)])
    (define-values (next _rule) (cursor-step c))
    (if next (loop next) (cursor->conf c))))

;; The final configuration, and the names of the rules applied, in order
(define (run/trace conf)
  (let loop ([c (conf->cursor conf)] [rules '()])
    (define-values (next rule) (cursor-step c))
    (if next
        (loop next (cons rule rules))
        (values (cursor->conf c) (reverse rules)))))
