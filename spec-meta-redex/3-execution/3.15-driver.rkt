#lang racket/base
;; A driver that computes ->al's steps.
;;
;; It holds (G (IN L e)) as a cursor at the last contractum. A step moves the
;; cursor to the redex, applies ->term to it, or ->term+ctx to (r G L) if no
;; rule of ->term applies, and puts the contractum and the new layer at the
;; cursor. To reach the redex, the cursor climbs past the nodes whose subterm
;; is done, and then descends through the one pending evaluation position of
;; each node. So a step matches only the nodes it enters, not the whole path
;; from the root. It fails on a node with two pending positions, and on a
;; redex to which no rule applies.

(require racket/match
         redex/reduction-semantics
         "3.03-machine.rkt"
         "3.14-step.rkt")
(provide run)

(define res? (redex-match? al res))
(define done? (redex-match? al done))

;; The one-level decompositions (in-hole Fr t_sub) of a node t, as
;; (Fr . t_sub) pairs
(define decompose
  (term-match al [(in-hole Fr any_sub) (cons (term Fr) (term any_sub))]))

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
    [_ (error 'run "not a configuration (G (IN L e)), with (IN L e): ~e" conf)]))

(define (cursor->conf c)
  (match c
    [(cursor G '() t L) (list G (list 'IN L t))]
    [_ (cursor->conf (up c))]))

;; c with its innermost ancestor rebuilt around its subterm, and in focus
(define (up c)
  (match c
    [(cursor G (cons (path-fr Fr) path) t L) (cursor G path (plug Fr t) L)]
    [(cursor G (cons (path-in L_outer) path) t L) (cursor G path (list 'IN L t) L_outer)]))

;; c moved to the next redex, or #f if c's configuration is final. A node whose
;; pending subterm is not done keeps that position, so the cursor climbs only
;; past done subterms.
(define (next-redex c)
  (match c
    [(cursor _ '() (? res?) _) #f]
    [(cursor _ path t _) #:when (or (null? path) (not (done? t))) (descend c)]
    [_ (next-redex (up c))]))

;; c moved to the redex in its subterm, which is not done unless it is the
;; root's body
(define (descend c)
  (match-define (cursor G path t L) c)
  (match t
    [(list 'IN L_t body)
     (if (done? body)
         c
         (descend (cursor G (cons (path-in L) path) body L_t)))]
    [_
     (match (pending t)
       ['() c]
       [(list (cons Fr sub)) (descend (cursor G (cons (path-fr Fr) path) sub L))]
       [positions
        (error 'run "~a pending positions, with subterms ~e, in ~e"
               (length positions) (map cdr positions) t)])]))

;; The evaluation positions of node t whose subterms are not done, as
;; (Fr . subterm) pairs
(define (pending t)
  (for/list ([position (in-list (decompose t))]
             #:unless (done? (cdr position)))
    position))

;; The contractum of redex r under G and L, and the new layer. The rules are
;; disjoint, so the first successor found is the only one.
(define (reduce-redex r G L)
  (match (apply-reduction-relation ->term r)
    [(cons r_1 _) (values r_1 L)]
    ['()
     (match (apply-reduction-relation ->term+ctx (list r G L))
       [(cons (list r_1 _ L_1) _) (values r_1 L_1)]
       ['() (error 'run "stuck: no rule applies to ~e" r)])]))

;; The final configuration that conf reduces to
(define (run conf)
  (let loop ([c (conf->cursor conf)])
    (match (next-redex c)
      [#f (cursor->conf c)]
      [(cursor G path r L)
       (define-values (r_1 L_1) (reduce-redex r G L))
       (loop (cursor G path r_1 L_1))])))
