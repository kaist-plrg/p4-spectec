#lang racket/base
;; spec-meta/common/0-stdlib.watsup.
;;
;; Type parameters are dropped: `any` stands for them in contracts.

(require "0.0-prelude.rkt")
(provide stdlib
         ite
         opt-as-seq-
         exists-
         forall-
         repeat-
         rev-
         assoc-
         transpose-
         empty-set
         empty-map
         find-map
         find-maps
         add-map
         adds-map)

(define-language stdlib
  ;; Metavariables for int, nat, bool, and text
  (bool b ::= boolean)
  (int i ::= integer)
  (nat n ::= natural)
  (text t ::= string)

  ;; Sets and maps, as lists and association lists
  (set ::= (any ...))
  (pair ::= (any any))
  (map ::= (pair ...)))

;;
;; General bool functions
;;

(define-dec stdlib
  ite : bool any any -> any
  [(ite #t any_t any_f) any_t]
  [(ite #f any_t any_f) any_f])

;;
;; General option functions
;;

(define-dec stdlib
  opt-as-seq- : (any ...) -> (any ...)
  [(opt-as-seq- ()) ()]
  [(opt-as-seq- (any)) (any)])

;;
;; General sequence functions
;;

(define-dec stdlib
  exists- : (bool ...) -> bool
  [(exists- ()) #f]
  [(exists- (b_h b_t ...)) ,(or (term b_h) (term b_r))
   (where b_r (exists- (b_t ...)))])

(define-dec stdlib
  forall- : (bool ...) -> bool
  [(forall- ()) #t]
  [(forall- (b_h b_t ...)) ,(and (term b_h) (term b_r))
   (where b_r (forall- (b_t ...)))])

(define-dec stdlib
  repeat- : any nat -> (any ...)
  [(repeat- any 0) ()]
  [(repeat- any n) (any any_r ...)
   (side-condition (not (= (term n) 0)))
   (where n_1 ,(- (term n) 1))
   (where (any_r ...) (repeat- any n_1))])

(define-dec stdlib
  rev- : (any ...) -> (any ...)
  [(rev- (any ...)) ,(reverse (term (any ...)))])

(define-dec stdlib
  assoc- : any (pair ...) -> (any ...)
  [(assoc- any_k (pair ...)) ,(lookup (term (pair ...)) (term any_k))])

(define-dec stdlib
  transpose- : ((any ...) ...) -> ((any ...) ...)
  [(transpose- ((any ...) ...)) ,(transpose (term ((any ...) ...)))])

;;
;; General set functions
;;

(define-dec stdlib
  empty-set : -> set
  [(empty-set) ()])

;;
;; General map functions
;;

(define-dec stdlib
  empty-map : -> map
  [(empty-map) ()])

(define-dec stdlib
  find-map : map any -> (any ...)
  [(find-map map any_k) ,(lookup (term map) (term any_k))])

(define-dec stdlib
  find-maps : (map ...) any -> (any ...)
  [(find-maps (map ...) any_k) ,(lookups (term (map ...)) (term any_k))])

(define-dec stdlib
  add-map : map any any -> map
  [(add-map map any_k any_v) ,(update (term map) (term any_k) (term any_v))])

(define-dec stdlib
  adds-map : map (any ...) (any ...) -> map
  [(adds-map map (any_k ...) (any_v ...))
   ,(updates (term map) (term (any_k ...)) (term (any_v ...)))])

;;
;; Builtins, as in p4spec/lib/interface/builtin/{lists,maps}.ml
;;

;; The value of the first pair with key k, as a list of length 0 or 1.
(define (lookup pairs k)
  (cond
    [(assoc k pairs) => (λ (pair) (list (cadr pair)))]
    [else '()]))

;; The value of key k in the first of maps that has it.
(define (lookups maps k)
  (or (for/or ([pairs (in-list maps)])
        (define v (lookup pairs k))
        (and (pair? v) v))
      '()))

;; Replaces the first pair with key k where it stands, or appends one.
(define (update pairs k v)
  (let loop ([pairs pairs])
    (cond
      [(null? pairs) (list (list k v))]
      [(equal? (caar pairs) k) (cons (list k v) (cdr pairs))]
      [else (cons (car pairs) (loop (cdr pairs)))])))

(define (updates pairs ks vs)
  (unless (= (length ks) (length vs))
    (error 'adds-map "~a keys but ~a values" (length ks) (length vs)))
  (for/fold ([pairs pairs]) ([k (in-list ks)] [v (in-list vs)])
    (update pairs k v)))

(define (transpose rows)
  (cond
    [(null? rows) '()]
    [else
     (define width (length (car rows)))
     (for ([row (in-list rows)])
       (unless (= (length row) width)
         (error 'transpose- "cannot transpose a matrix of values")))
     (apply map list rows)]))
