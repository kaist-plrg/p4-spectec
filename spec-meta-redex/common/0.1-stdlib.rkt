#lang racket/base
;; spec-meta/common/0-stdlib.watsup.
;;
;; Type parameters are dropped: `any` stands for them in contracts.

(require "0.0-prelude.rkt")
(provide Stdlib
         ite
         opt_as_seq_
         exists_
         forall_
         repeat_
         rev_
         assoc_
         transpose_
         empty_set
         empty_map
         find_map
         find_maps
         add_map
         adds_map)

(define-language Stdlib
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

(define-dec Stdlib
  ite : bool any any -> any
  [(ite #t any_t any_f) any_t]
  [(ite #f any_t any_f) any_f])

;;
;; General option functions
;;

(define-dec Stdlib
  opt_as_seq_ : (any ...) -> (any ...)
  [(opt_as_seq_ ()) ()]
  [(opt_as_seq_ (any)) (any)])

;;
;; General sequence functions
;;

(define-dec Stdlib
  exists_ : (bool ...) -> bool
  [(exists_ ()) #f]
  [(exists_ (b_h b_t ...)) ,(or (term b_h) (term b_r))
   (where b_r (exists_ (b_t ...)))])

(define-dec Stdlib
  forall_ : (bool ...) -> bool
  [(forall_ ()) #t]
  [(forall_ (b_h b_t ...)) ,(and (term b_h) (term b_r))
   (where b_r (forall_ (b_t ...)))])

(define-dec Stdlib
  repeat_ : any nat -> (any ...)
  [(repeat_ any 0) ()]
  [(repeat_ any n) (any any_r ...)
   (side-condition (not (= (term n) 0)))
   (where n_1 ,(- (term n) 1))
   (where (any_r ...) (repeat_ any n_1))])

(define-dec Stdlib
  rev_ : (any ...) -> (any ...)
  [(rev_ (any ...)) ,(reverse (term (any ...)))])

(define-dec Stdlib
  assoc_ : any (pair ...) -> (any ...)
  [(assoc_ any_k (pair ...)) ,(lookup (term (pair ...)) (term any_k))])

(define-dec Stdlib
  transpose_ : ((any ...) ...) -> ((any ...) ...)
  [(transpose_ ((any ...) ...)) ,(transpose (term ((any ...) ...)))])

;;
;; General set functions
;;

(define-dec Stdlib
  empty_set : -> set
  [(empty_set) ()])

;;
;; General map functions
;;

(define-dec Stdlib
  empty_map : -> map
  [(empty_map) ()])

(define-dec Stdlib
  find_map : map any -> (any ...)
  [(find_map map any_k) ,(lookup (term map) (term any_k))])

(define-dec Stdlib
  find_maps : (map ...) any -> (any ...)
  [(find_maps (map ...) any_k) ,(lookups (term (map ...)) (term any_k))])

(define-dec Stdlib
  add_map : map any any -> map
  [(add_map map any_k any_v) ,(update (term map) (term any_k) (term any_v))])

(define-dec Stdlib
  adds_map : map (any ...) (any ...) -> map
  [(adds_map map (any_k ...) (any_v ...))
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
    (error 'adds_map "~a keys but ~a values" (length ks) (length vs)))
  (for/fold ([pairs pairs]) ([k (in-list ks)] [v (in-list vs)])
    (update pairs k v)))

(define (transpose rows)
  (cond
    [(null? rows) '()]
    [else
     (define width (length (car rows)))
     (for ([row (in-list rows)])
       (unless (= (length row) width)
         (error 'transpose_ "cannot transpose a matrix of values")))
     (apply map list rows)]))
