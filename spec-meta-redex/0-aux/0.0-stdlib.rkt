#lang racket/base
;; Mirrors the parts of spec-meta/0-aux/0.0-stdlib.watsup that the port uses,
;; with the Racket helpers that rules use for texts, lists, and debug output.
;;
;; Type parameters are dropped: `any` stands for them in contracts.

(require racket/list
         redex/reduction-semantics)
(provide stdlib
         forall-
         repeat-
         transpose-
         empty-map
         find-map
         add-map
         adds-map
         map-ref
         text-len
         text-idx
         text-slice
         text-upd
         text-upd-slice
         list-idx
         list-slice
         list-upd
         list-upd-slice
         current-debug
         debug)

(define-language stdlib
  ;; Metavariables for int, nat, bool, and text
  (bool b ::= boolean)
  (int i ::= integer)
  (nat n ::= natural)
  (text t ::= string)

  ;; Maps, as association lists
  (pair ::= (any any))
  (map ::= (pair ...)))

;;
;; General sequence functions
;;

(define-metafunction stdlib
  forall- : (bool ...) -> bool
  [(forall- (b ...)) ,(andmap values (term (b ...)))])

(define-metafunction stdlib
  repeat- : any nat -> (any ...)
  [(repeat- any n) ,(make-list (term n) (term any))])

(define-metafunction stdlib
  transpose- : ((any ...) ...) -> ((any ...) ...)
  [(transpose- ((any ...) ...)) ,(transpose (term ((any ...) ...)))])

;;
;; General map functions
;;

(define-metafunction stdlib
  empty-map : -> map
  [(empty-map) ()])

(define-metafunction stdlib
  find-map : map any -> () ∨ (any)
  [(find-map map any_k) ,(map-ref (term map) (term any_k))])

(define-metafunction stdlib
  add-map : map any any -> map
  [(add-map map any_k any_v) ,(update (term map) (term any_k) (term any_v))])

(define-metafunction stdlib
  adds-map : map (any ...) (any ...) -> map
  [(adds-map map (any_k ...) (any_v ...))
   ,(updates (term map) (term (any_k ...)) (term (any_v ...)))])

;;
;; Builtins, as in p4spectec/src/interface/builtin/{lists,maps}.rs
;;

;; The value of the first pair with key k, as a list of length 0 or 1. Racket
;; code that must not pass a map to a metafunction calls this.
(define (map-ref pairs k)
  (cond
    [(assoc k pairs) => (λ (pair) (list (cadr pair)))]
    [else '()]))

;; Replaces the first pair with key k where it stands, or adds one in front.
;; New keys go in front because equal-hash-code, which Redex's caches use,
;; looks only at the start of a long list.
(define (update pairs k v)
  (if (assoc k pairs)
      (let loop ([pairs pairs])
        (if (equal? (caar pairs) k)
            (cons (list k v) (cdr pairs))
            (cons (car pairs) (loop (cdr pairs)))))
      (cons (list k v) pairs)))

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

;;
;; Texts and lists, as in p4spectec/src/interp/shared/eval/ops.rs
;;
;; Texts are indexed by characters, as spec-meta's are. An index or a slice
;; out of bounds raises an error. A slice is a start and a length.

(define (text-len t)
  (string-length t))

;; The character of t at n, as a text
(define (text-idx t n)
  (check-index 'text-idx n (string-length t))
  (string (string-ref t n)))

;; The n characters of t from i
(define (text-slice t i n)
  (check-slice 'text-slice i n (string-length t))
  (substring t i (+ i n)))

;; t with its character at n replaced by t_n, of one character
(define (text-upd t n t_n)
  (check-index 'text-upd n (string-length t))
  (check-length 'text-upd 1 (string-length t_n))
  (string-append (substring t 0 n) t_n (substring t (add1 n))))

;; t with its n characters from i replaced by t_n, of n characters
(define (text-upd-slice t i n t_n)
  (check-slice 'text-upd-slice i n (string-length t))
  (check-length 'text-upd-slice n (string-length t_n))
  (string-append (substring t 0 i) t_n (substring t (+ i n))))

(define (list-idx xs n)
  (check-index 'list-idx n (length xs))
  (list-ref xs n))

;; The n elements of xs from i
(define (list-slice xs i n)
  (check-slice 'list-slice i n (length xs))
  (take (drop xs i) n))

(define (list-upd xs n x)
  (check-index 'list-upd n (length xs))
  (list-set xs n x))

;; xs with its n elements from i replaced by xs_n, of n elements
(define (list-upd-slice xs i n xs_n)
  (check-slice 'list-upd-slice i n (length xs))
  (check-length 'list-upd-slice n (length xs_n))
  (append (take xs i) xs_n (drop xs (+ i n))))

(define (check-index who n len)
  (unless (< n len)
    (error who "index ~a out of bounds [0, ~a)" n len)))

(define (check-slice who i n len)
  (unless (<= (+ i n) len)
    (error who "slice [~a, ~a) out of bounds [0, ~a)" i (+ i n) len)))

(define (check-length who expected actual)
  (unless (= expected actual)
    (error who "the replacement has length ~a instead of ~a" actual expected)))

;;
;; Debugging
;;

;; Reports the value of `-- debug e`; it is written to stderr unless the
;; entry relays it to the host.
(define current-debug
  (make-parameter (λ (t) (writeln t (current-error-port)))))

(define (debug t)
  ((current-debug) t))
