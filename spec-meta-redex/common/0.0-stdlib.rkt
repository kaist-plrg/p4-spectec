#lang racket/base
;; The parts of spec-meta/common/0-stdlib.watsup that the port uses, and the
;; Racket helpers that rules use for texts, lists, and debug output.
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
;; Builtins, as in p4spec/lib/interface/builtin/{lists,maps}.ml
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
;; Texts and lists, as in p4spec/lib/interp/interp-al/interp.ml
;;
;; Texts are indexed by UTF-8 bytes, as OCaml's strings are. A result that
;; splits a character raises an error, since a Racket string cannot hold it.
;; So does an index or a slice out of bounds. A slice is a start and a length.

(define (text-len t)
  (bytes-length (string->bytes/utf-8 t)))

;; The byte of t at n, as a text
(define (text-idx t n)
  (define bs (string->bytes/utf-8 t))
  (check-index 'text-idx n (bytes-length bs))
  (bytes->text 'text-idx (subbytes bs n (add1 n))))

;; The n bytes of t from i
(define (text-slice t i n)
  (define bs (string->bytes/utf-8 t))
  (check-slice 'text-slice i n (bytes-length bs))
  (bytes->text 'text-slice (subbytes bs i (+ i n))))

;; t with its byte at n replaced by t_n, of one byte
(define (text-upd t n t_n)
  (define bs (string->bytes/utf-8 t))
  (define bs_n (string->bytes/utf-8 t_n))
  (check-index 'text-upd n (bytes-length bs))
  (check-length 'text-upd 1 (bytes-length bs_n))
  (bytes->text 'text-upd (bytes-append (subbytes bs 0 n) bs_n (subbytes bs (add1 n)))))

;; t with its n bytes from i replaced by t_n, of n bytes
(define (text-upd-slice t i n t_n)
  (define bs (string->bytes/utf-8 t))
  (define bs_n (string->bytes/utf-8 t_n))
  (check-slice 'text-upd-slice i n (bytes-length bs))
  (check-length 'text-upd-slice n (bytes-length bs_n))
  (bytes->text 'text-upd-slice
               (bytes-append (subbytes bs 0 i) bs_n (subbytes bs (+ i n)))))

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

(define (bytes->text who bs)
  (unless (bytes-utf-8-length bs #f)
    (error who "the result splits a UTF-8 character: ~s" bs))
  (bytes->string/utf-8 bs))

;;
;; Debugging
;;

;; Writes the term t to stderr, for `-- debug e`.
(define (debug t)
  (writeln t (current-error-port)))
