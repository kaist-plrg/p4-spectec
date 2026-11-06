#lang racket/base
;; Mirrors spec-meta/4-execution/4.03-eval-ops.watsup.
;;
;; Arithmetic follows p4spectec/src/lang/common/prim/num.rs: DIV and MOD
;; truncate. An undefined result (division by zero) raises an error, as
;; num.rs's NumericError does.

(require redex/reduction-semantics
         "3.03-machine.rkt")
(provide num-unop
         bool-binop
         num-binop
         poly-cmpop
         num-cmpop
         matches)

;; Unary numeric operations

(define-metafunction al
  num-unop : numUnop num -> val
  [(num-unop PLUS (NAT n)) (NAT n)]
  [(num-unop PLUS (INT i)) (INT i)]
  [(num-unop MINUS (NAT n)) (INT ,(- (term n)))]
  [(num-unop MINUS (INT i)) (INT ,(- (term i)))])

;; Binary boolean operations

(define-metafunction al
  bool-binop : boolBinop bool bool -> bool
  [(bool-binop AND b_l b_r) ,(and (term b_l) (term b_r))]
  [(bool-binop OR b_l b_r) ,(or (term b_l) (term b_r))]
  [(bool-binop IMPL b_l b_r) ,(or (not (term b_l)) (term b_r))]
  [(bool-binop EQUIV b_l b_r) ,(eq? (term b_l) (term b_r))])

;; Binary numeric operations

(define-metafunction al
  num-binop : numBinop num num -> numBinopRes
  [(num-binop ADD (NAT n_l) (NAT n_r)) (OK (NAT ,(+ (term n_l) (term n_r))))]
  [(num-binop ADD (INT i_l) (INT i_r)) (OK (INT ,(+ (term i_l) (term i_r))))]
  [(num-binop SUB (NAT n_l) (NAT n_r)) (OK (INT ,(- (term n_l) (term n_r))))]
  [(num-binop SUB (INT i_l) (INT i_r)) (OK (INT ,(- (term i_l) (term i_r))))]
  [(num-binop MUL (NAT n_l) (NAT n_r)) (OK (NAT ,(* (term n_l) (term n_r))))]
  [(num-binop MUL (INT i_l) (INT i_r)) (OK (INT ,(* (term i_l) (term i_r))))]
  [(num-binop DIV (NAT n_l) (NAT n_r)) (OK (NAT ,(quotient (term n_l) (term n_r))))]
  [(num-binop DIV (INT i_l) (INT i_r)) (OK (INT ,(quotient (term i_l) (term i_r))))]
  [(num-binop MOD (NAT n_l) (NAT n_r)) (OK (NAT ,(remainder (term n_l) (term n_r))))]
  [(num-binop MOD (INT i_l) (INT i_r)) (OK (INT ,(remainder (term i_l) (term i_r))))]
  ;; otherwise
  [(num-binop numBinop num_l num_r) ERROR])

;; Polymorphic comparison operations

(define-metafunction al
  poly-cmpop : polyCmpop val val -> bool
  [(poly-cmpop EQ val_l val_r) ,(equal? (term val_l) (term val_r))]
  [(poly-cmpop NE val_l val_r) ,(not (equal? (term val_l) (term val_r)))])

;; Numeric comparison operations

(define-metafunction al
  num-cmpop : numCmpop num num -> numCmpRes
  [(num-cmpop LT (NAT n_l) (NAT n_r)) (OK ,(< (term n_l) (term n_r)))]
  [(num-cmpop LT (INT i_l) (INT i_r)) (OK ,(< (term i_l) (term i_r)))]
  [(num-cmpop GT (NAT n_l) (NAT n_r)) (OK ,(> (term n_l) (term n_r)))]
  [(num-cmpop GT (INT i_l) (INT i_r)) (OK ,(> (term i_l) (term i_r)))]
  [(num-cmpop LE (NAT n_l) (NAT n_r)) (OK ,(<= (term n_l) (term n_r)))]
  [(num-cmpop LE (INT i_l) (INT i_r)) (OK ,(<= (term i_l) (term i_r)))]
  [(num-cmpop GE (NAT n_l) (NAT n_r)) (OK ,(>= (term n_l) (term n_r)))]
  [(num-cmpop GE (INT i_l) (INT i_r)) (OK ,(>= (term i_l) (term i_r)))]
  ;; otherwise
  [(num-cmpop numCmpop num_l num_r) ERROR])

;; Pattern tests

(define-metafunction al
  matches : val pat -> bool
  [(matches (INJ (mixop_v (val ...))) (INJ mixop_p)) ,(equal? (term mixop_v) (term mixop_p))]
  [(matches (LIST (val ...)) CONS) ,(> (length (term (val ...))) 0)]
  [(matches (LIST (val ...)) (FIXED n)) ,(= (length (term (val ...))) (term n))]
  [(matches (LIST (val ...)) NIL) ,(= (length (term (val ...))) 0)]
  [(matches (OPT (val ...)) SOME) ,(pair? (term (val ...)))]
  [(matches (OPT (val ...)) NONE) ,(null? (term (val ...)))]
  ;; otherwise
  [(matches val pat) #f])
