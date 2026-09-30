#lang racket/base

(require racket/list
         racket/path
         racket/runtime-path
         rackunit
         "../common/0.0-prelude.rkt"
         "../al/0-boot.rkt"
         "../al/3-context.rkt")

(define-runtime-path repo "../..")

(define (repo-path . parts) (simplify-path (apply build-path repo parts)))

;; A layer with the given TYP, REL, FUNC, and VAL maps
(define (layer-of typ rel func val)
  `(TYP ,typ REL ,rel FUNC ,func VAL ,val))

(define (layer-typ layer) (list-ref layer 1))
(define (layer-val layer) (list-ref layer 7))

;; layer with pairs added to its VAL map, at the end
(define (vals-added layer pairs)
  (append (take layer 7) (list (append (layer-val layer) pairs))))

(define-term L-empty (empty-layer))

(define-term L-vals
  ,(layer-of '() '() '()
             '((("x" ()) (NAT 1))
               (("xs" (STAR)) (LIST ((NAT 2) (NAT 3))))
               (("ys" (STAR)) (LIST ((NAT 4) (NAT 5))))
               (("zs" (STAR)) (LIST ((NAT 6))))
               (("es" (STAR)) (LIST ()))
               (("o" (QUEST)) (OPT ((NAT 7))))
               (("p" (QUEST)) (OPT ((NAT 8))))
               (("n" (QUEST)) (OPT ()))
               (("w" (QUEST)) (NAT 0))
               (("v" (STAR)) (NAT 0)))))

;;
;; Language
;;

(test-match al-context layer (term {TYP () REL () FUNC () VAL ()}))
(test-match al-context G (term L-vals))
(test-match al-context L (term L-vals))
(test-no-match al-context layer (term {TYP () REL () FUNC ()}))
(test-no-match al-context layer (term {TYP () REL () VAL () FUNC ()}))
(test-no-match al-context layer (term {TYP () REL () FUNC () VAL ((("x" ()) 1))}))

(define-term C-vals {GLOBAL L-empty LOCAL L-vals})

(test-match al-context ctx (term C-vals))
(test-match al-context C (term C-vals))
(test-no-match al-context ctx (term {LOCAL L-empty GLOBAL L-empty}))
(test-no-match al-context ctx (term {GLOBAL {TYP () REL () FUNC () VAL ((("x" ()) 1))}
                                     LOCAL L-empty}))

;; ctx-shallow checks the record shape only.
(test-match al-context ctx-shallow (term C-vals))
(test-match al-context ctx-shallow (term {GLOBAL {TYP () REL () FUNC () VAL ((("x" ()) 1))}
                                          LOCAL L-empty}))
(test-no-match al-context ctx-shallow (term {GLOBAL {TYP () REL () FUNC ()} LOCAL L-empty}))
(test-no-match al-context ctx-shallow (term {LOCAL L-empty GLOBAL L-empty}))

(test-equal (term (empty-layer)) '(TYP () REL () FUNC () VAL ()))
(test-equal (term (empty-ctx)) '(GLOBAL (TYP () REL () FUNC () VAL ())
                                 LOCAL (TYP () REL () FUNC () VAL ())))

;;
;; Loading
;;

(define-term C-empty (empty-ctx))

(define (ctx-of global) `(GLOBAL ,global LOCAL ,(term L-empty)))

(test-equal (term (load-typdef C-empty "t" EXT))
            (ctx-of (layer-of '(("t" EXT)) '() '() '())))
(test-equal (term (load-reldef C-empty "R" (EXT "R")))
            (ctx-of (layer-of '() '(("R" (EXT "R"))) '() '())))
(test-equal (term (load-funcdef C-empty "f" (EXT "f")))
            (ctx-of (layer-of '() '() '(("f" (EXT "f"))) '())))

(define-term clause-ex (((EXP (VAR "n"))) (VAR "n") ()))
(define-term tblrow-ex (((EXP (NAT 0))) (NAT 1) ()))
(define-term rulgroup-ex ("g" (((VAR "x")) ()) (("r" ((NAT 0)) ()))))
(define-term elsgroup-ex ("g" (((VAR "x")) ()) ("r" ((NAT 0)) ())))

;; One definition of each kind
(test-equal
 (term (load C-empty
             ((EXTTYP "json")
              (TYP "t" ("X") (ALIAS NAT))
              (EXTREL "Ext" (NAT) (BOOL))
              (REL "R" (NAT) (NAT) (rulgroup-ex) (elsgroup-ex))
              (REL "S" () () () ())
              (EXTFUNC "e" () () NAT)
              (BUILTINFUNC "rev_" ("X") ((EXP (ITER (VAR "X" ()) STAR))) (ITER (VAR "X" ()) STAR))
              (TABLEFUNC "tbl" ((EXP NAT)) NAT (tblrow-ex))
              (FUNC "f" () ((EXP NAT)) NAT (clause-ex) ())
              (FUNC "g" ("X") () NAT () (clause-ex)))))
 (ctx-of
  (layer-of '(("json" EXT) ("t" (DEF ("X") (ALIAS NAT))))
            `(("Ext" (EXT "Ext"))
              ("R" (DEF (,(term rulgroup-ex)) (,(term elsgroup-ex))))
              ("S" (DEF () ())))
            `(("e" (EXT "e"))
              ("rev_" (BUILTIN "rev_" ("X") ((EXP (ITER (VAR "X" ()) STAR)))))
              ("tbl" (TABLE ((EXP NAT)) (,(term tblrow-ex))))
              ("f" (DEF () (,(term clause-ex)) ()))
              ("g" (DEF ("X") () (,(term clause-ex)))))
            '())))

(test-equal (term (load C-empty ())) (term C-empty))

;; Loading keeps the local layer.
(test-equal (term (load C-vals ((EXTTYP "t"))))
            (term {GLOBAL {TYP (("t" EXT)) REL () FUNC () VAL ()} LOCAL L-vals}))

;; A definition that matches no clause gives ⊥, also after other definitions.
(test-equal (term (load/shallow C-empty ((BOGUS "x")))) '⊥)
(test-equal (term (load/shallow C-empty ((EXTTYP "t") (BOGUS "x")))) '⊥)

;; A later definition replaces an earlier one where it stands.
(test-equal
 (term (load C-empty
             ((FUNC "f" () () NAT () ())
              (EXTFUNC "g" () () NAT)
              (EXTFUNC "f" () () NAT))))
 (ctx-of (layer-of '() '() '(("f" (EXT "f")) ("g" (EXT "g"))) '())))

;; Loaded scripts: the ids of each kind, in order of first definition.
(define (ids-of script heads)
  (remove-duplicates
   (for/list ([defn (in-list script)] #:when (memq (car defn) heads))
     (cadr defn))))

(define (test-load path)
  (define script (boot-script path))
  (define C (term (load C-empty ,script)))
  (test-match al-context ctx C)
  (define global (list-ref C 1))
  (test-equal (map car (list-ref global 1)) (ids-of script '(EXTTYP TYP)))
  (test-equal (map car (list-ref global 3)) (ids-of script '(EXTREL REL)))
  (test-equal (map car (list-ref global 5))
              (ids-of script '(EXTFUNC BUILTINFUNC TABLEFUNC FUNC)))
  (test-equal (list-ref global 7) '())
  (test-equal (list-ref C 3) (term L-empty)))

(for ([file (in-list (directory-list (repo-path "examples") #:build? #t))]
      #:when (path-has-extension? file #".watsup"))
  (test-load file))

(test-load (repo-path "spec-meta" "al"))
(test-load (repo-path "spec"))

;;
;; Adders
;;

(test-equal (term (add-vari L-empty ("x" NAT (STAR)) (NAT 1)))
            (layer-of '() '() '() '((("x" (STAR)) (NAT 1)))))
(test-equal (term (add-varr L-empty ("x" (STAR)) (NAT 1)))
            (layer-of '() '() '() '((("x" (STAR)) (NAT 1)))))
;; The type of vari does not matter, and a bound variable is replaced where it
;; stands.
(test-equal (term (add-vari L-vals ("x" INT ()) (NAT 9)))
            (layer-of '() '() '() (cons '(("x" ()) (NAT 9)) (cdr (layer-val (term L-vals))))))
(test-equal (term (add-varr L-vals ("x" ()) (NAT 9)))
            (term (add-vari L-vals ("x" BOOL ()) (NAT 9))))

(test-equal (term (add-varis L-empty (("x" NAT ()) ("y" BOOL (QUEST))) ((NAT 1) (BOOL #t))))
            (layer-of '() '() '() '((("x" ()) (NAT 1)) (("y" (QUEST)) (BOOL #t)))))
(test-equal (term (add-varis L-empty () ())) (term L-empty))
(check-exn #rx"adds-map" (λ () (term (add-varis L-empty (("x" NAT ())) ()))))
(test-equal (term (add-varrs L-empty (("x" ()) ("x" ())) ((NAT 1) (NAT 2))))
            (layer-of '() '() '() '((("x" ()) (NAT 2)))))
(test-equal (term (add-varrs L-empty () ())) (term L-empty))

(test-equal (term (add-typ L-empty "X" PARAM))
            (layer-of '(("X" PARAM)) '() '() '()))
(test-equal (term (add-typ (add-typ L-empty "X" PARAM) "X" EXT))
            (layer-of '(("X" EXT)) '() '() '()))
(test-equal (term (add-func L-empty "f" (EXT "f")))
            (layer-of '() '() '(("f" (EXT "f"))) '()))

;;
;; Value finders
;;

(test-equal (term (find-vari L-vals ("x" INT ()))) '((NAT 1)))
(test-equal (term (find-vari L-vals ("xs" NAT (STAR)))) '((LIST ((NAT 2) (NAT 3)))))
;; No clause for an unbound variable: ⊥
(test-equal (term (find-vari L-vals ("x" NAT (STAR)))) '⊥)
(test-equal (term (find-vari L-empty ("x" NAT ()))) '⊥)

(test-equal (term (find-varr L-vals ("x" ()))) '((NAT 1)))
(test-equal (term (find-varr L-vals ("y" ()))) '())

(test-equal (term (find-varis L-vals (("x" NAT ()) ("n" NAT (QUEST)))))
            '(((NAT 1) (OPT ()))))
(test-equal (term (find-varis L-vals ())) '(()))
;; otherwise: some lookup fails
(test-equal (term (find-varis L-vals (("x" NAT ()) ("y" NAT ())))) '())
(test-equal (term (find-varis L-empty (("x" NAT ())))) '())

(test-equal (term (find-varrs L-vals (("x" ()) ("o" (QUEST))))) '(((NAT 1) (OPT ((NAT 7))))))
(test-equal (term (find-varrs L-vals ())) '(()))
;; otherwise: some lookup fails
(test-equal (term (find-varrs L-vals (("y" ()) ("x" ())))) '())

(define-term L-x9 ,(vals-added (term L-empty) '((("x" ()) (NAT 9)))))

(test-equal (term (finds-vari (L-vals L-x9) ("x" NAT ()))) '(((NAT 1) (NAT 9))))
(test-equal (term (finds-vari () ("x" NAT ()))) '(()))
;; otherwise: some lookup fails
(test-equal (term (finds-vari (L-vals L-empty) ("x" NAT ()))) '())
(test-equal (term (finds-vari (L-vals) ("y" NAT ()))) '())

;;
;; Typedef, function, and relation finders
;;

(define-term G-defs
  ,(layer-of '(("t" EXT) ("u" PARAM))
             '(("R" (EXT "R")))
             '(("f" (EXT "f")) ("g" (EXT "g")))
             '()))

(define-term L-defs
  ,(layer-of '(("t" PARAM))
             '(("S" (EXT "S")))
             '(("f" (BUILTIN "f" () ())))
             '()))

;; The local layer first
(test-equal (term (find-typ G-defs L-defs "t")) '(PARAM))
(test-equal (term (find-typ G-defs L-defs "u")) '(PARAM))
(test-equal (term (find-typ L-defs G-defs "t")) '(EXT))
;; Found in neither layer: the global lookup gives ()
(test-equal (term (find-typ G-defs L-defs "v")) '())

(test-equal (term (find-func G-defs L-defs "f")) '((BUILTIN "f" () ())))
(test-equal (term (find-func G-defs L-defs "g")) '((EXT "g")))
;; otherwise: found in neither layer
(test-equal (term (find-func G-defs L-defs "h")) '())

;; Only the global layer holds relations.
(test-equal (term (find-rel G-defs L-defs "R")) '((EXT "R")))
(test-equal (term (find-rel G-defs L-defs "S")) '())

;;
;; Sub-contexts
;;

;; $sub_opt: every lookup is OPT val, or every lookup is OPT eps
(test-equal (term (sub-opt L-vals (("o" NAT ()) ("p" NAT ()))))
            (list (vals-added (term L-vals) '((("o" ()) (NAT 7)) (("p" ()) (NAT 8))))))
(test-equal (term (sub-opt L-vals (("n" NAT ())))) '())
;; An empty vari* takes the first clause.
(test-equal (term (sub-opt L-vals ())) (list (term L-vals)))
;; A mix, an unbound variable, or a non-option: ⊥
(test-equal (term (sub-opt L-vals (("o" NAT ()) ("n" NAT ())))) '⊥)
(test-equal (term (sub-opt L-vals (("n" NAT ()) ("o" NAT ())))) '⊥)
(test-equal (term (sub-opt L-vals (("q" NAT ())))) '⊥)
(test-equal (term (sub-opt L-vals (("w" NAT ())))) '⊥)

;; $sub_list: one layer per element, with the lists transposed
(test-equal (term (sub-list L-vals (("xs" NAT ()) ("ys" NAT ()))))
            (list (vals-added (term L-vals) '((("xs" ()) (NAT 2)) (("ys" ()) (NAT 4))))
                  (vals-added (term L-vals) '((("xs" ()) (NAT 3)) (("ys" ()) (NAT 5))))))
(test-equal (term (sub-list L-vals (("zs" NAT ()))))
            (list (vals-added (term L-vals) '((("zs" ()) (NAT 6))))))
;; No iterated variables, or empty lists: no layers
(test-equal (term (sub-list L-vals ())) '())
(test-equal (term (sub-list L-vals (("es" NAT ())))) '())
;; An unbound variable or a non-list: ⊥
(test-equal (term (sub-list L-vals (("q" NAT ())))) '⊥)
(test-equal (term (sub-list L-vals (("v" NAT ())))) '⊥)
;; Lists of different lengths
(check-exn #rx"cannot transpose"
           (λ () (term (sub-list L-vals (("xs" NAT ()) ("zs" NAT ()))))))

;;
;; Contracts
;;

(when contracts?
  ;; A ctx where a layer is expected, and the other way around
  (check-exn #rx"not in my domain" (λ () (term (find-vari C-vals ("x" NAT ())))))
  (check-exn #rx"not in my domain" (λ () (term (find-rel C-vals L-empty "R"))))
  (check-exn #rx"not in my domain" (λ () (term (load L-empty ()))))
  ;; A varr where a vari is expected
  (check-exn #rx"not in my domain" (λ () (term (add-vari L-empty ("x" ()) (NAT 1)))))
  (check-exn #rx"not in my domain" (λ () (term (load C-empty ((BOGUS "x")))))))
