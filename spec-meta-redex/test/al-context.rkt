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

;; A context with the given global and local layers, each a list of the
;; TYP, REL, FUNC, and VAL maps.
(define (ctx-of global local)
  (define (layer maps) (append-map list '(TYP REL FUNC VAL) maps))
  (list 'GLOBAL (layer global) 'LOCAL (layer local)))

;; The LOCAL VAL map of a context
(define (local-vals C) (list-ref (list-ref C 3) 7))

(define-term C-vals
  ,(ctx-of '(() () () ())
           '(() () ()
             ((("x" ()) (NAT 1))
              (("xs" (STAR)) (LIST ((NAT 2) (NAT 3))))
              (("ys" (STAR)) (LIST ((NAT 4) (NAT 5))))
              (("zs" (STAR)) (LIST ((NAT 6))))
              (("es" (STAR)) (LIST ()))
              (("o" (QUEST)) (OPT ((NAT 7))))
              (("p" (QUEST)) (OPT ((NAT 8))))
              (("n" (QUEST)) (OPT ()))
              (("w" (QUEST)) (NAT 0))
              (("v" (STAR)) (NAT 0))))))

(define (vals-added C pairs)
  (ctx-of (for/list ([i '(1 3 5 7)]) (list-ref (list-ref C 1) i))
          (append (for/list ([i '(1 3 5)]) (list-ref (list-ref C 3) i))
                  (list (append (local-vals C) pairs)))))

;;
;; Language
;;

(test-match AL-context layer (term {TYP () REL () FUNC () VAL ()}))
(test-no-match AL-context layer (term {TYP () REL () FUNC ()}))
(test-no-match AL-context layer (term {TYP () REL () VAL () FUNC ()}))
(test-match AL-context ctx (term C-vals))
(test-match AL-context C (term C-vals))
(test-no-match AL-context ctx (term {LOCAL {TYP () REL () FUNC () VAL ()}
                                     GLOBAL {TYP () REL () FUNC () VAL ()}}))
(test-no-match AL-context ctx (term {GLOBAL {TYP () REL () FUNC () VAL ((("x" ()) 1))}
                                     LOCAL {TYP () REL () FUNC () VAL ()}}))

(test-equal (term (empty_layer)) '(TYP () REL () FUNC () VAL ()))
(test-equal (term (empty_ctx))
            '(GLOBAL (TYP () REL () FUNC () VAL ()) LOCAL (TYP () REL () FUNC () VAL ())))

;;
;; Loading
;;

(define-term C-empty (empty_ctx))

(test-equal (term (load_typdef C-empty "t" EXT))
            (ctx-of '((("t" EXT)) () () ()) '(() () () ())))
(test-equal (term (load_reldef C-empty "R" (EXT "R")))
            (ctx-of '(() (("R" (EXT "R"))) () ()) '(() () () ())))
(test-equal (term (load_funcdef C-empty "f" (EXT "f")))
            (ctx-of '(() () (("f" (EXT "f"))) ()) '(() () () ())))

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
              (EXTFUNC "e" () () NAT)
              (BUILTINFUNC "rev_" ("X") ((EXP (ITER (VAR "X" ()) STAR))) (ITER (VAR "X" ()) STAR))
              (TABLEFUNC "tbl" ((EXP NAT)) NAT (tblrow-ex))
              (FUNC "f" () ((EXP NAT)) NAT (clause-ex) ()))))
 (ctx-of `((("json" EXT) ("t" (DEF ("X") (ALIAS NAT))))
           (("Ext" (EXT "Ext")) ("R" (DEF (,(term rulgroup-ex)) (,(term elsgroup-ex)))))
           (("e" (EXT "e"))
            ("rev_" (BUILTIN "rev_" ("X") ((EXP (ITER (VAR "X" ()) STAR)))))
            ("tbl" (TABLE ((EXP NAT)) (,(term tblrow-ex))))
            ("f" (DEF () (,(term clause-ex)) ())))
           ())
         '(() () () ())))

(test-equal (term (load C-empty ())) (term C-empty))

;; A later definition replaces an earlier one where it stands.
(test-equal
 (term (load C-empty
             ((FUNC "f" () () NAT () ())
              (EXTFUNC "g" () () NAT)
              (EXTFUNC "f" () () NAT))))
 (ctx-of '(() () (("f" (EXT "f")) ("g" (EXT "g"))) ()) '(() () () ())))

;; Loaded scripts: the ids of each kind, in order of first definition.
(define (ids-of script heads)
  (remove-duplicates
   (for/list ([defn (in-list script)] #:when (memq (car defn) heads))
     (cadr defn))))

(define (test-load path)
  (define script (boot-script path))
  (define C (term (load C-empty ,script)))
  (test-match AL-context ctx C)
  (define global (list-ref C 1))
  (test-equal (map car (list-ref global 1)) (ids-of script '(EXTTYP TYP)))
  (test-equal (map car (list-ref global 3)) (ids-of script '(EXTREL REL)))
  (test-equal (map car (list-ref global 5))
              (ids-of script '(EXTFUNC BUILTINFUNC TABLEFUNC FUNC)))
  (test-equal (list-ref global 7) '())
  (test-equal (list-ref C 3) (term (empty_layer))))

(for ([file (in-list (directory-list (repo-path "examples") #:build? #t))]
      #:when (path-has-extension? file #".watsup"))
  (test-load file))

;; spec/ is left out: with precise patterns, loading it takes about 27 minutes
;; (see CROSS_REDEX.md, Step 4).
(test-load (repo-path "spec-meta" "al"))

;;
;; Adders
;;

(test-equal (term (add_vari C-empty ("x" NAT (STAR)) (NAT 1)))
            (ctx-of '(() () () ()) '(() () () ((("x" (STAR)) (NAT 1))))))
(test-equal (term (add_varr C-empty ("x" (STAR)) (NAT 1)))
            (ctx-of '(() () () ()) '(() () () ((("x" (STAR)) (NAT 1))))))
(test-equal (term (add_vari C-vals ("x" INT ()) (NAT 9)))
            (ctx-of '(() () () ())
                    `(() () () ,(cons '(("x" ()) (NAT 9)) (cdr (local-vals (term C-vals)))))))

(test-equal (term (add_varis C-empty (("x" NAT ()) ("y" BOOL (QUEST))) ((NAT 1) (BOOL #t))))
            (ctx-of '(() () () ())
                    '(() () () ((("x" ()) (NAT 1)) (("y" (QUEST)) (BOOL #t))))))
(test-equal (term (add_varis C-empty () ())) (term C-empty))
(check-exn #rx"adds_map" (λ () (term (add_varis C-empty (("x" NAT ())) ()))))
(test-equal (term (add_varrs C-empty (("x" ()) ("x" ())) ((NAT 1) (NAT 2))))
            (ctx-of '(() () () ()) '(() () () ((("x" ()) (NAT 2))))))

(test-equal (term (add_typ C-empty "X" PARAM))
            (ctx-of '(() () () ()) '((("X" PARAM)) () () ())))
(test-equal (term (add_func C-empty "f" (EXT "f")))
            (ctx-of '(() () () ()) '(() () (("f" (EXT "f"))) ())))

;;
;; Value finders
;;

(test-equal (term (find_vari C-vals ("x" INT ()))) '((NAT 1)))
(test-equal (term (find_vari C-vals ("xs" NAT (STAR)))) '((LIST ((NAT 2) (NAT 3)))))
;; No clause for an unbound variable: ⊥
(test-equal (term (find_vari C-vals ("x" NAT (STAR)))) '⊥)
(test-equal (term (find_vari C-empty ("x" NAT ()))) '⊥)

(test-equal (term (find_varr C-vals ("x" ()))) '((NAT 1)))
(test-equal (term (find_varr C-vals ("y" ()))) '())

(test-equal (term (find_varis C-vals (("x" NAT ()) ("n" NAT (QUEST)))))
            '(((NAT 1) (OPT ()))))
(test-equal (term (find_varis C-vals ())) '(()))
;; otherwise: some lookup fails
(test-equal (term (find_varis C-vals (("x" NAT ()) ("y" NAT ())))) '())
(test-equal (term (find_varis C-empty (("x" NAT ())))) '())

(test-equal (term (find_varrs C-vals (("x" ()) ("o" (QUEST))))) '(((NAT 1) (OPT ((NAT 7))))))
(test-equal (term (find_varrs C-vals ())) '(()))
;; otherwise: some lookup fails
(test-equal (term (find_varrs C-vals (("y" ()) ("x" ())))) '())

(define-term C-x9 ,(vals-added (term C-empty) '((("x" ()) (NAT 9)))))

(test-equal (term (finds_vari (C-vals C-x9) ("x" NAT ()))) '(((NAT 1) (NAT 9))))
(test-equal (term (finds_vari () ("x" NAT ()))) '(()))
;; otherwise: some lookup fails
(test-equal (term (finds_vari (C-vals C-empty) ("x" NAT ()))) '())
(test-equal (term (finds_vari (C-vals) ("y" NAT ()))) '())

;;
;; Typedef, function, and relation finders
;;

(define-term C-defs
  ,(ctx-of '((("t" EXT) ("u" PARAM))
             (("R" (EXT "R")))
             (("f" (EXT "f")) ("g" (EXT "g")))
             ())
           '((("t" PARAM))
             (("S" (EXT "S")))
             (("f" (BUILTIN "f" () ())))
             ())))

(test-equal (term (find_typ C-defs "t")) '(PARAM))
(test-equal (term (find_typ C-defs "u")) '(PARAM))
(test-equal (term (find_typ C-defs "v")) '())

(test-equal (term (find_func C-defs "f")) '((BUILTIN "f" () ())))
(test-equal (term (find_func C-defs "g")) '((EXT "g")))
;; otherwise: found in neither layer
(test-equal (term (find_func C-defs "h")) '())

;; Only the global layer holds relations.
(test-equal (term (find_rel C-defs "R")) '((EXT "R")))
(test-equal (term (find_rel C-defs "S")) '())

;;
;; Sub-contexts
;;

;; $sub_opt: every lookup is OPT val, or every lookup is OPT eps
(test-equal (term (sub_opt C-vals (("o" NAT ()) ("p" NAT ()))))
            (list (vals-added (term C-vals) '((("o" ()) (NAT 7)) (("p" ()) (NAT 8))))))
(test-equal (term (sub_opt C-vals (("n" NAT ())))) '())
;; An empty vari* takes the first clause.
(test-equal (term (sub_opt C-vals ())) (list (term C-vals)))
;; A mix, an unbound variable, or a non-option: ⊥
(test-equal (term (sub_opt C-vals (("o" NAT ()) ("n" NAT ())))) '⊥)
(test-equal (term (sub_opt C-vals (("q" NAT ())))) '⊥)
(test-equal (term (sub_opt C-vals (("w" NAT ())))) '⊥)

;; $sub_list: one context per element, with the lists transposed
(test-equal (term (sub_list C-vals (("xs" NAT ()) ("ys" NAT ()))))
            (list (vals-added (term C-vals) '((("xs" ()) (NAT 2)) (("ys" ()) (NAT 4))))
                  (vals-added (term C-vals) '((("xs" ()) (NAT 3)) (("ys" ()) (NAT 5))))))
(test-equal (term (sub_list C-vals (("zs" NAT ()))))
            (list (vals-added (term C-vals) '((("zs" ()) (NAT 6))))))
;; No iterated variables, or empty lists: no contexts
(test-equal (term (sub_list C-vals ())) '())
(test-equal (term (sub_list C-vals (("es" NAT ())))) '())
;; An unbound variable or a non-list: ⊥
(test-equal (term (sub_list C-vals (("q" NAT ())))) '⊥)
(test-equal (term (sub_list C-vals (("v" NAT ())))) '⊥)
;; Lists of different lengths
(check-exn #rx"cannot transpose"
           (λ () (term (sub_list C-vals (("xs" NAT ()) ("zs" NAT ()))))))

(test-results)
