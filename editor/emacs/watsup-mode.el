;;; watsup-mode.el --- Major mode for Watsup specification language -*- lexical-binding: t; -*-
;;; Commentary:
;; Derived from p4spec/lib/frontend/lexer.mll
;;; Code:

(defvar watsup-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Line comments ;; ... newline (comment style a)
    (modify-syntax-entry ?\; ". 12" st)
    (modify-syntax-entry ?\n ">" st)
    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    ;; Identifier constituents
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?' "w" st)
    (modify-syntax-entry ?$ "w" st)
    st)
  "Syntax table for `watsup-mode'.
Nested block comments (; ... ;) are applied by `watsup-syntax-propertize'.")

;; Nested block comments (; ... ;) as comment style b (so newline, which ends
;; the style-a ;; line comment, does not terminate them).  The `n' flag makes
;; them nest; delimiter chars are tagged only when they form (; or ;), so the
;; ;; line comment above is left untouched.
(defconst watsup-syntax-propertize
  (syntax-propertize-rules
   ("\\((\\)\\(;\\)" (1 ". 1nb") (2 ". 2nb"))
   ("\\(;\\)\\()\\)" (1 ". 3nb") (2 ". 4nb")))
  "Assign nestable style-b comment syntax to (; and ;) delimiters.")

(defconst watsup-keywords
  '("syntax" "extern" "tbl" "relation" "rulegroup" "rule" "var"
    "builtin" "dec" "def" "if" "otherwise" "debug")
  "Watsup keywords.")

(defconst watsup-types
  '("bool" "nat" "int" "text")
  "Watsup built-in types.")

(defconst watsup-constants
  '("eps" "true" "false")
  "Watsup constants.")

(defconst watsup-font-lock-keywords
  (list
   ;; Comments (;; and nested (; ;)) are fontified from syntax, not here.

   ;; %latex directive
   '("%latex\\_>" . font-lock-preprocessor-face)

   ;; hint( ... )
   '("\\_<hint\\ze(" . font-lock-preprocessor-face)

   ;; Keywords / types / constants
   `(,(regexp-opt watsup-keywords 'symbols) . font-lock-keyword-face)
   `(,(regexp-opt watsup-types 'symbols) . font-lock-type-face)
   `(,(regexp-opt watsup-constants 'symbols) . font-lock-constant-face)

   ;; Function names after dec/def (may be $-prefixed)
   '("\\_<\\(?:dec\\|def\\)\\s-+\\(\\$?[a-zA-Z_][a-zA-Z0-9_']*\\)"
     (1 font-lock-function-name-face))

   ;; Relation / var names after their keyword
   '("\\_<\\(?:relation\\|var\\)\\s-+\\([a-zA-Z_$][a-zA-Z0-9_$']*\\)"
     (1 font-lock-function-name-face))

   ;; Rule names (base part and /variant part)
   '("\\_<rule\\(?:group\\)?\\s-+\\([a-zA-Z_$][a-zA-Z0-9_$']*\\)\\(/[a-zA-Z0-9_$']*\\)?"
     (1 font-lock-function-name-face)
     (2 font-lock-variable-name-face nil t))

   ;; Silent tag: _UPID
   '("\\_<_[A-Z][a-zA-Z0-9_']*" . font-lock-constant-face)

   ;; Concrete operator literal '...'
   '("'[^'\n]*'" . font-lock-string-face)

   ;; Function calls with $ prefix
   '("\\$[a-zA-Z_][a-zA-Z0-9_']*" . font-lock-function-name-face)

   ;; Dot-prefixed field id: .id
   '("\\.[a-zA-Z_][a-zA-Z0-9_']*" . font-lock-variable-name-face)

   ;; Type arguments in angle brackets: <foo>
   '("<\\([a-zA-Z_][a-zA-Z0-9_]*\\)>" (1 font-lock-type-face))

   ;; Backtick target brackets: `( `) `[ `] `{ `} `< `>
   '("`[]()[{}<>]" . font-lock-preprocessor-face)

   ;; String literals
   '("\"[^\"]*\"" . font-lock-string-face)

   ;; Numbers: hex, nat, signed
   '("\\_<0x[0-9A-F]+\\(?:_[0-9A-F]+\\)*\\_>" . font-lock-constant-face)
   '("\\_<[0-9]+\\(?:_[0-9]+\\)*\\_>" . font-lock-constant-face)

   ;; Holes: !% %% %N %
   '("!?%\\(?:%\\|[0-9]+\\)?" . font-lock-builtin-face)

   ;; Operators (multi-character first)
   '("==>\\|<=>\\|~>\\*\\|~>\\|=>_\\|->_\\|=>\\|->\\|=/=\\|<=\\|>=\\|:=\\|::\\|:/\\|<:\\|<-\\|>(\\|~~\\|/\\\\\\|\\\\/\\|\\+\\+\\|--\\|\\.\\.\\.\\|\\.\\.\\||-\\|-|\\|##"
     . font-lock-builtin-face)
   '("[:;,.|=<>~?^$#*/\\\\+-]" . font-lock-builtin-face))
  "Keyword highlighting for Watsup mode.")

;;;###autoload
(define-derived-mode watsup-mode prog-mode "Watsup"
  "Major mode for editing Watsup specification files."
  :syntax-table watsup-mode-syntax-table
  (setq-local comment-start ";; ")
  (setq-local comment-end "")
  (setq-local comment-start-skip ";;+[ \t]*\\|(;[ \t]*")
  (setq-local parse-sexp-lookup-properties t)
  (setq-local syntax-propertize-function watsup-syntax-propertize)
  (setq-local font-lock-defaults '(watsup-font-lock-keywords))
  (setq-local indent-line-function 'indent-relative))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.watsup\\'" . watsup-mode))

(provide 'watsup-mode)

;;; watsup-mode.el ends here
