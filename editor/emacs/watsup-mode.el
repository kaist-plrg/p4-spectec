;;; watsup-mode.el --- Major mode for Watsup specification language -*- lexical-binding: t; -*-
;;; Code:

(defvar watsup-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Comments start with ;;
    (modify-syntax-entry ?\; ". 12" st)
    (modify-syntax-entry ?\n ">" st)
    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    ;; Operators
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?' "w" st)
    (modify-syntax-entry ?$ "w" st)
    st)
  "Syntax table for `watsup-mode'.")

(defconst watsup-keywords
  '("syntax" "var" "if" "otherwise" "hint" "input" "dec" "def" "rule" "relation")
  "Watsup keywords.")

(defconst watsup-types
  '("int" "nat" "text" "bool" "set" "map")
  "Watsup built-in types.")

(defconst watsup-constants
  '("eps" "true" "false" "infinity")
  "Watsup constants.")

(defconst watsup-font-lock-keywords
  (list
   ;; Comments (handled by syntax table, but we can add emphasis)
   '(";;.*$" . font-lock-comment-face)

   ;; Keywords
   `(,(regexp-opt watsup-keywords 'symbols) . font-lock-keyword-face)

   ;; Types
   `(,(regexp-opt watsup-types 'symbols) . font-lock-type-face)

   ;; Constants
   `(,(regexp-opt watsup-constants 'symbols) . font-lock-constant-face)

   ;; Function/relation names after dec/def/relation keywords
   '("\\<\\(dec\\|def\\|relation\\)\\s-+\\(\\$?[a-zA-Z_][a-zA-Z0-9_$']*\\(?:_<[^>]+>\\)?\\)"
     (2 font-lock-function-name-face))

   ;; Rule names (base part and comment part)
   '("\\<rule\\s-+\\([a-zA-Z_$][a-zA-Z0-9_$']*\\)\\(/[a-zA-Z0-9_$'-]*\\)?"
     (1 font-lock-function-name-face)
     (2 font-lock-variable-name-face nil t))

   ;; Type parameters in angle brackets
   '("<\\([a-zA-Z_][a-zA-Z0-9_]*\\)>" (1 font-lock-type-face))

   ;; Function calls with $ prefix
   '("\\$[a-zA-Z_][a-zA-Z0-9_$']*\\(?:_<[^>]+>\\)?" . font-lock-function-name-face)

   ;; Backtick expressions
   '("`{[^}]*}" . font-lock-preprocessor-face)
   '("`" . font-lock-preprocessor-face)

   ;; String literals
   '("\"[^\"]*\"" . font-lock-string-face)

   ;; Numbers
   '("\\<[0-9]+\\>" . font-lock-constant-face)

   ;; Operators (multi-character operators need to come before single-character)
   '("\\(<==>\\|\\~>\\*\\|\\~>\\|=>_\\|->_\\|=>\\|->\\|<=\\|>=\\|=/=\\|==\\|:=\\|::?\\|/\\\\\\|\\\\/\\|<:\\|:>\\|<<\\|>>\\|\\^|\\^\\|_|_\\|##\\|%%\\|!%\\|##?\\|\\+\\+\\|\\.\\.\\.\\|\\.\\.\\|--\\||-\\|-|\\|>(?\\)"
     . font-lock-builtin-face))
  "Keyword highlighting for Watsup mode.")

;;;###autoload
(define-derived-mode watsup-mode prog-mode "Watsup"
  "Major mode for editing Watsup specification files."
  :syntax-table watsup-mode-syntax-table
  (setq-local comment-start ";; ")
  (setq-local comment-end "")
  (setq-local font-lock-defaults '(watsup-font-lock-keywords))
  (setq-local indent-line-function 'indent-relative))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.watsup\\'" . watsup-mode))

(provide 'watsup-mode)

;;; watsup-mode.el ends here
