#lang racket/base
;; Required by every module in place of Redex itself.

(require redex/reduction-semantics)
(provide (all-from-out redex/reduction-semantics))

;; AL relations reach host state (externs, `$fresh_typeId`, `debug`), and
;; Redex's cache does not know it. See CROSS_REDEX.md, "Caching".
(caching-enabled? #f)
