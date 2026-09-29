#lang racket/base

(require rackunit
         "../common/0-prelude.rkt")

(check-false (caching-enabled?))
