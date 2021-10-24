#lang typed/racket/base

(provide (all-defined-out))

(define-syntax-rule (while pred ...)
  (let loop : Void ()
    (when pred
      ...
      (loop))))

