#lang typed/racket/base

(provide (all-defined-out))

(define-syntax-rule (while pred body ...)
  (let loop : Void ()
    (when pred
      body ...
      (loop))))

(define-syntax-rule (until pred body ...)
  (let loop : Void ()
    (unless pred
      body ...
      (loop))))