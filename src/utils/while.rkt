#lang racket/base

(provide while)

(define-syntax-rule (while pred ...)
  (let loop ()
    (when pred
      ...
      (loop))))