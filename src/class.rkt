#lang typed/racket/base

(provide (all-defined-out))

(define-type Class class)
(struct class ([name : String]))

(: make-class (-> String Class))
(define (make-class name)
  (class name))