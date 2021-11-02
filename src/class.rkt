#lang typed/racket/base

(require "function.rkt")

(provide (all-defined-out))

(define-type Class class)
(struct class ([name : String] [methods : (HashTable String Function)]))

(: make-class (-> String (HashTable String Function) Class))
(define (make-class name methods)
  (class name methods))

(: class-has-method? (-> Class String Boolean))
(define (class-has-method? c name)
  (hash-has-key? (class-methods c) name))

(: class-find-method (-> Class String (Option Function)))
(define (class-find-method c name)
  (hash-ref (class-methods c) name #f))