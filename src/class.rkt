#lang typed/racket/base

(require racket/match)
(require "function.rkt")

(provide (all-defined-out))

(define-type Class class)
(struct class ([name : String] [superclass : (Option Class)] [methods : (HashTable String Function)]))

(: make-class (-> String (Option Class) (HashTable String Function) Class))
(define (make-class name superclass methods)
  (class name superclass methods))

(: class-has-method? (-> Class String Boolean))
(define (class-has-method? c name)
  (hash-has-key? (class-methods c) name))

(: class-find-method (-> Class String (Option Function)))
(define (class-find-method c name)
  (match-define (class name superclass methods) c)
  (if superclass
      (class-find-method superclass name)
      (hash-ref (class-methods c) name #f)))