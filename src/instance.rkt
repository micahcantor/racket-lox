#lang typed/racket/base

(require racket/match)
(require "function.rkt")
(require "env.rkt")
(require "class.rkt")
(require "token.rkt")
(require "error.rkt")

(provide (all-defined-out))

(define-type Instance instance)
(struct instance ([class : Class] [fields : (HashTable String Any)]))

(: make-instance (-> Class Instance))
(define (make-instance class)
  (define fields : (HashTable String Any) (make-hash))
  (instance class fields))

(: instance-get (-> Instance Token Any))
(define (instance-get in name)
  (match-define (instance class fields) in)
  (define lexeme (token-lexeme name))
  (cond 
    [(hash-has-key? fields lexeme)
     (hash-ref fields lexeme)]
    [(class-has-method? class lexeme)
     (bind (assert (class-find-method class lexeme)) in)]
    [else 
     (raise-runtime-error name (format "Undefined property '~a'." lexeme))]))

(: instance-set! (-> Instance Token Any Void))
(define (instance-set! instance name value)
  (hash-set! (instance-fields instance) (token-lexeme name) value))

; Create a new function bound to an instance of a class, by
; creating a new environment with "this" local variable
; bound to the given instance.
(: bind (-> Function Instance Function))
(define (bind fun in)
  (match-define (function declaration closure is-initalizer?) fun)
  (define env (make-env closure))
  (env-define env "this" in)
  (function declaration env is-initalizer?))

(: instance->string (-> Instance String))
(define (instance->string instance)
  (define name (class-name (instance-class instance)))
  (format "~a instance" name))