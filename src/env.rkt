#lang typed/racket/base

(require "error.rkt")
(require "token.rkt")

(provide (all-defined-out))

(struct env ([values : (HashTable String Any)]
             [enclosing : (Option Env)])
  #:transparent)
(define-type Env env)

(: make-environment (->* () (Env) Env))
(define (make-environment [enclosure #f])
  (env (make-hash) enclosure))

(: env-define (-> Env String Any Void))
(define (env-define e name value)
  (hash-set! (env-values e) name value))

(: env-get (-> Env Token Any))
(define (env-get e name)
  (define lexeme (token-lexeme name))
  (define-values (val _) (env-member e name))
  (unless val
    (raise-undefined-variable-error name lexeme))
  val)

(: env-assign (-> Env Token Any Void))
(define (env-assign e name value)
  (define lexeme (token-lexeme name))
  (define-values (variable defined-env)
    (env-member e name))
  (unless variable
    (raise-undefined-variable-error name lexeme))
  (env-define defined-env lexeme value))

(: env-member (-> Env Token (Values Any Env)))
(define (env-member e name)
  (define val (hash-ref (env-values e) (token-lexeme name) #f))
  ; (printf "env: ~a, val: ~a\n" e val)
  (cond
    [val (values val e)]
    [(env-enclosing e) (env-member (env-enclosing e) name)]
    [else 
      (raise-undefined-variable-error name (token-lexeme name))
      (values null (make-environment))]))