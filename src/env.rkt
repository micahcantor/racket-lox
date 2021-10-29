#lang typed/racket/base

(require "error.rkt")
(require "token.rkt")

(provide (all-defined-out))

(struct env ([values : (HashTable String Any)]
             [enclosing : (Option Env)])
  #:transparent)
(define-type Env env)

(: make-env (->* () (Env) Env))
(define (make-env [enclosure #f])
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

(: env-get-at (-> Env Integer String Any))
(define (env-get-at e dist name)
  (define vals (env-values (env-ancestor e dist)))
  (hash-ref vals name))

(: env-ancestor (-> Env Integer Env))
(define (env-ancestor e dist)
  (define env : Env e)
  (for ([i (in-range dist)])
    (set! env (assert (env-enclosing env))))
  env)

(: env-assign (-> Env Token Any Void))
(define (env-assign e name value)
  (define lexeme (token-lexeme name))
  (define-values (variable defined-env)
    (env-member e name))
  (unless variable
    (raise-undefined-variable-error name lexeme))
  (env-define defined-env lexeme value))

(: env-assign-at (-> Env Integer Token Any Void))
(define (env-assign-at e dist name value)
  (define vals (env-values (env-ancestor e dist)))
  (hash-set! vals (token-lexeme name) value))

(: env-member (-> Env Token (Values Any Env)))
(define (env-member e name)
  (define val (hash-ref (env-values e) (token-lexeme name) #f))
  (cond
    [val (values val e)]
    [(env-enclosing e) (env-member (env-enclosing e) name)]
    [else 
      (raise-undefined-variable-error name (token-lexeme name))
      (values null (make-env))]))