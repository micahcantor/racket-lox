#lang typed/racket/base

(require racket/match)
(require "error.rkt")
(require "token.rkt")

(provide (all-defined-out))

(struct env ([values : (HashTable String Any)]
             [enclosing : (Option Env)]))
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
  val)

(: env-get-at (-> Env Integer String Any))
(define (env-get-at e dist name)
  (define vals (env-values (env-ancestor e dist)))
  (hash-ref vals name))

(: env-ancestor (-> Env Integer Env))
(define (env-ancestor e dist)
  (define env e)
  (for ([i (in-range dist)])
    (set! env (assert (env-enclosing env))))
  env)

(: env-assign (-> Env Token Any Void))
(define (env-assign e name value)
  (define lexeme (token-lexeme name))
  (define-values (variable defined-env)
    (env-member e name))
  (env-define defined-env lexeme value))

(: env-assign-at (-> Env Integer Token Any Void))
(define (env-assign-at e dist name value)
  (define vals (env-values (env-ancestor e dist)))
  (hash-set! vals (token-lexeme name) value))

(: env-member (-> Env Token (Values Any Env)))
(define (env-member e name)
  (match-define (env env-values enclosing) e)
  (define lexeme (token-lexeme name))
  (cond
    [(hash-has-key? env-values lexeme)
     (values (hash-ref env-values lexeme) e)]
    [enclosing (env-member enclosing name)]
    [else 
      (raise-undefined-variable-error name lexeme)
      (values null (make-env))]))