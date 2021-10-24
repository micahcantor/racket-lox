#lang typed/racket/base

(require "error.rkt")
(require "token.rkt")

(provide Env make-environment env-define env-get env-assign)

(struct env ([values : (HashTable String Any)]))
(define-type Env env)

(: make-environment (-> Env))
(define (make-environment)
  (env (make-hash)))

(: env-define (-> Env String Any Void))
(define (env-define e name value)
  (hash-set! (env-values e) name value))

(: env-get (-> Env Token Any))
(define (env-get e name)
  (define lexeme (token-lexeme name))
  (define val (hash-ref (env-values e) lexeme #f))
  (if val val (raise-undefined-variable-error name lexeme)))

(: env-assign (-> Env Token Any Void))
(define (env-assign e name value)
  (define lexeme (token-lexeme name))
  (unless (hash-has-key? (env-values e) lexeme)
    (raise-undefined-variable-error name lexeme))
  (env-define e lexeme value))