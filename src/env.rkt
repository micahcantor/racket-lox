#lang racket/base

(require "error.rkt")
(require "token.rkt")

(provide make-environment env-define env-get)

(struct env (values))

(define (make-environment)
  (env (make-hash)))

(define (env-define e name value)
  (hash-set! (env-values e) name value))

(define (env-get e name)
  (define lexeme (token-lexeme name))
  (if (hash-has-key? (env-values e) lexeme)
      (hash-ref (env-values e) lexeme)
      (raise-runtime-error 
       name 
       (format "Undefined variable '~a'." lexeme))))