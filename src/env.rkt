#lang racket/base

(require "error.rkt")
(require "token.rkt")

(provide make-environment env-define env-get env-assign)

(struct env (values))

(define (make-environment)
  (env (make-hash)))

(define (env-define e name value)
  (hash-set! (env-values e) name value))

(define (env-get e name)
  (define lexeme (token-lexeme name))
  (define val (hash-ref (env-values e) lexeme #f))
  (if val val (raise-undefined-variable-error name lexeme)))

(define (env-assign e name value)
  (define lexeme (token-lexeme name))
  (if (hash-has-key? (env-values e) lexeme)
      (env-define e lexeme value)
      (raise-undefined-variable-error name lexeme)))