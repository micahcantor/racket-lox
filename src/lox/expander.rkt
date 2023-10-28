#lang racket/base

(require "lib/interpreter.rkt")
(require "lib/resolver.rkt")
(require "lib/error.rkt")

(define interpreter (make-interpreter))
(define resolver (make-resolver interpreter))

(define-syntax-rule (run stmts)
  (unless had-error ; parser error
    (resolve-all! resolver stmts)
    (unless had-error ; resolver error
      (interpret! interpreter stmts))))

(define-syntax-rule (lox-module-begin STMT ...)
  (#%module-begin (run (list STMT ...))))

(provide
  (rename-out [lox-module-begin #%module-begin])
  #%top #%app #%datum #%top-interaction)