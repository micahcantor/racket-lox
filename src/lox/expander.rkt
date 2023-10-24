#lang racket/base

(require "lib/interpreter.rkt")
(require "lib/resolver.rkt")

(define interpreter (make-interpreter))
(define resolver (make-resolver interpreter))

(define-syntax-rule (lox-module-begin STMT ...)
  (#%module-begin 
    (void
      (resolve-all! resolver (list STMT ...))
      (interpret! interpreter (list STMT ...)))))

(provide #%top #%app #%datum #%top-interaction)
(provide (rename-out [lox-module-begin #%module-begin]))