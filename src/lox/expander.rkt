#lang racket/base

(require "interpreter.rkt")
(require "resolver.rkt")

(define interpreter (make-interpreter))
(define resolver (make-resolver interpreter))

(define-syntax-rule (lox-module-begin stmts)
  (#%module-begin 
    (void 
      (resolve-all! resolver stmts)
      (interpret! interpreter stmts))))

(provide (rename-out [lox-module-begin #%module-begin]))