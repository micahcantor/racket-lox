#lang typed/racket/base

(require "expr.rkt")
(require "token.rkt")
(provide (all-defined-out))

(struct stmt ())
(define-type Stmt stmt)

(struct print-stmt stmt ([value : Expr]))
(define-type PrintStmt print-stmt)

(struct expression-stmt stmt ([expr : Expr]))
(define-type ExpressionStmt expression-stmt)

(struct var-stmt stmt ([name : Token] [initializer : (Option Expr)]))
(define-type VarStmt var-stmt)