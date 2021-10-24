#lang typed/racket/base

(require "token.rkt")

(provide (all-defined-out))

(struct expr () #:transparent)
(define-type Expr expr)

(struct assign expr ([name : Token] [value : Expr]))
(define-type AssignExpr assign)

(struct binary expr ([left : Expr] [operator : Token] [right : Expr]) #:transparent)
(define-type BinaryExpr binary)

(struct grouping expr ([expression : Expr]) #:transparent)
(define-type GroupingExpr grouping)

(struct literal expr ([value : Lox-Literal]) #:transparent)
(define-type LiteralExpr literal)

(struct unary expr ([operator : Token] [right : Expr]) #:transparent)
(define-type UnaryExpr unary)

(struct variable expr ([name : Token]) #:transparent)
(define-type VariableExpr variable)
