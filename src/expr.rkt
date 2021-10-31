#lang typed/racket/base

(require "token.rkt")

(provide (all-defined-out))

(struct expr () #:transparent)
(define-type Expr expr)

(struct assign expr ([name : Token] [value : Expr]))
(define-type AssignExpr assign)

(struct binary expr ([left : Expr] [operator : Token] [right : Expr]) #:transparent)
(define-type BinaryExpr binary)

(struct call expr ([callee : Expr] [paren : Token] [args : (Listof Expr)]) #:transparent)
(define-type CallExpr call)

(struct get expr ([object : Expr] [name : Token]))
(define-type GetExpr get)

(struct grouping expr ([expression : Expr]) #:transparent)
(define-type GroupingExpr grouping)

(struct literal expr ([value : Lox-Literal]) #:transparent)
(define-type LiteralExpr literal)

(struct set-expr expr ([object : Expr] [name : Token] [value : Expr]) #:transparent)
(define-type SetExpr set-expr)

(struct this-expr expr ([keyword : Token]))
(define-type ThisExpr this-expr)

(struct unary expr ([operator : Token] [right : Expr]) #:transparent)
(define-type UnaryExpr unary)

(struct variable expr ([name : Token]) #:transparent)
(define-type VariableExpr variable)
