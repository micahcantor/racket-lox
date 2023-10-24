#lang typed/racket/base

(require "token.rkt")

(provide (all-defined-out))

(define-type Expr
  (U AssignExpr BinaryExpr CallExpr GetExpr GroupingExpr LiteralExpr
     SetExpr SuperExpr ThisExpr UnaryExpr VariableExpr EmptyExpr))

(struct assign ([name : Token] [value : Expr]))
(define-type AssignExpr assign)

(struct binary ([left : Expr] [operator : Token] [right : Expr]))
(define-type BinaryExpr binary)

(struct call ([callee : Expr] [paren : Token] [args : (Listof Expr)]))
(define-type CallExpr call)

(struct get ([object : Expr] [name : Token]))
(define-type GetExpr get)

(struct grouping ([expression : Expr]))
(define-type GroupingExpr grouping)

(struct literal ([value : Lox-Literal]))
(define-type LiteralExpr literal)

(struct set-expr ([object : Expr] [name : Token] [value : Expr]))
(define-type SetExpr set-expr)

(struct super-expr ([keyword : Token] [method : Token]))
(define-type SuperExpr super-expr)

(struct this-expr ([keyword : Token]))
(define-type ThisExpr this-expr)

(struct unary ([operator : Token] [right : Expr]))
(define-type UnaryExpr unary)

(struct variable ([name : Token]))
(define-type VariableExpr variable)

(struct empty-expr ())
(define-type EmptyExpr empty-expr)