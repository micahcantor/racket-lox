#lang typed/racket/base

(require "expr.rkt")
(require "token.rkt")
(provide (all-defined-out))

(struct stmt () #:transparent)
(define-type Stmt stmt)

(struct print-stmt stmt ([value : Expr]) #:transparent)
(define-type PrintStmt print-stmt)

(struct expression-stmt stmt ([expr : Expr]) #:transparent)
(define-type ExpressionStmt expression-stmt)

(struct var-decl stmt ([name : Token] [initializer : (Option Expr)]) #:transparent)
(define-type VarDecl var-decl)

(struct fun-decl stmt ([name : Token] [params : (Vectorof Token)] [body : BlockStmt]) #:transparent)
(define-type FunDecl fun-decl)

(struct class-decl stmt ([name : Token] [methods : (Listof FunDecl)]) #:transparent)
(define-type ClassDecl class-decl)

(struct block-stmt stmt ([statements : (Listof Stmt)]) #:transparent)
(define-type BlockStmt block-stmt)

(struct if-stmt stmt ([condition : Expr]
                      [consequent : Stmt]
                      [alternate : (Option Stmt)])
  #:transparent)
(define-type IfStmt if-stmt)

(struct while-stmt stmt ([condition : Expr] [body : Stmt]) #:transparent)
(define-type WhileStmt while-stmt)

(struct return-stmt stmt ([keyword : Token] [value : (Option Expr)]))
(define-type ReturnStmt return-stmt)
