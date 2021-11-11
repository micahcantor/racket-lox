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

(struct var-decl stmt ([name : Token] [initializer : (Option Expr)]))
(define-type VarDecl var-decl)

(struct fun-decl stmt ([name : Token] [params : (Vectorof Token)] [body : (Listof Stmt)]))
(define-type FunDecl fun-decl)

(struct class-decl stmt ([name : Token] [superclass : (Option VariableExpr)] [methods : (Listof FunDecl)]))
(define-type ClassDecl class-decl)

(struct block-stmt stmt ([statements : (Listof Stmt)]))
(define-type BlockStmt block-stmt)

(struct if-stmt stmt ([condition : Expr]
                      [consequent : Stmt]
                      [alternate : (Option Stmt)]))
(define-type IfStmt if-stmt)

(struct while-stmt stmt ([condition : Expr] [body : Stmt]))
(define-type WhileStmt while-stmt)

(struct return-stmt stmt ([keyword : Token] [value : (Option Expr)]))
(define-type ReturnStmt return-stmt)
