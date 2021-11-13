#lang typed/racket/base

(require "expr.rkt")
(require "token.rkt")
(provide (all-defined-out))

(define-type Stmt
  (U PrintStmt ExpressionStmt VarDecl FunDecl ClassDecl
     BlockStmt IfStmt WhileStmt ReturnStmt EmptyStmt))

(struct print-stmt ([value : Expr]))
(define-type PrintStmt print-stmt)

(struct expression-stmt ([expr : Expr]))
(define-type ExpressionStmt expression-stmt)

(struct var-decl ([name : Token] [initializer : (Option Expr)]))
(define-type VarDecl var-decl)

(struct fun-decl ([name : Token] [params : (Vectorof Token)] [body : (Listof Stmt)]))
(define-type FunDecl fun-decl)

(struct class-decl ([name : Token] [superclass : (Option VariableExpr)] [methods : (Listof FunDecl)]))
(define-type ClassDecl class-decl)

(struct block-stmt ([statements : (Listof Stmt)]))
(define-type BlockStmt block-stmt)

(struct if-stmt ([condition : Expr]
                 [consequent : Stmt]
                 [alternate : (Option Stmt)]))
(define-type IfStmt if-stmt)

(struct while-stmt ([condition : Expr] [body : Stmt]))
(define-type WhileStmt while-stmt)

(struct return-stmt ([keyword : Token] [value : (Option Expr)]))
(define-type ReturnStmt return-stmt)

; empty statement created after an error is found.
(struct empty-stmt ())
(define-type EmptyStmt empty-stmt)