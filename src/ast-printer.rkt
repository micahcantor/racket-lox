#lang racket/base

(require racket/match)
(require racket/format)
(require "expr.rkt")
(require "token.rkt")

(define (expr->string expr)
  (match expr
    [(struct binary (left op right)) 
     (parenthesize (token-lexeme op) left right)]
    [(struct grouping (expression))
     (parenthesize "group" expression)]
    [(struct literal (value))
     (~a value)]
    [(struct unary (op right))
     (parenthesize (token-lexeme op) right)]))

(define (parenthesize name . exprs)
  (define printed-exprs 
    (foldl (λ (x z) (string-append z " " (expr->string x))) "" exprs))
  (string-append "(" name printed-exprs ")"))

(define (test)
  (define expression
    (binary (unary (token (token-types 'MINUS) "-" #f 1)
                   (literal 123))
            (token (token-types 'STAR) "*" #f 1)
            (grouping (literal 45.67))))
  (displayln (expr->string expression)))