#lang racket/base

(require racket/match)
(require racket/format)
(require "expr.rkt")
(require "token.rkt")

(provide expr->string)

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
    (binary (unary (make-token (token-types 'MINUS) "-")
                   (literal 123))
            (make-token (token-types 'STAR) "*")
            (grouping (literal 45.67))))
  (displayln (expr->string expression)))