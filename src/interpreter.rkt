#lang racket/base

(require racket/match)
(require racket/string)
(require racket/format)
(require "expr.rkt")
(require "token.rkt")

(provide (struct-out exn:runtime-error) interpret)

(define (interpret expr)
  (define (handle-runtime-error e) null)
  (with-handlers ([exn:runtime-error? handle-runtime-error])
    (define value (evaluate expr))
    (value->string value)))

(define (evaluate expr)
  (cond
    [(literal? expr) (eval-literal expr)]
    [(grouping? expr) (eval-grouping expr)]
    [(unary? expr) (eval-unary expr)]
    [(binary? expr) (eval-binary expr)]))

(define (eval-literal expr)
  (literal-value expr))

(define (eval-grouping expr)
  (evaluate (grouping-expression expr)))

(define (eval-unary expr)
  (define right (evaluate (unary-right expr)))
  (match (token-type (unary-operator expr))
    [MINUS 
     (check-number-operand right)
     (- right)]
    [BANG (not (truthy? right))]))

(define (eval-binary expr)
  (match-define (binary operator l r) expr)
  (define left (evaluate l))
  (define right (evaluate r))
  (match (token-type operator)
    [BANG_EQUAL (not (lox-equal? left right))]
    [EQUAL_EQUAL (lox-equal? left right)]
    [GREATER
     (check-number-operands operator left right) 
     (> left right)]
    [GREATER_EQUAL 
     (check-number-operands operator left right) 
     (>= left right)]
    [LESS 
     (check-number-operands operator left right) 
     (< left right)]
    [LESS_EQUAL 
     (check-number-operands operator left right) 
     (<= left right)]
    [MINUS 
     (check-number-operands operator left right) 
     (- left right)]
    [SLASH 
     (check-number-operands operator left right) 
     (/ left right)]
    [STAR 
     (check-number-operands operator left right) 
     (* left right)]
    [PLUS 
     (cond
       [(and (string? left) (string? right))
        (string-append left right)]
       [(and (number? left) (number? right))
        (+ left right)]
       [else
        (raise-runtime-error operator "Operands must be two numbers or two strings.")])]))

; lox evaluates only false and null to false
(define (truthy? v)
  (and v (not (null? v))))

(define lox-equal? equal?)

(define (check-number-operand operator operand)
  (unless (number? operand)
    (raise-runtime-error operator "Operand must be a number.")))

(define (check-number-operands operator left right)
  (unless (and (number? left) (number? right))
    (raise-runtime-error operator "Operands must be numbers.")))

(define (value->string v) 
  (cond
    [(null? v) "nil"]
    [(number? v)
     (define text (number->string v))
     (if (string-suffix? ".0")
         (substring text (- (string-length text) 2))
         text)]
    [else (~a v)]))

#| Error Handling |#

(struct exn:runtime-error exn:fail (token))

(define (make-runtime-error token message)
  (exn:runtime-error token message (current-continuation-marks)))

(define (raise-runtime-error token message)
  (raise (make-runtime-error token message)))