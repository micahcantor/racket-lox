#lang racket/base

(require racket/match)
(require racket/string)
(require racket/format)
(require "expr.rkt")
(require "stmt.rkt")
(require "token.rkt")
(require "error.rkt")
(require "env.rkt")

(provide interpret make-interpreter)

(struct interpreter (env))

(define (make-interpreter [env (make-environment)])
  (interpreter env))

(define (interpret i statements)
  (define (handle-runtime-error e) null)
  (with-handlers ([exn:runtime-error? handle-runtime-error])
    (for ([statement statements])
      (execute i statement))))

(define (execute i stmt)
  (cond
    [(expression-stmt? stmt) (eval-expression-stmt i stmt)]
    [(print-stmt? stmt) (eval-print-stmt i stmt)]
    [(var-stmt? stmt) (eval-var-stmt i stmt)]))

(define (eval-expression-stmt i stmt)
  (evaluate i (expression-stmt-expr stmt)))

(define (eval-print-stmt i stmt)
  (define value (evaluate i (print-stmt-value stmt)))
  (displayln (value->string value)))

(define (eval-var-stmt i stmt)
  (match-define (var-stmt name initializer) stmt)
  (define value (if initializer (evaluate i initializer) null))
  (env-define (interpreter-env i) (token-lexeme name) value))

(define (evaluate i expr)
  (cond
    [(literal? expr) (eval-literal i expr)]
    [(variable? expr) (eval-variable-expression i expr)]
    [(grouping? expr) (eval-grouping i expr)]
    [(unary? expr) (eval-unary i expr)]
    [(binary? expr) (eval-binary i expr)]))

(define (eval-literal i expr)
  (literal-value expr))

(define (eval-variable-expression i expr)
  (env-get (interpreter-env i) (variable-name expr)))

(define (eval-grouping i expr)
  (evaluate i (grouping-expression expr)))

(define (eval-unary i expr)
  (match-define (unary operator r) expr)
  (define right (evaluate i r))
  (match (token-type operator)
    [(quote MINUS) 
     (check-number-operand operator right)
     (- right)]
    [(quote BANG) (not (truthy? right))]))

(define (eval-binary i expr)
  (match-define (binary l operator r) expr)
  (define left (evaluate i l))
  (define right (evaluate i r))
  (match (token-type operator)
    [(quote BANG_EQUAL) (not (lox-equal? left right))]
    [(quote EQUAL_EQUAL) (lox-equal? left right)]
    [(quote GREATER)
     (check-number-operands operator left right) 
     (> left right)]
    [(quote GREATER_EQUAL) 
     (check-number-operands operator left right) 
     (>= left right)]
    [(quote LESS) 
     (check-number-operands operator left right) 
     (< left right)]
    [(quote LESS_EQUAL) 
     (check-number-operands operator left right) 
     (<= left right)]
    [(quote MINUS) 
     (check-number-operands operator left right) 
     (- left right)]
    [(quote SLASH) 
     (check-number-operands operator left right) 
     (/ left right)]
    [(quote STAR) 
     (check-number-operands operator left right) 
     (* left right)]
    [(quote PLUS) 
     (cond
       [(and (string? left) (string? right))
        (string-append left right)]
       [(and (number? left) (number? right))
        (+ left right)]
       [else
        (raise-runtime-error operator "Operands must be two numbers or two strings.")])]))

; lox evaluates false and null literals to false
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
    [(equal? v #t) "true"]
    [(equal? v #f) "false"]
    [(null? v) "nil"]
    [(number? v)
     (define text (number->string v))
     (if (string-suffix? text ".0")
         (substring text (- (string-length text) 2))
         text)]
    [else (~a v)]))
