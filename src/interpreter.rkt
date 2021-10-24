#lang typed/racket/base

(require racket/match)
(require racket/string)
(require racket/format)
(require "expr.rkt")
(require "stmt.rkt")
(require "token.rkt")
(require "error.rkt")
(require "env.rkt")

(provide interpret! make-interpreter)

(struct interpreter ([env : Env]))
(define-type Interpreter interpreter)

(: make-interpreter (->* () (Env) Interpreter))
(define (make-interpreter [env (make-environment)])
  (interpreter env))

(: interpret! (-> Interpreter (Listof Stmt) Void))
(define (interpret! i statements)
  (define (handle-runtime-error e) (void))
  (with-handlers ([exn:runtime-error? handle-runtime-error])
    (for ([statement statements])
      (execute i statement))))

#| Statements |#

(: execute (-> Interpreter Stmt Void))
(define (execute i stmt)
  (cond
    [(expression-stmt? stmt) (eval-expression-stmt i stmt)]
    [(print-stmt? stmt) (eval-print-stmt i stmt)]
    [(var-stmt? stmt) (eval-var-stmt i stmt)]))

(: eval-expression-stmt (-> Interpreter ExpressionStmt Void))
(define (eval-expression-stmt i stmt)
  (evaluate i (expression-stmt-expr stmt))
  (void))

(: eval-print-stmt (-> Interpreter PrintStmt Void))
(define (eval-print-stmt i stmt)
  (define value (evaluate i (print-stmt-value stmt)))
  (displayln (value->string value)))

(: eval-var-stmt (-> Interpreter VarStmt Void))
(define (eval-var-stmt i stmt)
  (match-define (var-stmt name initializer) stmt)
  (define value (if initializer (evaluate i initializer) null))
  (env-define (interpreter-env i) (token-lexeme name) value))

#| Expressions |#

(: evaluate (-> Interpreter Expr Any))
(define (evaluate i expr)
  (cond
    [(literal? expr) (eval-literal i expr)]
    [(variable? expr) (eval-variable-expression i expr)]
    [(assign? expr) (eval-assign i expr)]
    [(grouping? expr) (eval-grouping i expr)]
    [(unary? expr) (eval-unary i expr)]
    [(binary? expr) (eval-binary i expr)]))

(: eval-literal (-> Interpreter LiteralExpr Any))
(define (eval-literal i expr)
  (literal-value expr))

(: eval-variable-expression (-> Interpreter VariableExpr Any))
(define (eval-variable-expression i expr)
  (env-get (interpreter-env i) (variable-name expr)))

(: eval-assign (-> Interpreter AssignExpr Any))
(define (eval-assign i expr)
  (match-define (assign name val) expr)
  (define value (evaluate i val))
  (env-assign (interpreter-env i) name value)
  value) ; return the assigned value

(: eval-grouping (-> Interpreter GroupingExpr Any))
(define (eval-grouping i expr)
  (evaluate i (grouping-expression expr)))

(: eval-unary (-> Interpreter UnaryExpr Any))
(define (eval-unary i expr)
  (match-define (unary operator r) expr)
  (define right (evaluate i r))
  (match (token-type operator)
    [(quote MINUS) 
     (check-number-operand operator right)
     (- right)]
    [(quote BANG) (not (truthy? right))]))

(: eval-binary (-> Interpreter BinaryExpr Any))
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

#| Helpers |#

; lox evaluates false and null literals to false
(: truthy? (-> Any Boolean))
(define (truthy? v)
  (and v (not (null? v))))

(define lox-equal? equal?)

(define-syntax-rule (check-number-operand operator operand)
  (begin
    (unless (number? operand)
      (raise-runtime-error operator "Operand must be a number."))
    (assert operand number?)))

(define-syntax-rule (check-number-operands operator left right)
  (begin
    (unless (and (real? left) (real? right))
      (raise-runtime-error operator "Operands must be numbers."))
    (assert left real?)
    (assert right real?)))

(: value->string (-> Any String))
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
