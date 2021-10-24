#lang typed/racket/base

(require racket/match)
(require racket/string)
(require racket/format)
(require "expr.rkt")
(require "stmt.rkt")
(require "token.rkt")
(require "error.rkt")
(require "env.rkt")
(require "utils/while.rkt")

(provide interpret! make-interpreter)

(struct interpreter ([env : Env]) #:mutable)
(define-type Interpreter interpreter)

(: make-interpreter (->* () (Env) Interpreter))
(define (make-interpreter [env (make-env)])
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
    [(expression-stmt? stmt) (exec-expression-stmt i stmt)]
    [(print-stmt? stmt) (exec-print-stmt i stmt)]
    [(block-stmt? stmt) (exec-block-stmt i stmt)]
    [(if-stmt? stmt) (exec-if-stmt i stmt)]
    [(while-stmt? stmt) (exec-while-stmt i stmt)]
    [(var-stmt? stmt) (exec-var-stmt i stmt)]))

(: exec-expression-stmt (-> Interpreter ExpressionStmt Void))
(define (exec-expression-stmt i stmt)
  (evaluate i (expression-stmt-expr stmt))
  (void))

(: exec-print-stmt (-> Interpreter PrintStmt Void))
(define (exec-print-stmt i stmt)
  (define value (evaluate i (print-stmt-value stmt)))
  (displayln (value->string value)))

(: exec-block-stmt (-> Interpreter BlockStmt Void))
(define (exec-block-stmt i stmt)
  (define statements (block-stmt-statements stmt))
  (define block-env (make-env (interpreter-env i)))
  (define previous (interpreter-env i))
  (set-interpreter-env! i block-env)
  (for ([statement statements])
    (execute i statement))
  (set-interpreter-env! i previous))

(: exec-if-stmt (-> Interpreter IfStmt Void))
(define (exec-if-stmt i stmt)
  (match-define (if-stmt condition consequent alternate) stmt)
  (cond
    [(truthy? (evaluate i condition)) (execute i consequent)]
    [alternate (execute i alternate)]))

(: exec-while-stmt (-> Interpreter WhileStmt Void))
(define (exec-while-stmt i stmt)
  (match-define (while-stmt condition body) stmt)
  (while (truthy? (evaluate i condition))
         (execute i body)))

(: exec-var-stmt (-> Interpreter VarStmt Void))
(define (exec-var-stmt i stmt)
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
    [(quote OR)
     (if (truthy? left) left right)]
    [(quote AND)
     (if (truthy? left) right left)]
    [(quote BANG_EQUAL)
     (not (lox-equal? left right))]
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

; raises error if operand is not number, then asserts the predicate.
(define-syntax-rule (check-number-operand operator operand)
  (begin
    (unless (number? operand)
      (raise-runtime-error operator "Operand must be a number."))
    (assert operand number?)))

; raises error if operands are not numbers, then asserts the predicates.
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
