#lang typed/racket/base

(require racket/match)
(require racket/string)
(require racket/format)
(require "expr.rkt")
(require "stmt.rkt")
(require "token.rkt")
(require "error.rkt")
(require "env.rkt")
(require "class.rkt")
(require "function.rkt")
(require "instance.rkt")
(require "utils/while.rkt")

(provide interpret! make-interpreter interpreter-resolve! Interpreter)

(define-type Interpreter interpreter)
(struct interpreter ([env : Env]
                     [globals : Env]
                     [locals : (HashTable Expr Integer)]) #:mutable)

(: make-interpreter (-> Interpreter))
(define (make-interpreter)
  (define globals (make-env))
  (define environment globals)
  (define locals : (HashTable Expr Integer) (make-hash))
  (env-define globals "clock" (make-clock))
  (interpreter environment globals locals))

(: interpret! (-> Interpreter (Listof Stmt) Void))
(define (interpret! i statements)
  (with-handlers ([exn:runtime-error? runtime-error])
    (for ([statement statements])
      (execute i statement))))

#| Resolving |#

(: interpreter-resolve! (-> Interpreter Expr Integer Void))
(define (interpreter-resolve! i expr depth)
  (hash-set! (interpreter-locals i) expr depth))

#| Statements |#

(: execute (-> Interpreter Stmt Void))
(define (execute i stmt)
  (cond
    [(expression-stmt? stmt) (exec-expression-stmt i stmt)]
    [(print-stmt? stmt) (exec-print-stmt i stmt)]
    [(block-stmt? stmt) (exec-block-stmt i stmt)]
    [(if-stmt? stmt) (exec-if-stmt i stmt)]
    [(while-stmt? stmt) (exec-while-stmt i stmt)]
    [(return-stmt? stmt) (exec-return-stmt i stmt)]
    [(class-decl? stmt) (exec-class-decl i stmt)]
    [(fun-decl? stmt) (exec-fun-decl i stmt)]
    [(var-decl? stmt) (exec-var-decl i stmt)]))

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
  (exec-block i statements (make-env (interpreter-env i))))

(: exec-block (-> Interpreter (Listof Stmt) Env Void))
(define (exec-block i statements env)
  (define previous (interpreter-env i))
  (: handle-return (-> Return Any))
  (define (handle-return v)
    (set-interpreter-env! i previous)
    (raise v))
  (set-interpreter-env! i env)
  (with-handlers ([return? handle-return])
    (for ([statement statements])
      (execute i statement)))
  (set-interpreter-env! i previous))

(: exec-if-stmt (-> Interpreter IfStmt Void))
(define (exec-if-stmt i stmt)
  (match-define (if-stmt condition consequent alternate) stmt)
  (cond
    [(truthy? (evaluate i condition))
     (execute i consequent)]
    [alternate
     (execute i alternate)]))

(: exec-while-stmt (-> Interpreter WhileStmt Void))
(define (exec-while-stmt i stmt)
  (match-define (while-stmt condition body) stmt)
  (while (truthy? (evaluate i condition))
         (execute i body)))

(: exec-return-stmt (-> Interpreter ReturnStmt Void))
(define (exec-return-stmt i stmt)
  (define value
    (if (return-stmt-value stmt)
        (evaluate i (return-stmt-value stmt))
        null))
  (raise (make-return value))
  (void))

(: exec-class-decl (-> Interpreter ClassDecl Void))
(define (exec-class-decl i stmt)
  (match-define (class-decl name stmt-superclass methods) stmt)
  (define superclass : (Option Class)
    (cond
      [stmt-superclass
       (define evaluated (evaluate i stmt-superclass))
       (unless (class? evaluated)
         (raise-runtime-error (variable-name stmt-superclass) "Superclass must be a class."))
       (assert evaluated class?)]
      [else #f]))
  (define env (interpreter-env i))
  (env-define env (token-lexeme name) null)
  (when stmt-superclass
    (set! env (make-env env))
    (env-define env "super" superclass))
  (define class-methods : (HashTable String Function) (make-hash))
  (for ([method methods])
    (define is-initalizer? (equal? "init" (token-lexeme (fun-decl-name method))))
    (define fun (function method env is-initalizer?))
    (define name (token-lexeme (fun-decl-name method)))
    (hash-set! class-methods name fun))
  (define lox-class (make-class (token-lexeme name) superclass class-methods))
  (when superclass (set! env (assert (env-enclosing env))))
  (env-assign env name lox-class))

(: exec-fun-decl (-> Interpreter FunDecl Void))
(define (exec-fun-decl i stmt)
  (define env (interpreter-env i))
  (define fun (function stmt env #f))
  (define fun-name (token-lexeme (fun-decl-name stmt)))
  (env-define env fun-name fun))

(: exec-var-decl (-> Interpreter VarDecl Void))
(define (exec-var-decl i stmt)
  (match-define (var-decl name initializer) stmt)
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
    [(call? expr) (eval-call i expr)]
    [(get? expr) (eval-get i expr)]
    [(set-expr? expr) (eval-set-expr i expr)]
    [(super-expr? expr) (eval-super-expr i expr)]
    [(this-expr? expr) (eval-this-expr i expr)]
    [(binary? expr) (eval-binary i expr)]))

(: eval-literal (-> Interpreter LiteralExpr Any))
(define (eval-literal i expr)
  (literal-value expr))

(: eval-variable-expression (-> Interpreter VariableExpr Any))
(define (eval-variable-expression i expr)
  (lookup-variable i (variable-name expr) expr))

(: lookup-variable (-> Interpreter Token Expr Any))
(define (lookup-variable i name expr)
  (match-define (interpreter env globals locals) i)
  (define distance (hash-ref locals expr #f))
  (if distance
      (env-get-at env distance (token-lexeme name))
      (env-get globals name)))

(: eval-assign (-> Interpreter AssignExpr Any))
(define (eval-assign i expr)
  (match-define (interpreter env globals locals) i)
  (match-define (assign name val) expr)
  (define value (evaluate i val))
  (define distance (hash-ref locals expr #f))
  (if distance
      (env-assign-at env distance name value)
      (env-assign globals name value))
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

(: eval-call (-> Interpreter CallExpr Any))
(define (eval-call i expr)
  (match-define (call callee-expr call-site args) expr)
  (define callee (evaluate i callee-expr))
  (check-callable callee call-site)
  (define arity (callable-arity callee))
  (define arguments : (Vectorof Any)
    (for/vector ([arg args])
      (evaluate i arg)))
  (define argc (vector-length arguments))
  (unless (= argc arity)
    (raise-runtime-error
     call-site (format "Expected ~a arguments but got ~a." arity argc)))
  (callable-call callee i arguments))

(: eval-get (-> Interpreter GetExpr Any))
(define (eval-get i expr)
  (match-define (get obj name) expr)
  (define object (evaluate i obj))
  (if (instance? object)
      (instance-get object name)
      (raise-runtime-error name (format "Undefined property '~a'." (token-lexeme name)))))

(: eval-set-expr (-> Interpreter SetExpr Any))
(define (eval-set-expr i expr)
  (match-define (set-expr expr-object expr-name expr-value) expr)
  (define object (evaluate i expr-object))
  (cond
    [(instance? object)
     (define value (evaluate i expr-value))
     (instance-set! object expr-name value)
     value]
    [else
     (raise-runtime-error expr-name "Only instance have fields.")]))

(: eval-super-expr (-> Interpreter SuperExpr Any))
(define (eval-super-expr i expr)
  (define method-name (token-lexeme (super-expr-method expr)))
  (define distance (hash-ref (interpreter-locals i) expr))
  (define superclass
    (env-get-at (interpreter-env i) distance "super"))
  (define object
    (env-get-at (interpreter-env i) (sub1 distance) "this"))
  (assert superclass class?)
  (assert object instance?)
  (define method (class-find-method superclass method-name))
  (if method
      (bind method object)
      (raise-runtime-error (super-expr-method expr) (format "Undefined property '~a'." method-name))))

(: eval-this-expr (-> Interpreter ThisExpr Any))
(define (eval-this-expr i expr)
  (lookup-variable i (this-expr-keyword expr) expr))

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
    [(quote EQUAL_EQUAL)
     (lox-equal? left right)]
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

#| Callable |#

(define-type Callable (U Function NativeFunction Class))

(: callable? (Any -> Boolean : Callable))
(define-predicate callable? Callable)

(define-syntax-rule (check-callable callee call-site)
  (begin
    (unless (callable? callee)
      (raise-runtime-error call-site "Can only call functions and classes."))
    (assert callee callable?)))

(: callable-call (-> Callable Interpreter (Vectorof Any) Any))
(define (callable-call callee i args)
  (match callee
    [(function _ _ _) (call-function callee i args)]
    [(native-function call-func _) (call-func callee i args)]
    [(class _ _ _) (call-class callee i args)]))

(: callable-arity (-> Callable Natural))
(define (callable-arity callee)
  (match callee
    [(function (fun-decl _ params _) _ _) (vector-length params)]
    [(native-function _ arity) arity]
    [(class _ _ _) (class-arity callee)]))

(: callable->string (-> Callable String))
(define (callable->string callee)
  (match callee
    [(function (fun-decl name _ _) _ _) (format "<fn ~a>" (token-lexeme name))]
    [(native-function _ _) "<native fn>"]
    [(class name _ _) name]))

(: call-function (-> Function Interpreter (Vectorof Any) Any))
(define (call-function func i args)
  (match-define (function (fun-decl _ params body) closure is-initalizer?) func)
  (define env (make-env (function-closure func)))
  (for ([param params] [arg args])
    (env-define env (token-lexeme param) arg))
  (: handle-return (-> Return Any))
  (define (handle-return r)
    (if is-initalizer?
        (env-get-at closure 0 "this")
        (return-value r)))
  (with-handlers ([return? handle-return])
    (exec-block i body env)
    (if is-initalizer?
        (env-get-at closure 0 "this") ; return 'this'
        null))) ; implicitly return null

(: call-class (-> Class Interpreter (Vectorof Any) Any))
(define (call-class c i args)
  (define instance (make-instance c))
  (define initializer (class-find-method c "init"))
  (when initializer
    (call-function (bind initializer instance) i args))
  instance)

(: class-arity (-> Class Natural))
(define (class-arity c)
  (define initializer (class-find-method c "init"))
  (if initializer (callable-arity initializer) 0))

(struct native-function ([call : (-> NativeFunction Interpreter (Vectorof Any) Any)] [arity : Natural]))
(define-type NativeFunction native-function)

(: make-clock (-> NativeFunction))
(define (make-clock)
  (native-function (λ (callee i args) (current-seconds)) 0))

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
    [(callable? v) (callable->string v)]
    [(instance? v) (instance->string v)]
    [else (~a v)]))
