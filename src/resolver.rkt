#lang typed/racket/base

(require (only-in racket/match match-define))
(require "utils/stack.rkt")
(require "error.rkt")
(require "interpreter.rkt")
(require "stmt.rkt")
(require "expr.rkt")
(require "token.rkt")

(provide resolve-all! make-resolver)

(define-type Resolver resolver)
(struct resolver ([interpreter : Interpreter]
                  [scopes : (Stackof (HashTable String Boolean))]
                  [current-function : FunctionType]
                  [current-class : ClassType]) 
                  #:mutable)

#| Function Type |#
(struct NONE ())
(struct FUNCTION ())
(struct INITIALIZER ())
(struct METHOD ())
(define-type FunctionType (U NONE FUNCTION INITIALIZER METHOD))

#| ClassType |#
(struct CLASS ())
(define-type ClassType (U NONE CLASS))


(: make-resolver (-> Interpreter Resolver))
(define (make-resolver i)
  (resolver i (make-stack) (NONE) (NONE)))

(: resolve-all! (-> Resolver (Listof Stmt) Void))
(define (resolve-all! r stmts)
  (for ([stmt stmts])
    (resolve! r stmt)))

(: resolve! (-> Resolver (U Stmt Expr) Void))
(define (resolve! r val)
  (cond
    [(block-stmt? val) (resolve-block-stmt! r val)]
    [(var-decl? val) (resolve-var-decl! r val)]
    [(fun-decl? val) (resolve-fun-decl! r val)]
    [(class-decl? val) (resolve-class-decl! r val)]
    [(variable? val) (resolve-var-expr! r val)]
    [(assign? val) (resolve-assign-expr! r val)]
    [(expression-stmt? val) (resolve-expr-stmt! r val)]
    [(if-stmt? val) (resolve-if-stmt! r val)]
    [(print-stmt? val) (resolve-print-stmt! r val)]
    [(return-stmt? val) (resolve-return-stmt! r val)]
    [(while-stmt? val) (resolve-while-stmt! r val)]
    [(binary? val) (resolve-binary-expr! r val)]
    [(unary? val) (resolve-unary-expr! r val)]
    [(call? val) (resolve-call-expr! r val)]
    [(get? val) (resolve-get-expr! r val)]
    [(set-expr? val) (resolve-set-expr! r val)]
    [(this-expr? val) (resolve-this-expr! r val)]
    [(grouping? val) (resolve-grouping-expr! r val)]
    [(literal? val) (resolve-literal-expr!)]))

(: resolve-block-stmt! (-> Resolver BlockStmt Void))
(define (resolve-block-stmt! r stmt)
  (begin-scope! r)
  (resolve-all! r (block-stmt-statements stmt))
  (end-scope! r))

(: resolve-var-decl! (-> Resolver VarDecl Void))
(define (resolve-var-decl! r stmt)
  (match-define (var-decl name initializer) stmt)
  (declare! r name)
  (when initializer
    (resolve! r initializer))
  (define! r name))

(: resolve-fun-decl! (-> Resolver FunDecl Void))
(define (resolve-fun-decl! r stmt)
  (define name (fun-decl-name stmt))
  (declare! r name)
  (define! r name)
  (resolve-function! r stmt (FUNCTION)))

(: resolve-class-decl! (-> Resolver ClassDecl Void))
(define (resolve-class-decl! r stmt)
  (match-define (class-decl name methods) stmt)
  (define enclosing-class (resolver-current-class r))
  (set-resolver-current-class! r (CLASS))
  (declare! r name)
  (define! r name)
  (begin-scope! r)
  ; resolve "this" to a local variable within class body.
  (hash-set! (stack-top (resolver-scopes r)) "this" #t)
  (for ([method methods])
    (define declaration
      (if (equal? "init" (token-lexeme (fun-decl-name method)))
          (INITIALIZER)
          (METHOD)))
    (resolve-function! r method declaration))
  (end-scope! r)
  (set-resolver-current-class! r enclosing-class))

(: resolve-var-expr! (-> Resolver VariableExpr Void))
(define (resolve-var-expr! r expr)
  (define scopes (resolver-scopes r))
  (define name (variable-name expr))
  (when (and (not (stack-empty? scopes))
             (hash-has-key? (stack-top scopes) (token-lexeme name))
             (not (hash-ref (stack-top scopes) (token-lexeme name))))
    (lox-error name "Can't read local variable in its own initializer."))
  (resolve-local! r expr name))

(: resolve-assign-expr! (-> Resolver AssignExpr Void))
(define (resolve-assign-expr! r expr)
  (match-define (assign name value) expr)
  (resolve! r value)
  (resolve-local! r expr name))

(: resolve-expr-stmt! (-> Resolver ExpressionStmt Void))
(define (resolve-expr-stmt! r stmt)
  (resolve! r (expression-stmt-expr stmt)))

(: resolve-if-stmt! (-> Resolver IfStmt Void))
(define (resolve-if-stmt! r stmt)
  (match-define (if-stmt condition consequent alternate) stmt)
  (resolve! r condition)
  (resolve! r consequent)
  (when alternate (resolve! r alternate)))

(: resolve-print-stmt! (-> Resolver PrintStmt Void))
(define (resolve-print-stmt! r stmt)
  (resolve! r (print-stmt-value stmt)))

(: resolve-return-stmt! (-> Resolver ReturnStmt Void))
(define (resolve-return-stmt! r stmt)
  (define current-function (resolver-current-function r))
  (when (equal? current-function (NONE))
    (lox-error (return-stmt-keyword stmt) "Can't return from top-level code."))
  (define value (return-stmt-value stmt))
  (when value 
    (if (equal? current-function (INITIALIZER))
        (lox-error (return-stmt-keyword stmt) "Can't return a value from an initializer.")
        (resolve! r value))))

(: resolve-while-stmt! (-> Resolver WhileStmt Void))
(define (resolve-while-stmt! r stmt)
  (match-define (while-stmt condition body) stmt)
  (resolve! r condition)
  (resolve! r body))

(: resolve-binary-expr! (-> Resolver BinaryExpr Void))
(define (resolve-binary-expr! r expr)
  (match-define (binary left _ right) expr)
  (resolve! r left)
  (resolve! r right))

(: resolve-call-expr! (-> Resolver CallExpr Void))
(define (resolve-call-expr! r expr)
  (match-define (call callee _ args) expr)
  (resolve! r callee)
  (for ([arg args])
    (resolve! r arg)))

(: resolve-get-expr! (-> Resolver GetExpr Void))
(define (resolve-get-expr! r expr)
  (resolve! r (get-object expr)))

; we don't resolve the caller because of dynamic dispatch.
(: resolve-set-expr! (-> Resolver SetExpr Void))
(define (resolve-set-expr! r expr)
  (resolve! r (set-expr-value expr))
  (resolve! r (set-expr-object expr)))

(: resolve-this-expr! (-> Resolver ThisExpr Void))
(define (resolve-this-expr! r expr)
  (define keyword (this-expr-keyword expr))
  (if (equal? (resolver-current-class r) (NONE))
      (lox-error keyword "Can't use 'this' outside of a class.")
      (resolve-local! r expr keyword)))

(: resolve-grouping-expr! (-> Resolver GroupingExpr Void))
(define (resolve-grouping-expr! r expr)
  (resolve! r (grouping-expression expr)))

(: resolve-unary-expr! (-> Resolver UnaryExpr Void))
(define (resolve-unary-expr! r expr)
  (resolve! r (unary-right expr)))

(define (resolve-literal-expr!) (void))

(: begin-scope! (-> Resolver Void))
(define (begin-scope! r)
  (define scopes (resolver-scopes r))
  (define scope : (HashTable String Boolean) (make-hash))
  (stack-push! scopes scope))

(: end-scope! (-> Resolver Void))
(define (end-scope! r)
  (define scopes (resolver-scopes r))
  (void (stack-pop! scopes)))

(: declare! (-> Resolver Token Void))
(define (declare! r name)
  (define scopes (resolver-scopes r))
  (unless (stack-empty? scopes)
    (define scope (stack-top scopes))
    (if (hash-has-key? scope (token-lexeme name))
        (lox-error name "Already a variable with this name in this scope.")
        (hash-set! scope (token-lexeme name) #f))))

(: define! (-> Resolver Token Void))
(define (define! r name)
  (define scopes (resolver-scopes r))
  (unless (stack-empty? scopes)
    (hash-set! (stack-top scopes) (token-lexeme name) #t)))

(: resolve-local! (-> Resolver Expr Token Void))
(define (resolve-local! r expr name)
  (define interpreter (resolver-interpreter r))
  (match-define (stack data size) (resolver-scopes r))
  (for/or ([scope (reverse data)]
           [i (in-naturals)]
           #:when (hash-has-key? scope (token-lexeme name)))
    (interpreter-resolve! interpreter expr (- size i 2)))
  (void))

(: resolve-function! (-> Resolver FunDecl FunctionType Void))
(define (resolve-function! r function type)
  (define enclosing-function (resolver-current-function r))
  (set-resolver-current-function! r type)
  (begin-scope! r)
  (for ([param (fun-decl-params function)])
    (declare! r param)
    (define! r param))
  (resolve! r (fun-decl-body function))
  (end-scope! r)
  (set-resolver-current-function! r enclosing-function))