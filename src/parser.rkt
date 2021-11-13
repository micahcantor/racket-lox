#lang typed/racket/base

(require racket/match)
(require "utils/while.rkt")
(require "token.rkt")
(require "expr.rkt")
(require "error.rkt")
(require "stmt.rkt")

(provide make-parser parse!)

(: parse! (-> Parser (Listof Stmt)))
(define (parse! p)
  (let loop ([statements : (Listof Stmt) null])
    (if (at-end? p)
        (reverse statements)
        (loop (cons (parse-declaration p) statements)))))

(struct parser ([tokens : (Vectorof Token)] [current : Integer]) #:mutable)
(define-type Parser parser)

(: make-parser (-> (Vectorof Token) Parser))
(define (make-parser [tokens (vector)])
  (parser tokens 0))

(: parser-next! (-> Parser Void))
(define (parser-next! p)
  (set-parser-current! p (add1 (parser-current p))))

#| Statement Parsing |#

(: parse-declaration (-> Parser Stmt))
(define (parse-declaration p)
  ; synchronize after a parse error on a statement.
  (define (handle-parse-error e) (synchronize p) (empty-stmt))
  (with-handlers ([exn:parse-error? handle-parse-error])
    (cond
      [(matches? p CLASS) (parse-class-declaration p)]
      [(matches? p FUN) (parse-fun-declaration p "function")]
      [(matches? p VAR) (parse-var-declaration p)]
      [else (parse-statement p)])))

(: parse-class-declaration (-> Parser ClassDecl))
(define (parse-class-declaration p)
  (define name (consume! p IDENTIFIER "Expect class name."))
  (define superclass
    (and (matches? p LESS)
         (variable (consume! p IDENTIFIER "Expect superclass name."))))
  (consume! p LEFT_BRACE "Expect '{' before class body.")
  (define methods : (Listof FunDecl) null)
  (while (and (not (check? p RIGHT_BRACE)) (not (at-end? p)))
         (set! methods (cons (parse-fun-declaration p "method") methods)))
  (consume! p RIGHT_BRACE "Expect '}' after class body.")
  (class-decl name superclass (reverse methods)))

(: parse-fun-declaration (-> Parser String FunDecl))
(define (parse-fun-declaration p kind)
  (define name (consume! p IDENTIFIER (format "Expect ~a name." kind)))
  (consume! p LEFT_PAREN (format "Expect '(' after ~a name." kind))
  (define params (parse-parameters p))
  (consume! p RIGHT_PAREN "Expect ')' after parameters.")
  (consume! p LEFT_BRACE (format "Expect '{' before ~a body." kind))
  (define body (parse-block p))
  (fun-decl name params body))

(: parse-parameters (-> Parser (Vectorof Token)))
(define (parse-parameters p)
  (define params : (Listof Token) null)
  (define paramc : Natural 0)
  (define (add-param!)
    (define name (consume! p IDENTIFIER "Expect parameter name."))
    (set! params (cons name params))
    (set! paramc (add1 paramc)))
  (unless (check? p RIGHT_PAREN)
    (add-param!)
    (while (matches? p COMMA)
           (when (>= paramc 255)
             (lox-error (peek p) "Can't have more than 255 parameters."))
           (add-param!)))
  (list->vector (reverse params)))

(: parse-var-declaration (-> Parser VarDecl))
(define (parse-var-declaration p)
  (define name (consume! p IDENTIFIER "Expect variable name."))
  (: initializer (Option Expr))
  (define initializer #f)
  (when (matches? p EQUAL)
    (set! initializer (parse-expression p)))
  (consume! p SEMICOLON "Expect ';' after variable declaration.")
  (var-decl name initializer))

(: parse-statement (-> Parser Stmt))
(define (parse-statement p)
  (cond
    [(matches? p PRINT) (parse-print-statement p)]
    [(matches? p LEFT_BRACE) (parse-block-statement p)]
    [(matches? p IF) (parse-if-statement p)]
    [(matches? p WHILE) (parse-while-statement p)]
    [(matches? p FOR) (parse-for-statement p)]
    [(matches? p RETURN) (parse-return-statement p)]
    [else (parse-expression-statement p)]))

(: parse-print-statement (-> Parser PrintStmt))
(define (parse-print-statement p)
  (define value (parse-expression p))
  (consume! p SEMICOLON "Expect ';' after value.")
  (print-stmt value))

(: parse-block-statement (-> Parser BlockStmt))
(define (parse-block-statement p)
  (block-stmt (parse-block p)))

(: parse-block (-> Parser (Listof Stmt)))
(define (parse-block p)
  (: stmts (Listof Stmt))
  (define stmts null)
  (while (and (not (check? p RIGHT_BRACE)) (not (at-end? p)))
         (set! stmts (cons (parse-declaration p) stmts)))
  (consume! p RIGHT_BRACE "Expect '}' after block.")
  (reverse stmts))

(: parse-if-statement (-> Parser IfStmt))
(define (parse-if-statement p)
  (consume! p LEFT_PAREN "Expect '(' after 'if'.")
  (define condition (parse-expression p))
  (consume! p RIGHT_PAREN "Expect ')' after if condition.")
  (define consequent (parse-statement p))
  (define alternate
    (and (matches? p ELSE) (parse-statement p)))
  (if-stmt condition consequent alternate))

(: parse-while-statement (-> Parser WhileStmt))
(define (parse-while-statement p)
  (consume! p LEFT_PAREN "Expect '(' after 'while'.")
  (define condition (parse-expression p))
  (consume! p RIGHT_PAREN "Expect ')' after 'while' condition.")
  (define body (parse-statement p))
  (while-stmt condition body))

(: parse-for-statement (-> Parser Stmt))
(define (parse-for-statement p)
  (consume! p LEFT_PAREN "Expect '(' after 'for'.")
  (define initializer
    (cond
      [(matches? p SEMICOLON) #f]
      [(matches? p VAR) (parse-var-declaration p)]
      [else (parse-expression-statement p)]))
  (define condition
    (if (check? p SEMICOLON)
        (literal #t)
        (parse-expression p)))
  (consume! p SEMICOLON "Expect ';' after loop condition.")
  (define increment
    (if (check? p RIGHT_PAREN) #f (parse-expression p)))
  (consume! p RIGHT_PAREN "Expect ')' after clauses.")
  (define body (parse-statement p))
  (desugar-for initializer condition increment body))

(: desugar-for (-> (Option Stmt) Expr (Option Expr) Stmt Stmt))
(define (desugar-for initializer condition increment body)
  (when increment
    (set! body
          (block-stmt (list body (expression-stmt increment)))))
  (unless condition (set! condition (literal #t)))
  (set! body (while-stmt condition body))
  (when initializer
    (set! body (block-stmt (list initializer body))))
  body)

(: parse-return-statement (-> Parser ReturnStmt))
(define (parse-return-statement p)
  (define keyword (previous p))
  (define value
    (and (not (check? p SEMICOLON))
         (parse-expression p)))
  (consume! p SEMICOLON "Expect ';' after return value.")
  (return-stmt keyword value))

(: parse-expression-statement (-> Parser ExpressionStmt))
(define (parse-expression-statement p)
  (define expr (parse-expression p))
  (consume! p SEMICOLON "Expect ';' after expression.")
  (expression-stmt expr))

#| Expression Parsing |#

(: parse-expression (-> Parser Expr))
(define (parse-expression p)
  (parse-assignment p))

; parses right-assosiative assignment expression.
; report error if the left side of assignment is not a variable name.
;   e.g: a = 2; // okay
;        newPoint(x + 2, 0).y = 2; // okay
;        a + b = 2; // not okay
(: parse-assignment (-> Parser Expr))
(define (parse-assignment p)
  (define expr (parse-or p))
  (cond
    [(matches? p EQUAL)
     (define equals (previous p))
     (define value (parse-assignment p))
     (match expr
       [(variable name)
        (assign name value)]
       [(get object name)
        (set-expr object name value)]
       [else
        (lox-error equals "Invalid assignment target.")
        expr])]
    [else expr]))

(: parse-or (-> Parser Expr))
(define (parse-or p)
  (define token-matches (list OR))
  (parse-left-assosiative-binary p parse-and token-matches))

(: parse-and (-> Parser Expr))
(define (parse-and p)
  (define token-matches (list AND))
  (parse-left-assosiative-binary p parse-equality token-matches))

(: parse-equality (-> Parser Expr))
(define (parse-equality p)
  (define token-matches (list BANG_EQUAL EQUAL_EQUAL))
  (parse-left-assosiative-binary p parse-comparison token-matches))

(: parse-comparison (-> Parser Expr))
(define (parse-comparison p)
  (define token-matches (list GREATER GREATER_EQUAL LESS LESS_EQUAL))
  (parse-left-assosiative-binary p parse-term token-matches))

(: parse-term (-> Parser Expr))
(define (parse-term p)
  (define token-matches (list MINUS PLUS))
  (parse-left-assosiative-binary p parse-factor token-matches))

(: parse-factor (-> Parser Expr))
(define (parse-factor p)
  (define token-matches (list SLASH STAR))
  (parse-left-assosiative-binary p parse-unary token-matches))

(: parse-unary (-> Parser Expr))
(define (parse-unary p)
  (cond
    [(matches? p BANG MINUS)
     (define operator (previous p))
     (define right (parse-unary p))
     (unary operator right)]
    [else (parse-call p)]))

(: parse-call (-> Parser Expr))
(define (parse-call p)
  (define expr (parse-primary p))
  (let loop ([expr expr])
    (cond
      [(matches? p LEFT_PAREN)
       (loop (finish-call p expr))]
      [(matches? p DOT)
       (define name (consume! p IDENTIFIER "Expect property name after '.'."))
       (loop (get expr name))]
      [else expr])))

(: finish-call (-> Parser Expr Expr))
(define (finish-call p callee)
  (define args : (Listof Expr) null)
  (define argc : Natural 0)
  (define (add-arg!)
    (set! args (cons (parse-expression p) args))
    (set! argc (add1 argc)))
  (unless (check? p RIGHT_PAREN)
    (add-arg!)
    (while (matches? p COMMA)
           (when (>= argc 255)
             (lox-error (peek p) "Can't have more than 255 arguments."))
           (add-arg!)))
  (define paren (consume! p RIGHT_PAREN "Expect ')' after arguments."))
  (call callee paren (reverse args)))

(: parse-primary (-> Parser Expr))
(define (parse-primary p)
  (cond
    [(matches? p FALSE)
     (literal #f)]
    [(matches? p TRUE)
     (literal #t)]
    [(matches? p NIL)
     (literal null)]
    [(matches? p STRING)
     (literal (token-literal (previous p)))]
    [(matches? p NUMBER)
     (literal (exact->inexact (assert (token-literal (previous p)) number?)))]
    [(matches? p IDENTIFIER)
     (variable (previous p))]
    [(matches? p SUPER)
     (define keyword (previous p))
     (consume! p DOT "Expect '.' after 'super'.")
     (define method (consume! p IDENTIFIER "Expect superclass method name."))
     (super-expr keyword method)]
    [(matches? p THIS)
     (this-expr (previous p))]
    [(matches? p LEFT_PAREN)
     (define expr (parse-expression p)) ; parse the following expression
     (consume! p RIGHT_PAREN "Expect ')' after expression.")
     (grouping expr)]
    [else
     (raise-parse-error (peek p) "Expect expression.")
     (empty-expr)]))

; (parse-left-assosiative-binary parser (parser -> token) (listof Token-type)) -> token
; First parse the left side, which can be any expression of higher precedence.
; Then recursively parse the right side while the desire tokens match, setting
; the final expression to the binary result of the two sides.
(: parse-left-assosiative-binary (-> Parser (-> Parser Expr) (Listof Symbol) Expr))
(define (parse-left-assosiative-binary p token-parser token-matches)
  (define expr (token-parser p))
  (while (apply matches? p token-matches)
         (define operator (previous p))
         (define right (token-parser p))
         (set! expr (binary expr operator right)))
  expr)

#| Helpers |#

(: matches? (-> Parser Symbol * (Option Token)))
(define (matches? p . types)
  (for/or ([type types])
    (if (check? p type)
        (advance! p)
        #f)))

(: consume! (-> Parser Symbol String Token))
(define (consume! p type message)
  (unless (check? p type)
    (raise-parse-error (peek p) message))
  (advance! p))

(: check? (-> Parser Symbol Boolean))
(define (check? p type)
  (and (not (at-end? p))
       (equal? (token-type (peek p)) type)))

(: advance! (-> Parser Token))
(define (advance! p)
  (unless (at-end? p) (parser-next! p))
  (previous p))

(: at-end? (-> Parser Boolean))
(define (at-end? p)
  (equal? (token-type (peek p)) EOF))

(: peek (-> Parser Token))
(define (peek p)
  (vector-ref (parser-tokens p) (parser-current p)))

(: previous (-> Parser Token))
(define (previous p)
  (vector-ref (parser-tokens p) (sub1 (parser-current p))))

; (synchronize parser) -> void
; Discard tokens until we're at the beginning of the next statement.
; After a semicolon, we are probably finished with a statement.
; When the next token is a keyword, we are probably beginning a statement.
(: synchronize (-> Parser Void))
(define (synchronize p)
  (advance! p)
  (define keywords (list CLASS FUN VAR FOR IF WHILE PRINT RETURN))
  (until (or (at-end? p)
             (equal? (token-type (previous p)) SEMICOLON)
             (member (token-type (peek p)) keywords))
         (advance! p)))