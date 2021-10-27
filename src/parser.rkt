#lang typed/racket/base

(require racket/function)
(require "utils/while.rkt")
(require "token.rkt")
(require "expr.rkt")
(require "error.rkt")
(require "stmt.rkt")

(provide make-parser parse!)

(: parse! (-> Parser (Listof Stmt)))
(define (parse! p)
  (define (handle-parse-error e) null)
  (with-handlers ([exn:parse-error? handle-parse-error])
    (: statements (Listof Stmt))
    (define statements null)
    (while (not (at-end? p))
           (set! statements (cons (parse-declaration p) statements)))
    (reverse statements)))

(struct parser ([tokens : (Vectorof Token)]
                [current : Integer])
  #:mutable #:transparent)

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
  (define (handle-parse-error e) (synchronize p) (stmt))
  (with-handlers ([exn:parse-error? handle-parse-error])
    (cond
      [(matches? p FUN) (parse-fun-declaration p "function")]
      [(matches? p VAR) (parse-var-declaration p)]
      [else (parse-statement p)])))

(: parse-fun-declaration (-> Parser String Stmt))
(define (parse-fun-declaration p kind)
  (define name (consume! p IDENTIFIER (format "Expect ~a name." kind)))
  (consume! p LEFT_PAREN (format "Expect '(' after ~a name." kind))
  (define params (parse-parameters p))
  (consume! p RIGHT_PAREN "Expect ')' after parameters.")
  (consume! p LEFT_BRACE (format "Expect '{' before ~a body." kind))
  (define body (parse-block-statement p))
  (fun-decl name params body))

(: parse-parameters (-> Parser (Vectorof Token)))
(define (parse-parameters p)
  (let loop ([params : (Listof Token) null] [num : Natural 0])
    (cond
      [(check? p RIGHT_PAREN) ((inst list->vector Token) (reverse params))]
      [(matches? p COMMA) (loop params num)]
      [else
       (when (>= num 255)
         (make-parse-error (peek p) "Can't have more than 255 parameters."))
       (define next-id (consume! p IDENTIFIER "Expect parameter name."))
       (loop (cons next-id params) (add1 num))])))

(: parse-var-declaration (-> Parser Stmt))
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
    [else (parse-expression-statement p)]))

(: parse-print-statement (-> Parser PrintStmt))
(define (parse-print-statement p)
  (define value (parse-expression p))
  (consume! p SEMICOLON "Expect ';' after value.")
  (print-stmt value))

(: parse-block-statement (-> Parser BlockStmt))
(define (parse-block-statement p)
  (: stmts (Listof Stmt))
  (define stmts null)
  (while (and (not (check? p RIGHT_BRACE)) (not (at-end? p)))
         (set! stmts (cons (parse-declaration p) stmts)))
  (consume! p RIGHT_BRACE "Expect '}' after block.")
  (block-stmt (reverse stmts)))

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
  (set! body (while-stmt condition body))
  (when initializer
    (set! body (block-stmt (list initializer body))))
  body)

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
     (unless (variable? expr)
       (lox-error equals "Invalid assignment target."))
     (assert expr variable?)
     (assign (variable-name expr) value)]
    [else expr]))

(: parse-or (-> Parser Expr))
(define (parse-or p)
  (define token-matches (list OR))
  (left-assosiative-binary p parse-and token-matches))

(: parse-and (-> Parser Expr))
(define (parse-and p)
  (define token-matches (list AND))
  (left-assosiative-binary p parse-equality token-matches))

(: parse-equality (-> Parser Expr))
(define (parse-equality p)
  (define token-matches (list BANG_EQUAL EQUAL_EQUAL))
  (left-assosiative-binary p parse-comparison token-matches))

(: parse-comparison (-> Parser Expr))
(define (parse-comparison p)
  (define token-matches (list GREATER GREATER_EQUAL LESS LESS_EQUAL))
  (left-assosiative-binary p parse-term token-matches))

(: parse-term (-> Parser Expr))
(define (parse-term p)
  (define token-matches (list MINUS PLUS))
  (left-assosiative-binary p parse-factor token-matches))

(: parse-factor (-> Parser Expr))
(define (parse-factor p)
  (define token-matches (list SLASH STAR))
  (left-assosiative-binary p parse-unary token-matches))

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
    (when (>= argc 255)
      (make-parse-error (peek p) "Can't have more than 255 arguments"))
    (while (matches? p COMMA)
           (add-arg!)))
  (define paren (consume! p RIGHT_PAREN "Expect ')' after arguments."))
  (call callee paren (reverse args)))
#|
(: finish-call2 (-> Parser Expr Expr))
(define (finish-call2 p callee)
  (define total-args : (Listof Expr)
    (let loop ([args : (Listof Expr) null] [argc : Natural 0])
      (cond
        [(check? p RIGHT_PAREN) args]
        [else
         (define next-arg (parse-expression p))
         (cond
           [(matches? p COMMA)
            (when (>= argc 254)
              (make-parse-error (peek p) "Can't have more than 255 arguments"))
            (loop (cons next-arg args) (add1 argc))]
           [else args])])))
  (define paren (consume! p RIGHT_PAREN "Expect ')' after arguments."))
  (call callee paren (reverse total-args))) |#

(: parse-primary (-> Parser Expr))
(define (parse-primary p)
  (cond
    [(matches? p FALSE)
     (literal #f)]
    [(matches? p TRUE)
     (literal #t)]
    [(matches? p NIL)
     (literal null)]
    [(matches? p NUMBER STRING)
     (literal (token-literal (previous p)))]
    [(matches? p IDENTIFIER)
     (variable (previous p))]
    [(matches? p LEFT_PAREN)
     (define expr (parse-expression p)) ; parse the following expression
     (consume! p RIGHT_PAREN "Expect ')' after expression")
     (grouping expr)]
    [else
     (raise-parse-error (peek p) "Expect expression")
     (expr)]))

; (left-assosiative-binary parser (parser -> token) (listof Token-type)) -> token
; First parse the left side, which can be any expression of higher precedence.
; Then recursively parse the right side while the desire tokens match, setting
; the final expression to the binary result of the two sides.
(: left-assosiative-binary (-> Parser (-> Parser Expr) (Listof Symbol) Expr))
(define (left-assosiative-binary p token-parser token-matches)
  (define expr (token-parser p))
  (while (apply ((curry matches?) p) token-matches)
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
  (while (not (at-end? p))
         (cond
           [(equal? (token-type (previous p)) SEMICOLON)
            (void)]
           [(member (token-type (peek p)) keywords)
            (void)]
           [else (advance! p)])))