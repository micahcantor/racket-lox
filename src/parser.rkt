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
      [(matches? p VAR) (parse-var-declaration p)]
      [else (parse-statement p)])))

(: parse-var-declaration (-> Parser Stmt))
(define (parse-var-declaration p)
  (define name (consume! p IDENTIFIER "Expect variable name."))
  (: initializer (Option Expr))
  (define initializer #f)
  (when (matches? p EQUAL)
    (set! initializer (parse-expression p)))
  (consume! p SEMICOLON "Expect ';' after variable declaration.")
  (var-stmt name initializer))

(: parse-statement (-> Parser Stmt))
(define (parse-statement p)
  (cond
    [(matches? p PRINT) (parse-print-statement p)]
    [(matches? p LEFT_BRACE) (parse-block-statement p)]
    [else (parse-expression-statement p)]))

(: parse-print-statement (-> Parser Stmt))
(define (parse-print-statement p)
  (define value (parse-expression p))
  (consume! p SEMICOLON "Expect ';' after value.")
  (print-stmt value))

(: parse-block-statement (-> Parser BlockStmt))
(define (parse-block-statement p)
  (: stmts (Listof Stmt))
  (define stmts null)
  (while (and (not (same-type? p RIGHT_BRACE)) (not (at-end? p)))
    (set! stmts (cons (parse-declaration p) stmts)))
  (consume! p RIGHT_BRACE "Expect '}' after block.")
  (block-stmt (reverse stmts)))

(: parse-expression-statement (-> Parser Stmt))
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
  (define expr (parse-equality p))
  (cond 
    [(matches? p EQUAL)
     (define equals (previous p))
     (define value (parse-assignment p))
     (unless (variable? expr)
       (lox-error equals "Invalid assignment target."))
     (assert expr variable?)
     (assign (variable-name expr) value)]
    [else expr]))

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
    [else (parse-primary p)]))

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
    (if (same-type? p type)
        (advance! p)
        #f)))

(: consume! (-> Parser Symbol String Token))
(define (consume! p type message)
  (unless (same-type? p type)
    (raise-parse-error (peek p) message))
  (advance! p))

(: same-type? (-> Parser Symbol Boolean))
(define (same-type? p type)
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