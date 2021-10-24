#lang racket/base

(require srfi/26)
(require racket/contract)
(require "utils/while.rkt")
(require "token.rkt")
(require "expr.rkt")
(require "error.rkt")
(require "stmt.rkt")

(provide make-parser parse!)

(define (parse! p)
  (define (handle-parse-error e) null)
  (with-handlers ([exn:parse-error? handle-parse-error])
    (let loop ([statements null])
      (if (at-end? p)
          (reverse statements)
          (loop (cons (parse-declaration p) statements))))))

(struct parser (tokens [current #:mutable]) #:transparent)

; ([vector] -> parser)
(define (make-parser [tokens (vector)])
  (parser tokens 0))

; (parser -> void)
(define (parser-next! p)
  (set-parser-current! p (add1 (parser-current p))))

#| Statement Parsing |#

(define (parse-declaration p)
  ; synchronize after a parse error on a statement.
  (define (handle-parse-error e) (synchronize p)) 
  (with-handlers ([exn:parse-error? handle-parse-error])
    (cond
      [(matches? p VAR) (parse-var-declaration p)]
      [else (parse-statement p)])))

(define (parse-var-declaration p)
  (define name (consume! p IDENTIFIER "Expect variable name."))
  (define initializer #f)
  (when (matches? p EQUAL)
    (set! initializer (parse-expression p)))
  (consume! p SEMICOLON "Expect ';' after variable declaration.")
  (var-stmt name initializer))

(define (parse-statement p)
  (cond
    [(matches? p PRINT) (parse-print-statement p)]
    [else (parse-expression-statement p)]))

(define (parse-print-statement p)
  (define value (parse-expression p))
  (consume! p SEMICOLON "Expect ';' after value.")
  (print-stmt value))

(define (parse-expression-statement p)
  (define expr (parse-expression p))
  (consume! p SEMICOLON "Expect ';' after expression.")
  (expression-stmt expr))

#| Expression Parsing |#

; (parse-expression parser) -> token
(define (parse-expression p)
  (parse-assignment p))

; (parse-assignment parser) -> token
; parses right-assosiative assignment expression.
; report error if the left side of assignment is not a variable name.
;   e.g: a = 2; // okay
;        newPoint(x + 2, 0).y = 2; // okay
;        a + b = 2; // not okay
(define (parse-assignment p)
  (define expr (parse-equality p))
  (cond 
    [(matches? p EQUAL)
     (define equals (previous p))
     (define value (parse-assignment p))
     (if (variable? expr)
         (assign (variable-name expr) value)
         (make-parse-error equals "Invalid assignment target."))]
    [else expr]))

; (parse-equality parser) -> token
(define (parse-equality p)
  (define token-matches (list BANG_EQUAL EQUAL_EQUAL))
  (left-assosiative-binary p parse-comparison token-matches))

; (parse-comparison parser) -> token
(define (parse-comparison p)
  (define token-matches (list GREATER GREATER_EQUAL LESS LESS_EQUAL))
  (left-assosiative-binary p parse-term token-matches))

; (parse-term parser) -> token
(define (parse-term p)
  (define token-matches (list MINUS PLUS))
  (left-assosiative-binary p parse-factor token-matches))

; (parse-factor parser) -> token
(define (parse-factor p)
  (define token-matches (list SLASH STAR))
  (left-assosiative-binary p parse-unary token-matches))

; (parse-unary parser -> token)
(define (parse-unary p)
  (cond
    [(matches? p BANG MINUS)
     (define operator (previous p))
     (define right (parse-unary p))
     (unary operator right)]
    [else (parse-primary p)]))

; (parse-primary parser) -> token
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
    [else (raise-parse-error (peek p) "Expect expression")]))

; (left-assosiative-binary parser (parser -> token) (listof token-type)) -> token
; First parse the left side, which can be any expression of higher precedence.
; Then recursively parse the right side while the desire tokens match, setting
; the final expression to the binary result of the two sides.
(define (left-assosiative-binary p token-parser token-matches)
  (define expr (token-parser p))
  (while (apply (cut matches? p <...>) token-matches)
         (define operator (previous p))
         (define right (token-parser p))
         (set! expr (binary expr operator right)))
  expr)

#| Helpers |#

; (matches? parser . (listof token-type)) -> bool
(define/contract (matches? p . types)
  ((parser?) #:rest (listof symbol?) . ->* . (or/c token? boolean? void?))
  (for/or ([type types])
    (if (same-type? p type)
        (advance! p)
        #f)))

; (consume parser token-typ string) -> void
(define (consume! p type message) 
  (if (same-type? p type)
      (advance! p)
      (raise-parse-error (peek p) message)))

; (same-type? parser token-type) -> bool
(define (same-type? p type)
  (and (not (at-end? p))
       (equal? (token-type (peek p)) type)))

; (advance! parser) -> token | void
(define (advance! p)
  (unless (at-end? p) (parser-next! p))
  (previous p))

; (at-end? parser) -> bool
(define (at-end? p)
  (equal? (token-type (peek p)) EOF))

; (peek parser) -> token
(define (peek p)
  (vector-ref (parser-tokens p) (parser-current p)))

; (previous parser) -> token
(define (previous p) 
  (vector-ref (parser-tokens p) (sub1 (parser-current p))))

; (synchronize parser) -> void
; Discard tokens until we're at the beginning of the next statement.
; After a semicolon, we are probably finished with a statement.
; When the next token is a keyword, we are probably beginning a statement.
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