#lang racket/base

(require srfi/26)
(require "utils/while.rkt")
(require "token.rkt")
(require "expr.rkt")
(require "error.rkt")

(provide make-parser parse!)

(struct parser (tokens [current #:mutable]) #:transparent)

; ([vector] -> parser)
(define (make-parser [tokens (vector)])
  (parser tokens 0))

; (parser -> void)
(define (parser-next! p)
  (set-parser-current! p (add1 (parser-current p))))

(define (parse! p)
  (define (handle-parse-error e) null)
  (with-handlers ([exn:parse-error? handle-parse-error])
    (parse-expression p)))

; (parse-expression parser) -> token
(define (parse-expression p)
  (parse-equality p))

; (parse-equality parser) -> token
(define (parse-equality p)
  (define token-matches (map token-types (list 'BANG_EQUAL 'EQUAL_EQUAL)))
  (left-assosiative-binary p parse-comparison token-matches))

; (parse-comparison parser) -> token
(define (parse-comparison p)
  (define token-matches (map token-types (list 'GREATER 'GREATER_EQUAL 'LESS 'LESS_EQUAL)))
  (left-assosiative-binary p parse-term token-matches))

; (parse-term parser) -> token
(define (parse-term p)
  (define token-matches (map token-types (list 'MINUS 'PLUS)))
  (left-assosiative-binary p parse-factor token-matches))

; (parse-factor parser) -> token
(define (parse-factor p)
  (define token-matches (map token-types (list 'SLASH 'STAR)))
  (left-assosiative-binary p parse-unary token-matches))

; (parse-unary parser -> token)
(define (parse-unary p)
  (cond
    [(matches? p (token-types 'BANG) (token-types 'MINUS))
     (define operator (previous p))
     (define right (parse-unary p))
     (unary operator right)]
    [else (parse-primary p)]))

; (parse-primary parser) -> token
(define (parse-primary p)
  (cond
    [(matches? p (token-types 'FALSE))
     (literal #f)]
    [(matches? p (token-types 'TRUE))
     (literal #t)]
    [(matches? p (token-types 'NIL))
     (literal null)]
    [(matches? p (token-types 'NUMBER) (token-types 'STRING))
     (literal (token-literal (previous p)))]
    [(matches? p (token-types 'LEFT_PAREN))
     (define expr (parse-expression p)) ; parse the following expression
     (consume p (token-types 'RIGHT_PAREN) "Expect ')' after expression")
     (grouping expr)]
    [else (raise-parse-error (peek p) "Expect expression")]))

; (left-assosiative-binary parser (parser -> token) (listof token-type)) -> token
(define (left-assosiative-binary p token-parser token-matches)
  (define expr (token-parser p))
  (while (apply (cut matches? p <...>) token-matches)
         (define operator (previous p))
         (define right (token-parser p))
         (set! expr (binary expr operator right)))
  expr)

; (matches? parser . (listof token-type)) -> bool
(define (matches? p . types)
  (for/or ([type types])
    (if (same-type? p type)
        (advance! p)
        #f)))

; (consume parser token-typ string) -> void
(define (consume p type message) 
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
  (equal? (token-type (peek p)) (token-types 'EOF)))

; (peek parser) -> token
(define (peek p)
  (vector-ref (parser-tokens p) (parser-current p)))

; (previous parser) -> token
(define (previous p) 
  (vector-ref (parser-tokens p) (sub1 (parser-current p))))

#| Error Handling |#

(struct exn:parse-error exn:fail ())

(define (make-parse-error token message)
  (lox-error token message) ; print error message and set had-error 
  (exn:parse-error (lox-error-message (token-line token) message)
                   (current-continuation-marks)))

(define (raise-parse-error token message)
  (raise (make-parse-error token message)))

; (synchronize parser) -> void
; Discard tokens until we're at the beginning of the next statement.
; After a semicolon, we are probably finished with a statement.
; When the next token is a keyword, we are probably beginning a statement.
(define (synchronize p)
  (advance! p)
  (define keywords (map token-type (list 'CLASS 'FUN 'VAR 'FOR 'IF 'WHILE 'PRINT 'RETURN)))
  (while (not (at-end? p))
         (cond
           [(equal? (token-type (previous p)) (token-types 'SEMICOLON))
            (void)]
           [(member (token-type (peek p)) keywords)
            (void)]
           [else (advance! p)])))