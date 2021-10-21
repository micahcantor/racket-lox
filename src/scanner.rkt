#lang racket/base

(provide make-scanner scan-tokens!)

(require racket/match)
(require racket/function)
(require "utils/while.rkt")
(require "token.rkt")
(require "lox.rkt")

(struct scanner
  (source tokens start current line)
  #:mutable)

(define (make-scanner [source ""] [tokens '()] [start 0] [current 0] [line 1])
  (scanner source tokens start current line))

(define (scanner-current-char s)
  (string-ref (scanner-source s) (scanner-current s)))

(define (scanner-next! s)
  (set-scanner-current! s (add1 (scanner-current s))))

(define (scanner-next-line! s)
  (set-scanner-line! s (add1 (scanner-line s))))

(define (scanner-substring s [start (scanner-start s)] [end (scanner-current s)])
  (substring (scanner-source s) start end))

(define (scan-tokens! s)
  (while (not (at-end? s))
         (set-scanner-start! s (scanner-current s)) ; set start of the current scan to the current pos
         (scan-token! s)) ; scan from current pos
  (define eof-token (make-token 'EOF "" (scanner-line s)))
  (set-scanner-tokens! s (cons eof-token (scanner-tokens s)))
  (reverse (scanner-tokens s)))

(define (scan-token! s)
  (match (advance! s)
    [#\( (add-token! s (token-types 'LEFT_PAREN))]
    [#\) (add-token! s (token-types 'RIGHT_PAREN))]
    [#\{ (add-token! s (token-types 'LEFT_BRACE))]
    [#\} (add-token! s (token-types 'RIGHT_BRACE))]
    [#\, (add-token! s (token-types 'COMMA))]
    [#\. (add-token! s (token-types 'DOT))]
    [#\- (add-token! s (token-types 'MINUS))]
    [#\+ (add-token! s (token-types 'PLUS))]
    [#\; (add-token! s (token-types 'SEMICOLON))]
    [#\* (add-token! s (token-types 'STAR))]
    [#\! (add-token! s (if (matches? s #\=) (token-types 'BANG_EQUAL) (token-types 'BANG)))]
    [#\= (add-token! s (if (matches? s #\=) (token-types 'EQUAL_EQUAL) (token-types 'EQUAL)))]
    [#\< (add-token! s (if (matches? s #\=) (token-types 'LESS_EQUAL) (token-types 'LESS)))]
    [#\> (add-token! s (if (matches? s #\=) (token-types 'GREATER_EQUAL) (token-types 'GREATER)))]
    [#\/ (if (matches? s #\/)
             (while (and (not (next-is? s #\n)) (not (at-end? s)))
                    (advance! s)) ; ignore comments to the end of a line
             (add-token! s (token-types 'SLASH)))]
    [#\newline (scanner-next-line! s)] ; increment line number
    [(? char-blank?) (void)] ; ignore whitespace
    [#\" (scan-string! s)] ; string literals
    [(? char-numeric?) (scan-number! s)] ; number literals
    [(? char-identifier-start?) (scan-identifier! s)] ; identifiers
    [_ (report-error (scanner-line s) "" "Unexpected character.")]))

(define (at-end? s) 
  (>= (scanner-current s) 
      (string-length (scanner-source s))))

(define (advance! s)
  (define current (scanner-current-char s))
  (scanner-next! s)
  current)

(define (peek s)
  (if (at-end? s)
      #\nul
      (scanner-current-char s)))

(define (peek-next s)
  (define next-pos (add1 (scanner-current s)))
  (if (>= next-pos (string-length (scanner-source s)))
      #\nul
      (string-ref (scanner-source s) next-pos)))

(define (next-is? s ch)
  (char=? (peek s) ch))

(define (matches? s expected)
  (and (not (at-end? s))
       (char=? (scanner-current-char s) expected)
       (scanner-next! s)
       #t))

(define (add-token! s type [literal #f])
  (define text (scanner-substring s))
  (define new-token (make-token type text literal (scanner-line s)))
  (set-scanner-tokens! s (cons new-token (scanner-tokens s))))

(define (scan-string! s)
  (while (and (not (next-is? s #\")) (not (at-end? s)))
         (when (next-is? s #\newline)
           (scanner-next-line! s))
         (advance! s))
  (cond
    [(at-end? s) 
     (report-error (scanner-line s) "" "Unterminated string.")]
    [else
     (advance! s) ; consume the closing "
     ; trim the surrounding quotes, add token
     (define value (scanner-substring s (add1 (scanner-start s)) (sub1 (scanner-current s))))
     (add-token! s (token-types 'STRING) value)]))

(define (scan-number! s)
  (while (char-numeric? (peek s)) (advance! s))
  ; look for fractional part
  (when (and (next-is? s #\.) (char-numeric? (peek-next s)))
    (advance! s) ; consume the "."
    (while (char-numeric? (peek s)) (advance! s)))
  (add-token! s 'NUMBER (string->number (scanner-substring s))))

(define (char-identifier-start? ch)
  (or (char-alphabetic? ch) (char=? ch #\_)))

(define char-alphanumeric?
  (disjoin char-identifier-start? char-numeric?))

(define (scan-identifier! s)
  (while (char-alphanumeric? (peek s))
         (advance! s))
  (define text (scanner-substring s))
  (define keyword-type (hash-ref keywords text #f))
  (if keyword-type
      (add-token! s keyword-type)
      (add-token! s 'IDENTIFIER)))