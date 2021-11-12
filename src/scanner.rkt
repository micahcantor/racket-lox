#lang typed/racket/base

(provide make-scanner scan-tokens!)

(require racket/match)
(require racket/function)
(require "utils/while.rkt")
(require "token.rkt")
(require "error.rkt")

(struct scanner ([source : String] 
                 [tokens : (Listof Token)] 
                 [start : Integer] 
                 [current : Integer] 
                 [line : Integer])
  #:mutable)

(define-type (Scanner) scanner)

(: make-scanner (->* () (String (Listof Token) Integer Integer Integer) scanner))
(define (make-scanner [source ""] [tokens '()] [start 0] [current 0] [line 1])
  (scanner source tokens start current line))

(: scanner-current-char (-> Scanner Char))
(define (scanner-current-char s)
  (string-ref (scanner-source s) (scanner-current s)))

(: scanner-next! (-> Scanner Void))
(define (scanner-next! s)
  (set-scanner-current! s (add1 (scanner-current s))))

(: scanner-next-line! (-> Scanner Void))
(define (scanner-next-line! s)
  (set-scanner-line! s (add1 (scanner-line s))))

(: scanner-substring (->* (Scanner) (Integer Integer) String))
(define (scanner-substring s [start (scanner-start s)] [end (scanner-current s)])
  (substring (scanner-source s) start end))

(: scan-tokens! (-> Scanner (Vectorof Token)))
(define (scan-tokens! s)
  (while (not (at-end? s))
         (set-scanner-start! s (scanner-current s)) ; set start of the current scan to the current pos
         (scan-token! s)) ; scan from current pos
  (define eof-token (make-token EOF "" (scanner-line s)))
  (set-scanner-tokens! s (cons eof-token (scanner-tokens s)))
  (list->vector (reverse (scanner-tokens s))))

(: scan-token! (-> Scanner Void))
(define (scan-token! s)
  (match (advance! s)
    [#\( (add-token! s LEFT_PAREN)]
    [#\) (add-token! s RIGHT_PAREN)]
    [#\{ (add-token! s LEFT_BRACE)]
    [#\} (add-token! s RIGHT_BRACE)]
    [#\, (add-token! s COMMA)]
    [#\. (add-token! s DOT)]
    [#\- (add-token! s MINUS)]
    [#\+ (add-token! s PLUS)]
    [#\; (add-token! s SEMICOLON)]
    [#\* (add-token! s STAR)]
    [#\! (add-token! s (if (matches? s #\=) BANG_EQUAL BANG))]
    [#\= (add-token! s (if (matches? s #\=) EQUAL_EQUAL EQUAL))]
    [#\< (add-token! s (if (matches? s #\=) LESS_EQUAL LESS))]
    [#\> (add-token! s (if (matches? s #\=) GREATER_EQUAL GREATER))]
    [#\/ (if (matches? s #\/)
             (while (and (not (next-is? s #\newline)) (not (at-end? s)))
                    (advance! s)) ; ignore comments to the end of a line
             (add-token! s  SLASH))]
    [#\newline (scanner-next-line! s)] ; increment line number
    [(? char-blank?) (void)] ; ignore whitespace
    [#\" (scan-string! s)] ; string literals
    [(? char-numeric?) (scan-number! s)] ; number literals
    [(? char-identifier-start?) (scan-identifier! s)] ; identifiers
    [_ (report-error (scanner-line s) "" "Unexpected character.")]))

(: at-end? (-> Scanner Boolean))
(define (at-end? s) 
  (>= (scanner-current s) 
      (string-length (scanner-source s))))

(: advance! (-> Scanner Char))
(define (advance! s)
  (define current (scanner-current-char s))
  (scanner-next! s)
  current)

(: peek (-> Scanner Char))
(define (peek s)
  (if (at-end? s)
      #\nul
      (scanner-current-char s)))

(: peek-next (-> Scanner Char))
(define (peek-next s)
  (define next-pos (add1 (scanner-current s)))
  (if (>= next-pos (string-length (scanner-source s)))
      #\nul
      (string-ref (scanner-source s) next-pos)))

(: next-is? (-> Scanner Char Boolean))
(define (next-is? s ch)
  (char=? (peek s) ch))

(: matches? (-> Scanner Char Boolean))
(define (matches? s expected)
  (and (not (at-end? s))
       (char=? (scanner-current-char s) expected)
       (scanner-next! s)
       #t))

(: add-token! (->* (Scanner Symbol) (Lox-Literal) Void))
(define (add-token! s type [literal null])
  (define text (scanner-substring s))
  (define new-token (make-token type text (scanner-line s) literal))
  (set-scanner-tokens! s (cons new-token (scanner-tokens s))))

(: scan-string! (-> Scanner Void))
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
     (add-token! s  STRING value)]))

(: scan-number! (-> Scanner Void))
(define (scan-number! s)
  (while (char-numeric? (peek s)) (advance! s))
  ; look for fractional part
  (when (and (next-is? s #\.) (char-numeric? (peek-next s)))
    (advance! s) ; consume the "."
    (while (char-numeric? (peek s)) (advance! s)))
  (define text (assert (scanner-substring s) string?))
  (add-token! s NUMBER (string->number text)))

(: char-identifier-start? (-> Char Boolean))
(define (char-identifier-start? ch)
  (or (char-alphabetic? ch) (char=? ch #\_)))

(: char-alphanumeric? (-> Char Boolean))
(define (char-alphanumeric? ch)
  (or (char-identifier-start? ch) (char-numeric? ch)))

(: scan-identifier! (-> Scanner Void))
(define (scan-identifier! s)
  (while (char-alphanumeric? (peek s))
         (advance! s))
  (define text (scanner-substring s))
  (define keyword-type (hash-ref keywords text #f))
  (if keyword-type
      (add-token! s keyword-type)
      (add-token! s IDENTIFIER)))