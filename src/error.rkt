#lang racket/base

(require racket/match)
(require "token.rkt")

(provide (all-defined-out))

(define had-error #f)

(define (set-had-error! bool)
  (set! had-error bool))

(define (lox-error t message)
  (match-define (token type lexeme _ line) t)
  (if (equal? type EOF)
      (report-error line "at end" message)
      (report-error "at '" lexeme message)))

(define (report-error line message [where ""])
  (displayln (lox-error-message line where message))
  (set-had-error! #t))

(define (lox-error-message line message [where ""])
  (format "[line ~a] Error ~a: ~a" line where message))
