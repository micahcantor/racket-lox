#lang racket/base

(require racket/match)
(require "token.rkt")
(require "interpreter.rkt")

(provide (all-defined-out))

(define had-error #f)
(define had-runtime-error #f)

(define (set-had-error! bool)
  (set! had-error bool))

(define (set-had-runtime-error! bool)
  (set! had-runtime-error bool))

(define (lox-error t message)
  (match-define (token type lexeme _ line) t)
  (if (equal? type EOF)
      (report-error line "at end" message)
      (report-error "at '" lexeme message)))

(define (report-error line message [where ""])
  (displayln (lox-error-message line where message))
  (set-had-error! #t))

(define (runtime-error e)
  (displayln 
   (format "~a\n[line ~a]" 
           (exn-message e) 
           (exn:runtime-error-token e)))
  (set-had-runtime-error! #t))

(define (lox-error-message line message [where ""])
  (format "[line ~a] Error ~a: ~a" line where message))
