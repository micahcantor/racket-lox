#lang racket/base

(require racket/match)
(require "token.rkt")

(provide (all-defined-out))

#| Global error flags |#

(define had-error #f)
(define had-runtime-error #f)

(define (set-had-error! bool)
  (set! had-error bool))

(define (set-had-runtime-error! bool)
  (set! had-runtime-error bool))

#| Parse errors |#

(struct exn:parse-error exn:fail ())

(define (make-parse-error token message)
  (lox-error token message) ; print error message and set had-error 
  (exn:parse-error (lox-error-message (token-line token) message)
                   (current-continuation-marks)))

(define (raise-parse-error token message)
  (raise (make-parse-error token message)))

#| Runtime errors |#

(struct exn:runtime-error exn:fail (token))

(define (make-runtime-error token message)
  (exn:runtime-error token message (current-continuation-marks)))

(define (raise-runtime-error token message)
  (raise (make-runtime-error token message)))

(define (runtime-error e)
  (displayln 
   (format "~a\n[line ~a]" 
           (exn-message e) 
           (exn:runtime-error-token e)))
  (set-had-runtime-error! #t))

(define (raise-undefined-variable-error name lexeme)
  (raise-runtime-error 
   name (format "Undefined variable '~a'." lexeme)))

#| Lox errors |#

(define (lox-error t message)
  (match-define (token type lexeme _ line) t)
  (if (equal? type EOF)
      (report-error line "at end" message)
      (report-error line (format "at '~a'" lexeme) message)))

(define (report-error line message [where ""])
  (displayln (lox-error-message line where message))
  (set-had-error! #t))

(define (lox-error-message line message [where ""])
  (format "[line ~a] Error ~a: ~a" line where message))
