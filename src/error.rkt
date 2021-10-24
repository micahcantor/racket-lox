#lang typed/racket/base

(require racket/match)
(require "token.rkt")

(provide (all-defined-out))

#| Global error flags |#

(: had-error Boolean)
(define had-error #f)

(: had-runtime-error Boolean)
(define had-runtime-error #f)

(: set-had-error! (-> Boolean Void))
(define (set-had-error! bool)
  (set! had-error bool))

(: set-had-runtime-error! (-> Boolean Void))
(define (set-had-runtime-error! bool)
  (set! had-runtime-error bool))

#| Parse errors |#

(struct exn:parse-error exn:fail ())
(define-type ParseError exn:parse-error)

(: make-parse-error (-> Token String exn:parse-error))
(define (make-parse-error token message)
  (lox-error token message) ; print error message and set had-error 
  (exn:parse-error (lox-error-message (token-line token) message)
                   (current-continuation-marks)))

(: raise-parse-error (-> Token String exn:parse-error))
(define (raise-parse-error token message)
  (raise (make-parse-error token message)))

#| Runtime errors |#

(struct exn:runtime-error exn:fail ([token : Token]))
(define-type RuntimeError exn:runtime-error)

(: make-runtime-error (-> Token String exn:runtime-error))
(define (make-runtime-error token message)
  (exn:runtime-error message (current-continuation-marks) token))

(: raise-runtime-error (-> Token String exn:runtime-error))
(define (raise-runtime-error token message)
  (raise (make-runtime-error token message)))

(: runtime-error (-> exn:runtime-error Void))
(define (runtime-error e)
  (displayln 
   (format "~a\n[line ~a]" 
           (exn-message e) 
           (exn:runtime-error-token e)))
  (set-had-runtime-error! #t))

(: raise-undefined-variable-error (-> Token String exn:runtime-error))
(define (raise-undefined-variable-error name lexeme)
  (raise-runtime-error 
   name (format "Undefined variable '~a'." lexeme)))

#| Lox errors |#

(: lox-error (-> Token String Void))
(define (lox-error t message)
  (match-define (token type lexeme _ line) t)
  (if (equal? type EOF)
      (report-error line "at end" message)
      (report-error line (format "at '~a'" lexeme) message)))

(: report-error (->* (Integer String) (String) Void))
(define (report-error line message [where ""])
  (displayln (lox-error-message line where message))
  (set-had-error! #t))

(: lox-error-message (->* (Integer String) (String) String))
(define (lox-error-message line message [where ""])
  (format "[line ~a] Error ~a: ~a" line where message))
