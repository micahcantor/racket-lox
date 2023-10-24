#lang racket/base

(require racket/port)
(require "parser.rkt")
(require "scanner.rkt")

(define (read-syntax _ port)
  (define source (port->string port))
  (define scanner (make-scanner source))
  (define tokens (scan-tokens! scanner))
  (define parser (make-parser tokens))
  (define statements (parse! parser))
  (define module-datum `(module lox-mod lox/expander
                          ,@statements))
  (datum->syntax #f module-datum))

(provide read-syntax)