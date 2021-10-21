#lang racket/base

(provide (struct-out globals) report-error)

(struct globals 
  ([had-error #:mutable #:auto]) 
  #:auto-value #f
  #:transparent)

(define (report-error line where message)
  (printf "[line %d ] Error %s: %s" line where message))