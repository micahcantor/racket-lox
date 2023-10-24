#lang typed/racket/base

(require "stmt.rkt")
(require "env.rkt")

(provide (all-defined-out))

(struct function ([declaration : FunDecl] [closure : Env] [is-initalizer? : Boolean]))
(define-type Function function)

(struct return exn ([value : Any]))
(define-type Return return)

(: make-return (-> Any Return))
(define (make-return v)
  (return "" (current-continuation-marks) v))
