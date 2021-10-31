#lang typed/racket/base

(require "class.rkt")

(struct instance ([class : Class]))

(define (make-instance class)
  (instance class))
