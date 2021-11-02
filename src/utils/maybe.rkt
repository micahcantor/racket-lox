#lang typed/racket/base

(provide (all-defined-out))

(struct Nothing ())
(struct (A) Just ([v : A]))

(define-type (Maybe A) (U Nothing (Just A)))