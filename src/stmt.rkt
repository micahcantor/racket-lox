#lang racket/base

(provide (all-defined-out))

(struct stmt ())
(struct print-stmt stmt (value))
(struct expression-stmt stmt (expr))
(struct var-stmt stmt (name initializer))