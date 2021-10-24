#lang racket/base

(provide (all-defined-out))

(struct expr () #:transparent)
(struct assign expr (name value))
(struct binary expr (left operator right) #:transparent)
(struct grouping expr (expression) #:transparent)
(struct literal expr (value) #:transparent)
(struct unary expr (operator right) #:transparent)
(struct variable expr (name) #:transparent)
