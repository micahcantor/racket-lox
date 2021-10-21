#lang racket/base

(provide 
  (all-defined-out))

(struct expr ())
(struct binary expr (left operator right))
(struct grouping expr (expression))
(struct literal expr (value))
(struct unary expr (operator right))
