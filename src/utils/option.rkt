#lang typed/racket/base

(provide (all-defined-out))

(: filter-option (All (A) (-> (Listof (Option A)) (Listof A))))
(define (filter-option xs)
  (if (null? xs)
      xs
      (if (car xs)
          (cons (car xs) (filter-option (cdr xs)))
          (filter-option (cdr xs)))))