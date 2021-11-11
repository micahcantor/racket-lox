#lang typed/racket/base

(provide (all-defined-out))

(define-type Stackof stack)
(struct (A) stack ([data : (Listof A)] [size : Integer]) #:mutable)

(: make-stack (All (A) (-> A * (Stackof A))))
(define (make-stack . vals)
  (stack vals (length vals)))

(: stack-push! (All (A) (-> (Stackof A) A Void)))
(define (stack-push! s x)
  (set-stack-data! s (cons x (stack-data s)))
  (set-stack-size! s (add1 (stack-size s))))

(: stack-pop! (All (A) (-> (Stackof A) A)))
(define (stack-pop! s)
  (define top (stack-top s))
  (set-stack-data! s (cdr (stack-data s)))
  (set-stack-size! s (sub1 (stack-size s)))
  top)

(: stack-top (All (A) (-> (Stackof A) A)))
(define (stack-top s)
  (car (stack-data s)))

(: stack-empty? (All (A) (-> (Stackof A) Boolean)))
(define (stack-empty? s)
  (define data (stack-data s))
  (and (list? data) (null? data)))