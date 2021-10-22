#lang racket/base

(require racket/match)
(require racket/file)
(require "parser.rkt")
(require "scanner.rkt")
(require "error.rkt")
(require "pretty-print.rkt")

(define (main)
  (define args (current-command-line-arguments))
  (match args
    [null (run-prompt)]
    [(list f) (run-file f)]
    [_ (println "Usage: racket-lox [script]")]))

(define (run-prompt)
  (let loop ()
    (display "> ")
    (define line (read-line))
    (unless (zero? (string-length line))
      (run line)
      (set-had-error! #f)
      (loop))))

(define (run-file filename)
  (run (file->string filename))
  (when had-error
    (exit 65)))

(define (run source)
  (define scanner (make-scanner source))
  (define tokens (scan-tokens! scanner))
  (define parser (make-parser tokens))
  (define expression (parse! parser))
  (unless had-error
    (displayln (expr->string expression))))

(main)