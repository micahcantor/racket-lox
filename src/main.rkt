#lang racket/base

(require racket/match)
(require racket/file)
(require "parser.rkt")
(require "scanner.rkt")
(require "interpreter.rkt")
(require "error.rkt")

(define (main)
  (define args (current-command-line-arguments))
  (match args
    [(vector) (run-prompt)]
    [(vector f) (run-file f)]
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
  (when had-error (exit 65))
  (when had-runtime-error (exit 70)))

(define (run source)
  (define scanner (make-scanner source))
  (define tokens (scan-tokens! scanner))
  (define parser (make-parser tokens))
  (define statements (parse! parser))
  (define interpreter (make-interpreter))
  (unless had-error
    (interpret interpreter statements)))

(main)