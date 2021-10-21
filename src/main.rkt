#lang racket/base

(require racket/match)
(require racket/file)
(require "lox.rkt")
(require "scanner.rkt")

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
      (set-globals-had-error! g #f)
      (loop))))

(define (run-file filename)
  (run (file->string filename))
  (when (globals-had-error g)
    (exit 65)))

(define (run source)
  (define scanner (make-scanner source))
  (define tokens (scan-tokens! scanner))
  (for-each println tokens))

(define g (globals))
(main)