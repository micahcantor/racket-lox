#lang racket/base

(module reader typed/racket/base
  (require "reader.rkt")
  (provide read-syntax #%top #%app #%datum #%top-interaction))