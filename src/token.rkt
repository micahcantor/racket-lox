#lang typed/racket/base

(provide (struct-out token) (all-defined-out))

(define-type Lox-Literal (U String Number Boolean Null))

(struct token ([type : Symbol] 
               [lexeme : String] 
               [literal : Lox-Literal] 
               [line : Integer])
  #:mutable
  #:transparent)

(define-type Token token)

(: make-token (->* (Symbol String Integer) (Lox-Literal) Token))
(define (make-token type lexeme line [literal null])
  (token type lexeme literal line))

(: keywords (HashTable String Symbol))
(define keywords
  (hash
   "and" 'AND
   "class" 'CLASS
   "else" 'ELSE
   "false" 'FALSE
   "for" 'FOR
   "fun" 'FUN
   "if" 'IF
   "nil" 'NIL
   "or" 'OR
   "print" 'PRINT
   "return" 'RETURN
   "super" 'SUPER
   "this" 'THIS
   "true" 'TRUE
   "var" 'VAR
   "while" 'WHILE))

(define LEFT_PAREN 'LEFT_PAREN)
(define RIGHT_PAREN 'RIGHT_PAREN)
(define LEFT_BRACE 'LEFT_BRACE)
(define RIGHT_BRACE 'RIGHT_BRACE)
(define COMMA 'COMMA)
(define DOT 'DOT)
(define MINUS 'MINUS)
(define PLUS 'PLUS)
(define SEMICOLON 'SEMICOLON)
(define SLASH 'SLASH)
(define STAR 'STAR)
(define BANG 'BANG)
(define BANG_EQUAL 'BANG_EQUAL)
(define EQUAL 'EQUAL)
(define EQUAL_EQUAL 'EQUAL_EQUAL)
(define GREATER 'GREATER)
(define GREATER_EQUAL 'GREATER_EQUAL)
(define LESS 'LESS)
(define LESS_EQUAL 'LESS_EQUAL)
(define IDENTIFIER 'IDENTIFIER)
(define STRING 'STRING)
(define NUMBER 'NUMBER)
(define AND 'AND)
(define CLASS 'CLASS)
(define ELSE 'ELSE)
(define FALSE 'FALSE)
(define FUN 'FUN)
(define FOR 'FOR)
(define IF 'IF)
(define NIL 'NIL)
(define OR 'OR)
(define PRINT 'PRINT)
(define RETURN 'RETURN)
(define SUPER 'SUPER)
(define THIS 'THIS)
(define TRUE 'TRUE)
(define VAR 'VAR)
(define WHILE 'WHILE)
(define EOF 'EOF)
