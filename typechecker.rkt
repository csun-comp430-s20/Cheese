#lang racket
(require "parser.rkt")

(struct Int_Type ([hash_code #:auto])
  #:auto-value 0)

(struct String_Type ([hash_code #:auto])
  #:auto-value 1)

(struct Bool_type ([hash_code #:auto])
  #:auto-value 2)

(define gamma (make-hash))

(define (type_of gamma exp)
  (cond
    [(Integer_Expression? exp) (Int_Type)]
    [(String_Expression? exp) (String_Type)]
    [(Boolean_Expression? exp) (Boolean_Type)]
    [(Variable_Expression? exp) (if (hash-has-key? gamma (Variable_Expression-value exp)) (hash-ref (Variable_Expression-value exp)) (error "Variable " (Variable_Expression-value exp) " is out of scope."))]
    [else (error "unrecognized expression")]))