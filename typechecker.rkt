#lang racket
(require "parser.rkt")

(struct Int_Type ([hash_code #:auto])
  #:auto-value 0)

(struct String_Type ([hash_code #:auto])
  #:auto-value 1)

(struct Bool_Type ([hash_code #:auto])
  #:auto-value 2)

(define gamma (make-hash))

(define (type_of gamma exp)
  (cond
    [(Integer_Expression? exp) (Int_Type)]
    [(String_Expression? exp) (String_Type)]
    [(Boolean_Expression? exp) (Bool_Type)]
    [(Variable_Expression? exp) (if (hash-has-key? gamma (Variable_Expression-value exp)) (hash-ref (Variable_Expression-value exp)) (error "Variable " (Variable_Expression-value exp) " is out of scope."))]
    [(Additive_Expression? exp) (if (and (Int_Type? (type_of gamma (Additive_Expression-primary1 exp))) (Int_Type? (type_of gamma (Additive_Expression-primary2 exp)))) (Int_Type) (error "one or more expressions is not an int in an additive expression"))]
    [(Multiplicative_Expression? exp) (if (and (Int_Type? (type_of gamma (Multiplicative_Expression-primary1 exp))) (Int_Type? (type_of gamma (Multiplicative_Expression-primary2 exp)))) (Int_Type) (error "one or more expressions is not an int in a multiplicative expression"))]
    [(Boolean_Operation_Expression? exp) (if (and (Bool_Type? (type_of gamma (Boolean_Operation_Expression-primary1 exp))) (Bool_Type? (type_of gamma (Boolean_Operation_Expression-primary2 exp)))) (Bool_Type) (error "one or more expressions is not a boolean in a boolean operation"))]
    [else (error "unrecognized expression")]))