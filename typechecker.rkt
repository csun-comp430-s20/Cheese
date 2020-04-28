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
    [(Additive_Expression? exp) (if (and (Int_Type? (type_of gamma (ParseResult-result (Additive_Expression-primary1 exp)))) (Int_Type? (type_of gamma (ParseResult-result (Additive_Expression-primary2 exp))))) (Int_Type) (error "one or more expressions is not an int in an additive expression"))]
    [(Multiplicative_Expression? exp) (if (and (Int_Type? (type_of gamma (ParseResult-result (Multiplicative_Expression-primary1 exp)))) (Int_Type? (type_of gamma (ParseResult-result (Multiplicative_Expression-primary2 exp))))) (Int_Type) (error "one or more expressions is not an int in a multiplicative expression"))]
    [(Boolean_Operation_Expression? exp)
     (let ([exp1 (type_of gamma (ParseResult-result (Boolean_Operation_Expression-primary1 exp)))] [exp2 (type_of gamma (ParseResult-result (Boolean_Operation_Expression-primary2 exp)))])
       (cond
         [(and (Bool_Type? exp1) (Bool_Type? exp2)) (Bool_Type)]
         [(and (Int_Type? exp1) (Int_Type? exp2)) (Bool_Type)]
         [else (error "one or more expressions is not a boolean in a boolean operation")]))]
    [(Assignment_Statement? exp)
     (let ([tau (determin_type_of (ParseResult-result (Assignment_Statement-type exp)))] [name (ParseResult-result (Assignment_Statement-name exp))] [e (type_of gamma (ParseResult-result (Assignment_Statement-exp exp)))])
       (if (equal? (object-name tau) (object-name e)) (hash-set! gamma (name tau)) (error "Type " tau " cannot be converted to " e)))]
    [else (error "unrecognized expression")]))







(define (determine_type_of tau)
  (cond
    [(equal? tau "int") (Int_Type)]
    [(equal? tau "String") (String_Type)]
    [(equal? tau "boolean") (Bool_Type)]
    [else (error "unrecognized type: " tau)]))