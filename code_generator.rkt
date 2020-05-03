#lang racket
(require "parser.rkt")
(require "typechecker.rkt")

(define (generate_assignment_statement name exp)
  (cond
    [(or (Additive_Expression? exp) (Multiplicative_Expression? exp)) (string-join '(name "is" (generate_additive_or_multiplicative_expression exp) "\n"))]
    [(Integer_Expression? exp) (string-join '(name "is" (generate_expression exp)))]
    [else (string-join '(name "=" (generate_expression exp)))]))

(define (generate_additive_or_multiplicative_expression exp)
  (if (Additive_Expression? exp)
      (string-join '((generate_expression (ParseResult-result (Additive_Expression-primary1 exp))) (ParseResult-result (Additive_Expression-operand exp)) (generate_expression (ParseResult-result (Additive_Expression-primary2 exp)))))
      (string-join '((generate_expression (ParseResult-result (Multiplicative_Expression-primary1 exp))) (ParseResult-result (Multiplicative_Expression-operand exp)) (generate_expression (ParseResult-result (Multiplicative_Expression-primary2 exp)))))))

(define (generate_expression exp)
  (cond
    [(Integer_Expression? exp) (Integer_Expression-value exp)]
    [(String_Expression? exp) (string-join '(#\" (String_Expression-value exp) #\"))]
    [(Boolean_Expression? exp) (Boolean_Expression-value exp)]
    [(Variable_Expression? exp) (Variable_Expression-value exp)]
    [(Boolean_Operation_Expression? exp) (string-join '((generate_expression (ParseResult-result (Boolean_Operation_Expression-primary1))) (ParseResult-result (Boolean_Operation_Expression-operand exp)) (generate_expression (ParseResult-result (Boolean_Operation_expression-primary2 exp)))))]
    [(or (Additive_Expression? exp) (Multiplicative_Expression? exp)) (generate_additive_or_multiplicative_expression exp)]
    [(Call_Expression? exp) (string-join '((generate_expression (ParseResult-result (Call_Expression-identifier exp))) (grab_call_arguments (ParseResult-result (Call_Expression-arguments exp)) "(")))]
    [else (error "temporary")]))


(define (grab_call_arguments arguments collection)
  (if (null? arguments)
      (string-append collection (list ")"))
      (grab_call_arguments (rest arguments) (string-append collection (list (generate_expression (first (first arguments))))))))