#lang racket
(require "parser.rkt")
(require "typechecker.rkt")

(define (generate_assignment_statement name exp)
  (cond
    [(or (Additive_Expression? exp) (Multiplicative_Expression? exp)) (string-join '(name "is" (generate_additive_or_multiplicative_expression exp) "\n"))]
    [else (string-join '(name "=" (generate_expression exp)))]))

(define (generate_additive_or_multiplicative_expression exp)
  (if (Additive_Expression? exp)
      (string-join '((generate_expression (ParseResult-result (Additive_Expression-primary1 exp))) (ParseResult-result (Additive_Expression-operand exp)) (generate_expression (ParseResult-result (Additive_Expression-primary2 exp)))))
      (string-join '((generate_expression (ParseResult-result (Multiplicative_Expression-primary1 exp))) (ParseResult-result (Multiplicative_Expression-operand exp)) (generate_expression (ParseResult-result (Multiplicative_Expression-primary2 exp)))))))

      